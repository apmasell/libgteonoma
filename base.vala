// vim: set ts=2 sw=2 tw=0 :
/**
 * Include source file location information in parsed objects.
 *
 * If the source location from where an object was parsed is desired, a parsed
 * object should implement this interface. When constructed, the {@link source}
 * property will be filled.
 */
public interface GTeonoma.SourceInfo {
	/**
	 * The location in the source file where this object was parsed.
	 */
	public abstract source_location source {
		get;
		set;
	}
}

/**
 * The result of an attempt to parse a particular value.
 */
public enum GTeonoma.Result {
	/**
	 * Successful parsing.
	 */
	OK,
	/**
	 * Could not parse the value, but was not commited.
	 */
	FAIL,
	/**
	 * Failed to parse the value and was commited.
	 *
	 * Once commited, the parse is sure that is along the right path. When
	 * aborting, the parser will not attempt to recover.
	 */
	ABORT,
	/**
	 * Parsing failed due to reaching the end of the input stream in one case.
	 */
	EOI
}

/**
 * A location in an input stream.
 */
public struct GTeonoma.source_location {
	/**
	 * Some human-readable text describing the context.
	 */
	string source;
	/**
	 * The current line.
	 */
	int line;
	/**
	 * The Unicode character offset on the current line.
	 */
	int offset;
}

/**
 * A marked location in the input stream during parsing.
 *
 * Marks allow rewinding the parse stream during back-tracking.
 */
internal struct GTeonoma.mark {
	public mark (long index = 0, int lines = 1, int offset = 0, unichar last = Parser.NO_CHAR, string[]? errors = null) {
		this.index = index;
		this.lines = lines;
		this.offset = offset;
		this.last = last;
		this.last_space = true;
		this.this_space = true;
	}
	/**
	 * The position in a stream in whatever way is appropriate to the parser.
	 */
	long index;
	/**
	 * The number of lines seen so far.
	 */
	int lines;
	/**
	 * The offset, in Unicode characters, on the current line.
	 */
	int offset;
	/**
	 * The last character seen.
	 */
	unichar last;

	bool last_space;
	bool this_space;
}

internal class GTeonoma.ErrorRoot : Object {
	private Gee.List<string> errors;
	internal ErrorRoot? parent;
	private ErrorRoot? sibling;
	private source_location source;
	public ErrorRoot (source_location source) {
		errors = new Gee.ArrayList<string> ();
		parent = null;
		sibling = null;
		this.source = source;
	}
	public ErrorRoot.with_parent (source_location source, ErrorRoot parent) {
		this(source);
		this.parent = parent;
	}

	public ErrorRoot.with_sibling (source_location source, ErrorRoot sibling) {
		this(source);
		this.sibling = sibling;
		this.parent = sibling.parent;
	}

	internal void add (string error) {
		errors.add (error);
	}

	internal void clear_sibling () {
		sibling = null;
	}

	internal void visit (Parser.ErrorHandler visitor) {
		for (var er = this; er != null; er = er.sibling) {
			foreach (var str in er.errors) {
				visitor (source, str);
			}
		}
	}
}

/**
 * General object parser
 */
public abstract class GTeonoma.Parser : Object {
	internal const unichar NO_CHAR = (unichar) (-1);
	/**
	 * Indicies where tokens have marked the input stream.
	 */
	private mark[] marks;

	/**
	 * Some kind of label for the source of this data.
	 */
	public string source {
		get;
		protected set;
	}

	/**
	 * The parsing rules for the types known by this parser.
	 */
	private Rules rules;

	/**
	 * All the memorized (aka packratted) parse results.
	 *
	 * There are locations in the parse tree where we have successfully parsed an
	 * object, but the containing rule failed later. We store the parse results
	 * here and if we parse the same rule in the same location, we don't reparse,
	 * we just retrieve from our memory.
	 */
	private MemoryBank memories;

	/**
	 * All of the parse errors for the current context.
	 *
	 * This is more than just a list because the errors we need to store will
	 * depend on whether parse rules commit or succeed later. We want to collect
	 * errors if parsing fails, but we might try another rule and succeed, and so
	 * need to ignore those parse errors.
	 */
	private ErrorRoot errors;

	protected Parser (Rules rules) {
		marks = { mark () };
		this.rules = rules;
		memories = new MemoryBank ();
		errors = new ErrorRoot (get_location ());
	}

	/**
	 * Compare a string, with formatting commands against the current input.
	 *
	 * If parsing fails, the input stream will be rewound to the position before.
	 */
	internal bool check_string (string s, string? display_error = null, out bool end_of_input = null) {
		mark_set ();
		var space = marks[marks.length - 1].last_space ? 1 : 0;
		unichar c;
		for (int i = 0; s.get_next_char (ref i, out c); ) {
			if (c.isspace ()) {
				space += consume_whitespace ();
				if (space == 0) {
					if (display_error != null) {
						push_error (@"Expected whitespace in $(display_error).");
					}
					end_of_input = get (false) == '\0';
					mark_rewind ();
					return false;
				}
				continue;
			} else if (c == '%') {
				assert (s.get_next_char (ref i, out c));
				switch (c) {
				 case '%' :
					 /* Handle like regular character. */
					 break;

				 case 'n' :
				 case ' ' :
				 case '_' :
				 case '-':
					 space += consume_whitespace ();
					 if (c == '_' && space == 0) {
						 if (display_error != null) {
							 push_error (@"Expected whitespace in $(display_error).");
						 }
						 end_of_input = get (false) == '\0';
						 mark_rewind ();
						 return false;
					 }
					 continue;

				 default:
					 continue;
				}
			}
			unichar n = get (true);
			if (n != c) {
				if (display_error != null) {
					push_error (@"Expected `$(c.to_string())' but got `$(n.to_string())' in $(display_error).");
				}
				end_of_input = get (false) == '\0';
				mark_rewind ();
				return false;
			} else {
				space = 0;
			}
		}
		mark_clear ();
		end_of_input = false;
		return true;
	}

	/**
	 * Consume any white space tokens in the input stream and return the number collected.
	 */
	internal int consume_whitespace () {
		unichar c;
		var count = 0;
		while ((c = get (false)) != '\0' && c.isspace ()) {
			get (true);
			count++;
		}
		return count;
	}

	/**
	 * Pull the next character in the input stream.
	 * @param consume If true, the input stream is advanced to the next position.
	 * Otherwise, the next character is examined, but the stream remains
	 * unchanged (i.e., peek).
	 * @return the character or nul if there are no more characters
	 */
	internal new unichar get (bool consume = true) {
		if (marks[marks.length - 1].last == NO_CHAR) {
			unichar c = get_c (ref marks[marks.length - 1].index);
			if (c == '\n' || c == '\r') {
				marks[marks.length - 1].lines++;
				marks[marks.length - 1].offset = 0;
			} else {
				marks[marks.length - 1].offset++;
			}
			marks[marks.length - 1].last_space = marks[marks.length - 1].this_space;
			marks[marks.length - 1].this_space = c.isspace ();
			marks[marks.length - 1].last = consume ? NO_CHAR : c;
			return c;
		} else if (consume) {
			marks[marks.length - 1].last_space = marks[marks.length - 1].this_space;
			unichar c = marks[marks.length - 1].last;
			marks[marks.length - 1].last = NO_CHAR;
			return c;
		} else {
			return marks[marks.length - 1].last;
		}
	}

	/**
	 * Get a character from the underlying stream at the current position.
	 *
	 * Read one character from the next position in the stream. A derived class
	 * should assume that the index is left unchanged from the last call unless
	 * {@link reset} was called.
	 * @param index the last position read in the stream. It should be updated to
	 * the next position in the stream.
	 */
	protected new abstract unichar get_c (ref long index);

	/**
	 * Describe the current location of the input stream.
	 */
	public source_location get_location () {
		return source_location () {
			       source = this.source, line = marks[marks.length - 1].lines, offset = marks[marks.length - 1].offset
		};
	}

	/**
	 * Pull one C-style identifier from the input stream.
	 */
	internal string get_word (out bool end_of_input) {
		var buffer = new StringBuilder ();
		var first = true;
		while (is_identifier (get (false), first)) {
			buffer.append_unichar (get (true));
			first = false;
		}
		end_of_input = get (false) == '\0';
		return buffer.str;
	}

	/**
	 * Checks if there is more data in the input stream.
	 */
	public bool is_finished () {
		consume_whitespace ();
		return get (false) == '\0';
	}

	/**
	 * Check if a charcter is part of a valid identifier.
	 * @param first is this the first character in the identifier?
	 */
	public static bool is_identifier (unichar c, bool first) {
		return c.isalpha () || c == '_' || (!first && c.isdigit ());
	}

	/**
	 * Make a comma-separated list of names for a type.
	 */
	internal string names_for (Type type) {
		var buffer = new StringBuilder ();
		var first = true;
		foreach (var rule in rules[type]) {
			if (rule is FailRule) {
				continue;
			}
			if (first) {
				first = false;
			} else {
				buffer.append (", ");
			}
			buffer.append (rule.name);
		}
		if (first) {
			return type.name ();
		} else {
			return buffer.str;
		}
	}

	/**
	 * Discard the last mark set.
	 * @see mark_set
	 */
	internal void mark_clear () {
		assert (marks.length > 1);
		marks[marks.length - 2] = marks[marks.length - 1];
		marks.length--;
	}

	/**
	 * Mark the current position in the input.
	 *
	 * Mark the current input position by pushing it on a stack. The input stream
	 * can then be rewound to the last marked position using {@link mark_rewind}
	 * or the last mark can be discarded with {@link mark_clear}.
	 */
	internal void mark_set () {
		marks += marks[marks.length - 1];
	}

	/**
	 * Reset the current mark.
	 *
	 * Discard the current mark and set a new one at the current position.
	 */
	internal void mark_reset () {
		marks[marks.length - 2] = marks[marks.length - 1];
	}

	/**
	 * Rewind the input position to the last mark set.
	 *
	 * Reset the position of the input stream to the last position marked with
	 * {@link mark_set} and pop this mark off the internal stack.
	 */
	internal void mark_rewind () {
		assert (marks.length > 1);
		marks.length--;
		reset (marks[marks.length - 1].index);
	}

	/**
	 * Describe the location of the last marked position in the input stream.
	 *
	 * If no location is marked, the beginning of the input stream is described.
	 */
	internal source_location get_marked_location () {
		if (marks.length > 1) {
			return source_location () {
				       source = this.source, line = marks[marks.length - 2].lines, offset = marks[marks.length - 2].offset
			};
		} else {
			return get_location ();
		}
	}

	/**
	 * Parse an object out of the input stream.
	 *
	 * The type must be registered with the {@link Rules} associated with this
	 * parser. There may be more parseable data in the stream. This can be tested
	 * with {@link is_finished}.
	 */
	public Result parse (Type type, out Value @value) {
		return parse_type (type, out @value, 0, 0);
	}

	public signal void attempting_parse (string rule, uint precedence, uint depth, int offset, int lines);
	public signal void finished_parse (Result result, string rule, uint precedence, uint depth, int offset, int lines);
	public signal void cache_hit (Result result, string rule, uint precedence, uint depth, int offset, int lines);
	public signal void start_property (string rule, string property, uint depth);
	public signal void end_property (string rule, string property, uint depth);

	internal Result parse_type (Type type, out Value @value, uint precedence, uint depth) {
		var result = Result.FAIL;
		bool end_of_input = false;
		var old_error = errors;
		errors = new ErrorRoot.with_parent (get_location (), errors);
		int mark_length = marks.length;
		var start_lines = marks[marks.length - 1].lines;
		var start_offset = marks[marks.length - 1].offset;

		@value = Value (type);
		foreach (var rule in rules[type, precedence]) {
			var memory = memories[marks[marks.length - 1].lines, marks[marks.length - 1].offset, rule];
			if (memory == null) {
				mark_set ();
				attempting_parse (rule.name, precedence, depth, start_offset, start_lines);
				result = rule.parse (this, out @value, depth);

				memory = new Memory ();
				memory.value = @value;
				memory.post_mark = marks[marks.length - 1];
				memory.rule = rule;
				memory.result = result;
				memories[start_lines, start_offset] = memory;
				finished_parse (result, rule.name, precedence, depth, start_offset, start_lines);
				if (result == Result.OK || result == Result.ABORT) {
					mark_clear ();
					break;
				} else {
					errors = new ErrorRoot.with_sibling (get_location (), errors);
					mark_rewind ();
				}
			} else {
				cache_hit (memory.result, memory.rule.name, precedence, depth, start_offset, start_lines);
				@value = memory.@value;
				result = memory.result;
				if (result == Result.OK || result == Result.ABORT) {
					marks[marks.length - 1] = memory.post_mark;
					reset (marks[marks.length - 1].index);
					break;
				}
			}
			if (result == Result.EOI) {
				end_of_input = true;
			}
		}
		if (result == Result.OK) {
			errors = old_error;
		} else if (result == Result.ABORT) {
			errors.clear_sibling ();
		}

		assert (mark_length == marks.length);
		return (result == Result.FAIL && end_of_input) ? Result.EOI : result;
	}

	/**
	 * Parse a stream of objects from the parse stream.
	 *
	 * @param separator the format string used in between objects
	 */
	public Result parse_all<T> (out Gee.List<T> list, string separator = "%n") {
		list = new Gee.ArrayList<T> ();
		while (!is_finished ()) {
			Value @value;
			var result = parse (typeof (T), out @value);
			if (result == Result.OK) {
				list.add (@value.get_object ());
			} else {
				return result;
			}
			if (!check_string (separator) && !is_finished ()) {
				return Result.FAIL;
			}
		}
		return Result.OK;
	}

	/**
	 * Note a parse error at the current position in the input stream.
	 */
	internal void push_error (string error) {
		errors.add (error);
	}

	/**
	 * Called when ever the stream position needs to be reset.
	 *
	 * Readjust the input stream such that the next {@link get} will occur at the
	 * correct place.
	 */
	protected abstract void reset (long index);

	/**
	 * Examine all current parse errors.
	 */
	public void visit_errors (ErrorHandler handler) {
		errors.visit (handler);
	}

	/**
	 * Visit an error message in the current parser state.
	 */
	public delegate void ErrorHandler (source_location source, string error);
}

internal class GTeonoma.Memory : Object {
	internal Value @value {
		get;
		set;
	}
	internal mark post_mark {
		get;
		set;
	}
	internal Rule rule {
		get;
		set;
	}
	internal Result result {
		get; set;
	}
}

internal class GTeonoma.MemoryBank : Object {
	private Gee.TreeMap<int, Gee.TreeMap<int, Gee.List<Memory> > > memories;
	internal MemoryBank () {
		memories = new Gee.TreeMap<int, Gee.TreeMap<int, Gee.List<Memory> > > ();
	}

	internal new Memory? get (int lines, int offset, Rule rule) {
		if (!memories.has_key (lines)) {
			return null;
		}
		var line = memories[lines];
		if (!line.has_key (offset)) {
			return null;
		}
		foreach (var memory in line[offset]) {
			if (memory.rule == rule) {
				return memory;
			}
		}
		return null;
	}
	internal new void set (int lines, int offset, Memory memory) {
		Gee.TreeMap<int, Gee.List<Memory> > line;
		if (memories.has_key (lines)) {
			line = memories[lines];
		} else {
			line = new Gee.TreeMap<int, Gee.List<Memory> > ();
			memories[lines] = line;
		}
		Gee.List<Memory> list;
		if (line.has_key (offset)) {
			list = line[offset];
		} else {
			list = new Gee.ArrayList<Memory> ();
			line[offset] = list;
		}
		list.add (memory);
	}
}

/**
 * Parse data in a string.
 */
public class GTeonoma.StringParser : Parser {
	/**
	 * The string being parsed.
	 */
	private string data;

	/**
	 * Create a new parsing state over a string.
	 *
	 * @param rules the parsing grammar to use
	 * @param str the string to parse
	 * @param name the source name to use
	 */
	public StringParser (Rules rules, string str, string name = "string") {
		base (rules);
		data = str;
		source = name;
	}

	protected override unichar get_c (ref long index) {
		int idx = (int) index;
		if (idx >= data.length) {
			return '\0';
		}
		unichar c;
		index = data.get_next_char (ref idx, out c) ? idx : data.length;
		return c;
	}

	protected override void reset (long index) {}
}

/**
 * Parse data in a file.
 */
public class GTeonoma.FileParser : Parser {
	private FileStream file;

	private FileParser (Rules rules, owned FileStream file, string filename) {
		base (rules);
		this.file = (owned) file;
		source = filename;
	}
	/**
	 * Open an underlying file for reading and establish a parser.
	 *
	 * This will create a new parsing state to parse the contents of a file.
	 *
	 * @param rules the grammar rules to be used
	 * @param filename the path to the file to be parsed
	 * @return the new parser, or null if the file cannot be opened
	 */
	public static FileParser? open (Rules rules, string filename) {
		var file = FileStream.open (filename, "r");
		if (file == null) {
			return null;
		}
		return new FileParser (rules, (owned) file, filename);
	}

	protected override unichar get_c (ref long index) {
		uint8[] chars = {};
		while (chars.length < 7 && (chars.length == 0 || !((string) chars).validate (chars.length))) {
			int val;
			if ((val = file.getc ()) == FileStream.EOF) {
				return '\0';
			}
			chars += (uint8) val;
		}
		index = file.tell ();
		var c = ((string) chars).get_char_validated (chars.length);
		return c == NO_CHAR ? '\0' : c;
	}

	protected override void reset (long index) {
		file.seek (index, FileSeek.SET);
	}
}

/**
 * Pretty printer for parsed object tree.
 */
public abstract class GTeonoma.Printer : Object {
	/**
	 * The current level of indentation for text on new lines.
	 */
	private int indent;
	/**
	 * The rule set for the objects to be printed.
	 */
	private Rules rules;
	/**
	 * Was the last character printed a space?
	 */
	private bool last_space;

	protected Printer (Rules rules) {
		this.rules = rules;
	}

	/**
	 * Write a formatted string to the output.
	 *
	 * This supports the format modifiers.
	 */
	internal void append (string str) {
		unichar c;
		for (var i = 0; str.get_next_char (ref i, out c); ) {
			if (c == '\n') {
				append_c ('\n');
				for (var count = 0; count < indent; count++) {
					append_c ('\t');
				}
				last_space = true;
			} else if (c == '%') {
				str.get_next_char (ref i, out c);
				switch (c) {
				 case 'I' :
					 indent++;
					 break;

				 case 'i' :
					 indent--;
					 break;

				 case '%':
					 append_c ('%');
					 last_space = false;
					 break;

				 case 'n':
					 append_c ('\n');
					 for (var count = 0; count < indent; count++) {
						 append_c ('\t');
					 }
					 last_space = true;
					 break;

				 case '-':
					 append_c (' ');
					 last_space = true;
					 break;

				 case '_':
					 if (!last_space) {
						 append_c (' ');
						 last_space = true;
					 }
					 break;

				 case ' ':
					 break;

				 default:
					 assert_not_reached ();
				}
			} else {
				append_c (c);
				last_space = c.isspace ();
			}
		}
	}

	/**
	 * Write a Unicode character to output.
	 *
	 * To extend this class, this method will be called for every character to be
	 * written to output. Nothing else need be done.
	 */
	protected abstract void append_c (unichar c);

	/**
	 * Write a typed object to the output stream.
	 */
	public void print (Value @value) {
		rules[@value.type ()].first ().print (this, @value);
	}

	/**
	 * Write an object to the output stream, determining type automatically.
	 */
	public void print_obj (Object item) {
		Value @value = Value (item.get_type ());
		@value.set_object (item);
		print (@value);
	}

	/**
	 * Write a list of objects to the output stream.
	 */
	public void print_all (Gee.List<Object> items, string separator = "%n") {
		var first = true;
		foreach (var item in items) {
			if (first) {
				first = false;
			} else {
				append (separator);
			}
			print_obj (item);
		}
	}
}

/**
 * Pretty-print output to standard output
 */
public class GTeonoma.ConsolePrinter : Printer {
	public ConsolePrinter (Rules rules) {
		base (rules);
	}

	internal override void append_c (unichar c) {
		stdout.puts (c.to_string ());
	}
}

/**
 * Pretty-print output to a file
 */
public class GTeonoma.FilePrinter : Printer {
	private FileStream stream;
	private FilePrinter (Rules rules, owned FileStream stream) {
		base (rules);
		this.stream = (owned) stream;
	}

	public static FilePrinter? open (Rules rules, string filename) {
		var stream = FileStream.open (filename, "w");
		if (stream == null) {
			return null;
		}
		return new FilePrinter (rules, (owned) stream);
	}

	internal override void append_c (unichar c) {
		stream.puts (c.to_string ());
	}
}

/**
 * Pretty-print output to a string
 */
public class GTeonoma.StringPrinter : Printer {
	private StringBuilder buffer;

	/**
	 * The output string.
	 */
	public string str {
		get {
			return buffer.str;
		}
	}

	public StringPrinter (Rules rules) {
		base (rules);
		buffer = new StringBuilder ();
	}

	internal override void append_c (unichar c) {
		buffer.append_unichar (c);
	}
}
