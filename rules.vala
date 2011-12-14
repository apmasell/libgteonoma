// vim: set ts=2 sw=2 tw=0 :
/**
 * Template for a rule understood by the parser
 *
 * Rules convert parts of the parse stream in to a {@link Value}.
 */
internal abstract class GTeonoma.Rule : Object {
	internal string name { get; protected set; }
	/**
	 * Parse a particular value from the parse stream.
	 */
	internal abstract Result parse(Parser p, out Value ret_value);
	/**
	 * Pretty-print the parsed value.
	 */
	internal abstract void print(Printer p, Value @value);
}

/**
 * The kinds of actions used by {@link ObjectRule}.
 */
internal enum GTeonoma.Token {
	/**
	 * A boolean property controls the presence or absence of a keyword.
	 */
	BOOL,
	/**
	 * Stops the parser from back-tracking (i.e., commits it to the current path).
	 */
	COMMIT,
	/**
	 * A {@link Gee.List} property containing other objects.
	 *
	 * Although the generics in Vala allow passing of structs, this is not
	 * possible with the parser; they must be a list of objects extending
	 * {@link Object}.
	 */
	LIST,
	/**
	 * A property containing an object, number or string.
	 */
	PROPERTY,
	/**
	 * Some literal text to be parsed.
	 */
	SYMBOL
}

/**
 * A unit of parsing to be done for an object.
 */
internal struct GTeonoma.chunk {
	chunk(Token token, bool optional = false, string? property = null, string? word = null, Type type = Type.INVALID) {
		this.token = token;
		this.optional = optional;
		this.property = property;
		this.word = word;
		this.type = type;
	}
	/**
	 * The kind of parse action needed.
	 */
	Token token;
	/**
	 * The name of the property to be manipulated, if any.
	 */
	string? property;
	/**
	 * The symbol to be parsed, if any.
	 */
	string? word;
	/**
	 * Whether this action can be skipped.
	 */
	bool optional;
	/**
	 * The type relevant to this action.
	 */
	Type type;
#if DEBUG
	/**
	 * Format this chunk back into something approximating the string that created it.
	 */
	internal string to_string() {
		switch (token) {
			case Token.BOOL:
				return @"%b{$(property)}{$(word)}";
			case Token.COMMIT:
				return "%%!";
			case Token.LIST:
				return @"%$(optional ? 'l' : 'L'){$(property)::$(type.name())}{$(word)}";
			case Token.PROPERTY:
				return @"%$(optional ? 'p' : 'P'){$(property)}";
			case Token.SYMBOL:
				return word;
		}
		assert_not_reached();
	}
#endif
}

/**
 * Internal state machine for parsing the strings to {@link GTeonoma.Rules.register}
 */
internal enum GTeonoma.ObjectParseState {
	COMMAND,
	PARSE_BOOL,
	PARSE_LIST,
	PROPERTY_BOOL,
	PROPERTY_LIST,
	PROPERTY_PROPERTY,
	START_BOOL,
	START_LIST,
	START_PARSE_BOOL,
	START_PARSE_LIST,
	START_PROPERTY,
	TEXT
}

/**
 * A collection of parse rules
 *
 * This is where the parser and pretty printer will look up rules for the
 * corresponding types.
 */
public class GTeonoma.Rules : Object {
	private Gee.HashMap<Type, Gee.List<Rule>> rules;

	public Rules() {
		rules = new Gee.HashMap<Type, Gee.List<Rule>>();
	}

	/**
	 * Retrieve the correct rule for a particular type
	 */
	internal new Gee.List<Rule> get(Type type) {
		if (rules.has_key(type)) {
			return rules[type].read_only_view;
		} else {
			var list = new Gee.ArrayList<Rule>();
			list.add(new FailRule(type));
			rules[type] = list;
			return list.read_only_view;
		}
	}

	/**
	 * Add a new rule to parse an object, flags, or enum.
	 *
	 * When adding a flags or enum, the nicks of the various values will be
	 * parsed from the input stream. For flags, the flags can appear in any order
	 * and multiple times in the input stream.
	 *
	 * For an object, a format string describing how to parse and display the
	 * object is needed. Any characters in the format string are taken literally.
	 * Whitespace is considered, but the amount and type of white space is
	 * ignored.<<BR>> ''%%'' is used to indicate a percent sign.<<BR>> ''%n''
	 * causes a new line to be emitted, but no white space is required when
	 * parsing.<<BR>> ''%''(space) emits no space, but allows white space while
	 * parsing.<<BR>> ''%_'' emits space only if the last character was not white
	 * space.<<BR>> ''%-'' does not require space, but emits it when printing.
	 * ''%I'' and ''%i'' increase and decrease the indentation of subsequent
	 * lines, respectively.<<BR>> ''%p{''//name//''}'' parses a property
	 * //name//; using ''p'' versus ''P'' causes the parsing to be optional
	 * versus mandatory.<<BR>> ''%b{''//name//''}{''//text//''}'' causes a
	 * boolean property //name// to be set to true if //text// can be parsed from
	 * the input stream.<<BR>> ''%l{''//name//''}{''//text//''}'' cause a list of
	 * items, separated by //text// to be parsed an placed into a property
	 * //name// of type {@link Gee.List}. Using ''l'' versus ''L'' controls
	 * whether the list may be empty or must be non-empty. A type argument must
	 * be provided after the format string for the contents of the list; this
	 * type must extend {@link GLib.Object}.<<BR>> ''%!'' causes the parser to
	 * commit to this course of parsing; the parser will not back-track after
	 * committing.
	 *
	 * @param name the pretty name for this type when errors are printed
	 *
	 * @param format the format string if the type is an object; null otherwise.
	 */
	public void register<T>(string? name = null, string? format = null, Type[]? types = null) throws RegisterError {
		var type = typeof(T);

		if (type.is_enum()) {
			if (format != null) {
				throw new RegisterError.UNNECESSARY_FORMAT(@"Enumeration $(type.name()) does not require a format string.");
			}
			this[type] = new EnumRule(name, type);
			return;
		}
		if (type.is_flags()) {
			if (format != null) {
				throw new RegisterError.UNNECESSARY_FORMAT(@"Flags $(type.name()) does not require a format string.");
			}
			this[type] = new FlagsRule(name, type);
		}

		if (!type.is_a(typeof(Object))) {
			throw new RegisterError.BAD_TYPE(@"Type $(type.name()) is not a flag, enum, or subclass of Object.");
		}
		if (format == null || format.length == 0) {
			throw new RegisterError.BAD_FORMAT(@"Format string missing for $(type.name()).\n");
		}

		var index = 0;
		var obj_class = (ObjectClass)type.class_ref();
		assert(obj_class != null);
		chunk[] chunks = {};
		var buffer = new StringBuilder();
		var state = ObjectParseState.TEXT;
		string? prop_name = null;
		var optional = false;
		var left_recursion = true;
		var first = false;
		unichar c;
		for (int i = 0; format.get_next_char (ref i, out c);) {
			switch (state) {
				case ObjectParseState.TEXT:
					switch (c) {
						case '%':
							state = ObjectParseState.COMMAND;
							break;
						default:
							left_recursion &= c.isspace();
							buffer.append_unichar(c);
							break;
					}
					break;
			case ObjectParseState.COMMAND:
				if (c == ' ' || c == 'i' || c == 'I' || c == '%' || c == 'n' || c == '_' || c == '-') {
					left_recursion &= c != '%';
					buffer.append_c('%');
					buffer.append_unichar(c);
					state = ObjectParseState.TEXT;
					continue;
				}
				if (buffer.len > 0) {
					chunks += chunk(Token.SYMBOL, false, null, buffer.str);
					buffer.truncate();
				}
				optional = c.islower();
				switch (c) {
					case '!':
						chunks += chunk (Token.COMMIT);
						state = ObjectParseState.TEXT;
						break;
					case 'b':
						state = ObjectParseState.START_BOOL;
						break;
					case 'l':
					case 'L':
						state = ObjectParseState.START_LIST;
						break;
					case 'p':
					case 'P':
						state = ObjectParseState.START_PROPERTY;
						break;
					default:
						throw new RegisterError.BAD_FORMAT(@"Unrecognized command $(c) in format string for $(type.name()).\n");
				}
				break;
			case ObjectParseState.START_BOOL:
			case ObjectParseState.START_LIST:
			case ObjectParseState.START_PROPERTY:
			case ObjectParseState.START_PARSE_BOOL:
			case ObjectParseState.START_PARSE_LIST:
					if(c != '{') {
						throw new RegisterError.BAD_FORMAT(@"Expected { in format string for $(type.name()).\n");
					}
				first = true;
				switch (state) {
					case ObjectParseState.START_BOOL:
						state = ObjectParseState.PROPERTY_BOOL;
						break;
					case ObjectParseState.START_LIST:
						state = ObjectParseState.PROPERTY_LIST;
						break;
					case ObjectParseState.START_PROPERTY:
						state = ObjectParseState.PROPERTY_PROPERTY;
						break;
					case ObjectParseState.START_PARSE_BOOL:
						state = ObjectParseState.PARSE_BOOL;
						break;
					case ObjectParseState.START_PARSE_LIST:
						state = ObjectParseState.PARSE_LIST;
						break;
				}
				break;
			case ObjectParseState.PROPERTY_BOOL:
			case ObjectParseState.PROPERTY_LIST:
			case ObjectParseState.PROPERTY_PROPERTY:
				if (c == '}') {
					prop_name = buffer.str;
					var prop = obj_class.find_property(prop_name);
					if(prop == null) {
						throw new RegisterError.MISSING_PROPERTY(@"Property $(prop_name) is not found in $(type.name()).\n");
					}
					buffer.truncate();
					switch (state) {
						case ObjectParseState.PROPERTY_BOOL:
							state = ObjectParseState.START_PARSE_BOOL;
							break;
						case ObjectParseState.PROPERTY_LIST:
							state = ObjectParseState.START_PARSE_LIST;
							break;
						case ObjectParseState.PROPERTY_PROPERTY:
							if (left_recursion) {
								if (type.is_a(prop.value_type)) {
									throw new RegisterError.LEFT_RECURSION(@"Left-deep recursion detected for list $(prop_name) in $(type.name()).\n");
								} else {
									left_recursion &= !optional;
								}
							}
							chunks += chunk(Token.PROPERTY, optional, prop_name);
							prop_name = null;
							state = ObjectParseState.TEXT;
							break;
					}
				} else if (Parser.is_identifier(c, first)) {
					buffer.append_unichar(c);
					first = false;
				} else {
					if(obj_class.find_property(prop_name) == null) {
						throw new RegisterError.BAD_FORMAT(@"Unexpected $(c) in format string for $(type.name()).\n");
					}
				}
				break;
			case ObjectParseState.PARSE_BOOL:
			case ObjectParseState.PARSE_LIST:
				if (c == '}') {
					var keyword = buffer.str;
					buffer.truncate();
					switch (state) {
						case ObjectParseState.PARSE_BOOL:
							if(obj_class.find_property(prop_name) == null) {
								throw new RegisterError.MISSING_PROPERTY(@"Property $(prop_name) is not found in $(type.name()).\n");
							}
							chunks += chunk(Token.BOOL, false, prop_name, keyword);
							break;
						case ObjectParseState.PARSE_LIST:
							if(obj_class.find_property(prop_name) == null) {
								throw new RegisterError.MISSING_PROPERTY(@"Property $(prop_name) is not found in $(type.name()).\n");
							}
							if (types == null || index >= types.length) {
								throw new RegisterError.MISSING_TYPES(@"Missing generic type information for list $(prop_name) in $(type.name()).\n");
							}
							if (left_recursion) {
								if (type.is_a(types[index])) {
									throw new RegisterError.LEFT_RECURSION(@"Left-deep recursion detected for list $(prop_name) in $(type.name()).\n");
								} else {
									left_recursion &= !optional;
								}
							}
							chunks += chunk(Token.LIST, optional, prop_name, keyword, types[index++]);
							break;
						}
						state = ObjectParseState.TEXT;
				} else {
					buffer.append_unichar(c);
				}
				break;
			}
		}
		if (state != ObjectParseState.TEXT) {
			throw new RegisterError.BAD_FORMAT(@"Unexpected end of format string for $(type.name()).\n");
		}
		if (buffer.len > 0) {
			chunks += chunk(Token.SYMBOL, false, null, buffer.str);
			buffer.truncate();
		}
		if (chunks.length == 0) {
			throw new RegisterError.BAD_FORMAT(@"Empty format string for $(type.name()).\n");
		}
		var rule = new ObjectRule(name, type, chunks);
		this[type] = rule;
	}

	/**
	 * Add support for C string literals using {@link StringLiteral}.
	 */
	public void register_string_literal() {
		register_custom<StringLiteral>("string", () => { return new StringLiteralParser(); }, (literal) => { return @"\"$(literal.str.escape(""))\"";});
	}

	/**
	 * Add support for C/Vala-ish identifers using {@link Identifier}.
	 *
	 * @param allow_unicode controls of Unicode characters are allowed in an identifier (similar to Java).
	 * @param allow_at permits an ''\@'' character to preceed an identifier like Vala and C#.
	 */
	public void register_identifier(bool allow_unicode = false, bool allow_at = false) {
		register_custom<Identifier>("identifier", () => { return new IdentifierParser(allow_unicode, allow_at); }, (identifier) => { return identifier.name;});
	}

	/**
	 * Register a new signed integer type.
	 * @param size in bits (8, 32, or 64)
	 */
	public void register_int(int size, Type type) throws RegisterError requires (type.is_fundamental()) {
		switch (size) {
			case 8:
				this[type] = new Int8Rule(type);
				break;
			case 32:
				this[type] = new Int32Rule(type);
				break;
			case 64:
				this[type] = new Int64Rule(type);
				break;
			default:
				throw new RegisterError.BAD_TYPE("Unsupported size.");
		}
	}

	/**
	 * Register a new unsigned integer type.
	 * @param size in bits (8, 32, or 64)
	 */
	public void register_uint(int size, Type type) throws RegisterError requires (type.is_fundamental()) {
		switch (size) {
			case 8:
				this[type] = new UInt8Rule(type);
				break;
			case 32:
				this[type] = new UInt32Rule(type);
				break;
			case 64:
				this[type] = new UInt64Rule(type);
				break;
			default:
				throw new RegisterError.BAD_TYPE("Unsupported size.");
		}
	}

	/**
	 * Register a new custom rule.
	 *
	 * To parse a truly unsual object, for instance a string literal or a regular
	 * expression literal, extend {@link CustomParser} and then add the rule using
	 * this method.
	 *
	 * @param name a friendly name to be printed during compilation
	 *
	 * @param parser_factory a delegate that creates new instances of the parser.
	 * Everytime an object must be parsed, this method will be called.
	 *
	 * @param stringifier a delegate that will convert a previously parsed object
	 * to a string representation
	 */
	public void register_custom<T>(string name, owned CustomParser.CustomParserFactory<T> parser_factory, owned CustomParser.StringifyObject<T> stringifier) {
		this[typeof(T)] = new CustomRule<T>(name, (owned) parser_factory, (owned) stringifier);
	}

	/**
	 * Register a rule for a specified type and all of its ancestral types.
	 */
	internal new void set(Type type, Rule rule) {
		var interfaces = type.interfaces();
		if (interfaces != null) {
			foreach (var interface_type in interfaces) {
				set_type(interface_type, rule);
			}
		}
		for(; type != Type.INVALID; type = type.parent()) {
			set_type(type, rule);
		}
	}

	private void set_type(Type type, Rule rule) {
		Gee.List<Rule> list;
		if (rules.has_key(type)) {
			list = rules[type];
		} else {
			list = new Gee.ArrayList<Rule>();
			rules[type] = list;
		}
		list.add(rule);
	}
}

/**
 * Errors raised while adding new types to {@link Rules}
 */
public errordomain GTeonoma.RegisterError {
	/**
	 * Format string supplied when no required.
	 */
	UNNECESSARY_FORMAT,
	/**
	 * Error informat string.
	 */
	BAD_FORMAT,
	/**
	 * Unsupported type.
	 */
	BAD_TYPE,
	/**
	 * Missing supplementary type information.
	 */
	MISSING_TYPES,
	/**
	 * Left deep-recursion detected.
	 *
	 * Parsing expression grammars cannot handle non-syntax directed left-deep
	 * recursion (i.e., ''S = S x'' is invalid, but ''S = "[" S x "]"'' is
	 * valid).
	 */
	LEFT_RECURSION,
	/**
	 * Non-existent property reference.
	 */
	MISSING_PROPERTY
}
/**
 * A rule that causes failure
 *
 * This rule is automatically invoked for invalid types.
 */
internal class GTeonoma.FailRule : Rule {
	string error;
	public FailRule(Type type) {
		name = "failure";
		error = @"Internal error: No rule for type `$(type.name())'.";
	}

	internal override Result parse(Parser p, out Value @value) {
		@value = Value(typeof(Object));
		p.push_error(error);
		return Result.ABORT;
	}

	internal override void print(Printer p, Value @value) {
		p.append("\n");
		p.append(error);
		p.append("\n");
	}
}

/**
 * A rule to parse data into and out of the properties of an object.
 */
internal class GTeonoma.ObjectRule : Rule {
	private Type type;
	private ObjectClass obj_type;
	private chunk[] chunks;

	internal ObjectRule(string name, Type type, chunk[] chunks) {
		this.name = name;
		this.type = type;
		this.obj_type = (ObjectClass)type.class_ref();
		this.chunks = chunks;
	}

	internal override Result parse(Parser p, out Value ret_value) {
		ret_value = Value(type);
		var obj = Object.new(type);
		if (obj is SourceInfo) {
			((SourceInfo)obj).source = p.get_location();
		}
		bool committed = false;
		for(var index = 0; index < chunks.length; index++) {
			switch (chunks[index].token) {
				case Token.BOOL:
					if (p.check_string(chunks[index].word)) {
						var prop_value = Value(typeof(bool));
						prop_value.set_boolean(true);
						obj.set_property(chunks[index].property, prop_value);
					}
					break;
				case Token.COMMIT:
					committed = true;
					continue;
				case Token.PROPERTY:
					Value prop_value;
					p.mark_set();
					var prop = obj_type.find_property(chunks[index].property);
					assert (prop != null);
					var result = p.parse(prop.value_type, out prop_value);
					if (result == Result.OK) {
						obj.set_property(chunks[index].property, prop_value);
						p.mark_clear();
					} else if (chunks[index].optional && result == Result.FAIL) {
						/* Ignore. */
						p.mark_rewind();
					} else {
						p.push_error(@"Expected $(p.names_for(prop.value_type)) missing in $(name).");
						p.mark_clear();
						return result;
					}
					break;
				case Token.LIST:
					var list = new Gee.ArrayList<Object>();
					Value child_value;
					Result result;
					p.mark_set();
					while ((result = p.parse(chunks[index].type, out child_value)) == Result.OK) {
						list.add(child_value.get_object());
						p.mark_reset();
						if (!p.check_string(chunks[index].word))
								break;
					}
					if (result == Result.ABORT) {
						p.mark_clear();
						return result;
					} else if (!chunks[index].optional && list.size == 0) {
						p.push_error(@"Expected $(p.names_for(chunks[index].type)) missing in $(name).");
						p.mark_clear();
						return committed ? Result.ABORT : Result.FAIL;
					} else {
						p.push_error(@"Expected one of $(p.names_for(chunks[index].type)) in $(name).");
						p.mark_rewind();
						Value list_value = Value(typeof(Gee.List));
						list_value.set_object(list);
						obj.set_property(chunks[index].property, list_value);
					}
					break;
				case Token.SYMBOL:
					if (!p.check_string(chunks[index].word, name)) {
						return committed ? Result.ABORT : Result.FAIL;
					}
					break;
			}
		}
		ret_value.set_object(obj);
		return Result.OK;
	}
	Value get_prop(string prop_name, Object obj) {
		Value @value = Value(obj_type.find_property(prop_name).value_type);
		obj.get_property(prop_name, ref @value);
		return @value;
	}

	internal override void print(Printer p, Value @value) requires (@value.type().is_object()) {
		Object obj = @value.get_object();
		assert(obj.get_type().is_a(type));
		for (var index = 0; index < chunks.length; index++) {
			switch (chunks[index].token) {
				case Token.BOOL:
					if (get_prop(chunks[index].property, obj).get_boolean()) {
						p.append(chunks[index].word);
					}
					break;
				case Token.COMMIT:
					break;
				case Token.PROPERTY:
					var prop_value = get_prop(chunks[index].property, obj);
					if (chunks[index].optional && prop_value.type().is_object() && prop_value.get_object() == null)
						continue;
					if (prop_value.type().is_object()) {
						/*
						 * Make our type more specific in the case of alternate rules. Otherwise, we might pick the wrong branch.
						 */
						var prop_obj = prop_value.get_object();
						prop_value = Value(prop_obj.get_type());
						prop_value.set_object(prop_obj);
					}
					p.print(prop_value);
					break;
				case Token.LIST:
					var list = get_prop(chunks[index].property, obj).get_object() as Gee.List<Object>;
					if (list == null) {
						continue;
					}
					var first = true;
					foreach(var child in list) {
						if (first) {
							first = false;
						} else {
							p.append(chunks[index].word);
						}
						var child_value = Value(child.get_type());
						child_value.set_object(child);
						p.print(child_value);
					}
					break;
				case Token.SYMBOL:
					p.append(chunks[index].word);
					break;
			}
		}
	}
}

/**
 * Rule to parse an enum.
 */
internal class GTeonoma.EnumRule : Rule {
	EnumClass enum_class;
	Type type;
	public EnumRule(string? name, Type type) {
		this.name = name ?? type.name();
		this.type = type;
		this.enum_class = (EnumClass)type.class_ref();
	}

	internal override Result parse(Parser p, out Value @value) {
		@value = Value(type);
		var word = p.get_word();
		var enum_value = enum_class.get_value_by_nick(word);
		if (enum_value == null) {
			p.push_error(@"Unexpected symbol `$word'.");
			return Result.FAIL;
		} else {
			@value.set_enum(enum_value.value);
			return Result.OK;
		}
	}

	internal override void print(Printer p, Value @value) requires (@value.type() == type) {
		var enum_value = enum_class.get_value(@value.get_enum());
		assert (enum_value != null);
		p.append(enum_value.value_nick);
	}
}

/**
 * Rule to parse a flags (a.k.a. enum with Flags attribute)
 */
internal class GTeonoma.FlagsRule : Rule {
	private Type type;
	private FlagsClass flags_class;
	public FlagsRule(string? name, Type type) {
		this.name = name ?? type.name();
		this.type = type;
		this.flags_class = (FlagsClass)type.class_ref();
	}

	internal override Result parse(Parser p, out Value @value) {
		@value = Value(type);
		while(true) {
			p.mark_set();
			p.consume_whitespace();
			var word = p.get_word();
			if (word.length == 0) {
				p.mark_rewind();
				break;
			}
			unowned FlagsValue? flag = flags_class.get_value_by_nick(word);
			if (flag == null) {
				p.mark_rewind();
				break;
			}
			@value.set_flags(@value.get_flags() & flag.value);
			p.mark_clear();
		}
		return Result.OK;
	}

	internal override void print(Printer p, Value @value) requires(@value.type() == type) {
		var first = true;
		foreach (unowned FlagsValue flag in flags_class.values) {
			if ((flag.value & @value.get_flags()) == flag.value) {
				if (first) {
					first = false;
				} else {
					p.append("%_");
				}
				p.append(flag.value_nick);
			}
		}
	}
}

/**
 * Rule to parse an integer
 *
 * Because of oddness in {@link Value} overrides are needed to complete the right action.
 */
internal abstract class GTeonoma.IntegerRule : Rule {
	Type type;
	public IntegerRule(Type type) requires (type.is_fundamental()) {
		this.name = "integer constant";
		this.type = type;
	}

	internal override Result parse(Parser p, out Value @value) {
		@value = Value(type);
		unichar c;
		uint64 accumulator = 0;
		var negate = false;
		switch ((c = p[true])) {
			case '0':
				switch(p[false]) {
					case 'x':
					case 'X':
						p.get(true);
						while((c = p[false]).isxdigit()) {
							accumulator = accumulator * 16 + p[true].xdigit_value();
						}
						break;
					case '0':
					case '1':
					case '2':
					case '3':
					case '4':
					case '5':
					case '6':
					case '7':
						accumulator = p[true].digit_value();
						while((c = p[false]).isdigit()) {
							if (c >= '8') {
								p.push_error("Non-octal digit in octal number.");
								return Result.ABORT;
							}
							accumulator = accumulator * 8 + p[true].digit_value();
						}
						break;
					}
					break;
				case '-':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					if (c == '-') {
						negate = true;
					} else {
						accumulator = c.digit_value();
					}
					while(p[false].isdigit()) {
						accumulator = accumulator * 10 + p[true].digit_value();
					}
					break;
			default:
				return Result.FAIL;
		}

		var long_var = false;
		var unsigned_var = false;
		switch (p[false]) {
			case 'l':
			case 'L':
				long_var = true;
				p.get(true);
				break;
			case 'u':
			case 'U':
				unsigned_var = true;
				p.get(true);
				break;
		}
		//TODO longs and unsigned don't make sense, do they?
		return set_value(accumulator, negate, ref @value) ?  Result.OK : Result.FAIL;
	}
	protected abstract bool set_value(uint64 number, bool negate, ref Value @value);
}

internal class GTeonoma.Int8Rule : IntegerRule {
	public Int8Rule(Type type) {
		base(type);
	}
	internal override void print(Printer p, Value @value) {
		p.append(@value.get_char().to_string());
	}
	protected override bool set_value(uint64 number, bool negate, ref Value @value) {
		if (number > int8.MAX) {
			return false;
		}
		@value.set_char(negate ? - ((char) number) : (char) number);
		return true;
	}
}
internal class GTeonoma.Int32Rule : IntegerRule {
	public Int32Rule(Type type) {
		base(type);
	}
	internal override void print(Printer p, Value @value) {
		p.append(@value.get_int().to_string());
	}
	protected override bool set_value(uint64 number, bool negate, ref Value @value) {
		if (number > int32.MAX) {
			return false;
		}
		@value.set_int(negate ? - ((int32) number) : (int32) number);
		return true;
	}
}
internal class GTeonoma.Int64Rule : IntegerRule {
	public Int64Rule(Type type) {
		base(type);
	}
	internal override void print(Printer p, Value @value) {
		p.append(@value.get_int64().to_string());
	}
	protected override bool set_value(uint64 number, bool negate, ref Value @value) {
		if (number > int64.MAX) {
			return false;
		}
		@value.set_int64(negate ? - ((int64) number) : (int64) number);
		return true;
	}
}
internal class GTeonoma.UInt8Rule : IntegerRule {
	public UInt8Rule(Type type) {
		base(type);
	}
	internal override void print(Printer p, Value @value) {
		p.append(@value.get_uchar().to_string());
	}
	protected override bool set_value(uint64 number, bool negate, ref Value @value) {
		if (number > uint8.MAX || negate) {
			return false;
		}
		@value.set_uchar((uint8)number);
		return true;
	}
}
internal class GTeonoma.UInt32Rule : IntegerRule {
	public UInt32Rule(Type type) {
		base(type);
	}
	internal override void print(Printer p, Value @value) {
		p.append(@value.get_uint().to_string());
	}
	protected override bool set_value(uint64 number, bool negate, ref Value @value) {
		if (number > uint32.MAX || negate) {
			return false;
		}
		@value.set_uint((uint32)number);
		return true;
	}
}
internal class GTeonoma.UInt64Rule : IntegerRule {
	public UInt64Rule(Type type) {
		base(type);
	}
	internal override void print(Printer p, Value @value) {
		p.append(@value.get_uint64().to_string());
	}
	protected override bool set_value(uint64 number, bool negate, ref Value @value) {
		if (negate) {
			return false;
		}
		@value.set_uint64(number);
		return true;
	}
}

internal class GTeonoma.CustomRule<T> : Rule {
	private Type type;
	private CustomParser.CustomParserFactory<T> constructor;
	private CustomParser.StringifyObject<T> stringifier;

	internal CustomRule(string? name, owned CustomParser.CustomParserFactory<T> constructor, owned CustomParser.StringifyObject<T> stringifier) {
		type = typeof(T);
		assert(type.is_a(typeof(Object)));
		this.name = name ?? type.name();
		this.constructor = (owned)constructor;
		this.stringifier = (owned)stringifier;
	}

	internal override Result parse(Parser p, out Value @value) {
		@value = Value(type);
		CustomParser<T> state = constructor();
		var seen_accepting = false;
		long last_buffer_len = 0;
		var buffer = new StringBuilder();

		var source_location = p.get_location();
		p.mark_set();

		var finished = false;
		while (!finished) {
			var c = p[true];
			if (c == '\0') {
				p.push_error(@"Unexpected end of input in $name.");
				p.mark_rewind();
				finished = true;
				break;
			}
			buffer.append_unichar(c);
			var statetype = state.next_state(c);
			switch (statetype) {
				case CustomParser.StateType.ACCEPTING:
				case CustomParser.StateType.FINAL:
					seen_accepting = true;
					last_buffer_len = buffer.len;
					if (statetype == CustomParser.StateType.FINAL) {
						finished = true;
						p.mark_clear();
					} else {
						p.mark_reset();
					}
					break;
				case CustomParser.StateType.INTERMEDIATE:
					break;
				case CustomParser.StateType.INVALID:
					p.push_error(@"Unexpected $(c.to_string()) in $name.");
					p.mark_rewind();
					finished = true;
					break;
			}
		}
		if (seen_accepting) {
			buffer.truncate(last_buffer_len);
			var obj = (Object)state.build_object(buffer.str);
			if (obj is SourceInfo) {
				((SourceInfo)obj).source = source_location;
			}
			@value.set_object(obj);
			return Result.OK;
		} else {
			return Result.FAIL;
		}
	}

	internal override void print(Printer p, Value @value) requires (@value.type() == type) {
		Object obj = @value.get_object();
		assert(obj.get_type().is_a(type));
		p.append(stringifier((T)obj));
	}
}

/**
 * Rule to parse an unusual object
 *
 * Unusual objects include string literals and regular expressions. This is
 * needed when parsing an object where spacing is important and/or each element
 * in the grammar does/should not correspond to an object in the syntax tree.
 */
public abstract class GTeonoma.CustomParser<T> : Object {
	/**
	 * Update the current state given the next character of input.
	 *
	 * The parser will feed characters to the custom parser one at a time by
	 * calling this method. After this class has seen a character, it must return
	 * what it thinks of that character. From a theoretical perspective, this
	 * class is assumed to be a DFA and is send character one at a time. The type
	 * of the state the DFA is in is returned, not the DFA state itself. The
	 * parser then makes a decision about whether to continue with this DFA or
	 * return.
	 *
	 * The parser will mark the last time a state was {@link StateType.ACCEPTING}
	 * and will continue until it is in state {@link StateType.INVALID}. Once a
	 * character is rejected, it will rewind to the last accepting state and
	 * present this input to {@link build_object}. If no accepting state is ever
	 * reached, parsing will fail.
	 *
	 * Obviously, the underlying algorithm need not be a DFA.
	 *
	 * @return the type of the new state of the DFA.
	 */
	public abstract StateType next_state(unichar input);

	/**
	 * The type of state the parser is currently in.
	 */
	public enum StateType {
		/**
		 * The current input has been accepted.
		 *
		 * This signals that the input seen so far is valid and a resulting object
		 * can be constructed. If the current input is not invalid, but it is not yet
		 * possible to build the object, this method should return false.
		 */
		ACCEPTING,
		/**
		 * The current input is insufficient to determine if it is valid or invalid.
		 *
		 * It is not a valid state, but it is not invalid and needs more input to settle.
		 */
		INTERMEDIATE,
		/**
		 * The current input is accepted and no future input will be accepted.
		 *
		 * Effectively, this is {@link ACCEPTING} followed by {@link INVALID} for
		 * any subsequent input.
		 */
		FINAL,
		/**
		 * The current input is invalid.
		 *
		 * When the current parsing state is an error state, the parser rewinds to
		 * the last accepting state it has seen and then calls {@link build_object}.
		 * If no accepting state has been found, the parsing fails.
		 */
		INVALID
	}

	/**
	 * Construct the output object given the validated input.
	 */
	public abstract T build_object(string str);

	/**
	 * Pretty print a custom-parsed object.
	 */
	public delegate string StringifyObject<T>(T obj);

	/**
	 * Create a new custom parser to parse an object.
	 */
	public delegate CustomParser<T> CustomParserFactory<T>();
}
