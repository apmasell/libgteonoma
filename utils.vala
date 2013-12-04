// vim: set ts=2 sw=2 tw=0 :
/**
 * A parsed C-style string literal
 *
 * That is, a string surrounded by double quotation marks with back-slash escape codes.
 */
public class GTeonoma.StringLiteral : Object, SourceInfo {
	/**
	 * The string literal parsed.
	 */
	public string str {
		get;
		set;
	}
	public source_location source {
		get;
		set;
	}
}

internal class GTeonoma.StringLiteralParser : CustomParser<StringLiteral> {
	internal enum StringState {
		INIT,
		CONTENTS,
		ESCAPE,
		OCTAL1,
		OCTAL2,
		OCTAL3,
		HEX1,
		HEX2,
		END,
		JUNK;
		internal CustomParser.StateType get_state (bool has_quotes) {
			switch (this) {
			 case StringState.CONTENTS:
				 return has_quotes ? StateType.INTERMEDIATE : StateType.ACCEPTING;

			 case StringState.INIT:
			 case StringState.ESCAPE:
			 case StringState.OCTAL2:
			 case StringState.OCTAL3:
			 case StringState.HEX1:
			 case StringState.HEX2:
				 return StateType.INTERMEDIATE;

			 case StringState.END:
				 return has_quotes ? StateType.ACCEPTING : StateType.INVALID;

			 default:
				 return StateType.INVALID;
			}
		}
	}

	private StringState state;
	private bool has_quotes;

	public StringLiteralParser (bool has_quotes) {
		state = has_quotes ? StringState.INIT : StringState.CONTENTS;
		this.has_quotes = has_quotes;
	}

	public override CustomParser.StateType next_state (unichar input) {
		switch (state) {
		 case StringState.INIT:
			 if (input == '"') {
				 return (state = StringState.CONTENTS).get_state (has_quotes);
			 } else {
				 return (state = StringState.JUNK).get_state (has_quotes);
			 }

		 case StringState.CONTENTS:
			 if (input == '\\') {
				 return (state = StringState.ESCAPE).get_state (has_quotes);
			 } else if (input == '"') {
				 return (state = StringState.END).get_state (has_quotes);
			 } else {
				 return (state = StringState.CONTENTS).get_state (has_quotes);
			 }

		 case StringState.ESCAPE:
			 if (input == 'x') {
				 return (state = StringState.HEX1).get_state (has_quotes);
			 } else if (input >= '0' && input <= '7') {
				 return (state = StringState.OCTAL2).get_state (has_quotes);
			 } else if (input == 'a' || input == 'b' || input == 'f' || input == 'n' || input == 'r' || input == 't' || input == 'v' || input == '"' || input == '\'' || input == '\\' || input == '?') {
				 return (state = StringState.CONTENTS).get_state (has_quotes);
			 } else {
				 return (state = StringState.JUNK).get_state (has_quotes);
			 }

		 case StringState.OCTAL2:
			 if (input >= '0' && input <= '7') {
				 return (state = StringState.OCTAL3).get_state (has_quotes);
			 } else {
				 return (state = StringState.JUNK).get_state (has_quotes);
			 }

		 case StringState.OCTAL3:
			 if (input >= '0' && input <= '7') {
				 return (state = StringState.CONTENTS).get_state (has_quotes);
			 } else {
				 return (state = StringState.JUNK).get_state (has_quotes);
			 }

		 case StringState.HEX1:
			 if (input.isxdigit ()) {
				 return (state = StringState.HEX2).get_state (has_quotes);
			 } else {
				 return (state = StringState.JUNK).get_state (has_quotes);
			 }

		 case StringState.HEX2:
			 if (input.isxdigit ()) {
				 return (state = StringState.CONTENTS).get_state (has_quotes);
			 } else {
				 return (state = StringState.JUNK).get_state (has_quotes);
			 }

		 default:
			 return (state = StringState.JUNK).get_state (has_quotes);
		}
	}

	public override StringLiteral build_object (string str) {
		var literal = new StringLiteral ();
		var buffer = new StringBuilder ();
		for (var it = has_quotes ? 1 : 0; it < str.length - (has_quotes ? 1 : 0); it++) {
			if (str[it] == '\\') {
				switch (str[++it]) {
				 case 'a':
					 buffer.append_c (7);
					 break;

				 case 'b':
					 buffer.append_c ('\b');
					 break;

				 case 'f':
					 buffer.append_c ('\f');
					 break;

				 case 'n':
					 buffer.append_c ('\n');
					 break;

				 case 'r':
					 buffer.append_c ('\r');
					 break;

				 case 't':
					 buffer.append_c ('\t');
					 break;

				 case 'v':
					 buffer.append_c (11);
					 break;

				 case '"':
					 buffer.append_c ('"');
					 break;

				 case '\'':
					 buffer.append_c ('\'');
					 break;

				 case '\\':
					 buffer.append_c ('\\');
					 break;

				 case '?':
					 buffer.append_c ('?');
					 break;

				 case 'x':
					 buffer.append_c ((char) (str[it + 1].xdigit_value () * 16 + str[it + 2].xdigit_value ()));
					 it += 2;
					 break;

				 default:
					 buffer.append_c ((char) (str[it + 1].digit_value () * 64 + str[it + 2].digit_value () * 8 + str[it + 3].digit_value ()));
					 it += 3;
					 break;
				}
			} else {
				buffer.append_c (str[it]);
			}
		}
		literal.str = buffer.str;
		return literal;
	}
}

/**
 * A parsed identifier.
 *
 * That is, a bunch of characters that look like a valid variable or function
 * name. This may be preceeded by an @ or include Unicode characters.
 */
public class GTeonoma.Identifier : Object, SourceInfo {
	public Identifier (string name) {
		this.name = name;
	}
	public string name {
		get;
		set;
	}
	public source_location source {
		get;
		set;
	}
}

internal class GTeonoma.IdentifierParser : CustomParser<Identifier> {
	internal enum IdentifierState {
		AT,
		START,
		PART,
		JUNK;
		internal CustomParser.StateType get_state () {
			switch (this) {
			 case IdentifierState.AT:
			 case IdentifierState.START:
				 return StateType.INTERMEDIATE;

			 case IdentifierState.PART:
				 return StateType.ACCEPTING;

			 default:
				 return StateType.INVALID;
			}
		}
	}

	private IdentifierState state;
	private bool allow_unicode;

	public IdentifierParser (bool allow_at, bool allow_unicode) {
		state = allow_at ? IdentifierState.AT : IdentifierState.START;
		this.allow_unicode = allow_unicode;
	}

	public override CustomParser.StateType next_state (unichar input) {
		if (input == '@') {
			if (state == IdentifierState.AT) {
				return (state = IdentifierState.START).get_state ();
			} else {
				return (state = IdentifierState.JUNK).get_state ();
			}
		} else if (state == IdentifierState.JUNK) {
			return (state = IdentifierState.JUNK).get_state ();
		} else if (state == IdentifierState.PART && (allow_unicode ? input.isdigit () : (input >= '0' && input <= '9'))) {
			return (state = IdentifierState.PART).get_state ();
		} else if (input == '_' || (allow_unicode ? input.isalpha () : (input >= 'a' && input <= 'z' || input >= 'A' && input <= 'Z'))) {
			return (state = IdentifierState.PART).get_state ();
		} else {
			return (state = IdentifierState.JUNK).get_state ();
		}
	}
	public override Identifier build_object (string str) {
		return new Identifier (str);
	}
}
[CCode (gir_namespace = "libgteonoma", gir_version = "1")]
namespace GTeonoma {
	public void log_to_console (Parser parser) {
		parser.attempting_parse.connect ((rule, precedence, depth, offset, lines) => {
							 for (var i = 0; i < depth; i++) {
								 stdout.putc (' ');
							 }
							 stdout.printf ("%s[%u]: Attempting parse at %d:%d...\n", rule, precedence, lines, offset);
						 }
						 );
		parser.finished_parse.connect ((result, rule, precedence, depth, offset, lines) => {
						       for (var i = 0; i < depth; i++) {
							       stdout.putc (' ');
						       }
						       stdout.printf ("%s[%u]: %s at %d:%d.\n", rule, precedence, result.to_string (), lines, offset);
					       }
					       );
		parser.cache_hit.connect ((result, rule, precedence, depth, offset, lines) => {
						  for (var i = 0; i < depth; i++) {
							  stdout.putc (' ');
						  }
						  stdout.printf ("%s[%u]: %s at %d:%d. (cached)\n", rule, precedence, result.to_string (), lines, offset);
					  }
					  );
		parser.start_property.connect ((rule, property, depth) => {
						       for (var i = 0; i < depth; i++) {
							       stdout.putc (' ');
						       }
						       stdout.printf ("%s: Parsing property %s...\n", rule, property);
					       }
					       );
		parser.end_property.connect ((rule, property, depth) => {
						     for (var i = 0; i < depth; i++) {
							     stdout.putc (' ');
						     }
						     stdout.printf ("%s: Finished property %s.\n", rule, property);
					     }
					     );
	}
}