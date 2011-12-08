/**
 * A parsed C-style string literal
 *
 * That is, a string surrounded by double quotation marks with back-slash escape codes.
 */
public class GTeonoma.StringLiteral : Object, SourceInfo {
	/**
	 * The string literal parsed.
	 */
	public string str { get; set;}
	public source_location source { get; set; }
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
		internal CustomParser.State get_state() {
			switch (this) {
				case StringState.INIT:
				case StringState.CONTENTS:
				case StringState.ESCAPE:
				case StringState.OCTAL2:
				case StringState.OCTAL3:
				case StringState.HEX1:
				case StringState.HEX2:
					return State.INTERMEDIATE;
				case StringState.END:
					return State.ACCEPTING;
				default:
					return State.INVALID;
			}
		}
	}

	private StringState state;

	public StringLiteralParser() {
		state = StringState.INIT;
	}

	public override CustomParser.State next_state(unichar input) {
		switch (state) {
			case StringState.INIT:
				if (input == '"') {
					return (state = StringState.CONTENTS).get_state();
				} else {
					return (state = StringState.JUNK).get_state();
				}
			case StringState.CONTENTS:
				if (input == '\\') {
					return (state = StringState.ESCAPE).get_state();
				} else if (input == '"') {
					return (state = StringState.END).get_state();
				} else {
					return (state = StringState.CONTENTS).get_state();
				}
			case StringState.ESCAPE:
				if (input == 'x') {
					return (state = StringState.HEX1).get_state();
				} else if (input >= '0' && input <= '7') {
					return (state = StringState.OCTAL2).get_state();
				} else if (input == 'a' || input == 'b' || input == 'f' || input == 'n' || input == 'r' || input == 't' || input == 'v' || input == '"' || input == '\'' || input == '\\' || input == '?') {
					return (state = StringState.CONTENTS).get_state();
				} else {
					return (state = StringState.JUNK).get_state();
				}
			case StringState.OCTAL2:
				if (input >= '0' && input <= '7') {
					return (state = StringState.OCTAL3).get_state();
				} else {
					return (state = StringState.JUNK).get_state();
				}
			case StringState.OCTAL3:
				if (input >= '0' && input <= '7') {
					return (state = StringState.CONTENTS).get_state();
				} else {
					return (state = StringState.JUNK).get_state();
				}
			case StringState.HEX1:
				if (input.isxdigit()) {
					return (state = StringState.HEX2).get_state();
				} else {
					return (state = StringState.JUNK).get_state();
				}
			case StringState.HEX2:
				if (input.isxdigit()) {
					return (state = StringState.CONTENTS).get_state();
				} else {
					return (state = StringState.JUNK).get_state();
				}
			default:
				return (state = StringState.JUNK).get_state();
		}
	}

	public override StringLiteral build_object(string str) {
		var literal = new StringLiteral();
		var buffer = new StringBuilder();
		for(var it = 1; it < str.length - 1; it++) {
			if (str[it] == '\\') {
				switch(str[++it]) {
						case 'a':
							buffer.append_c(7);
							break;
						case 'b':
							buffer.append_c('\b');
							break;
						case 'f':
							buffer.append_c('\f');
							break;
						case 'n':
							buffer.append_c('\n');
							break;
						case 'r':
							buffer.append_c('\r');
							break;
						case 't':
							buffer.append_c('\t');
							break;
						case 'v':
							buffer.append_c(11);
							break;
						case '"':
							buffer.append_c('"');
							break;
						case '\'':
							buffer.append_c('\'');
							break;
						case '\\':
							buffer.append_c('\\');
							break;
						case '?':
							buffer.append_c('?');
							break;
						case 'x':
							buffer.append_c((char)(str[it + 1].xdigit_value() * 16 + str[it + 2].xdigit_value()));
							it += 2;
							break;
						default:
							buffer.append_c((char)(str[it + 1].digit_value() * 64 + str[it + 2].digit_value() * 8 + str[it + 3].digit_value()));
							it += 3;
							break;
				}
			} else {
				buffer.append_c(str[it]);
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
	public Identifier(string name) {
		this.name = name;
	}
	public string name { get; set;}
	public source_location source { get; set; }
}

internal class GTeonoma.IdentifierParser : CustomParser<Identifier> {
	internal enum IdentifierState {
		AT,
		START,
		PART,
		JUNK;
		internal CustomParser.State get_state() {
			switch (this) {
				case IdentifierState.AT:
				case IdentifierState.START:
					return State.INTERMEDIATE;
				case IdentifierState.PART:
					return State.ACCEPTING;
				default:
					return State.INVALID;
			}
		}
	}

	private IdentifierState state;
	private bool allow_unicode;

	public IdentifierParser(bool allow_at, bool allow_unicode) {
		state = allow_at ? IdentifierState.AT : IdentifierState.START;
		this.allow_unicode = allow_unicode;
	}

	public override CustomParser.State next_state(unichar input) {
			if (input == '@') {
				if (state == IdentifierState.AT) {
					return (state = IdentifierState.START).get_state();
				} else {
					return (state = IdentifierState.JUNK).get_state();
				}
			} else if (state == IdentifierState.JUNK) {
					return (state = IdentifierState.JUNK).get_state();
			} else if (state == IdentifierState.PART && (allow_unicode ? input.isdigit() : (input >= '0' && input <= '9'))) {
				return (state = IdentifierState.PART).get_state();
			} else if (input == '_' || (allow_unicode ? input.isalpha() : (input >= 'a' && input <= 'z' || input >= 'A' && input <= 'Z'))) {
				return (state = IdentifierState.PART).get_state();
			} else {
				return (state = IdentifierState.JUNK).get_state();
			}
	}
	public override Identifier build_object(string str) {
		return new Identifier(str);
	}
}
