using GTeonoma;

class Foo : Object, SourceInfo {
	public source_location source {get; set;}
	public Identifier id { get; set; }
	public Access access { get; set;}
	public StringLiteral info {get; set;}
	public Gee.List<Stmt> statements {get; set;}
}

abstract class Stmt : Object {
}

class Bar : Stmt {
	public bool is_bar { get; set; }
	public int64 val { get; set; }
}

class DoStmt : Stmt {
	public Stmt expr { get; set;}
}

class Say : Stmt {
	public StringLiteral expr { get; set; }
}

public enum Access {
	PUBLIC, PRIVATE
}

static void main() {
	var rules = new Rules();
	rules.register_int(64, typeof(int64));
	rules.register_string_literal();
	rules.register_identifier();
	rules.register<Foo>("foo", 0, "%p{access}%_foo %P{id}% (% %P{info}% ) {%I%n%l{statements}{% ;%n}%i%n}%n", new Type[] {typeof(Stmt)});
	rules.register<Access>();
	rules.register<Bar>("bar", 0, "%b{is_bar}{bar}%_%P{val}");
	rules.register<DoStmt>("do", 0, "do%_{% %P{expr}% }");
	rules.register<Say>("say", 0, "say %P{expr}");

	var parser = new StringParser(rules, "private foo _s (\"f\\x0A\\n\") { 3; 0; do {bar 0x5f}; say \"yo\"}");
	Gee.List<Foo> l;
	switch (parser.parse_all<Foo>(out l)) {
		case Result.OK:
			stdout.printf("OK\n");
			var printer = new ConsolePrinter(rules);
			printer.print_all(l);
			break;
		case Result.FAIL:
			stdout.printf("FAIL\n");
			break;
		case Result.ABORT:
			stdout.printf("ABORT\n");
			break;
	}
	parser.visit_errors((loc, s) => { stdout.printf("Parse error %ld:%ld: %s\n", loc.line, loc.offset, s);});
}
