using GTeonoma;

abstract class Expression : Object, SourceInfo {
	public source_location source {get; set;}
	public abstract double compute();
}

class Addition : Expression {
	public Expression left { get; set; }
	public Expression right { get; set; }
	public override double compute() {
		return left.compute() + right.compute();
	}
}

class Subtraction : Expression {
	public Expression left { get; set; }
	public Expression right { get; set; }
	public override double compute() {
		return left.compute() - right.compute();
	}
}

abstract class BalanceProduct : Expression {
	public Expression left { get; set; }
	public Expression right { get; set; }
	protected abstract double compute_val(double left, double right);
	public override double compute() {
		if (right is BalanceProduct) {
			var bright = (BalanceProduct) right;
			var v = compute_val(left.compute(), bright.left.compute());
			return bright.compute_val(v, bright.right.compute());
		}
		return compute_val(left.compute(), right.compute());
	}
}

class Multiplication : BalanceProduct {
	protected override double compute_val(double left, double right) {
		return left * right;
	}
}

class Division : BalanceProduct {
	protected override double compute_val(double left, double right) {
		return left / right;
	}
}

class Literal : Expression {
	public int32 val { get; set; }
	public override double compute() {
		return val;
	}
}

class Parenthetical : Expression {
	public Expression expr { get; set; }
	public override double compute() {
		return expr.compute();
	}
}

void parse(Rules rules, string expression) {
	var parser = new StringParser(rules, expression);
	Value val;
	stdout.printf("Parsing `%s'.\n", expression);
	switch (parser.parse(typeof(Expression), out val)) {
		case Result.OK:
			var printer = new ConsolePrinter(rules);
			printer.print(val);
			stdout.printf(" = %f\n", ((Expression)val.get_object()).compute());
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

static void main() {
	var rules = new Rules();
	rules.register_int(32, typeof(int32));
	rules.register<Multiplication>("multiplication", 1, "%P{+left}% *% %P{right}");
	rules.register<Division>("division", 2, "%P{+left}% /% %P{+right}");
	rules.register<Addition>("addition", 0, "%P{+left}% +% %P{right}");
	rules.register<Subtraction>("subtraction", 0, "%P{+left}% -% %P{+right}");
	rules.register<Parenthetical>("subexpression", 3, "% (% %P{-expr}% )");
	rules.register<Literal>("number", 3, "% %P{val}");

	parse(rules, "3+ 5");
	parse(rules, "3+ 5*10");
	parse(rules, "3 + 5*10");
	parse(rules, "(3 + 5)*10");
	parse(rules, "100/5 * 3");
}
