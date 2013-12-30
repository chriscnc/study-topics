
public class Test {
	public static void main(String[] args) {

		// test parser
		LookaheadLexer lexer = new LookaheadLexer(args[0]);
		LookaheadParser parser = new LookaheadParser(lexer, 2);
		parser.list();

		// // test lexer
		// ListLexer lexer = new ListLexer(args[0]);
		// Token t = lexer.nextToken();
		// while(t.type != Lexer.EOF_TYPE) {
		// 	System.out.println(t);
		// 	t = lexer.nextToken();
		// }
		// System.out.println(t); // EOF
	}
}