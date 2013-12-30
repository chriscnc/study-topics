
public class LookaheadParser extends Parser {
	public LookaheadParser(Lexer input, int k) { 
		super(input, k); 
	}

	// list : '[' elements ']'
	public void list() {
		match(LookaheadLexer.LBRACK);
		elements();
		match(LookaheadLexer.RBRACK);
	}

	// elements : element (',' element)*
	public void elements() {
		element();
		while(LA(1) == LookaheadLexer.COMMA) {
			match(LookaheadLexer.COMMA);
			element();
		}
	}

	// element : NAME '=' NAME | NAME | list 
	public void element() {
		if(LA(1) == LookaheadLexer.NAME && LA(2) == LookaheadLexer.EQUALS){
			match(LookaheadLexer.NAME);
			match(LookaheadLexer.EQUALS);
			match(LookaheadLexer.NAME);
		} else if (LA(1) == LookaheadLexer.NAME) {
			match(LookaheadLexer.NAME);
		} else if (LA(1) == LookaheadLexer.LBRACK) {
			list();
		} else {
			throw new Error("expecting name or list; found " + LT(1));
		}
	}
}