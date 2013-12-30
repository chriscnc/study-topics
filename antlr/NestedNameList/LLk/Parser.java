
public abstract class Parser {
	Lexer input;		// from where do we get tokens?
	Token[] lookahead;	// circular lookahead buffer
	int k;				// how many lookahead symbols
	int p = 0;			// circular index of next token position to fill

	public Parser(Lexer input, int k) {
		this.input = input;
		this.k = k;
		lookahead = new Token[k];
		for(int i = 1; i <= k; i++) consume();
	}

	public int LA(int i) { 
		return LT(i).type;	
	}

	public Token LT(int i) {
		return lookahead[(p+i-1) % k];
	}
	
	public void match(int x) {
		if(LA(1) == x) consume();
		else throw new Error("expecting " + input.getTokenName(x) + 
			"; found " + LT(1));
	}

	public void consume() {
		lookahead[p] = input.nextToken();	// fill next position with token 
		p = (p + 1) % k;	// increment circular index
	}
}