import java.io.*;
/*===============================================================
	パーザ用例外
  ===============================================================*/
class ParseError extends Exception {
    String message;
    ParseError() {
	message = "";
    }
    ParseError(String s) {
	message = s;
    }
    public String toString() {
	return "Error: " + message;
    }
}
class PCFeval {
/*===============================================================
	初期化
  ===============================================================*/
    String filename;      // プログラムファイル名
    BufferedReader input; // 入力用

    void initialize(String fn) throws Exception {
	filename = fn;
	InputStream inStream;
	if (fn == null)
	    inStream = System.in;
	else
	    inStream = new FileInputStream(fn);
        input = new BufferedReader(new InputStreamReader(inStream));
    }
/*===============================================================
	入力文字処理
  ===============================================================*/
    char lastChar;                        // 最後の入力文字
    boolean ungetCharSwt = false;         // １文字分入力を戻したか
    final static char CHAR_EOF = (char)0; // ファイルの終わり

    char getChar() throws Exception {
	if (ungetCharSwt) {
	    ungetCharSwt = false;
	    return lastChar;
	}
	int c = input.read();
	if (c == -1) {
	    return lastChar = CHAR_EOF;
	}
	else {
	    return lastChar = (char)c;
	}
    }
    // 入力を１文字戻す
    void ungetChar() {
	ungetCharSwt = true;
    }
    // 行末まで読み飛ばす
    void skipLine() throws Exception {
	if (lastChar != '\n') {
	    input.readLine();
	    ungetCharSwt = false;
	}
    }
/*===============================================================
	字句解析
  ===============================================================*/
    int lastTok;           // 最後に読み込んだトークン
    int tokvalNum;         // 数値のトークンの値
    String tokvalVar;      // 識別子のトークンの値

    // トークンの種類
    final static int TOK_SEM    = 1;   // ;
    final static int TOK_COL    = 2;   // :
    final static int TOK_LP     = 3;   // (
    final static int TOK_RP     = 4;   // )
    final static int TOK_DOT    = 5;   // .
    final static int TOK_ARROW  = 10;  // ->
    final static int TOK_NUM    = 11;  // num
    final static int TOK_BOOL   = 12;  // bool
    final static int TOK_LAMBDA = 21;  // lambda \  
    final static int TOK_MU     = 22;  // mu  ?
    final static int TOK_ZERO   = 30;  // 0
    final static int TOK_TRUE   = 31;  // true
    final static int TOK_FALSE  = 32;  // false
    final static int TOK_SUCC   = 33;  // succ
    final static int TOK_PRED   = 34;  // pred
    final static int TOK_ISZERO = 35;  // iszero
    final static int TOK_IF     = 36;  // if
    final static int TOK_THEN   = 37;  // then
    final static int TOK_ELSE   = 38;  // else
    final static int TOK_VAR    = 50;  // 変数名
    final static int TOK_EOF    = -4;  // End of File
    final static int TOK_ERROR  = -99; // エラー

    // 名前(変数)のトークンを生成
    final static int MAX_LEN_NAME = 50; // 変数名の最大長
    char[] buf = new char[MAX_LEN_NAME];
    void getName(char c) throws Exception {
	int i;
        for (i = 0;
	     Character.isLetterOrDigit(c) && i<MAX_LEN_NAME; 
	     i++) {
	    buf[i] = c;
	    c = getChar();
	}
        tokvalVar = new String(buf, 0, i);
	// 最大長を越える名前の後ろは読み飛ばす */
	while (Character.isLetterOrDigit(c)) c = getChar();
	ungetChar();
    }
    // トークンを生成して大域変数 lastTok に代入
    void nextToken() throws Exception {
	char c = getChar();
        // 空白文字の読み飛ばし
	while (Character.isWhitespace(c)) c = getChar();
	// ファイルの終わり
	if (c == CHAR_EOF) {
	    lastTok = TOK_EOF;
	}
	// 名前(識別子)
	else if (Character.isLetter(c)) {
	    getName(c);
	    if (tokvalVar.equals("true"))
		lastTok = TOK_TRUE;
	    else if (tokvalVar.equals("false"))
		lastTok = TOK_FALSE;
	    else if (tokvalVar.equals("succ"))
		lastTok = TOK_SUCC;
	    else if (tokvalVar.equals("pred"))
		lastTok = TOK_PRED;
	    else if (tokvalVar.equals("iszero"))
		lastTok = TOK_ISZERO;
	    else if (tokvalVar.equals("if"))
		lastTok = TOK_IF;
	    else if (tokvalVar.equals("then"))
		lastTok = TOK_THEN;
	    else if (tokvalVar.equals("else"))
		lastTok = TOK_ELSE;
	    else if (tokvalVar.equals("lambda"))
		lastTok = TOK_LAMBDA;
	    else if (tokvalVar.equals("mu"))
		lastTok = TOK_MU;
	    else
		lastTok = TOK_VAR;
	}
	// 特殊記号
	else {
	    switch (c) {
	    case '0' : lastTok = TOK_ZERO;  break;
	    case ';' : lastTok = TOK_SEM;   break;
	    case ':' : lastTok = TOK_COL;   break;
	    case '(' : lastTok = TOK_LP;    break;
	    case ')' : lastTok = TOK_RP;    break;
	    case '.' : lastTok = TOK_DOT;   break;
	    case '\\' : lastTok = TOK_LAMBDA; break;
	    case '?' : lastTok = TOK_MU;    break;
	    default:   lastTok = TOK_ERROR; break;
	    }
	}
    }
/*===============================================================
        構文解析
  ===============================================================*/
/*
    statement ::= term1 ";"
    term1 ::= "lambda" NAME "." term1
	    | "mu" NAME "." term1
	    | term2
    term2 ::= term3 { term3 }
    term3 ::= NAME 
           | "0"
	   | "true"
	   | "false"
           | "succ" "(" term1 ")"
           | "pred" "(" term1 ")"
           | "iszero" "(" term1 ")"
	   | "if" term2 "then" term2 "else" term2
           | "(" term1 ")"
*/
    void procStatement() throws Exception {
	try {
	    Term term = procTerm1();
	    if (lastTok != TOK_SEM) { // 式の終わりは ;
		throw new ParseError();
	    }
	    System.out.println("Input: " + term);
	    System.out.println("Value: " + term.eval());
	    nextToken();              // ; の次トークンを読む
	}
	catch (ParseError e) {
	    System.out.println(e);
	    skipLine();
	    nextToken();
	}
    }
    Term procTerm1() throws Exception {
	if (lastTok == TOK_LAMBDA) {
            // "lambda" NAME "." term1
	    nextToken();
	    if (lastTok != TOK_VAR)
		throw new ParseError();
	    String name = tokvalVar;
	    nextToken();
	    if (lastTok != TOK_DOT) 
		throw new ParseError();
	    nextToken();
	    return new Abst(new Var(name), procTerm1());
	}
	else if (lastTok == TOK_MU) {
            // "mu" NAME "." term1
	    nextToken();
	    if (lastTok != TOK_VAR)
		throw new ParseError();
	    String name = tokvalVar;
	    nextToken();
	    if (lastTok != TOK_DOT) 
		throw new ParseError();
	    nextToken();
	    return new Rec(new Var(name), procTerm1());
	}
	else
	    return procTerm2();
    }
    Term procTerm2() throws Exception {
	Term term = procTerm3();
	for (;;) {
	    Term t = procTerm3();
	    if (t == null)
		return term;
	    term = new Appl(term, t);
	}
    }
    Term procTerm3() throws Exception {
	Term term;
	switch (lastTok) {
	case TOK_VAR:
	    term = new Var(tokvalVar);
	    nextToken();
	    return term;
	case TOK_ZERO:
	    nextToken();
	    return new Zero();
	case TOK_TRUE:
	    nextToken();
	    return new True();
	case TOK_FALSE:
	    nextToken();
	    return new False();
	case TOK_SUCC:
	    nextToken();
	    if (lastTok != TOK_LP) 
		throw new ParseError();
	    nextToken();
	    term = procTerm1();
	    if (lastTok != TOK_RP) 
		throw new ParseError();
	    nextToken();
	    return new Succ(term);
	case TOK_PRED:
	    nextToken();
	    if (lastTok != TOK_LP) 
		throw new ParseError();
	    nextToken();
	    term = procTerm1();
	    if (lastTok != TOK_RP) 
		throw new ParseError();
	    nextToken();
	    return new Pred(term);
	case TOK_ISZERO:
	    nextToken();
	    if (lastTok != TOK_LP) 
		throw new ParseError();
	    nextToken();
	    term = procTerm1();
	    if (lastTok != TOK_RP) 
		throw new ParseError();
	    nextToken();
	    return new Iszero(term);
	case TOK_IF:
	    nextToken();
	    term = procTerm2();
	    if (lastTok != TOK_THEN)
		throw new ParseError();
	    nextToken();
	    Term term1 = procTerm2();
	    if (lastTok != TOK_ELSE)
		throw new ParseError();
	    nextToken();
	    Term term2 = procTerm2();
	    return new If(term, term1, term2);
	case TOK_LP:
	    nextToken();
	    term = procTerm1();
	    if (lastTok != TOK_RP) 
		throw new ParseError();
	    nextToken();
	    return term;
	default:
	    return null;
	}
    }
/*===============================================================
	main
  ===============================================================*/
    void proc(String[] args) {
	try {
	    if (args.length >= 1) 
		initialize(args[0]);
	    else
		initialize(null);
	    nextToken();
	    while (lastTok != TOK_EOF) {
		procStatement();
	    }
	}
	catch (Exception e) {
	    System.out.println(e);
	}
    }
    public static void main(String[] args) {
	PCFeval eval = new PCFeval();
	eval.proc(args);
    }
}
/*===============================================================
	Term データ構造
  ===============================================================*/
abstract class Term {
    Term eval() { return this; }   // 式を評価
    Term subst(Var v, Term term) { // 変数 v を term で置き換える
	return this; 
    }
}
class Pred extends Term {
    Term arg;
    Pred(Term arg) { 
	this.arg = arg;
    }
    Term eval() {
	Term val = arg.eval();
	if (val instanceof Zero) {
	    return val;
	}
	else if (val instanceof Succ) {
	    Succ v = (Succ)val;
	    return v.getArg();
	}
	else {
	    return Undef.con;
	}
    }
    Term subst(Var v, Term term) { 
	return new Pred(arg.subst(v, term));
    }
    public String toString() {
	return "pred(" + arg + ")";
    }
}
class Succ extends Term {
    Term arg;
    Succ(Term arg) {
	this.arg = arg;
    }
    Term getArg() { return arg; }
    Term eval() {
	Term val = arg.eval();
	if (val instanceof Undef) {
	    return val;
	}
	else {
	    return new Succ(val);
	}
    }
    Term subst(Var v, Term term) { 
	return new Succ(arg.subst(v, term));
    }
    public String toString() {
	return "succ(" + arg + ")";
    }
}
class Iszero extends Term {
    Term arg;
    Iszero(Term arg) {
	this.arg = arg;
    }
    Term eval() {
	Term val = arg.eval();
	if (val instanceof Zero) {
	    return True.con;
	}
	else if (val instanceof Succ) {
	    return False.con;
	}
	else {
	    return Undef.con;
	}
    }
    Term subst(Var v, Term term) { 
	return new Iszero(arg.subst(v, term));
    }
    public String toString() {
	return "iszero(" + arg + ")";
    }
}
class If extends Term {
    Term argCond;
    Term argThen;
    Term argElse;
    If(Term argCond, Term argThen, Term argElse) {
	this.argCond = argCond;
	this.argThen = argThen;
	this.argElse = argElse;
    }
    Term eval() {
	Term val = argCond.eval();
	if (val instanceof True) {
	    return argThen.eval();
	}
	else if (val instanceof False) {
	    return argElse.eval();
	}
	else {
	    return Undef.con;
	}
    }
    Term subst(Var v, Term term) { 
	return new If(argCond.subst(v, term), 
		      argThen.subst(v, term),
		      argElse.subst(v, term));
    }
    public String toString() {
	return "if " + argCond + " then " 
               + argThen + " else " + argElse;
    }
}
class Appl extends Term {
    Term fun;
    Term para;
    Appl(Term fun, Term para) {
	this.fun = fun;
	this.para = para;
    }
    Term eval() {
	Term f = fun.eval();
        if (f instanceof Abst) {
	    Abst abst = (Abst)f;
	    Var var = abst.getVar();
	    Term body = abst.getBody();
	    Term v = body.subst(var, para); 
	    return v.eval();
	}
	else {
	    return Undef.con;
	}
    }
    Term subst(Var v, Term term) { 
	return new Appl(fun.subst(v, term), para.subst(v, term));
    }
    public String toString() {
	if (fun instanceof Abst || fun instanceof Rec || fun instanceof If)
	    return "(" + fun + ")" + "(" + para + ")";
	else
	    return fun + "(" + para + ")";
    }
}
class Abst extends Term {
    Var var;
    Term body;
    Abst(Var var, Term body) {
	this.var = var;
	this.body = body;
    }
    Term subst(Var v, Term term) { 
	if (var.name.equals(v.name)) {
	    return this;
	}
	else {
	    return new Abst(var, body.subst(v, term));
	}
    }
    Var getVar() {
	return var;
    }
    Term getBody() {
	return body;
    }
    public String toString() {
	return "λ" + var + "." + body;
    }
}
class Rec extends Term {
    Var var;
    Term body;
    Rec(Var var, Term body) {
	this.var = var;
	this.body = body;
    }
    Term eval() {
	return body.subst(var, this);
    }
    Term subst(Var v, Term term) {
	if (var.name.equals(v.name)) {
	    return this;
	}
	else {
	    return new Rec(var, body.subst(v, term));
	}
    }
    public String toString() {
	return "μ" + var + "." + body;
    }
}
class Var extends Term {
    String name;
    Var(String name) {
	this.name = name;
    }
    Term subst(Var v, Term term) {
	if (name.equals(v.name)) {
	    return term;
	}
	else {
	    return this;
	}
    }
    public String toString() {
	return name;
    }
}
class Zero extends Term {
    static Zero con = new Zero();
    public String toString() {
	return "0";
    }
}
class True extends Term {
    static True con = new True();
    public String toString() {
	return "true";
    }
}
class False extends Term {
    static False con = new False();
    public String toString() {
	return "false";
    }
}
class Undef extends Term {
    static Undef con = new Undef();
    public String toString() {
	return "undef";
    }
}
