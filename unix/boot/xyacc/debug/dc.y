# SPP/Yacc specification for a simple desk calculator.  Input consists
# of simple arithmetic expressions; output is the value of the expression.
# Operands are restricted to integer and real numeric constants.

%{
include	<ctype.h>
include	<lexnum.h>

define	YYMAXDEPTH	150		# length of parser stack

task	dc = t_dc

# Operand Structure (parser stack)
define	YYOPLEN		2		# size of operand structure
define	OPTYPE		Memi[$1]	# operand datatype
define	OPVALI		Memi[$1+1]	# integer value of operand
define	OPVALR		Memr[$1+1]	# real value of operand

%}

%token	CONST LETTER YYEOF

%left	'+' '-'
%left	'*' '/'
%left	UMINUS

%%

prog	:	# Empty
	|	prog stmt eost {
			return (OK)
		    }
	|	YYEOF {
			return (EOF)
		    }
	|	prog error '\n' {
			yyerrok
		    }
	;

stmt	:	expr {
			# Print the value of an expression.
			if (OPTYPE($1) == TY_INT) {
			    call printf ("%d\n")
				call pargi (OPVALI($1))
			} else {
			    call printf ("%g\n")
				call pargr (OPVALR($1))
			}
		    }
	|	LETTER '=' expr {
			# Set the value of a register (from a-z).
			call putreg (OPVALI($1), $3)
		    }
	;

expr	:	'(' expr ')' {
			YYMOVE ($2, $$)
		    }
	|	expr '+' opnl expr {
			call binop ($1, $4, $$, '+')
		    }
	|	expr '-' opnl expr {
			call binop ($1, $4, $$, '-')
		    }
	|	expr '*' opnl expr {
			call binop ($1, $4, $$, '*')
		    }
	|	expr '/' opnl expr {
			call binop ($1, $4, $$, '/')
		    }
	|	'-' expr %prec UMINUS {
			call unop ($2, $$, '-')
		    }
	|	LETTER {
			call getreg (OPVALI($1), $$)
		    }
	|	CONST
	;

eost	:	';'
	|	'\n'
	;

opnl	:	# Empty
	|	opnl '\n'
	;

%%


# DC -- Main routine for the desk calculator.

procedure t_dc()

bool	debug
int	status
bool	clgetb()
int	yyparse()
extern	yylex()

begin
	debug = clgetb ("debug")

	repeat {
	    status = yyparse (STDIN, debug, yylex)
	    if (status == ERR)
		call eprintf ("syntax error")
	} until (status == EOF)
end


# BINOP -- Perform an arithmetic binary operation on two operands (passed
# by pointer), returning the result in a third.

procedure binop (a, b, c, operation)

pointer	a, b, c			# c = a op b
int	operation		# i.e., '+', '-', etc.
int	i, j, k
real	x, y, z

begin
	if (OPTYPE(a) == TY_INT && OPTYPE(b) == TY_INT) {
	    # Both operands are of type int, so return an integer result.

	    i = OPVALI(a)
	    j = OPVALI(b)

	    switch (operation) {
	    case '+':
		k = i + j
	    case '-':
		k = i - j
	    case '*':
		k = i * j
	    case '/':
		k = i / j
	    default:
		call error (1, "unknown binary operator")
	    }
	    OPVALI(c) = k
	    OPTYPE(c) = TY_INT

	} else {
	    # At least one of the two operands is a real.  Perform the
	    # calculation in type real, producing a real result.

	    if (OPTYPE(a) == TY_INT)
		x = OPVALI(a)
	    else
		x = OPVALR(a)
	    if (OPTYPE(b) == TY_INT)
		y = OPVALI(b)
	    else
		y = OPVALR(b)

	    switch (operation) {
	    case '+':
		z = x + y
	    case '-':
		z = x - y
	    case '*':
		z = x * y
	    case '/':
		z = x / y
	    default:
		call error (1, "unknown binary operator")
	    }

	    OPVALR(c) = z
	    OPTYPE(c) = TY_REAL
	}
end


# UNOP -- Perform a unary operation.  Since there is only one operand, the
# datatype does not change.

procedure unop (a, b, operation)

pointer	a, b
int	operation

begin
	OPTYPE(b) = OPTYPE(a)

	switch (operation) {
	case '-':
	    switch (OPTYPE(a)) {
	    case TY_INT:
		OPVALI(b) = -OPVALI(a)
	    case TY_REAL:
		OPVALR(b) = -OPVALR(a)
	    }
	default:
	    call error (2, "unknown unary operator")
	}
end


# GETREG, PUTREG -- Fetch or store the contents of a register variable.
# Registers are referred to by letter, A-Z or a-z.

define	MAXREG		('z'-'a'+1)


procedure getreg (regchar, op)

int	regchar
pointer	op

bool	store
int	regbuf[MAXREG*YYOPLEN]
int	reg, offset

begin
	store = false
	goto 10

entry	putreg (regchar, op)
	store = true

	# Compute offset into storage.  Structures are stored in buffer
	# by a binary copy, knowing only the length of the structure.
10	if (IS_UPPER(regchar))
	    reg = regchar - 'A' + 1
	else
	    reg = regchar - 'a' + 1
	reg = max(1, min(MAXREG, reg))
	offset = (reg-1) * YYOPLEN + 1

	# Copy the operand structure either in or out.
	if (store)
	    call amovi (Memi[op], regbuf[offset], YYOPLEN)
	else
	    call amovi (regbuf[offset], Memi[op], YYOPLEN)
end


# YYLEX -- Lexical input routine.  Return next token from the input
# stream.  Recognized tokens are CONST (numeric constants), LETTER,
# and the operator characters.

int procedure yylex (fd, yylval)

int	fd
pointer	yylval
char	ch, lbuf[SZ_LINE]
int	ip, nchars, token, junk
double	dval
int	lexnum(), getline(), gctod()
data	ip /0/

begin
	# Fetch a nonempty input line, or advance to start of next token
	# if within a line.  Newline is a token.
	repeat {
	    if (ip <= 0 || lbuf[ip] == EOS) {
		if (getline (fd, lbuf) == EOF) {
		    ip = 0
		    return (YYEOF)
		} else
		    ip = 1
	    }
	    while (IS_WHITE (lbuf[ip]))
		ip = ip + 1
	} until (lbuf[ip] != EOS)

	# Determine type of token.  If numeric constant, convert to binary
	# and return value in op structure (yylval).  If letter (register
	# variable) return value and advance input one char.  If any other
	# character, return char itself as the token, and advance input one
	# character.

	if (IS_DIGIT (lbuf[ip]))
	    token = lexnum (lbuf, ip, nchars)
	else
	    token = LEX_NONNUM

	switch (token) {
	case LEX_OCTAL, LEX_DECIMAL, LEX_HEX:
	    junk = gctod (lbuf, ip, dval)
	    OPTYPE(yylval) = TY_INT
	    OPVALI(yylval) = int (dval)
	    return (CONST)

	case LEX_REAL:
	    junk = gctod (lbuf, ip, dval)
	    OPTYPE(yylval) = TY_REAL
	    OPVALR(yylval) = dval
	    return (CONST)

	default:
	    ch = lbuf[ip]
	    ip = ip + 1
	    if (IS_ALPHA (ch)) {
		OPTYPE(yylval) = LETTER
		OPVALI(yylval) = ch
		return (LETTER)
	    } else {
		OPTYPE(yylval) = ch
		return (OPTYPE(yylval))
	    }
	}
end
