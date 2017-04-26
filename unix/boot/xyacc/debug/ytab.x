# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<lexnum.h>

define	YYMAXDEPTH	150		# length of parser stack

task	dc = t_dc

# Operand Structure (parser stack)
define	YYOPLEN		2		# size of operand structure
define	OPTYPE		Memi[$1]	# operand datatype
define	OPVALI		Memi[$1+1]	# integer value of operand
define	OPVALR		Memr[$1+1]	# real value of operand

define	CONST		257
define	LETTER		258
define	YYEOF		259
define	UMINUS		260
define	yyclearin	yychar = -1
define	yyerrok		yyerrflag = 0
define	YYMOVE		call amovi (Memi[$1], Memi[$2], YYOPLEN)
define	YYERRCODE	256

# line 89 "dc.y"



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

procedure binop (a, b, c, opchar)

pointer	a, b, c		# c = a op b
char	opchar		# i.e., '+', '-', etc.
int	i, j, k
real	x, y, z

begin
	if (OPTYPE(a) == TY_INT && OPTYPE(b) == TY_INT) {
	    # Both operands are of type int, so return an integer result.

	    i = OPVALI(a)
	    j = OPVALI(b)

	    switch (opchar) {
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

	    switch (opchar) {
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

procedure unop (a, b, opchar)

pointer	a, b
char	opchar

begin
	OPTYPE(b) = OPTYPE(a)

	switch (opchar) {
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

char	regchar
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
define	YYNPROD		19
define	YYLAST		249

# Parser for yacc output, translated to the IRAF SPP language.  The contents
# of this file form the bulk of the source of the parser produced by Yacc.
# Yacc recognizes several macros in the yaccpar input source and replaces
# them as follows:
#	A	user suppled "global" definitions and declarations
# 	B	parser tables
# 	C	user supplied actions (reductions)
# The remainder of the yaccpar code is not changed.

define	yystack_	10		# statement labels for gotos
define	yynewstate_	20
define	yydefault_	30
define	yyerrlab_	40
define	yyabort_	50

define	YYFLAG		(-1000)		# defs used in user actions
define	YYERROR		goto yyerrlab_
define	YYACCEPT	return (OK)
define	YYABORT		return (ERR)


# YYPARSE -- Parse the input stream, returning OK if the source is
# syntactically acceptable (i.e., if compilation is successful),
# otherwise ERR.  The parameters YYMAXDEPTH and YYOPLEN must be
# supplied by the caller in the %{ ... %} section of the Yacc source.
# The token value stack is a dynamically allocated array of operand
# structures, with the length and makeup of the operand structure being
# application dependent.

int procedure yyparse (fd, yydebug, yylex)

int	fd			# stream to be parsed
bool	yydebug			# print debugging information?
int	yylex()			# user-supplied lexical input function
extern	yylex()

short	yys[YYMAXDEPTH]		# parser stack -- stacks tokens
pointer	yyv			# pointer to token value stack
pointer	yyval			# value returned by action
pointer	yylval			# value of token
int	yyps			# token stack pointer
pointer	yypv			# value stack pointer
int	yychar			# current input token number
int	yyerrflag		# error recovery flag
int	yynerrs			# number of errors

short	yyj, yym		# internal variables
pointer	sp, yypvt
short	yystate, yyn
int	yyxi

int	toksp			# declarations for status entry points
int	uups, uuchar
pointer	valsp, uuop, uupv, uuval, uulval
int	yygtok(), yygval(), yystat()
errchk	salloc, yylex

short	yyexca[6]
data	(yyexca(i),i=  1,  6)	/  -1,   1,   0,  -1,  -2,   0/
short	yyact[249]
data	(yyact(i),i=  1,  8)	/  29,   7,   2,   7,  18,  12,   8,  16/
data	(yyact(i),i=  9, 16)	/   8,  27,  16,  14,  17,  15,   5,  17/
data	(yyact(i),i= 17, 24)	/  16,  14,  13,  15,  10,  17,  19,  21/
data	(yyact(i),i= 25, 32)	/   3,  22,   1,   0,   0,   0,   7,   0/
data	(yyact(i),i= 33, 40)	/   0,  26,   0,   8,   0,  28,  30,  31/
data	(yyact(i),i= 41, 48)	/  32,  23,  24,  25,   0,   0,   0,   0/
data	(yyact(i),i= 49, 56)	/   0,   0,   0,   0,   0,   0,  11,   0/
data	(yyact(i),i= 57, 64)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i= 65, 72)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i= 73, 80)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i= 81, 88)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i= 89, 96)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i= 97,104)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=105,112)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=113,120)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=121,128)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=129,136)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=137,144)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=145,152)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=153,160)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=161,168)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=169,176)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=177,184)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=185,192)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=193,200)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=201,208)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=209,216)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=217,224)	/   0,   4,   9,   6,   9,  20,   0,   0/
data	(yyact(i),i=225,232)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=233,240)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=241,248)	/   0,   0,   0,   0,   0,   0,   0,   9/
data	(yyact(i),i=249,249)	/  20/
short	yypact[33]
data	(yypact(i),i=  1,  8)	/-257, -39,-1000,  -5,   8, -26, -57, -37/
data	(yypact(i),i=  9, 16)	/ -37,-1000,-1000,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i= 17, 24)	/-1000,-1000, -37, -32,-1000,-1000, -10, -10/
data	(yypact(i),i= 25, 32)	/ -10, -10, -26,-1000, -35,-1000, -35,-1000/
data	(yypact(i),i= 33, 33)	/-1000/
short	yypgo[6]
data	(yypgo(i),i=  1,  6)	/   0,  26,  24,  20,  14,  25/
short	yyr1[19]
data	(yyr1(i),i=  1,  8)	/   0,   1,   1,   1,   1,   2,   2,   4/
data	(yyr1(i),i=  9, 16)	/   4,   4,   4,   4,   4,   4,   4,   3/
data	(yyr1(i),i= 17, 19)	/   3,   5,   5/
short	yyr2[19]
data	(yyr2(i),i=  1,  8)	/   0,   0,   3,   1,   3,   1,   3,   3/
data	(yyr2(i),i=  9, 16)	/   4,   4,   4,   4,   2,   1,   1,   1/
data	(yyr2(i),i= 17, 19)	/   1,   0,   2/
short	yychk[33]
data	(yychk(i),i=  1,  8)	/-1000,  -1, 259,  -2, 256,  -4, 258,  40/
data	(yychk(i),i=  9, 16)	/  45, 257,  -3,  59,  10,  10,  43,  45/
data	(yychk(i),i= 17, 24)	/  42,  47,  61,  -4, 258,  -4,  -5,  -5/
data	(yychk(i),i= 25, 32)	/  -5,  -5,  -4,  41,  -4,  10,  -4,  -4/
data	(yychk(i),i= 33, 33)	/  -4/
short	yydef[33]
data	(yydef(i),i=  1,  8)	/   1,  -2,   3,   0,   0,   5,  13,   0/
data	(yydef(i),i=  9, 16)	/   0,  14,   2,  15,  16,   4,  17,  17/
data	(yydef(i),i= 17, 24)	/  17,  17,   0,   0,  13,  12,   0,   0/
data	(yydef(i),i= 25, 32)	/   0,   0,   6,   7,   8,  18,   9,  10/
data	(yydef(i),i= 33, 33)	/  11/

begin
	call smark (sp)
	call salloc (yyv, (YYMAXDEPTH+2) * YYOPLEN, TY_STRUCT)

	# Initialization.  The first element of the dynamically allocated
	# token value stack (yyv) is used for yyval, the second for yylval,
	# and the actual stack starts with the third element.

	yystate = 0
	yychar = -1
	yynerrs = 0
	yyerrflag = 0
	yyps = 0
	yyval = yyv
	yylval = yyv + YYOPLEN
	yypv = yylval

yystack_
	# SHIFT -- Put a state and value onto the stack.  The token and
	# value stacks are logically the same stack, implemented as two
	# separate arrays.

	if (yydebug) {
	    call printf ("state %d, char 0%o\n")
		call pargs (yystate)
		call pargi (yychar)
	}
	yyps = yyps + 1
	yypv = yypv + YYOPLEN
	if (yyps > YYMAXDEPTH) {
	    call sfree (sp)
	    call eprintf ("yacc stack overflow\n")
	    return (ERR)
	}
	yys[yyps] = yystate
	YYMOVE (yyval, yypv)

yynewstate_
	# Process the new state.
	yyn = yypact[yystate+1]

	if (yyn <= YYFLAG)
	    goto yydefault_			# simple state

	# The variable "yychar" is the lookahead token.
	if (yychar < 0) {
	    yychar = yylex (fd, yylval)
	    if (yychar < 0)
		yychar = 0
	}
	yyn = yyn + yychar
	if (yyn < 0 || yyn >= YYLAST)
	    goto yydefault_

	yyn = yyact[yyn+1]
	if (yychk[yyn+1] == yychar) {		# valid shift
	    yychar = -1
	    YYMOVE (yylval, yyval)
	    yystate = yyn
	    if (yyerrflag > 0)
		yyerrflag = yyerrflag - 1
	    goto yystack_
	}

yydefault_
	# Default state action.

	yyn = yydef[yystate+1]
	if (yyn == -2) {
	    if (yychar < 0) {
		yychar = yylex (fd, yylval)
		if (yychar < 0)
		    yychar = 0
	    }

	    # Look through exception table.
	    yyxi = 1
	    while ((yyexca[yyxi] != (-1)) || (yyexca[yyxi+1] != yystate))
		yyxi = yyxi + 2
	    for (yyxi=yyxi+2;  yyexca[yyxi] >= 0;  yyxi=yyxi+2) {
		if (yyexca[yyxi] == yychar)
		    break
	    }

	    yyn = yyexca[yyxi+1]
	    if (yyn < 0) {
		call sfree (sp)
		return (OK)			# ACCEPT -- all done
	    }
	}


	# SYNTAX ERROR -- resume parsing if possible.

	if (yyn == 0) {
	    switch (yyerrflag) {
	    case 0, 1, 2:
		if (yyerrflag == 0) {		# brand new error
		    call eprintf ("syntax error\n")
yyerrlab_
		    yynerrs = yynerrs + 1
		    # fall through...
		}

	    # case 1:
	    # case 2: incompletely recovered error ... try again
		yyerrflag = 3

		# Find a state where "error" is a legal shift action.
		while (yyps >= 1) {
		    yyn = yypact[yys[yyps]+1] + YYERRCODE
		    if ((yyn >= 0) && (yyn < YYLAST) &&
			(yychk[yyact[yyn+1]+1] == YYERRCODE)) {
			    # Simulate a shift of "error".
			    yystate = yyact[yyn+1]
			    goto yystack_
		    }
		    yyn = yypact[yys[yyps]+1]

		    # The current yyps has no shift on "error", pop stack.
		    if (yydebug) {
			call printf ("error recovery pops state %d, ")
			    call pargs (yys[yyps])
			call printf ("uncovers %d\n")
			    call pargs (yys[yyps-1])
		    }
		    yyps = yyps - 1
		    yypv = yypv - YYOPLEN
		}

		# ABORT -- There is no state on the stack with an error shift.
yyabort_
		call sfree (sp)
		return (ERR)


	    case 3: # No shift yet; clobber input char.

		if (yydebug) {
		    call printf ("error recovery discards char %d\n")
			call pargi (yychar)
		}

		if (yychar == 0)
		    goto yyabort_		# don't discard EOF, quit
		yychar = -1
		goto yynewstate_		# try again in the same state
	    }
	}


	# REDUCE -- Reduction by production yyn.

	if (yydebug) {
	    call printf ("reduce %d\n")
		call pargs (yyn)
	}
	yyps  = yyps - yyr2[yyn+1]
	yypvt = yypv
	yypv  = yypv - yyr2[yyn+1] * YYOPLEN
	YYMOVE (yypv + YYOPLEN, yyval)
	yym   = yyn

	# Consult goto table to find next state.
	yyn = yyr1[yyn+1]
	yyj = yypgo[yyn+1] + yys[yyps] + 1
	if (yyj >= YYLAST)
	    yystate = yyact[yypgo[yyn+1]+1]
	else {
	    yystate = yyact[yyj+1]
	    if (yychk[yystate+1] != -yyn)
		yystate = yyact[yypgo[yyn+1]+1]
	}

	# Perform action associated with the grammar rule, if any.
	switch (yym) {
	    
case 2:
# line 30 "dc.y"
{
			return (OK)
		    }
case 3:
# line 33 "dc.y"
{
			return (EOF)
		    }
case 4:
# line 36 "dc.y"
{
			yyerrok
		    }
case 5:
# line 41 "dc.y"
{
			# Print the value of an expression.
			if (OPTYPE(yypvt) == TY_INT) {
			    call printf ("%d\n")
				call pargi (OPVALI(yypvt))
			} else {
			    call printf ("%g\n")
				call pargr (OPVALR(yypvt))
			}
		    }
case 6:
# line 51 "dc.y"
{
			# Set the value of a register (from a-z).
			call putreg (char(OPVALI(yypvt-2*YYOPLEN)), yypvt)
		    }
case 7:
# line 57 "dc.y"
{
			YYMOVE (yypvt-YYOPLEN, yyval)
		    }
case 8:
# line 60 "dc.y"
{
			call binop (yypvt-3*YYOPLEN, yypvt, yyval, '+')
		    }
case 9:
# line 63 "dc.y"
{
			call binop (yypvt-3*YYOPLEN, yypvt, yyval, '-')
		    }
case 10:
# line 66 "dc.y"
{
			call binop (yypvt-3*YYOPLEN, yypvt, yyval, '*')
		    }
case 11:
# line 69 "dc.y"
{
			call binop (yypvt-3*YYOPLEN, yypvt, yyval, '/')
		    }
case 12:
# line 72 "dc.y"
{
			call unop (yypvt, yyval, '-')
		    }
case 13:
# line 75 "dc.y"
{
			call getreg (char(OPVALI(yypvt)), yyval)
		    }	}

	goto yystack_				# stack new state and value


# The following entry points are provided so that lexical routines
# and actions may get information of the parser status, i.e., how
# deep is the stack, what tokens are currently stacked, and so on.
# Conceivably there could be reentrancy problems here...

	# YYGTOK -- Read an element from the token stack.
entry	yygtok (toksp)
	return (yys[toksp])

	# YYGVAL -- Read an element from the value stack.
entry	yygval (valsp, uuop)
	YYMOVE (valsp, uuop)
	return (OPTYPE(uuop))

	# YYSTAT -- Return parser state variables.  The code for the token
	# currently on top of the stack is returned as the function value.

entry	yystat (uups, uupv, uuchar, uuval, uulval)
	uups = yyps
	uupv = yypv
	uuchar = yychar
	YYMOVE (yyval, uuval)
	YYMOVE (yylval, uulval)

	if (yyps <= 0)
	    return (0)
	else
	    return (yys[yyps])
end
