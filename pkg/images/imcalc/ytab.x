# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imcalc.h"
define	yyparse	imc_parse
define	EQUALS		257
define	SEMICOLON		258
define	NEWLINE		259
define	CONSTANT		260
define	IDENTIFIER		261
define	YYEOF		262
define	NOT		263
define	PLUS		264
define	MINUS		265
define	STAR		266
define	SLASH		267
define	EXPON		268
define	QUEST		269
define	COLON		270
define	LT		271
define	GT		272
define	LE		273
define	EQ		274
define	NE		275
define	AND		276
define	OR		277
define	GE		278
define	UMINUS		279
define	yyclearin	yychar = -1
define	yyerrok		yyerrflag = 0
define	YYMOVE		call amovi (Memi[$1], Memi[$2], YYOPLEN)
define	YYERRCODE	256

# line 158 "gram.y"

define	YYNPROD		32
define	YYLAST		270

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
int	uups, uuchar, i
pointer	valsp, uuop, uupv, uuval, uulval
int	yygtok(), yygval(), yystat()
errchk	salloc, yylex


include	"imcalc.com"
short	yyexca[70]
data	(yyexca(i),i=  1,  8)	/  -1,   1,   0,  -1,  -2,   0,  -1,  61/
data	(yyexca(i),i=  9, 16)	/ 271,   0, 272,   0, 273,   0, 278,   0/
data	(yyexca(i),i= 17, 24)	/  -2,  17,  -1,  62, 271,   0, 272,   0/
data	(yyexca(i),i= 25, 32)	/ 273,   0, 278,   0,  -2,  18,  -1,  63/
data	(yyexca(i),i= 33, 40)	/ 271,   0, 272,   0, 273,   0, 278,   0/
data	(yyexca(i),i= 41, 48)	/  -2,  19,  -1,  64, 271,   0, 272,   0/
data	(yyexca(i),i= 49, 56)	/ 273,   0, 278,   0,  -2,  20,  -1,  65/
data	(yyexca(i),i= 57, 64)	/ 274,   0, 275,   0,  -2,  21,  -1,  66/
data	(yyexca(i),i= 65, 70)	/ 274,   0, 275,   0,  -2,  22/
short	yyact[270]
data	(yyact(i),i=  1,  8)	/  19,  20,  21,  22,  23,  32,  70,  26/
data	(yyact(i),i=  9, 16)	/  27,  28,  30,  31,  24,  25,  29,  53/
data	(yyact(i),i= 17, 24)	/  19,  20,  21,  22,  23,  32,  23,  26/
data	(yyact(i),i= 25, 32)	/  27,  28,  30,  31,  24,  25,  29,  19/
data	(yyact(i),i= 33, 40)	/  20,  21,  22,  23,  18,   9,  26,  27/
data	(yyact(i),i= 41, 48)	/  28,  30,  31,  24,  18,  29,  19,  20/
data	(yyact(i),i= 49, 56)	/  21,  22,  23,   5,   3,  26,  27,  28/
data	(yyact(i),i= 57, 64)	/  30,  31,   7,   8,  29,  19,  20,  21/
data	(yyact(i),i= 65, 72)	/  22,  23,  11,  10,  26,  27,  28,  21/
data	(yyact(i),i= 73, 80)	/  22,  23,  35,  29,  19,  20,  21,  22/
data	(yyact(i),i= 81, 88)	/  23,  68,  33,  34,  69,  36,  51,  37/
data	(yyact(i),i= 89, 96)	/  38,  39,  40,  41,  42,  43,  44,  45/
data	(yyact(i),i= 97,104)	/  46,  47,  48,  49,  50,   6,  52,   2/
data	(yyact(i),i=105,112)	/  54,  55,  56,  57,  58,  59,  60,  61/
data	(yyact(i),i=113,120)	/  62,  63,  64,  65,  66,  67,  13,   4/
data	(yyact(i),i=121,128)	/   1,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=129,136)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=137,144)	/  71,   0,  72,  73,   0,   0,   0,   0/
data	(yyact(i),i=145,152)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=153,160)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=161,168)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=169,176)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=177,184)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=185,192)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=193,200)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=201,208)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=209,216)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=217,224)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=225,232)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yyact(i),i=233,240)	/   0,   0,   0,   0,   0,   0,  19,  20/
data	(yyact(i),i=241,248)	/  21,  22,  23,  32,   0,  26,  27,  28/
data	(yyact(i),i=249,256)	/  30,  31,  24,  25,  29,   0,   0,  12/
data	(yyact(i),i=257,264)	/  14,  17,   0,  16,   0,  15,   0,   0/
data	(yyact(i),i=265,270)	/  14,  17,   0,  16,   0,  15/
short	yypact[74]
data	(yypact(i),i=  1,  8)	/-210,-1000,-200,-1000,-220,-1000,-1000,-1000/
data	(yypact(i),i=  9, 16)	/-1000,-1000,  -4,-248,-1000,-1000,-1000,   4/
data	(yypact(i),i= 17, 24)	/   4,  34,   4,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i= 25, 32)	/-1000,-1000,-1000,-1000,-1000,-1000,-1000,-1000/
data	(yypact(i),i= 33, 40)	/-1000,-1000,-1000,   4, -26,  -4,  -4,  -4/
data	(yypact(i),i= 41, 48)	/  -4,  -4,  -4,  -4,  -4,  -4,  -4,  -4/
data	(yypact(i),i= 49, 56)	/  -4,  -4,  -4,  40,-248,-1000,-195,-195/
data	(yypact(i),i= 57, 64)	/-246,-246,-1000,-218,-233,-188,-188,-188/
data	(yypact(i),i= 65, 72)	/-188,-203,-203,-264,-1000,   4,-1000,-248/
data	(yypact(i),i= 73, 74)	/  -4,-248/
short	yypgo[8]
data	(yypgo(i),i=  1,  8)	/   0, 120, 103, 101, 118,  67,  66,  86/
short	yyr1[32]
data	(yyr1(i),i=  1,  8)	/   0,   1,   1,   2,   3,   3,   6,   6/
data	(yyr1(i),i=  9, 16)	/   6,   6,   6,   6,   6,   6,   6,   6/
data	(yyr1(i),i= 17, 24)	/   6,   6,   6,   6,   6,   6,   6,   6/
data	(yyr1(i),i= 25, 32)	/   6,   6,   7,   7,   7,   4,   5,   5/
short	yyr2[32]
data	(yyr2(i),i=  1,  8)	/   0,   2,   1,   4,   1,   1,   1,   1/
data	(yyr2(i),i=  9, 16)	/   2,   2,   4,   4,   4,   4,   4,   4/
data	(yyr2(i),i= 17, 24)	/   4,   4,   4,   4,   4,   4,   4,   7/
data	(yyr2(i),i= 25, 32)	/   4,   3,   0,   1,   3,   1,   0,   2/
short	yychk[74]
data	(yychk(i),i=  1,  8)	/-1000,  -1,  -2, 262,  -4, 261,  -3, 258/
data	(yychk(i),i=  9, 16)	/ 259, 257,  -5,  -6, 259,  -4, 260, 265/
data	(yychk(i),i= 17, 24)	/ 263, 261,  40, 264, 265, 266, 267, 268/
data	(yychk(i),i= 25, 32)	/ 276, 277, 271, 272, 273, 278, 274, 275/
data	(yychk(i),i= 33, 40)	/ 269,  -6,  -6,  40,  -6,  -5,  -5,  -5/
data	(yychk(i),i= 41, 48)	/  -5,  -5,  -5,  -5,  -5,  -5,  -5,  -5/
data	(yychk(i),i= 49, 56)	/  -5,  -5,  -5,  -7,  -6,  41,  -6,  -6/
data	(yychk(i),i= 57, 64)	/  -6,  -6,  -6,  -6,  -6,  -6,  -6,  -6/
data	(yychk(i),i= 65, 72)	/  -6,  -6,  -6,  -6,  41,  44, 270,  -6/
data	(yychk(i),i= 73, 74)	/  -5,  -6/
short	yydef[74]
data	(yydef(i),i=  1,  8)	/   0,  -2,   0,   2,   0,  29,   1,   4/
data	(yydef(i),i=  9, 16)	/   5,  30,   0,   3,  31,   6,   7,   0/
data	(yydef(i),i= 17, 24)	/   0,  29,   0,  30,  30,  30,  30,  30/
data	(yydef(i),i= 25, 32)	/  30,  30,  30,  30,  30,  30,  30,  30/
data	(yydef(i),i= 33, 40)	/  30,   8,   9,  26,   0,   0,   0,   0/
data	(yydef(i),i= 41, 48)	/   0,   0,   0,   0,   0,   0,   0,   0/
data	(yydef(i),i= 49, 56)	/   0,   0,   0,   0,  27,  25,  10,  11/
data	(yydef(i),i= 57, 64)	/  12,  13,  14,  15,  16,  -2,  -2,  -2/
data	(yydef(i),i= 65, 72)	/  -2,  -2,  -2,   0,  24,   0,  30,  28/
data	(yydef(i),i= 73, 74)	/   0,  23/

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
	    
case 1:
# line 35 "gram.y"
{
			return (OK)
		    }
case 2:
# line 38 "gram.y"
{
			return (EOF)
		    }
case 3:
# line 43 "gram.y"
{
			# Put a line to the output image.
			call imc_store (O_VALC(yypvt-3*YYOPLEN), yypvt, yyval)
		    }
case 6:
# line 53 "gram.y"
{
			# Load the next line of an input image.
			call imc_load (O_VALC(yypvt), yyval)
		    }
case 7:
# line 57 "gram.y"
{
			# Numeric constant.
			call imc_putconst (yypvt, yyval)
		    }
case 8:
# line 61 "gram.y"
{
			# Unary arithmetic minus.
			call imc_unop (OP_MINUS, yypvt, yyval)
		    }
case 9:
# line 65 "gram.y"
{
			# Boolean not.
			call imc_unop (OP_NOT, yypvt, yyval)
		    }
case 10:
# line 69 "gram.y"
{
			# Addition.
			call imc_binop (OP_ADD, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 11:
# line 73 "gram.y"
{
			# Subtraction.
			call imc_binop (OP_SUB, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 12:
# line 77 "gram.y"
{
			# Multiplication.
			call imc_binop (OP_MUL, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 13:
# line 81 "gram.y"
{
			# Division.
			call imc_binop (OP_DIV, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 14:
# line 85 "gram.y"
{
			# Exponentiation.
			call imc_binop (OP_POW, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 15:
# line 89 "gram.y"
{
			# Boolean and.
			call imc_boolean (OP_AND, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 16:
# line 93 "gram.y"
{
			# Boolean or.
			call imc_boolean (OP_OR, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 17:
# line 97 "gram.y"
{
			# Boolean less than.
			call imc_boolean (OP_LT, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 18:
# line 101 "gram.y"
{
			# Boolean greater than.
			call imc_boolean (OP_GT, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 19:
# line 105 "gram.y"
{
			# Boolean less than or equal.
			call imc_boolean (OP_LE, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 20:
# line 109 "gram.y"
{
			# Boolean greater than or equal.
			call imc_boolean (OP_GE, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 21:
# line 113 "gram.y"
{
			# Boolean equal.
			call imc_boolean (OP_EQ, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 22:
# line 117 "gram.y"
{
			# Boolean not equal.
			call imc_boolean (OP_NE, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 23:
# line 121 "gram.y"
{
			# Conditional expression.
			call imc_quest (yypvt-6*YYOPLEN, yypvt-3*YYOPLEN, yypvt, yyval)
		    }
case 24:
# line 125 "gram.y"
{
			# Function call.
			call imc_call (OP_VALC(yypvt-3*YYOPLEN), OP_VALI(yypvt-YYOPLEN), yyval)
		    }
case 25:
# line 129 "gram.y"
{
			YYMOVE (yypvt-YYOPLEN, yyval)
		    }
case 26:
# line 134 "gram.y"
{
			# Empty.
			call imc_startarglist (NULL, yyval)
		    }
case 27:
# line 138 "gram.y"
{
			# First arg; start a nonnull list.
			call imc_startarglist (yypvt, yyval)
		    }
case 28:
# line 142 "gram.y"
{
			# Add an argument to an existing list.
			call imc_addarg (yypvt-2*YYOPLEN, yypvt, yyval)
		    }
case 29:
# line 148 "gram.y"
{
			# Image or image section.
			YYMOVE (yypvt, yyval)
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
