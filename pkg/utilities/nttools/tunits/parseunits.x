include	<ctype.h>

#* B.Simon	??		original
#  Phil Hodge	12-Jul-2005	add 'bool yydebug' and 'int get_token()' to
#				parse_units

define	YYMAXDEPTH	32
define	YYOPLEN		1
define	yyparse		unit_parse

define	SZ_SHORTSTR	31

define	Y_WRONG		257
define	Y_DONE		258
define	Y_LPAR		259
define	Y_RPAR		260
define	Y_CU		261
define	Y_SQ		262
define	Y_ID		263
define	Y_NUM		264
define	Y_DIV		265
define	Y_MUL		266
define	Y_POW		267
define	yyclearin	yychar = -1
define	yyerrok		yyerrflag = 0
define	YYMOVE		call amovi (Memi[$1], Memi[$2], YYOPLEN)
define	YYERRCODE	256

# line 159 "parseunits.y"


# PARSE_UNITS -- Parse a units string into the internal format

pointer procedure parse_units (ab, units)

pointer	ab		# i: abbreviation hash table
char	units[ARB]	# i: expression to be parsed
#--
include "parseunits.com"

int	len, fd
pointer	sp

string	syntax	"Syntax error in units"

bool	yydebug
int	strlen(), stropen(), yyparse()
int	get_token()
extern	get_token

begin
	len = strlen (units) + 1
	fd = stropen (units, len, READ_ONLY)

	call smark (sp)
	call salloc (tokbuf, SZ_FNAME, TY_CHAR)

	debug = NO
	yydebug = (debug == YES)
	nxttok = 0
	abrev = ab
	tun = NULL	

	if (yyparse (fd, yydebug, get_token) == ERR)
	    call tuniterr (syntax, units)

	call close (fd)
	call sfree (sp)
	return (tun)
end

# GET_TOKEN -- Retrieve next token from units string

int procedure get_token (fd, value)

int	fd		# i: File containing expression to be lexed
pointer	value		# o: Address on parse stack to store token
#--
include	"parseunits.com"

char	ch
int	type, index, powers[4]
pointer	sp, typename, token

string	pownames  "sq,square,cu,cubic"
data	powers    / Y_SQ, Y_SQ, Y_CU, Y_CU /

bool	streq()
int	getc(), word_match()

begin
	call smark (sp)
	call salloc (typename, SZ_FNAME, TY_CHAR)

	token = tokbuf + nxttok
	Memi[value] = token

	repeat {
	    ch = getc (fd, ch)
	} until (ch != ' ' && ch != '\t')

	if (ch == EOF) {
	    type = Y_DONE
	    call strcpy ("END", Memc[typename], SZ_FNAME)

	} else if (IS_ALPHA (ch)) {
	    type = Y_ID
	    call strcpy ("IDENT", Memc[typename], SZ_FNAME)

	    while (IS_ALPHA (ch)) {
		Memc[tokbuf+nxttok] = ch
		nxttok = nxttok + 1
		ch = getc (fd, ch)
	    }
	    call ungetc (fd, ch)

	    Memc[tokbuf+nxttok] = EOS
	    index = word_match (Memc[token], pownames)

	    if (index > 0) {
		type = powers[index]
		call strcpy ("POWER", Memc[typename], SZ_FNAME)

	    } else if (streq (Memc[token], "per")) {
		type = Y_DIV
		call strcpy ("DIV", Memc[typename], SZ_FNAME)
	    }

	} else if (ch == '-' || IS_DIGIT (ch)) {
	    type = Y_NUM
	    call strcpy ("NUMBER", Memc[typename], SZ_FNAME)

	    Memc[tokbuf+nxttok] = ch
	    nxttok = nxttok + 1
	    ch = getc (fd, ch)

	    while (IS_DIGIT (ch)) {
		Memc[tokbuf+nxttok] = ch
		nxttok = nxttok + 1
		ch = getc (fd, ch)
	    }
	    call ungetc (fd, ch)

	} else {
	    Memc[tokbuf+nxttok] = ch
	    nxttok = nxttok + 1

	    switch (ch) {
	    case '*':
		ch = getc (fd, ch)
		if (ch == '*') {
		    type = Y_POW
		    call strcpy ("EXPON", Memc[typename], SZ_FNAME)

		    Memc[tokbuf+nxttok] = ch
		    nxttok = nxttok + 1
		} else {
		    type = Y_MUL
		    call strcpy ("MUL", Memc[typename], SZ_FNAME)

		    call ungetc (fd, ch)
		}

	    case '/':
		type = Y_DIV
		call strcpy ("DIV", Memc[typename], SZ_FNAME)

	    case '^':
		type = Y_POW
		call strcpy ("EXPON", Memc[typename], SZ_FNAME)

	    default:
		type = Y_WRONG
		call strcpy ("ERROR", Memc[typename], SZ_FNAME)
	    }
	}

	Memc[tokbuf+nxttok] = EOS
	nxttok = nxttok + 1

	if (debug == YES) {
	    call eprintf ("Token is %s [%s]\n")
	    if (Memc[token] == EOS)  {
		call pargstr ("EOS")
	    } else {
		call pargstr (Memc[token])
	    }
	    call pargstr (Memc[typename])
	}

	call sfree (sp)
	return (type)
end
define	YYNPROD		15
define	YYLAST		43
# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

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
pointer	yysp, yypvt
short	yystate, yyn
int	yyxi, i
errchk	salloc, yylex


include	"parseunits.com"

char		units[SZ_FNAME]

int		num_unstr()
pointer		mul_unstr(), div_unstr(), pow_unstr(), set_unstr()

short	yyexca[6]
data	(yyexca(i),i=  1,  6)	/  -1,   1,   0,  -1,  -2,   0/
short	yyact[43]
data	(yyact(i),i=  1,  8)	/  23,  10,  11,  13,  13,  12,  11,  13/
data	(yyact(i),i=  9, 16)	/  12,  11,  13,  17,   3,  24,  16,   4/
data	(yyact(i),i= 17, 24)	/  22,   8,   9,   7,   4,  19,   8,   9/
data	(yyact(i),i= 25, 32)	/   7,   8,   9,   7,  18,   2,   6,   5/
data	(yyact(i),i= 33, 40)	/   1,   0,  14,   0,  15,   0,   0,   0/
data	(yyact(i),i= 41, 43)	/   0,  20,  21/
short	yypact[25]
data	(yypact(i),i=  1,  8)	/-244,-1000,-257,-1000,-239,-236,-1000,-253/
data	(yypact(i),i=  9, 16)	/-235,-242,-1000,-239,-239,-248,-260,-1000/
data	(yypact(i),i= 17, 24)	/-251,-1000,-1000,-1000,-263,-264,-1000,-1000/
data	(yypact(i),i= 25, 25)	/-1000/
short	yypgo[5]
data	(yypgo(i),i=  1,  5)	/   0,  32,  29,  31,  30/
short	yyr1[15]
data	(yyr1(i),i=  1,  8)	/   0,   1,   1,   2,   2,   2,   2,   2/
data	(yyr1(i),i=  9, 15)	/   3,   3,   4,   4,   4,   4,   4/
short	yyr2[15]
data	(yyr2(i),i=  1,  8)	/   0,   2,   1,   3,   3,   3,   3,   1/
data	(yyr2(i),i=  9, 15)	/   2,   1,   3,   2,   2,   2,   1/
short	yychk[25]
data	(yychk(i),i=  1,  8)	/-1000,  -1,  -2, 256, 259,  -3,  -4, 263/
data	(yychk(i),i=  9, 16)	/ 261, 262, 258, 266, 265, 267,  -2,  -4/
data	(yychk(i),i= 17, 24)	/ 267, 264, 263, 263,  -2,  -2, 264, 260/
data	(yychk(i),i= 25, 25)	/ 264/
short	yydef[25]
data	(yydef(i),i=  1,  8)	/   0,  -2,   0,   2,   0,   7,   9,  14/
data	(yydef(i),i=  9, 16)	/   0,   0,   1,   0,   0,   0,   0,   8/
data	(yydef(i),i= 17, 24)	/   0,  11,  12,  13,   4,   5,   6,   3/
data	(yydef(i),i= 25, 25)	/  10/

begin
	call smark (yysp)
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
	    call sfree (yysp)
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
		call sfree (yysp)
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
		call sfree (yysp)
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
# line 28 "parseunits.y"
{
		    # Normal exit. Return pointer to units structure
		    if (debug == YES)
			call eprintf ("\n")

		    tun = Memi[yypvt-YYOPLEN]
		    return (OK)
		}
case 2:
# line 36 "parseunits.y"
{
		    # Syntax error
		    if (debug == YES)
			call eprintf ("\n")

		    return (ERR)
		}
case 3:
# line 45 "parseunits.y"
{
		    # Parenthesized expression
		    Memi[yyval] = Memi[yypvt-2*YYOPLEN]
		}
case 4:
# line 49 "parseunits.y"
{
		    # Multiply two units expressions
		    Memi[yyval] = mul_unstr (Memi[yypvt-2*YYOPLEN], Memi[yypvt])
		    call free_unstr (Memi[yypvt-2*YYOPLEN])
		    call free_unstr (Memi[yypvt])

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
case 5:
# line 61 "parseunits.y"
{
		    # Divide two units expressions
		    Memi[yyval] = div_unstr (Memi[yypvt-2*YYOPLEN], Memi[yypvt])
		    call free_unstr (Memi[yypvt-2*YYOPLEN])
		    call free_unstr (Memi[yypvt])

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
case 6:
# line 73 "parseunits.y"
{
		    # Raise expression to a power
		    Memi[yyval] = pow_unstr (Memi[yypvt-2*YYOPLEN], num_unstr (Memc[Memi[yypvt]]))
		    call free_unstr (Memi[yypvt-2*YYOPLEN])

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
case 7:
# line 84 "parseunits.y"
{
		    # List of terms
		    Memi[yyval] = Memi[yypvt]
		}
case 8:
# line 89 "parseunits.y"
{
		    # Implicit multiplication
		    Memi[yyval] = mul_unstr (Memi[yypvt-YYOPLEN], Memi[yypvt])
		    call free_unstr (Memi[yypvt-YYOPLEN])
		    call free_unstr (Memi[yypvt])

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
case 9:
# line 101 "parseunits.y"
{
		    # Simple term
		    Memi[yyval] = Memi[yypvt]
		}
case 10:
# line 106 "parseunits.y"
{
		    # Raise units to a power
		    Memi[yyval] = set_unstr (abrev, Memc[Memi[yypvt-2*YYOPLEN]], 
				    	num_unstr (Memc[Memi[yypvt]]))

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
case 11:
# line 117 "parseunits.y"
{
		    # Implicitly raise to a power
		    Memi[yyval] = set_unstr (abrev, Memc[Memi[yypvt-YYOPLEN]], 
				    num_unstr (Memc[Memi[yypvt]]))

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
case 12:
# line 128 "parseunits.y"
{
		    # Cubic prefix
		    Memi[yyval] = set_unstr (abrev, Memc[Memi[yypvt]], 3)

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
case 13:
# line 138 "parseunits.y"
{
		    # Square prefix
		    Memi[yyval] = set_unstr (abrev, Memc[Memi[yypvt]], 2)

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
case 14:
# line 148 "parseunits.y"
{
		    # Simple name
		    Memi[yyval] = set_unstr (abrev, Memc[Memi[yypvt]], 1)

		    if (debug == YES) {
			call str_unstr (Memi[yyval], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}	}

	goto yystack_				# stack new state and value
end
