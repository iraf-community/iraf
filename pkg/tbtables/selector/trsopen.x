include "trs.h"

#* HISTORY *
#* B.Simon	04-Nov-94	original
#* B.Simon	23-Dec-97	row optimization added
#  Phil Hodge	12-Jul-2005	In trsopen, declare 'debug' to be bool rather
#				than int, and add 'int trslex()'

define	YYMAXDEPTH	64
define	YYOPLEN		1
define	yyparse		trsparse

define	YNIL		257
define	YBANG		258
define	YCOMMA		259
define	YCOLON		260
define	YEQUAL		261
define	YERR		262
define	YEOF		263
define	YLPAR		264
define	YINC		265
define	YNUM		266
define	YPER		267
define	YRPAR		268
define	YSEMI		269
define	YSTR		270
define	yyclearin	yychar = -1
define	yyerrok		yyerrflag = 0
define	YYMOVE		call amovi (Memi[$1], Memi[$2], YYOPLEN)
define	YYERRCODE	256

# line 131 "trsopen.y"


# TRSOPEN -- Compile a table row selection expression 

pointer procedure trsopen (tp, expr)

pointer	tp		# i: table descriptor
char	expr[ARB]	# i: expression to be parsed
#--
include	"trsopen.com"

char	nil
int	fd, jtop
bool	debug
pointer	sp, root

data	nil     / EOS /
data	debug	/ false /
string	syntax  "syntax error"

errchk	stropen, trsparse, trserr

int	trslex()
extern	trslex
pointer	trsinit(), trsparse()
int	stropen(), strlen()

begin
	# Initialize common block used by parser

	tabptr = tp

	call smark (sp)
	call salloc (tokbuf, SZ_TOKEN, TY_CHAR)
	call salloc (errbuf, SZ_LINE, TY_CHAR)
	call salloc (treebuf, SZ_BUFFER, TY_INT)

	call amovkc (nil, Memc[tokbuf], SZ_TOKEN)
	call strcpy (syntax, Memc[errbuf], SZ_LINE)

	itree = 0
	itop = 0
	itok = 0
	ival = 0

	# Convert expression to pseudocode

	fd = stropen (expr, strlen(expr), READ_ONLY)
	pcode = trsinit ()

	root = trsparse (fd, debug, trslex)
	if (root != NULL) {
	    call trsgencode (tp, root, pcode)

	} else {
	    # Error exit: free memory and close open files

	    do jtop = 1, itop
		call close (stack[jtop])
	    call close (fd)

	    call trserr
	}

	# Free memory and close files

	call close (fd)
	call sfree (sp)
	return (pcode)
end

# TRSADDNODE -- Add a node to the binary tree

pointer procedure trsaddnode (oper, lfield, rfield)

int	oper		# i: pseudocode operation
pointer	lfield		# i: left field of operation
pointer	rfield		# i: right field of operation
#--
include	"trsopen.com"

pointer	ptr

string	noroom  "Table row selection expression too complex"

begin
	if (itree >= SZ_BUFFER)
	    call error (1, noroom)

	ptr = treebuf + itree

	TREE_OPER(ptr) = oper
	TREE_INST(ptr) = ERR
	TREE_LEFT(ptr) = lfield
	TREE_RIGHT(ptr) = rfield
	TREE_UP(ptr) = NULL

	if (lfield > 0)
	    TREE_UP(lfield) = ptr

	if (rfield > 0)
	    TREE_UP(rfield) = ptr

	itree = itree + SZ_NODE
	return (ptr)
end

# TRSCNAME -- Retrieve a column pointer, given its name

bool procedure trscname (cname, cptr)

pointer	cname		# i: column name
pointer	cptr		# o: column pointer
#--
include "trsopen.com"

bool	streq()

begin
	call tbcfnd (tabptr, Memc[cname], cptr, 1)

	# "row" is a special filter indicating column number

	if (cptr == NULL) {
	    return (streq (Memc[cname], "row"))
	} else {
	    return (true)
	}

end

# TRSCNUM -- Retrieve a column pointer, given its number

bool procedure trscnum (cnum, cptr)

pointer	cnum		# i: column number
pointer	cptr		# o: column pointer
#--
include "trsopen.com"

int	col
pointer	tbcnum()

begin
	col = Memd[cnum]
	cptr = tbcnum (tabptr, col)

	return (cptr != NULL)
end

# TRSERR -- Print error message from table row selector parser

procedure trserr

#--
include "trsopen.com"

int	nc
pointer	sp, token, errmsg

string	errfmt  "Error in table row selector, %s. Last read: %s"

begin
	# Allocate memory to hold token

	call smark (sp)
	call salloc (token, SZ_TOKEN, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Copy token from token buffer. Since token buffer is maintained 
	# as a queue, the copy is in two parts, after and before the
	# queue pointer.

	nc = 0
	if (Memc[tokbuf+itok] != EOS) {
	    nc = SZ_TOKEN - itok
	    call amovc (Memc[tokbuf+itok], Memc[token], nc)
	}

	itok = mod (itok - 1, SZ_TOKEN)
	call amovc (Memc[tokbuf], Memc[token+nc], itok)

	nc = nc + itok
	Memc[token+nc] = EOS

	# Exit with error message

	call sprintf (Memc[errmsg], SZ_LINE, errfmt)
	call pargstr (Memc[errbuf])
	call pargstr (Memc[token])

	call error (1, Memc[errmsg])
	call sfree (sp)
end

# TRSINIT -- Allocate and intialize the trs pseudocode data structure

pointer procedure trsinit ()

#--
pointer	buf

begin
	call malloc (buf, LEN_TRSBUF, TY_INT)

	TRS_IDENT(buf) = TRS_MAGIC
	TRS_ROWS(buf) = NULL

	call malloc (TRS_CODE(buf), SZ_BUFFER, TY_INT)
	call malloc (TRS_VALUE(buf), SZ_BUFFER, TY_DOUBLE)

	return (buf)
end

# TRSLEX -- Lexical analyzer for table row selector

int procedure trslex (fd, value)

int	fd		# u: file descriptor of currently open file
pointer	value		# i: Pointer to current token value
#--
include	"trsopen.com"

int	type

string	badfile  "bad file name"
string	maxfile  "files nested too deep"

errchk	open
int	open()

begin
	# This procedure sits on top of the procedure that fetches
	# the next token and handles file openings and closings

	type = YNIL
	while (type == YNIL) {
	    call trstok (fd, value, type)

	    if (type == YEOF) {
		# End of file token. Pop deferred file off of stack
		# if no deferred file, return end of file token

		if (itop != 0) {
		    call amovkc (EOS, Memc[tokbuf], SZ_TOKEN)
		    itok = 0

		    call close (fd)
		    fd = stack[itop]
		    itop = itop - 1
		    type = YNIL
		}

	    } else if (type == YINC) {
		# Include file token. Next token should be file name
		# Push current file descriptor on deferred file stack
		# and open new file

		call trstok (fd, value, type)

		if (type != YSTR) {
		    call strcpy (badfile, Memc[errbuf], SZ_LINE)
		    type = YERR

		} else if (itop == MAXSTACK) {
		    call strcpy (maxfile, Memc[errbuf], SZ_LINE)
		    type = YERR

		} else {
		    itop = itop + 1
		    stack[itop] = fd

		    ifnoerr {
			fd = open (Memc[Memi[value]], READ_ONLY, TEXT_FILE)
		    } then {
			call amovkc (EOS, Memc[tokbuf], SZ_TOKEN)
			itok = 0
			type = YNIL

		    } else {
			fd = stack[itop]
			itop =  itop - 1

			call strcpy (badfile, Memc[errbuf], SZ_LINE)
			type = YERR
		    }
		}
	    }
	}

	return (type)
end

# TRSNEXTCH -- Read next character from input stram, save in buffer

int procedure trsnextch (fd, ch)

int	fd		# i: input file descriptor
char	ch		# o: character read from input
#--
include	"trsopen.com"

int	getc()

begin
	Memc[tokbuf+itok] = getc (fd, ch)
	itok = mod (itok+1, SZ_TOKEN)

	return (ch)
end

# TRSTOK -- Read next token from current file

procedure trstok (fd, value, type)

int	fd		# u: file descriptor of currently open file
pointer	value		# i: Pointer to current token value
int	type		# i: Token type
#--
include	"trsopen.com"

char	ch, stop
double	dval
int	stoptype[10]
int	nc, ic, index, delta, size

pointer	sp, token, ptr, valbuf

string	notnum   "not a number"
string	noroom   "expression too complex"
string	nostop   "trailing quote not found"

string	stopset  " ,;:%=!()@"

data	stoptype / YNIL, YCOMMA, YSEMI, YCOLON, YPER, 
		   YEQUAL, YBANG, YLPAR, YRPAR, YINC /

int	trsnextch(),trstrim(), stridx(), ctod()

begin
	# Eat leading whitespace, watch for end of file

	while (trsnextch (fd, ch) <= ' ') {
	    if (ch == EOF) {
		Memi[value] = NULL
		type = YEOF
		return
	    }

	}

	# Check if first character is a delimeter
	# if so, return the corresponding token

	index = stridx (ch, stopset)
	if (index > 0) {
	    Memi[value] = NULL
	    type = stoptype[index] 
	    return
	}

	# The tougher case: token is a number or string
	# First, gather all characters in token

	call smark (sp)
	call salloc (token, SZ_LINE, TY_CHAR)

	if (ch == '\'' || ch == '"') {
	    # First case: token is a quoted string
	    # gather characters until matching quote is found

	    nc = 0
	    stop = ch

	    while (trsnextch (fd, ch) != EOF) {
		if (ch == stop)
		    break

		Memc[token+nc] = ch
		nc = nc + 1
	    }

	    # Handle situation where trailing quote is missing

	    if (ch == EOF) {
		call strcpy (nostop, Memc[errbuf], SZ_LINE)
		Memi[value] = NULL
		type = YERR

		call sfree (sp)
		return
	    }

	} else {
	    # Second case: no quotes
	    # gather characters until delimeter or whitespace

	    nc = 1
	    Memc[token] = ch
	    stop = ' '

	    while (trsnextch (fd, ch) != EOF) {
		if (ch < ' ')
		    ch = ' '

		if (stridx (ch, stopset) > 0) {
		    itok = itok - 1
		    if (itok < 0)
			itok = SZ_TOKEN - 1

		    call ungetc (fd, ch)
		    break
		}

		Memc[token+nc] = ch
		nc = nc + 1
	    }
	}

	Memc[token+nc] = EOS
	nc = trstrim (Memc[token])

	ic = 1
	valbuf = TRS_VALUE(pcode)

	if (stop == ' ' && ctod (Memc[token], ic, dval) == nc) {
	    # Token is a number. Convert it to a double
	    # and store in the value buffer

	    if (ival + 1 >= SZ_BUFFER) {
		call strcpy (noroom, Memc[errbuf], SZ_LINE)
		Memi[value] = NULL
		type = YERR

	    } else {
		ptr = valbuf + ival
		ival = ival + 1

		Memd[ptr] = dval
		Memi[value] = ptr
		type = YNUM
	    }

	} else {
	    # Token is a string. Find how much space it will take
	    # and store in the value buffer

	    size = nc + 1
	    delta = mod (size, SZ_DOUBLE)
	    if (delta != 0)
		size = size + (SZ_DOUBLE - delta)
	    size = size / SZ_DOUBLE

	    if (ival + size >= SZ_BUFFER) {
		call strcpy (noroom, Memc[errbuf], SZ_LINE)
		Memi[value] = NULL
		type = YERR

	    } else {
		ptr = ((valbuf + ival - 1) * SZ_DOUBLE) + 1
		ival = ival + size

		call strcpy (Memc[token], Memc[ptr], size*SZ_DOUBLE-1)
		Memi[value] = ptr
		type = YSTR
	    }
	}

	call sfree (sp)
end

define	YYNPROD		22
define	YYLAST		60
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


include "trsopen.com"

int	cptr

errchk	trslex, trsaddnode
bool	trscname(), trscnum()
pointer	trsaddnode

string	badcol  "column not found"

short	yyexca[6]
data	(yyexca(i),i=  1,  6)	/  -1,   1,   0,  -1,  -2,   0/
short	yyact[60]
data	(yyact(i),i=  1,  8)	/  20,  37,  23,   3,  36,   5,  19,  33/
data	(yyact(i),i=  9, 16)	/  21,  24,   5,   4,  22,   6,   9,  31/
data	(yyact(i),i= 17, 24)	/   4,   7,   6,  32,  26,   9,   7,  17/
data	(yyact(i),i= 25, 32)	/  10,   8,  14,  13,  30,  35,  29,  10/
data	(yyact(i),i= 33, 40)	/  18,   2,   1,   0,   0,   0,  11,  12/
data	(yyact(i),i= 41, 48)	/   0,   0,   0,  15,  16,   0,   0,  25/
data	(yyact(i),i= 49, 56)	/   0,   0,   0,   0,  27,  28,   0,   0/
data	(yyact(i),i= 57, 60)	/   0,   0,   0,  34/
short	yypact[38]
data	(yypact(i),i=  1,  8)	/-253,-1000,-238,-1000,-248,-248,-234,-235/
data	(yypact(i),i=  9, 16)	/-1000,-248,-248,-245,-1000,-258,-258,-1000/
data	(yypact(i),i= 17, 24)	/-1000,-1000,-1000,-258,-258,-230,-232,-251/
data	(yypact(i),i= 25, 32)	/-259,-1000,-258,-239,-1000,-262,-269,-1000/
data	(yypact(i),i= 33, 38)	/-1000,-1000,-1000,-1000,-1000,-1000/
short	yypgo[4]
data	(yypgo(i),i=  1,  4)	/   0,  34,  33,  32/
short	yyr1[22]
data	(yyr1(i),i=  1,  8)	/   0,   1,   1,   2,   2,   2,   2,   2/
data	(yyr1(i),i=  9, 16)	/   2,   2,   3,   3,   3,   3,   3,   3/
data	(yyr1(i),i= 17, 22)	/   3,   3,   3,   3,   3,   3/
short	yyr2[22]
data	(yyr2(i),i=  1,  8)	/   0,   2,   1,   0,   3,   3,   3,   2/
data	(yyr2(i),i=  9, 16)	/   3,   3,   3,   3,   2,   1,   1,   2/
data	(yyr2(i),i= 17, 22)	/   2,   3,   3,   2,   2,   2/
short	yychk[38]
data	(yychk(i),i=  1,  8)	/-1000,  -1,  -2, 256, 264, 258, 266, 270/
data	(yychk(i),i=  9, 16)	/ 263, 259, 269,  -2,  -2, 261, 261,  -2/
data	(yychk(i),i= 17, 24)	/  -2, 268,  -3, 264, 258, 266, 270, 260/
data	(yychk(i),i= 25, 32)	/ 267,  -3, 259,  -3,  -3, 260, 260, 266/
data	(yychk(i),i= 33, 38)	/ 270, 266,  -3, 268, 266, 270/
short	yydef[38]
data	(yydef(i),i=  1,  8)	/   3,  -2,   0,   2,   3,   3,   0,   0/
data	(yydef(i),i=  9, 16)	/   1,   3,   3,   0,   7,   0,   0,   5/
data	(yydef(i),i= 17, 24)	/   6,   4,   8,   0,   0,  13,  14,   0/
data	(yydef(i),i= 25, 32)	/   0,   9,   0,   0,  12,  19,  20,  15/
data	(yydef(i),i= 33, 38)	/  16,  21,  11,  10,  17,  18/

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
# line 34 "trsopen.y"
{
		    # Normal exit. Code a stop instruction.
		    Memi[yyval] = trsaddnode (YDONE, Memi[yypvt-YYOPLEN], NULL)
		    return (Memi[yyval])
		}
case 2:
# line 39 "trsopen.y"
{
		    # Parser error
		    return (NULL)
		}
case 3:
# line 44 "trsopen.y"
{
			# Empty filter 
			Memi[yyval] = NULL
		}
case 4:
# line 48 "trsopen.y"
{
			# Parentheses for grouping
			Memi[yyval] = Memi[yypvt-YYOPLEN]
		}
case 5:
# line 52 "trsopen.y"
{
			# And instruction
			Memi[yyval] = trsaddnode (YAND, Memi[yypvt-2*YYOPLEN], Memi[yypvt])
		}
case 6:
# line 56 "trsopen.y"
{
			# And instruction
			Memi[yyval] = trsaddnode (YAND, Memi[yypvt-2*YYOPLEN], Memi[yypvt])
		}
case 7:
# line 60 "trsopen.y"
{
			# Not instruction
			Memi[yyval] = trsaddnode (YNOT, Memi[yypvt], NULL)
		}
case 8:
# line 64 "trsopen.y"
{
			# Filter with singleton range
			if (! trscnum (Memi[yypvt-2*YYOPLEN], cptr)) {
				call strcpy (badcol, Memc[errbuf], SZ_LINE)
				return (NULL)
			}

			Memi[yyval] = trsaddnode (YRANGE, Memi[yypvt], -cptr)
		}
case 9:
# line 73 "trsopen.y"
{
			# Filter with singleton range
			if (! trscname (Memi[yypvt-2*YYOPLEN], cptr)) {
				call strcpy (badcol, Memc[errbuf], SZ_LINE)
				return (NULL)
			}
			Memi[yyval] = trsaddnode (YRANGE, Memi[yypvt], -cptr)
		}
case 10:
# line 82 "trsopen.y"
{
			# Parentheses for grouping
			Memi[yyval] = Memi[yypvt-YYOPLEN]
		}
case 11:
# line 86 "trsopen.y"
{
			# Or instruction
			Memi[yyval] = trsaddnode (YOR, Memi[yypvt-2*YYOPLEN], Memi[yypvt])
		}
case 12:
# line 90 "trsopen.y"
{
			# Not instruction
			Memi[yyval] = trsaddnode (YNOT, Memi[yypvt], NULL)
		}
case 13:
# line 94 "trsopen.y"
{
			# Numeric equality instruction
			Memi[yyval] = trsaddnode (YEQN, -Memi[yypvt], NULL)
		}
case 14:
# line 98 "trsopen.y"
{
			# String equality instruction
			Memi[yyval] = trsaddnode (YEQS, -Memi[yypvt], NULL)
		}
case 15:
# line 102 "trsopen.y"
{
			# Numeric less than or equal instruction
			Memi[yyval] = trsaddnode (YLEN, -Memi[yypvt], NULL)
		}
case 16:
# line 106 "trsopen.y"
{
			# String less than or equal instruction
			Memi[yyval] = trsaddnode (YLES, -Memi[yypvt], NULL)
		}
case 17:
# line 110 "trsopen.y"
{
			# Numeric inside instruction
			Memi[yyval] = trsaddnode (YINN, -Memi[yypvt-2*YYOPLEN], -Memi[yypvt])
		}
case 18:
# line 114 "trsopen.y"
{
			# String inside instruction
			Memi[yyval] = trsaddnode (YINS, -Memi[yypvt-2*YYOPLEN], -Memi[yypvt])
		}
case 19:
# line 118 "trsopen.y"
{
			# Numeric greater than or equal instruction
			Memi[yyval] = trsaddnode (YGEN, -Memi[yypvt-YYOPLEN], NULL)
		}
case 20:
# line 122 "trsopen.y"
{
			# Numeric greater than or equal instruction
			Memi[yyval] = trsaddnode (YGES, -Memi[yypvt-YYOPLEN], NULL)
		}
case 21:
# line 126 "trsopen.y"
{
			# Bit mask instruction
			Memi[yyval] = trsaddnode (YMSK, -Memi[yypvt], NULL)
		}	}

	goto yystack_				# stack new state and value
end
