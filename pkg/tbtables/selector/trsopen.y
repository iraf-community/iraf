%{
include "trs.h"

#* HISTORY *
#* B.Simon	04-Nov-94	original
#* B.Simon	23-Dec-97	row optimization added

define	YYMAXDEPTH	64
define	YYOPLEN		1
define	yyparse		trsparse

%L
include "trsopen.com"

int	cptr

errchk	trslex, trsaddnode
bool	trscname(), trscnum()
pointer	trsaddnode

string	badcol  "column not found"

%}

%token		YNIL, YBANG, YCOMMA, YCOLON, YEQUAL, YERR, YEOF 
%token		YLPAR, YINC, YNUM, YPER, YRPAR, YSEMI, YSTR

%left		YSEMI, YCOMMA
%nonassoc	YEQUAL
%right		YBANG

%%

expr :		filter YEOF {
		    # Normal exit. Code a stop instruction.
		    Memi[$$] = trsaddnode (YDONE, Memi[$1], NULL)
		    return (Memi[$$])
		}
	|	error {
		    # Parser error
		    return (NULL)
		}
	;
filter :	{
			# Empty filter 
			Memi[$$] = NULL
		}
	|	YLPAR filter YRPAR {
			# Parentheses for grouping
			Memi[$$] = Memi[$2]
		}
	|	filter YCOMMA filter {
			# And instruction
			Memi[$$] = trsaddnode (YAND, Memi[$1], Memi[$3])
		}
	|	filter YSEMI filter {
			# And instruction
			Memi[$$] = trsaddnode (YAND, Memi[$1], Memi[$3])
		}
	|	YBANG filter {
			# Not instruction
			Memi[$$] = trsaddnode (YNOT, Memi[$2], NULL)
		}
	| 	YNUM YEQUAL range {
			# Filter with singleton range
			if (! trscnum (Memi[$1], cptr)) {
				call strcpy (badcol, Memc[errbuf], SZ_LINE)
				return (NULL)
			}

			Memi[$$] = trsaddnode (YRANGE, Memi[$3], -cptr)
		}
	|	YSTR YEQUAL range {
			# Filter with singleton range
			if (! trscname (Memi[$1], cptr)) {
				call strcpy (badcol, Memc[errbuf], SZ_LINE)
				return (NULL)
			}
			Memi[$$] = trsaddnode (YRANGE, Memi[$3], -cptr)
		}
	;
range :		YLPAR range YRPAR {
			# Parentheses for grouping
			Memi[$$] = Memi[$2]
		}
	|	range YCOMMA range {
			# Or instruction
			Memi[$$] = trsaddnode (YOR, Memi[$1], Memi[$3])
		}
	|	YBANG range {
			# Not instruction
			Memi[$$] = trsaddnode (YNOT, Memi[$2], NULL)
		}
	|	YNUM {
			# Numeric equality instruction
			Memi[$$] = trsaddnode (YEQN, -Memi[$1], NULL)
		}
	|	YSTR {
			# String equality instruction
			Memi[$$] = trsaddnode (YEQS, -Memi[$1], NULL)
		}
	|	YCOLON YNUM {
			# Numeric less than or equal instruction
			Memi[$$] = trsaddnode (YLEN, -Memi[$2], NULL)
		}
	|	YCOLON YSTR {
			# String less than or equal instruction
			Memi[$$] = trsaddnode (YLES, -Memi[$2], NULL)
		}
	|	YNUM YCOLON YNUM {
			# Numeric inside instruction
			Memi[$$] = trsaddnode (YINN, -Memi[$1], -Memi[$3])
		}
	|	YSTR YCOLON YSTR {
			# String inside instruction
			Memi[$$] = trsaddnode (YINS, -Memi[$1], -Memi[$3])
		}
	|	YNUM YCOLON {
			# Numeric greater than or equal instruction
			Memi[$$] = trsaddnode (YGEN, -Memi[$1], NULL)
		}
	|	YSTR YCOLON {
			# Numeric greater than or equal instruction
			Memi[$$] = trsaddnode (YGES, -Memi[$1], NULL)
		}
	|	YPER YNUM {
			# Bit mask instruction
			Memi[$$] = trsaddnode (YMSK, -Memi[$2], NULL)
		}
	;
%%

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

errchk	stropen, trsparse, trserr, trsgencode

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

string	errfmt  "Error in table row selector, %s. Last read: %s\n"

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

