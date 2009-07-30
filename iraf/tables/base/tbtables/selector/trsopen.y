%{
include "trs.h"

#* HISTORY *
#* B.Simon	04-Nov-94	original
#* B.Simon	23-Dec-97	row optimization added
#  Phil Hodge	12-Jul-2005	In trsopen, declare 'debug' to be bool rather
#				than int, and add 'int trslex()'

define	YYMAXDEPTH	64
define	YYOPLEN		1
define	yyparse		trsparse

%L
include "trsopen.com"

pointer	cptr
bool	trscname(), trscnum()
pointer	trsaddnode()
errchk	trslex, trsaddnode
string	badcol  "column not found"
include	<nullptr.inc>

%}

%token		YNIL, YBANG, YCOMMA, YCOLON, YEQUAL, YERR, YEOF 
%token		YLPAR, YINC, YNUM, YPER, YRPAR, YSEMI, YSTR

%left		YSEMI, YCOMMA
%nonassoc	YEQUAL
%right		YBANG

%%

expr :		filter YEOF {
		    # Normal exit. Code a stop instruction.
		    Memp[$$] = trsaddnode (YDONE, Memp[$1], NULLPTR)
		    return (Memp[$$])
		}
	|	error {
		    # Parser error
		    return (NULL)
		}
	;
filter :	{
			# Empty filter 
			Memp[$$] = NULL
		}
	|	YLPAR filter YRPAR {
			# Parentheses for grouping
			Memp[$$] = Memp[$2]
		}
	|	filter YCOMMA filter {
			# And instruction
			Memp[$$] = trsaddnode (YAND, Memp[$1], Memp[$3])
		}
	|	filter YSEMI filter {
			# And instruction
			Memp[$$] = trsaddnode (YAND, Memp[$1], Memp[$3])
		}
	|	YBANG filter {
			# Not instruction
			Memp[$$] = trsaddnode (YNOT, Memp[$2], NULLPTR)
		}
	| 	YNUM YEQUAL range {
			# Filter with singleton range
			if (! trscnum (Memp[$1], cptr)) {
				call strcpy (badcol, Memc[errbuf], SZ_LINE)
				return (NULL)
			}

			Memp[$$] = trsaddnode (YRANGE, Memp[$3], -cptr)
		}
	|	YSTR YEQUAL range {
			# Filter with singleton range
			if (! trscname (Memp[$1], cptr)) {
				call strcpy (badcol, Memc[errbuf], SZ_LINE)
				return (NULL)
			}
			Memp[$$] = trsaddnode (YRANGE, Memp[$3], -cptr)
		}
	;
range :		YLPAR range YRPAR {
			# Parentheses for grouping
			Memp[$$] = Memp[$2]
		}
	|	range YCOMMA range {
			# Or instruction
			Memp[$$] = trsaddnode (YOR, Memp[$1], Memp[$3])
		}
	|	YBANG range {
			# Not instruction
			Memp[$$] = trsaddnode (YNOT, Memp[$2], NULLPTR)
		}
	|	YNUM {
			# Numeric equality instruction
			Memp[$$] = trsaddnode (YEQN, -Memp[$1], NULLPTR)
		}
	|	YSTR {
			# String equality instruction
			Memp[$$] = trsaddnode (YEQS, -Memp[$1], NULLPTR)
		}
	|	YCOLON YNUM {
			# Numeric less than or equal instruction
			Memp[$$] = trsaddnode (YLEN, -Memp[$2], NULLPTR)
		}
	|	YCOLON YSTR {
			# String less than or equal instruction
			Memp[$$] = trsaddnode (YLES, -Memp[$2], NULLPTR)
		}
	|	YNUM YCOLON YNUM {
			# Numeric inside instruction
			Memp[$$] = trsaddnode (YINN, -Memp[$1], -Memp[$3])
		}
	|	YSTR YCOLON YSTR {
			# String inside instruction
			Memp[$$] = trsaddnode (YINS, -Memp[$1], -Memp[$3])
		}
	|	YNUM YCOLON {
			# Numeric greater than or equal instruction
			Memp[$$] = trsaddnode (YGEN, -Memp[$1], NULLPTR)
		}
	|	YSTR YCOLON {
			# Numeric greater than or equal instruction
			Memp[$$] = trsaddnode (YGES, -Memp[$1], NULLPTR)
		}
	|	YPER YNUM {
			# Bit mask instruction
			Memp[$$] = trsaddnode (YMSK, -Memp[$2], NULLPTR)
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
pointer	fd
int	jtop, i_fd
bool	debug
pointer	sp, root
size_t	sz_val

data	nil     / EOS /
data	debug	/ false /
string	syntax  "syntax error"

errchk	stropen, trsparse, trserr

int	trslex()
extern	trslex
pointer	trsinit()
long	trsparse()
int	stropen(), strlen()

begin
	# Initialize common block used by parser

	tabptr = tp

	call smark (sp)
	sz_val = SZ_TOKEN
	call salloc (tokbuf, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (errbuf, sz_val, TY_CHAR)
	sz_val = SZ_BUFFER
	call salloc (treebuf, sz_val, TY_POINTER)

	sz_val = SZ_TOKEN
	call amovkc (nil, Memc[tokbuf], sz_val)
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
	    i_fd = fd
	    call close (i_fd)

	    call trserr
	}

	# Free memory and close files

	i_fd = fd
	call close (i_fd)
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
size_t	sz_val
int	imod()

string	errfmt  "Error in table row selector, %s. Last read: %s"

begin
	# Allocate memory to hold token

	call smark (sp)
	sz_val = SZ_TOKEN
	call salloc (token, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (errmsg, sz_val, TY_CHAR)

	# Copy token from token buffer. Since token buffer is maintained 
	# as a queue, the copy is in two parts, after and before the
	# queue pointer.

	nc = 0
	if (Memc[tokbuf+itok] != EOS) {
	    nc = SZ_TOKEN - itok
	    sz_val = nc
	    call amovc (Memc[tokbuf+itok], Memc[token], sz_val)
	}

	itok = imod (itok - 1, SZ_TOKEN)
	sz_val = itok
	call amovc (Memc[tokbuf], Memc[token+nc], sz_val)

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
size_t	sz_val

begin
	sz_val = LEN_TRSBUF
	call malloc (buf, sz_val, TY_POINTER)

	TRS_IDENT(buf) = TRS_MAGIC
	TRS_ROWS(buf) = NULL

	sz_val = SZ_BUFFER
	call malloc (TRS_CODE(buf), sz_val, TY_STRUCT)
	call malloc (TRS_VALUE(buf), sz_val, TY_DOUBLE)

	return (buf)
end

# TRSLEX -- Lexical analyzer for table row selector

int procedure trslex (fd, value)

pointer	fd		# u: file descriptor of currently open file
pointer	value		# i: Pointer to current token value
#--
include	"trsopen.com"

int	i_fd
int	type
size_t	sz_val
char	c_eos

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
		    c_eos = EOS
		    sz_val = SZ_TOKEN
		    call amovkc (c_eos, Memc[tokbuf], sz_val)
		    itok = 0

		    i_fd = fd
		    call close (i_fd)
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
			fd = open (Memc[Memp[value]], READ_ONLY, TEXT_FILE)
		    } then {
			c_eos = EOS
			sz_val = SZ_TOKEN
			call amovkc (c_eos, Memc[tokbuf], sz_val)
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

pointer	fd		# i: input file descriptor
char	ch		# o: character read from input
#--
int	i_fd
include	"trsopen.com"

char	getc()
int	imod()

begin
	i_fd = fd
	Memc[tokbuf+itok] = getc (i_fd, ch)
	itok = imod (itok+1, SZ_TOKEN)

	return (ch)
end

# TRSTOK -- Read next token from current file

procedure trstok (fd, value, type)

pointer	fd		# u: file descriptor of currently open file
pointer	value		# i: Pointer to current token value
int	type		# i: Token type
#--
include	"trsopen.com"

char	ch, stop
double	dval
int	stoptype[10]
int	nc, ic, index, delta, size
int	i_fd

pointer	sp, token, ptr, valbuf
size_t	sz_val

string	notnum   "not a number"
string	noroom   "expression too complex"
string	nostop   "trailing quote not found"

string	stopset  " ,;:%=!()@"

data	stoptype / YNIL, YCOMMA, YSEMI, YCOLON, YPER, 
		   YEQUAL, YBANG, YLPAR, YRPAR, YINC /

int	trsnextch(), trstrim(), stridx(), ctod(), imod()

begin
	# Eat leading whitespace, watch for end of file

	while (trsnextch (fd, ch) <= ' ') {
	    if (ch == EOF) {
		Memp[value] = NULL
		type = YEOF
		return
	    }

	}

	# Check if first character is a delimeter
	# if so, return the corresponding token

	index = stridx (ch, stopset)
	if (index > 0) {
	    Memp[value] = NULL
	    type = stoptype[index] 
	    return
	}

	# The tougher case: token is a number or string
	# First, gather all characters in token

	call smark (sp)
	sz_val = SZ_LINE
	call salloc (token, sz_val, TY_CHAR)

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
		Memp[value] = NULL
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

		    i_fd = fd
		    call ungetc (i_fd, ch)
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
		Memp[value] = NULL
		type = YERR

	    } else {
		ptr = valbuf + ival
		ival = ival + 1

		Memd[ptr] = dval
		Memp[value] = ptr
		type = YNUM
	    }

	} else {
	    # Token is a string. Find how much space it will take
	    # and store in the value buffer

	    size = nc + 1
	    delta = imod (size, SZ_DOUBLE)
	    if (delta != 0)
		size = size + (SZ_DOUBLE - delta)
	    size = size / SZ_DOUBLE

	    if (ival + size >= SZ_BUFFER) {
		call strcpy (noroom, Memc[errbuf], SZ_LINE)
		Memp[value] = NULL
		type = YERR

	    } else {
		ptr = ((valbuf + ival - 1) * SZ_DOUBLE) + 1
		ival = ival + size

		call strcpy (Memc[token], Memc[ptr], size*SZ_DOUBLE-1)
		Memp[value] = ptr
		type = YSTR
	    }
	}

	call sfree (sp)
end

