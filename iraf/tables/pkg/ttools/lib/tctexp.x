include <tbset.h>
include	<ctype.h>
include	"reloperr.h"

define	MAX_STACK	8
define	BLANK		' '
define	DELIM		','
define  ESCAPE		'\\'
define	NEGCHAR		'~'	# negation character
define	ALT_NEGCHAR	'!'	# alternate negation character

.help tctexp
.nf___________________________________________________________________________
Column template package

This package contains subroutines to expand a column name template into
an array of column pointers which match the template.  The template is a
list of column patterns separated by commas or whitespace.  The column
pattern is either a column name, a file name containing a list of column
names, or a pattern using the usual IRAF pattern matching syntax. For
example, the string

	a[1-9], b, time*, @column.lis

would be expanded as the column names a1 through a9, b, any column name
beginning with "time", and all the column names in the file column.lis.
If the column template is entirely whitespace, the array of column pointers
will include all the columns in the table, as this seems the most reasonable
default. If the first non-white character is the negation character (~),
the array of column pointers will include all columns not matched by the
template. The negation character only has this meaning as the first character
in the column template, and is interpreted as part of a column name if
found later in the template or in a file.

.endhelp______________________________________________________________________

# TCTEXP -- Expand a column template into an array of column pointers
#
# Given a table pointed to by a table descriptor and a column name template,
# return an array of column pointers. The size of the column pointer array
# is given by numcol and should be greater than or equal to the number of
# columns in the table. The actual number of columns found that match the
# template is returned as numptr.
#
# B.Simon	24-Jul-1987	First Code
# Phil Hodge	 1-Jun-1989	make search for columns case insensitive
# Phil Hodge	28-Jan-1999	add ! as an alternate negation character

procedure tctexp (tp, template, numcol, numptr, colptr)

pointer	tp		# i: pointer to table descriptor
char	template[ARB]	# i: column template
int	numcol		# i: size of column pointer array
int	numptr		# o: number of columns matched
pointer	colptr[ARB]	# o: array of column pointers
#--

bool	nometa		# true if pattern does not contain metacharacters
bool	negate		# true if template starts with negation character

int	fd_ptr		# pointer to stack of open list file descriptors
int	ic		# first non-white character in template

pointer fd_stack[MAX_STACK]
			# stack of file descriptors for open list files

pointer	sp, colpat, pattern, auxcol, fd

string	stkovflerr	"List files are nested too deeply, stack overflow"

int	strlen(), tctgetpat()
pointer	stropen(), open()

errchk	salloc, stropen, open, close
errchk	tctgetpat, tctmakpat, tctstrmatch, tctpatmatch

begin
	numptr = 0
	negate = false

	call smark (sp)
	call salloc (colpat, SZ_FNAME, TY_CHAR)
	call salloc (pattern, SZ_FNAME, TY_CHAR)

	# Check the column name template to find the first non-white character.

 	for (ic = 1; IS_WHITE (template[ic]); ic = ic + 1)
		;

	if (template[ic] == EOS) {

	    # If the template is blank, include all columns in the array

	    call allcolumns (tp, numptr, auxcol)
	    call amovi (Memi[auxcol], colptr, numptr)
	    call mfree (auxcol, TY_INT)
	    fd_ptr = 0

	} else {

	    # If the first non-white character is the negation character
	    # (either ~ or !), the meaning of the column name template is
	    # negated, that is, the array of column pointers will include
	    # those columns whose names were not matched by the column template

	    if (template[ic] == NEGCHAR || template[ic] == ALT_NEGCHAR) {
		negate = true
		ic = ic + 1
	    }

	    # Open the column name template as a file and push on
	    # the list file stack

	    fd_ptr = 1
	    fd_stack[1] =
		stropen (template[ic], strlen(template[ic]), READ_ONLY)

	}

	while (fd_ptr > 0) {

	    # Pop file descriptor off of the list file stack

	    fd = fd_stack[fd_ptr]
	    fd_ptr = fd_ptr - 1

	    # Loop over all column patterns in the file

	    while (tctgetpat (fd, Memc[colpat], SZ_FNAME) > 0) {

		if (Memc[colpat] == '@') {

		    # If this pattern is a list file name, push the
		    # current descriptor on the stack and open the file

		    if (fd_ptr == MAX_STACK)
			call error (BOUNDS, stkovflerr)
		    fd_ptr = fd_ptr + 1
		    fd_stack[fd_ptr] = fd
		    fd = open (Memc[colpat+1], READ_ONLY, TEXT_FILE)

		} else {

		    # Otherwise, encode the pattern and search the table
		    # for matching column names. To speed the search, use
		    # a special routine if the pattern does not include
		    # metacharacters

		    call strlwr (Memc[colpat])	# for case insensitivity
		    call tctmakpat (Memc[colpat], Memc[pattern], SZ_FNAME,
				    nometa)
		    if (nometa)
			call tctstrmatch (tp, Memc[pattern], numcol,
					  numptr, colptr)
		    else
			call tctpatmatch (tp, Memc[pattern], numcol,
					  numptr, colptr)
		}
	    }
	    call close (fd)
	}

	if (negate)
	    call invert (tp, numptr, colptr)

	call sfree (sp)
end

# TCTGETPAT -- Get next comma or whitespace delimeted pattern from file
#
# Copy characters into colpat until a field delimeter or the maximum number of
# characters is reached. The number of characters in colpat is returned as the
# value of the function, so the procedure which calls this one can test for
# the last field in the template.
#
# B. Simon	24-Jul-87	First Code

int procedure tctgetpat (fd, colpat, maxch)

pointer	fd		# i: template file descriptor
char	colpat[ARB]	# o: pattern from column name template
int	maxch		# i: maximum number of characters in field
#--
char	ch		# next character from template
int	iq		# pointer to character in colpat

char	getc()

begin
	# Skip leading whitespace or commas

	ch = getc (fd, ch)
	while (IS_CNTRL(ch) || ch == BLANK || ch == DELIM)
	    ch = getc (fd, ch)

	# Copy characters to colpat. End when maxch is reached, or
	# when comma, whitespace, or EOF is found

	for (iq = 1; iq <= maxch; iq = iq + 1) {

	    if (IS_CNTRL(ch) || ch == BLANK || ch == DELIM || ch == EOF)
		break

	    colpat[iq] = ch
	    ch = getc (fd, ch)
	}
	colpat[iq] = EOS

	# If loop is terminated because of maxch, eat remaining characters
	# in field

	while (! IS_CNTRL(ch) && ch != BLANK && ch != DELIM && ch != EOF)
	    ch = getc (fd, ch)

	# Return number of characters in colpat

	return (iq-1)
end

# TCTMAKPAT -- Encode the column pattern
#
# Create the pattern used by the matching routines. Check for metacharacters
# (unescaped pattern matching characters) to see if the faster constant
# pattern routine can be used.
#
# B.Simon	24-Jul-87	First Code

procedure tctmakpat (colpat, pattern, maxch, nometa)

char	colpat[ARB]	# i: Column pattern string
char	pattern[ARB]	# o: Encoded pattern string
int	maxch		# i: Maximum length of encoded pattern string
bool	nometa		# o: True if no metacharacters in string
#--
int	ic, ip
pointer	sp, buffer, buffer2, errtxt, ib

int	stridx(), strlen(), patmake()

string	patovflerr "Column pattern too long (%s)"
string	badpaterr  "Column pattern has bad syntax (%s)"

begin
	call smark (sp)
	call salloc (buffer, maxch, TY_CHAR)
	call salloc (buffer2, maxch, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	nometa = true
	ib = buffer

	# Copy the column pattern to a temporary buffer

	for (ic = 1; colpat[ic] != EOS ; ic = ic + 1) {

	    # Copy escape sequences, but do not count as metacharacters

	    if (colpat[ic] == ESCAPE && colpat[ic+1] != EOS) {
		Memc[ib] = ESCAPE
		ib = ib + 1
		ic = ic + 1

	    # Covert '*' to '?*', count as a metacharacter

	    } else if (colpat[ic] == '*') {
		nometa = false
		Memc[ib] = '?'
		ib = ib + 1

	    # Check for the other metacharacters

	    } else if (stridx (colpat[ic], "[?{") > 0)
		nometa = false

	    Memc[ib] = colpat[ic]
	    ib = ib + 1
	}
	Memc[ib] = EOS

	# Check the buffer length against maximum pattern length

	if (strlen (Memc[buffer]) > maxch) {
	    call sprintf (Memc[errtxt], SZ_LINE, patovflerr)
	    call pargstr (colpat)
	    call error (BOUNDS, Memc[errtxt])
	}

	# If no metacharacters, strip escape sequences

	if (nometa) {
	    ip = 1
	    for (ib = buffer; Memc[ib] != EOS; ib = ib + 1) {
		if (Memc[ib] == ESCAPE && Memc[ib+1] != EOS)
		    ib = ib + 1
		pattern[ip] = Memc[ib]
		ip = ip + 1
	    }
	    pattern[ip] = EOS

	# Otherwise, encode with patmake

	} else {
	    call sprintf (Memc[buffer2], maxch, "^%s$")
	    call pargstr (Memc[buffer])

	    if (patmake (Memc[buffer2], pattern, SZ_LINE) == ERR) {
		call sprintf (Memc[errtxt], SZ_LINE, badpaterr)
		call pargstr (colpat)
		call error (SYNTAX, Memc[errtxt])
	    }
	}

	call sfree (sp)
end

# TCTSTRMATCH -- Add a column pointer for a column name to the array
#
# Used to match column names when the column pattern contains no
# metacharacters.
#
# B. Simon	24-Jul-87	First Code

procedure tctstrmatch (tp, pattern, numcol, numptr, colptr)

pointer	tp		# i: pointer to table descriptor
char	pattern[ARB]	# i: column pattern
int	numcol		# i: size of column pointer array
int	numptr		# o: number of columns matched
pointer	colptr[ARB]	# o: array of column pointers
#--
int	iptr
pointer	sp, errtxt, cp

string	maxcolerr "Maximum number of columns in table exceeded (%d)"

errchk	tbcfnd

begin
	call smark (sp)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	# Find the column pointer corresponding to the column name

	call tbcfnd (tp, pattern, cp, 1)

	# Pointer is null if column not found in table

	if (cp == NULL)
	    return

	# See if the column name has already been matched

	for (iptr = 1; iptr <= numptr; iptr = iptr +1) {
	    if (cp == colptr[iptr])
		break
	    }

	# If not, add its pointer in the array of pointers
	# after checking for array overflow

	if (iptr > numptr) {
	    if (numptr >= numcol) {
		call sprintf (Memc[errtxt], SZ_LINE, maxcolerr)
		call pargi (numcol)
		call error (BOUNDS, Memc[errtxt])
	    }
	    numptr = numptr + 1
	    colptr[numptr] = cp
	}

	call sfree (sp)
end

# TCTPATMATCH -- Find column pointers for columns matching a pattern
#
# This routine is called when the column pattern contains metacharacters.
#
# B.Simon	27-Jul-87	First Code

procedure tctpatmatch (tp, pattern, numcol, numptr, colptr)

pointer	tp		# i: pointer to table descriptor
char	pattern[ARB]	# i: column pattern
int	numcol		# i: size of column pointer array
int	numptr		# o: number of columns matched
pointer	colptr[ARB]	# o: array of column pointers
#--
int	maxcol, icol, iptr
pointer	sp, errtxt, cp
pointer	colname

string	maxcolerr "Maximum number of columns in table exceeded (%d)"

int	tbpsta(), tbcnum(), patmatch()

errchk	tbpsta, tbcnum, tbcinf, patmatch

begin
	call smark (sp)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (errtxt, SZ_LINE, TY_CHAR)

	maxcol = tbpsta (tp, TBL_NCOLS)

	# Compare the column pattern to each column name in the table

	do icol = 1, maxcol {

	    # Get the next column name in the table

	    cp = tbcnum (tp, icol)
	    call tbcigt (cp, TBL_COL_NAME, Memc[colname], SZ_COLNAME)
	    call strlwr (Memc[colname])		# for case insensitivity

	    # Check the column name for a match

	    if (patmatch (Memc[colname], pattern) > 0) {
		# See if the column name has already been matched

		for (iptr = 1; iptr <= numptr; iptr = iptr +1) {
		    if (cp == colptr[iptr])
			break
		}

		# If not, add its pointer in the array of pointers
		# after checking for array overflow

		if (iptr > numptr) {
		    if (numptr >= numcol) {
			call sprintf (Memc[errtxt], SZ_LINE, maxcolerr)
			call pargi (numcol)
			call error (BOUNDS, Memc[errtxt])
		    }
		    numptr = numptr + 1
		    colptr[numptr] = cp
		}
	    }
	}

	call sfree (sp)

end
