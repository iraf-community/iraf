include <tbset.h>
include "tcs.h"

define	MAX_STACK	8		# max file depth in column list
define	DELIM		','		# column name separator
define	COMMENT		'#'		# comment character
define	ESCAPE		'\\'		# escape character
define	SQUOTE		'\''		# single quote
define	DQUOTE		'"'		# double quote
define	LPAREN		'('		# left parenthesis
define	RPAREN		')'		# right parenthesis
define	NEWLINE		'\n'		# end of line character
define	NOTWHITE	($1 > ' ')	# private definition of white space

.help tcs_open
.nf___________________________________________________________________________
Table column selector

This file contains procedures to expand a list of column names into an
array of column descriptors which match the list.  The list is a list
of column patterns separated by commas.  The column pattern is either
a column name, a file name containing a list of column names, or a
pattern using the usual IRAF pattern matching syntax. For example, the
string

	a[1-9], b, time*, @column.lis

would be expanded as the column names a1 through a9, b, any column
name beginning with "time", and all the column names in the file
column.lis.  If the column list is entirely whitespace, the array of
column descriptors will include all the columns in the table, as this
seems the most reasonable default. If the first non-white character is
the negation character (either ~ or !), the array of column descriptors
will include all columns not matched by the list. The negation character
only has this meaning at the beginning of the list.

Column names may also contain array sections having the same format
as image sections. The sections are surrounded by parentheses. For example

	spec(1:200:2) image(*,30) spec (20:*)

are valid array sections.

.endhelp______________________________________________________________________

# TCS_OPEN -- Convert a list of column names to a list of descriptors

procedure tcs_open (tp, columns, descrip, ndescrip, maxdescrip)

pointer	tp		# i: table descriptor
char	columns[ARB]	# i: list of column names
pointer	descrip[ARB]	# o: list of column array selectors
int	ndescrip	# o: number of column array selectors
int	maxdescrip	# i: length of descrip array
#--
bool	negate, file
int	ncols, top, fd_stack[MAX_STACK]
pointer	sp, token, pattern, section, errmsg

string	overflow  "Column list has too many nested files"

bool	tcs_hasmeta()
int	tcs_token(), strlen(), stropen(), open()

errchk	tcs_patmatch

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (token, SZ_FNAME, TY_CHAR)
	call salloc (pattern, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Keep track of the number of column patterns and the negation
	# pattern. At the end of the procedure, if no patterns were read,
	# the list is blank, which signifies all columns should be used.
	# If the negation character is encountered, the list of columns
	# to use is inverted.

	ncols = 0
	negate = false

	# Initialize the number of columns matched to zero

	ndescrip = 0

	# Since the column list may contain filenames, which in turn will
	# contain other lists, we use a stack of file descriptors to keep
	# track of the current file. The column list is also opened as a
	# file, for the sake of generality in the code.

	top = 1
	file = false
	fd_stack[1] = stropen (columns, strlen(columns), READ_ONLY)

	while (top > 0) {
	    # The tokenizer either returns a negation character (! or ~)
	    # a filename (preceded by a @) or a column name. Tokens,
	    # except for the negation character, are separated by commas.

	    while (tcs_token (fd_stack[top], file, Memc[token], SZ_FNAME) > 0){
		ncols = ncols + 1

		if (Memc[token] == '!') {
		    # Negation character. Only is significant as first
		    # character in the column list.

		    negate = (ncols == 1)
		    ncols = ncols - 1

		} else if (Memc[token] == '@') {
		    # Filename. Open the file and push it on the stack.

		    if (top == MAX_STACK)
			call error (1, overflow)

		    top = top + 1
		    fd_stack[top] = open (Memc[token+1], READ_ONLY, TEXT_FILE)

		    ncols = ncols - 1

		} else {
		    # Column pattern. Remove the section from the pattern

		    call tcs_breakname (Memc[token], Memc[pattern],
					Memc[section])

		    # Look for metacode characters. If found, call the
		    # pattern matching routine, otherwise call the string
		    # matching routine. The division between the routines
		    # is for reasons of efficiency.

		    call strlwr (Memc[pattern])

		    if (tcs_hasmeta (Memc[pattern], SZ_FNAME)) {
			call tcs_patmatch (tp, Memc[pattern], Memc[section],
					   descrip, ndescrip, maxdescrip)
		    } else {
			call tcs_strmatch (tp, Memc[pattern], Memc[section],
					   descrip, ndescrip, maxdescrip)
		    }
		}

		file = top > 1
	    }

	    # All columns have been read from this file,
	    # so pop it from the stack

	    call close (fd_stack[top])
	    top = top - 1
	}

	# A blank list signifies select all columns from the table

	if (ncols == 0)
	    call tcs_allcols (tp, descrip, ndescrip, maxdescrip)

	# The negation character signifies those columns not in the list
	# should be selected

	if (negate)
	    call tcs_invert (tp, descrip, ndescrip, maxdescrip)

	call sfree (sp)
end

# TCS_TOKEN -- Extract the next token from a column list

int procedure tcs_token (fd, file, token, maxch)

int	fd		# i: descriptor of file containing column list
bool	file		# i: is the read coming from a file?
char	token[ARB]	# o: token string
int	maxch		# i: declared length of token string
#--
char	ch
int	nc, endch, paren

char	getc()

begin
	# Eat leading whitespace and delimeters

	repeat {
	    ch = getc (fd, ch)

	    # Eat comment if we are reading from a file

	    if (ch == COMMENT && file) {
		repeat {
		    ch = getc (fd, ch)
		} until (ch == EOF || ch == NEWLINE)
	    }

	} until (ch == EOF || (NOTWHITE(ch) && ch != DELIM))


	# Leading character determines rest of processing

	if (ch == EOF) {
	    # End of file. Return null string
	    token[1] = EOS
	    return (0)

	} else if (ch == '!' || ch == '~') {	# ~ added on 1999 Jan 29
	    # Negation character. Return the character.

	    token[1] = '!'	# same token for both negation characters
	    token[2] = EOS
	    return (1)

	} else if (ch == '@') {
	    # A filename. Return all characters up to whitespace or
	    # the next delimeter.

	    nc = 1
	    while (NOTWHITE(ch) && ch != DELIM) {
		if (nc <= maxch) {
		    token[nc] = ch
		    nc = nc + 1
		}

		ch = getc (fd, ch)
	    }

	    token[nc] = EOS
	    return (nc - 1)

	} else if (ch == SQUOTE || ch == DQUOTE){
	    # A quoted string. Return all characters up to and including
	    # the closing quote.

	    endch = ch

	    nc = 1
	    repeat {
		if (nc < maxch) {
		    token[nc] = ch
		    nc = nc + 1
		}

		ch = getc (fd, ch)
	    } until (ch == EOF || ch == endch)

	    token[nc] = endch
	    token[nc+1] = EOS
	    return (nc)

	} else {
	    # An ordinary column name. Return all characters up to the next
	    # whitespace or delimeter. Delimeters inside parentheses
	    # are part of the column section and are not treated as delimeters.

	    nc = 1
	    paren = 0
	    while (NOTWHITE(ch)  && (paren > 0 || ch != DELIM)) {
		if (nc <= maxch) {
		    token[nc] = ch
		    nc = nc + 1
		}

		if (ch == LPAREN) {
		    paren = paren + 1
		} else if (ch == RPAREN) {
		    paren = paren - 1
		}

		ch = getc (fd, ch)
	    }

	    token[nc] = EOS
	    return (nc - 1)
	}

end

# TCS_BREAKNAME -- Break a column name into root and section

procedure tcs_breakname (name, root, section)

char	name[ARB]	# i: column name
char	root[ARB]	# o: root (everything up to the parentheses)
char	section[ARB]	# o: section (everything in the parentheses)
#--
int	ic, jc, kc, paren, state

begin
	jc = 1
	kc = 1
	paren = 0
	state = 1

	# There are three states: Before the first parenthesis
	# where characters are copied to the root, inside the
	# parentheses where characters are copied to the section
	# and after the parentheses where characters are again
	# copied to the root. The variable paren keeps track of
	# parentheses so we can transition between the second and
	# third state at the parenthesis that matches the first.

	for (ic = 1; name[ic] != EOS; ic = ic + 1) {
	    if (state == 1) {
		if (name[ic] == LPAREN) {
		    section[kc] = name[ic]
		    kc = kc + 1

		    state = 2
		    paren = 1
		} else {
		    root[jc] = name[ic]
		    jc = jc + 1
		}

	    } else if (state == 2) {
		if (paren == 0) {
		    state = 3
		} else {
		    # Whitespace is not copied to the section

		    if (NOTWHITE(name[ic])) {
			section[kc] = name[ic]
			kc = kc + 1
		    }

		    if (name[ic] == LPAREN) {
			paren = paren + 1
		    } else if (name[ic] == RPAREN) {
			paren = paren - 1
		    }
		}
	    } else if (state == 3) {
		root[jc] = name[ic]
		jc = jc +1
	    }
	}

	root[jc] = EOS
	section[kc] = EOS

end

# TCS_HASMETA -- Check for presence of metacharacters

bool procedure tcs_hasmeta (pattern, maxch)

char	pattern[ARB]	# u: character string
int	maxch		# i: declared length of pattern
#--
bool	meta
int	ic, jc
pointer	sp, buffer

int	stridx()

begin
	# If the pattern is enclosed in quotes, all characters are
	# interpreted as literals. Strip quotes from the pattern and
	# return false.

	if (pattern[1] == SQUOTE || pattern[1] == DQUOTE) {
	    for (ic = 1; pattern[ic] != EOS; ic = ic + 1)
		pattern[ic] = pattern[ic+1]

	    pattern[ic-2] = EOS
	    return (false)
	}

	# Copy the pattern to a temporary buffer

	call smark (sp)
	call salloc (buffer, maxch, TY_CHAR)

	jc = 0
	meta = false
	for (ic = 1; pattern[ic] != EOS; ic = ic + 1) {

	    if (pattern[ic] == ESCAPE && pattern[ic+1] != EOS) {
		# Copy escape sequences but do not count as metacharacters

		ic = ic + 1
		if (jc <= maxch) {
		    Memc[buffer+jc] = ESCAPE
		    jc = jc + 1
		}

	    } else if (pattern[ic] == '*') {
		# Convert '*' to '?*', count as metacharacter

		meta = true
		if (jc <= maxch) {
		    Memc[buffer+jc] = '?'
		    jc = jc + 1
		}

	    } else if (stridx (pattern[ic], "[?{") > 0) {
		# Check for other metacharacters

		meta = true
	    }

	    if (jc <= maxch) {
		Memc[buffer+jc] = pattern[ic]
		jc = jc + 1
	    }
	}

	Memc[buffer+jc] = EOS

	if (meta) {
	    # Enclose pattern in "^pattern$" to force match 
	    # of entire column name

	    call sprintf (pattern, maxch, "^%s$")
	    call pargstr (Memc[buffer])

	} else {
	    # Remove escape characters from pattern
	    # if there are no metacharacters

	    jc = 1
	    for (ic = 0; Memc[buffer+ic] != EOS; ic = ic + 1) {
		if (Memc[buffer+ic] == ESCAPE && Memc[buffer+ic+1] != EOS)
		    ic = ic + 1

		pattern[jc] = Memc[buffer+ic]
		jc = jc + 1
	    }

	    pattern[jc] = EOS
	}

	call sfree (sp)
	return (meta)
end

# TCS_PATMATCH -- Match column names containing metacharacters

procedure tcs_patmatch (tp, pattern, section, descrip, ndescrip, maxdescrip)
			
pointer	tp		# i: table descriptor
char	pattern[ARB]	# i: pattern to match
char	section[ARB]	# i: array section
pointer	descrip[ARB]	# u: list of column array selectors
int	ndescrip	# u: number of column array selectors
int	maxdescrip	# i: length of descrip array
#--
int	icol, ncols, id
pointer	sp, buffer, colname, errmsg, cp

string	badpattern  "Syntax error in wildcard pattern (%s)"

int	tbpsta(), patmake(), patmatch()
pointer tbcnum()

errchk	tcs_fillstruct

begin
	# Allocate temporary strings

	call smark (sp)
	call salloc (buffer, SZ_LINE, TY_CHAR)
	call salloc (colname, SZ_COLNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Compile the pattern

	if (patmake (pattern, Memc[buffer], SZ_LINE) == ERR) {
	    call sprintf (Memc[errmsg], SZ_LINE, badpattern)
	    call pargstr (pattern)
	    call error (1, Memc[errmsg])
	}

	# Look at each column name to see if it matches the pattern.
	# If the pattern matches, add it to the list if the column
	# has not already been matched.

	ncols = tbpsta (tp, TBL_NCOLS)

	do icol = 1, ncols {
	    # Get column name from column number

	    cp = tbcnum (tp, icol)
	    call tbcigt (cp, TBL_COL_NAME, Memc[colname], SZ_COLNAME)
	    call strlwr (Memc[colname])

	    # Pattern matching test

	    if (patmatch (Memc[colname], Memc[buffer]) > 0) {
		# Check to see if already matched

		for (id = 1; id <= ndescrip; id = id + 1) {
		    if (cp == TCS_COLUMN(descrip[id]))
			break
		}

		# Add to array if not already matched and array not full

		if (id > ndescrip && ndescrip < maxdescrip) {
		    ndescrip = ndescrip + 1
		    call tcs_fillstruct (tp, cp, section, descrip[ndescrip])
		}
	    }
	}

	call sfree (sp)
end

# TCS_STRMATCH -- Match column names to table columns

procedure tcs_strmatch (tp, pattern, section, descrip, ndescrip, maxdescrip)
			
pointer	tp		# i: table descriptor
char	pattern[ARB]	# i: pattern to match
char	section[ARB]	# i: array section
pointer	descrip[ARB]	# u: list of column array selectors
int	ndescrip	# u: number of column array selectors
int	maxdescrip	# i: length of descrip array
#--
int	id
pointer	cp

errchk	tcs_fillstruct

begin
	# Find column pointer corresponding to column name

	call tbcfnd (tp, pattern, cp, 1)

	if (cp == NULL)
	    return

	# Check to see if already matched

	for (id = 1; id <= ndescrip; id = id + 1) {
	    if (cp == TCS_COLUMN(descrip[id]))
		break
	}

	# Add to array if not already matched and array not full

	if (id > ndescrip && ndescrip < maxdescrip) {
	    ndescrip = ndescrip + 1
	    call tcs_fillstruct (tp, cp, section, descrip[ndescrip])
	}
end

# TCS_FILLSTRUCT -- Fill structure with info about the column

procedure tcs_fillstruct (tp, cp, section, descrip)

pointer	tp		# i: table descriptor
pointer	cp		# i: column descriptor
char	section[ARB]	# i: column array section
pointer	descrip		# i: column array selector
#--
int	ic, idim, ndim, first, last, inc, axlen[MAXDIM]

string	baddimen  "Dimension of section does not match column"

int	tcs_getsect()
errchk	tcs_getsect

begin
	# Get dimension of array and length of each axis

	call tbciga (tp, cp, ndim, axlen, MAXDIM)

	# Allocate column selector descriptor

	call malloc (descrip, TCS_LENGTH(ndim), TY_INT)

	if (section[1] == EOS) {
	    # If there is no section, copy the array dimensions
	    # to the descriptor


	    do idim = 1, ndim {
		TCS_FIRST(descrip,idim) = 1
		TCS_LAST(descrip,idim) = axlen[idim]
		TCS_INC(descrip,idim) = 1
	    }

	} else {
	    # If there is a section, parse it and copy it to descriptor

	    ic = 2
	    do idim = 1, ndim {
		if (tcs_getsect (section, ic, first, last, inc) <= 0){
		    # Not enough dimensions in section

		    call mfree (descrip, TY_INT)
		    call error (1, baddimen)
		}

		TCS_FIRST(descrip,idim) = first
		TCS_INC(descrip,idim) = inc

		# Indef indicates an asterisk in the section, for which
		# we substitute the actual array dimension

		if (IS_INDEFI (last)) {
		    TCS_LAST(descrip,idim) = axlen[idim]
		} else {
		    TCS_LAST(descrip,idim) = last
		}
	    }

	    # It is an error if the section has more dimensions than the array

	    if (section[ic] != EOS) {
		call mfree (descrip, TY_INT)
		call error (1, baddimen)
	    }
	}

	# Eliminate spurious dimensions from the array

	for (idim = ndim; idim > 0; idim = idim - 1) {
	    if (axlen[idim] > 1)
		break
	}

	ndim = idim

	# Save the column pointer and number of dimensions in the descriptor

	TCS_COLUMN(descrip) = cp
	TCS_DIMEN(descrip) = ndim

end

# TCS_GETSECT -- Parse the array section string

int procedure tcs_getsect (section, ic, first, last, inc)

char	section[ARB]	# i: section string
int	ic		# u: starting character in string
int	first		# o: first element in array
int	last		# o: last element in array
int	inc		# o: array increment
#--
bool	done
int	jc, nc, ival, old_ic, value
pointer	sp, number

bool	streq()
int	stridx(), ctoi()

string	badsect  "Syntax error in array section"

begin
	# Temporary string to hold numeric token

	call smark (sp)
	call salloc (number, SZ_FNAME, TY_CHAR)

	# Set defaults for outputs

	first = 1
	last = 1
	inc = 1

	# Read charcaters from section until a delimeter is found.
	# Then check to see if it is a wildcard. If not, convert it
	# to a number and set the appropriate output.

	jc = 0
	ival = 1
	old_ic = ic
	done = false

	while (! done && section[ic] != EOS) {
	    if (stridx (section[ic], "(),:") == 0) {
		# Copy characters until delimeter

		Memc[number+jc] = section[ic]
		jc = jc + 1

	    } else {
		Memc[number+jc] = EOS

		if (streq (Memc[number], "*")) {
		    last = INDEFI

		} else {
		    # Convert string to number

		    jc = 1
		    nc = ctoi (Memc[number], jc, value)

		    # Check for trailing non-numeric chars

		    if (Memc[number+nc] != EOS)
			call error (1, badsect)

		    # Set appropriate output

		    switch (ival) {
		    case 1:
			first = value
		    case 2:
			last = value
			if (last < first)
			    call error (1, badsect)
		    case 3:
			inc = value
		    default:
			call error (1, badsect)
		    }

		    ival = ival + 1
		}

		# Reset to read next string

		jc = 0

		# Exit loop when delimeter or closing parenthesis seen

		done = (section[ic] == DELIM || section[ic] == RPAREN)
	    }

	    ic = ic + 1
	}

	# A single number indicates one element in the array

	if (last == 1 && first > 1)
	    last = first

	call sfree (sp)
	return (ic - old_ic)

end

# TCS_ALLCOLS -- Get descriptors for all columns in the table

procedure tcs_allcols (tp, descrip, ndescrip, maxdescrip)

pointer	tp		# i: table descriptor
pointer	descrip[ARB]	# o: list of column array selectors
int	ndescrip	# o: number of column array selectors
int	maxdescrip	# i: length of descrip array
#--
int	icol, ncols
pointer	cp

int	tbpsta()
pointer	tbcnum()

begin
	ncols = tbpsta (tp, TBL_NCOLS)
	ncols = min (ncols, maxdescrip)

	do icol = 1, ncols {
	    cp = tbcnum (tp, icol)

	    ndescrip = ndescrip + 1
	    call tcs_fillstruct (tp, cp, "", descrip[ndescrip])
	}

end

# TCS_INVERT -- Get descriptors for all columns not currently in list

procedure tcs_invert (tp, descrip, ndescrip, maxdescrip)

pointer	tp		# i: table descriptor
pointer	descrip[ARB]	# o: list of column array selectors
int	ndescrip	# o: number of column array selectors
int	maxdescrip	# i: length of descrip array
#--
int	id, icol, jcol, ncols
pointer	cp, sp, clist

int	tbpsta()
pointer	tbcnum()

begin
	# Allocate temporary array for column list

	ncols = tbpsta (tp, TBL_NCOLS)

	call smark (sp)
	call salloc (clist, ncols, TY_INT)

	# Get each column pointer and search column selectors for a match
	# If none is, found, copy the pointer to the column list

	jcol = 0
	do icol = 1, ncols {
	    cp = tbcnum (tp, icol)
	    for (id = 1; id <= ndescrip; id = id + 1) {
		if (TCS_COLUMN(descrip[id]) == cp)
		    break
	    }

	    if (id > ndescrip) {
		Memi[clist+jcol] = cp
		jcol = jcol + 1
	    }
	}

	# Free the old descriptors

	call tcs_close (descrip, ndescrip)

	# Get the column descriptors for the columns in the list

	ndescrip = min (jcol, maxdescrip)
	do id = 1, ndescrip
	    call tcs_fillstruct (tp, Memi[clist+id-1], "", descrip[id])

	call sfree (sp)
end
