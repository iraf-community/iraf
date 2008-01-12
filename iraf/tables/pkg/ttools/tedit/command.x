include <tbset.h>
include "screen.h"
include "table.h"
include "paste.h"
include "field.h"
include "command.h"

define	BLANK		' '
define	SQUOTE		'\''
define	DQUOTE		'"'
define	HARMLESS	0.11
define	MAXROWS		10000

# ADD_CMD -- Add a column or row to the table

procedure add_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
int	which, iarg
pointer	sp, what, tab

string	options   "|row|column|"
string	notadded  "Cannot add column to table"

int	strdic(), option_cmd()

begin
	call smark (sp)
	call salloc (what, SZ_LINE, TY_CHAR)

	tab = TED_TABLE(scr)

	# Determine whether a row or column should be added

	which = option_cmd (options, nargs, arglist)

	while (which == 0) {
	    iarg = nargs + 1
	    call getstr_cmd ("Add row or column", iarg, nargs, arglist, 
			     Memc[what], SZ_LINE)
	    which = strdic (Memc[what], Memc[what], SZ_LINE, options)
	}

	# Call the appropriate routine

	if (which == 1) {
	    call addrow_cmd (scr, nargs, arglist)

	} else if (TED_ALLCOLS(tab) == YES) {
	    call addcol_cmd (scr, nargs, arglist)

	} else {
	    call warn1_prompt (scr, notadded)
	}

	call sfree (sp)
end

# ADDCOL_CMD -- Add a new column to the table

procedure addcol_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
int	iarg, type, icol, ncol, code, clen
pointer	sp, cname, cunits, ftnfmt, sppfmt, ctype, errmsg
pointer	tab, paste, tp, cp

string	nowrite   "Cannot change read only table"
string	nullcol   "No column added to table"
string	nocolumn  "Cannot add column"
string	nopaste   "Could not add column to paste table"

int	errget(), tbcigi(), strlen()
pointer	tbcnum()

begin
	# Check for read only table

	tab = TED_TABLE(scr)
	if (TED_READONLY(tab) == YES) {
	    call warn1_prompt (scr, nowrite)
	    return
	}

	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (cname, SZ_COLNAME, TY_CHAR)
	call salloc (cunits, SZ_COLUNITS, TY_CHAR)
	call salloc (ftnfmt, SZ_COLFMT, TY_CHAR)
	call salloc (sppfmt, SZ_COLFMT, TY_CHAR)
	call salloc (ctype, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Get table descriptors from screen structure

	paste = TED_PASTE(scr)
	tp = TED_TABPTR(tab)

	# Get parameters defining new column

	call getstr_cmd ("Column name", 2, nargs, arglist, 
			 Memc[cname], SZ_COLNAME)

	if (Memc[cname] == EOS) {
	   call write_prompt (scr, NO, nullcol)
	   return
	}

	iarg = 3
	repeat {
	    call getstr_cmd ("Column type (r,d,i,s,b,ch*n)", iarg, nargs, 
			     arglist, Memc[ctype], SZ_FNAME)

	    iferr (call tbbtyp (Memc[ctype], type)) {
		iarg = nargs + 1
		call ring_bell
	    } else {
		break
	    }
	} 

	iarg = 4
	repeat {
	    call getstr_cmd ("Column print format", iarg, nargs, arglist, 
			     Memc[ftnfmt], SZ_COLFMT)
	    call tbbftp (Memc[ftnfmt], Memc[sppfmt])

	    if (Memc[sppfmt] == EOS && Memc[ftnfmt] != EOS) {
		iarg = nargs + 1
		call ring_bell
	    } else {
		break
	    }
	}

	call getstr_cmd ("Column units", 5, nargs, arglist, 
			 Memc[cunits], SZ_COLUNITS)

	# Add new column to paste table

	if (paste != NULL) {
	    iferr {
		call tbcdef (TED_PSTPTR(paste), cp, Memc[cname], Memc[cunits], 
			     Memc[sppfmt], type, 1, 1)
	    } then {
		call warn1_prompt (scr, nopaste)
		return
	    }
	}

	# Add new column to table

	iferr {
	    call tbcdef (tp, cp, Memc[cname], Memc[cunits], 
			 Memc[sppfmt], type, 1, 1)
	} then {
	    code = errget (Memc[errmsg], SZ_LINE)
	    call warn2_prompt (scr, nocolumn, Memc[errmsg])
	    return
	}

	# Free old arrays containing table info and create new ones

	ncol = TED_NCOLS(tab) + 1
	call mfree (TED_COLARY(tab), TY_INT)
	call mfree (TED_TYPARY(tab), TY_INT)
	call mfree (TED_LENARY(tab), TY_INT)

	call malloc (TED_COLARY(tab), ncol, TY_INT)
	call malloc (TED_TYPARY(tab), ncol, TY_INT)
	call malloc (TED_LENARY(tab), ncol, TY_INT)

	# Load new column info into arrays

	TED_DIRTY(tab) = YES
	TED_NCOLS(tab) = ncol
	do icol = 1, ncol {
	    cp = tbcnum (tp, icol)
	    TED_COLPTR(tab, icol) = cp
	    TED_COLTYPE (tab,icol) = tbcigi (cp, TBL_COL_DATATYPE)
	    clen = tbcigi (cp, TBL_COL_FMTLEN)
	    call tbcigt (cp, TBL_COL_NAME, Memc[cname], SZ_COLNAME)
	    TED_COLLEN(tab,icol) = max (clen, strlen(Memc[cname]))
	}

	# Redraw screen

	call move_screen (scr, LEFT, YES)
	call sfree (sp)
end

# ADDROW_CMD -- Add null rows to the table

procedure addrow_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
int	nrow, row, numadd
pointer	tab, tptr

string	nowrite   "Cannot change read only table"

int	tbpsta()

begin
	# Check for read only table

	tab = TED_TABLE(scr)
	if (TED_READONLY(tab) == YES) {
	    call warn1_prompt (scr, nowrite)
	    return
	}

	# Get current number of rows in the table

	tptr = TED_TABPTR(tab)
	nrow = tbpsta (tptr ,TBL_NROWS)

	# Read command parameters

	call getint_cmd ("Add after row", 2, nargs, arglist, 
			 TED_CURROW(scr), 0, nrow, row)

	call getint_cmd ("Number of rows to add", 3, nargs, arglist, 
			 1, 0, INDEFI, numadd)

	# Return if number of rows to add is zero

	if (numadd == 0)
	    return

	# Add null rows to table

	TED_DIRTY(tab) = YES
	if (row == nrow) {
	    call tbtwer (tptr, row+numadd)

	} else {
	    call tbrsft (tptr, row, numadd)
	    call tbrnll (tptr, row+1, row+numadd)
	}

	# Reset label width if table has grown a lot

	TED_LABWIDTH(tab) = log10 (real(nrow + numadd + 1000)) + 1.0
	TED_LABWIDTH(tab) = max (6, TED_LABWIDTH(tab))

	# Redraw screen

	if (row <= TED_HIROW(scr)) 
	    call move_screen (scr, LEFT, YES)

end

# COPY_CMD -- Copy a range of lines to the paste buffer

procedure copy_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
bool	append
int	nrow, first, last, irow, orow, ncopy
pointer	tab, paste

int	tbpsta(), option_cmd()
pointer opn_paste()

begin
	tab = TED_TABLE(scr)
	paste = TED_PASTE(scr)
	nrow = tbpsta (TED_TABPTR(tab) ,TBL_NROWS)

	# Open paste buffer if not yet open

	if (paste == NULL) {
	    paste = opn_paste (scr)
	    if (paste == NULL)
		return
	}

	# Read copy command parameters

	append = option_cmd ("|append|", nargs, arglist) != 0

	call getint_cmd ("First row to copy", 2, nargs, arglist, 
			 TED_CURROW(scr), 1, nrow, first)
	call getint_cmd ("Last row to copy", 3, nargs, arglist, 
			 TED_CURROW(scr), 1, nrow, last)

	if (first < last) {
	    irow = first
	    ncopy = last - first + 1
	} else {
	    irow = last
	    ncopy = first - last + 1
	}

	if (append) {
	    orow = TED_PSTROWS(paste) + 1
	    TED_PSTROWS(paste) = TED_PSTROWS(paste) + ncopy
	} else {
	    orow = 1
	    TED_PSTROWS(paste) = ncopy
	}

	call move_paste (TED_TABPTR(tab), TED_PSTPTR(paste), irow, orow, ncopy)

end

# COUNT_CMD -- Count the number of words in a string

int procedure count_cmd (str)

char	str[ARB]	# i: String containing words
#--
char	ch
int	count, ic

begin
	# The absolute value of count is the number of the current
	# word of the string, count is negative if we are currently
	# between words.

	count = 0

	# Loop over all characters in the string

	for (ic = 1 ; str[ic] != EOS; ic = ic + 1) {
	    ch = str[ic]

	    if (count > 0) {
		if (ch <= ' ')
		    count = - count

	    } else if (ch > ' ') {
		count = - count + 1
	    }
	}

	return (abs(count))
end

# DELETE_CMD -- Delete a range of lines, copy them to the paste buffer

procedure delete_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
bool	append
int	nrow, first, last, irow, orow, ncopy
pointer	tab, tptr, paste

string	nowrite   "Cannot change read only table"

int	tbpsta(), option_cmd()
pointer opn_paste()

begin
	# Check for read only table

	tab = TED_TABLE(scr)
	tptr = TED_TABPTR(tab)
	if (TED_READONLY(tab) == YES) {
	    call warn1_prompt (scr, nowrite)
	    return
	}

	# Get paste table

	paste = TED_PASTE(scr)
	nrow = tbpsta (tptr ,TBL_NROWS)

	if (paste == NULL) {
	    paste = opn_paste (scr)
	    if (paste == NULL)
		return
	}

	# Read command parameters

	append = option_cmd ("|append|", nargs, arglist) != 0

	call getint_cmd ("First row to delete", 2, nargs, arglist, 
			 TED_CURROW(scr), 1, nrow, first)
	call getint_cmd ("Last row to delete", 3, nargs, arglist, 
			 TED_CURROW(scr), 1, nrow, last)

	if (first < last) {
	    irow = first
	} else {
	    irow = last
	    last = first
	    first = irow
	}

	# Copy deleted rows to paste table, then delete from original table

	ncopy = last - first + 1

	if (append) {
	    orow = TED_PSTROWS(paste) + 1
	    TED_PSTROWS(paste) = TED_PSTROWS(paste) + ncopy
	} else {
	    orow = 1
	    TED_PSTROWS(paste) = ncopy
	}

	call move_paste (tptr, TED_PSTPTR(paste), irow, orow, ncopy)
	call tbrdel (tptr, first, last)
	TED_DIRTY(tab) = YES

	# Add single blank row if all rows were deleted

	nrow = nrow - ncopy
	if (nrow < 1) {
	     nrow = 1
	     call tbtwer (tptr, 1)
	}

	# Set current row number and redraw screen

	if (TED_CURROW(scr) >= first && TED_CURROW(scr) <= last)
	    TED_CURROW(scr) = max (1, first-1)

	TED_CURROW(scr) = min (TED_CURROW(scr), nrow)

	if (first <= TED_HIROW(scr))
	    call move_screen (scr, LEFT, YES)

end

# EXIT_CMD -- Process the exit command

procedure exit_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
int	iscr
pointer	scr2

int	get_window()

begin
	for (iscr = 1; get_window (iscr, scr2) != EOF; iscr = iscr + 1)
	    call del_screen (scr2, YES)

end

# FIND_CMD -- Find the row which makes the expression true

procedure find_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
include "command.com"

int	first, last, row
pointer	tab

string	badexpr   "Syntax error"
string	blankexp  "No expression given"
string	notfound  "No rows matched expression"

int	tbpsta(), option_cmd(), count_cmd(), tbl_search()

begin
	tab = TED_TABLE(scr)

	# Get arguments of find command

	direction = option_cmd ("|forward|backwards|", nargs, arglist)
	if (direction == 0)
	    direction = 1

	call getstr_cmd ("Find expression", 2, nargs, arglist, 
			 search_exp, SZ_LINE)

	if (count_cmd (search_exp) == 0) {
	    call warn1_prompt (scr, blankexp)
	    search_exp[1] = EOS
	    return
	}

	# Set limits for search

	if (direction == 2) {
	    first = TED_CURROW(scr)
	    last = 1
	} else {
	    first = TED_CURROW(scr)
	    last = tbpsta (TED_TABPTR(tab) ,TBL_NROWS)
	}

	# Perform search and report results

	row = tbl_search (TED_TABPTR(tab), search_exp, first, last)

	if (row == ERR) {  # syntax error 
	    # Redraw screen to hide error message from evexpr()
	    call move_screen (scr, LEFT, YES)
	    call warn2_prompt (scr, badexpr, search_exp)
	    search_exp[1] = EOS

	} else if (row == 0) {  # row not found
	    call write_prompt (scr, NO, notfound)

	} else {  # row found, update screen descriptor
	    TED_CURROW(scr) = row
	}

end

# FUNC_CMD -- Change a single column using a function

procedure func_cmd (scr, nargs, arglist, func)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
extern	func		# i: Function which modifies a string in place
#--
int	col, irow, nrow, len
pointer	sp, defcol, colstr, tab, cptr, tptr

string	nowrite   "Cannot change read only table"
string	numeric   "Cannot change numeric column"

int	tbpsta()

begin
	# Check for read only table

	tab = TED_TABLE(scr)
	if (TED_READONLY(tab) == YES) {
	    call warn1_prompt (scr, nowrite)
	    return
	}

	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (defcol, SZ_COLNAME, TY_CHAR)

	# Get name of column

	cptr = TED_COLPTR(tab, TED_CURCOL(scr))
	call tbcigt (cptr, TBL_COL_NAME, Memc[defcol], SZ_COLNAME)

	call getcol_cmd ("Column to change", 2,  nargs, arglist, tab, 
			 Memc[defcol], col)

	# Make sure it's a string column

	if (TED_COLTYPE(tab,col) > 0) {
	    call warn1_prompt (scr, numeric)

	} else {

	    # Allocate array to hold field

	    len = - TED_COLTYPE(tab,col)
	    call salloc (colstr, len, TY_CHAR)

	    # Get current number of rows in the table

	    tptr = TED_TABPTR(tab)
	    cptr = TED_COLPTR(tab, col)
	    nrow = tbpsta (tptr ,TBL_NROWS)

	    # Retrieve each field and convert case

	    TED_DIRTY(tab) = YES
	    do irow = 1, nrow {
		call tbegtt (tptr, cptr, irow, Memc[colstr], len)
		call func (Memc[colstr])
		call tbeptt (tptr, cptr, irow, Memc[colstr])
	    }
	}

	# Redraw screen if column is displayed

	if (col >= TED_LOCOL(scr) && col <= TED_HICOL(scr))
	    call move_screen (scr, LEFT, YES)

	call sfree (sp)

end

# GETCOL_CMD -- Get a column name from the argument list

procedure getcol_cmd (argname, index, nargs, arglist, tab, defcol, icol)

char	argname[ARB]	# i: Argument name (used as prompt if not found)
int	index		# i: Index to string within argument list
int	nargs		# i: Number of arguments in list
char	arglist[ARB]	# i: List of arguments, separated by EOS characters
pointer	tab		# i: Table descriptor
char	defcol[ARB]	# i: Default column name (or EOS)
int	icol		# o: Column number
#--
int	ic, ncol, jcol, iarg, junk
pointer	sp, cname, cprompt, colptr[1]

int	ctoi()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (cname, SZ_COLNAME, TY_CHAR)
	call salloc (cprompt, SZ_LINE, TY_CHAR)

	# Get the string containing the column name

	if (defcol[1] == EOS) {
	    call strcpy (argname, Memc[cprompt], SZ_LINE)
	} else {
	    call sprintf (Memc[cprompt], SZ_LINE, "%s (%s)")
	    call pargstr (argname)
	    call pargstr (defcol)
	}

	call getstr_cmd (Memc[cprompt], index, nargs, arglist, 
			 Memc[cname], SZ_COLNAME)

	if (Memc[cname] == EOS)
	    call strcpy (defcol, Memc[cname], SZ_COLNAME)

	# Loop until valid column name found

	icol = 0
	while (icol == 0) {
	    colptr[1] = NULL
	    Memc[cprompt] = EOS

	    # Get a column pointer from the column template

	    iferr {
		call tctexp (TED_TABPTR(tab), Memc[cname], 1, ncol, colptr)
	    } then {
		# More than one column matches the name
		call strcpy ("Ambiguous column name. ", Memc[cprompt], SZ_LINE)

	    } else {
		# If one name matched, check against list of column pointers

		if (ncol == 1) {
		    for (jcol = 1; jcol <= TED_NCOLS(tab); jcol = jcol + 1) {
			if (colptr[1] == TED_COLPTR(tab,jcol)) {
			    icol = jcol
			    break
			}
		    }
		}

		# Convert name to number, see if number is within range

		if (icol == 0) {
		    ic = 1
		    junk = ctoi (Memc[cname], ic, icol)

		    if (Memc[cname+ic-1] != EOS)
			icol = 0
		    else if (icol < 1 || icol > TED_NCOLS(tab))
			icol = 0
		}

		if (icol == 0)
		    call strcpy ("Column not found. ", Memc[cprompt], SZ_LINE)
	    }

	    # If column not matched, read new name interactively

	    if (icol == 0) {
		iarg = nargs + 1
		call strcat (argname, Memc[cprompt], SZ_LINE)

		call getstr_cmd (Memc[cprompt], iarg, nargs, arglist, 
				 Memc[cname], SZ_FNAME)

		if (Memc[cname] == EOS)
		    call strcpy (defcol, Memc[cname], SZ_COLNAME)
	    }	    		
	}

	call sfree (sp)
end

# GETINT_CMD -- Get an integer from the argument list

procedure getint_cmd (argname, index, nargs, arglist, defval,
		      minval, maxval, value)

char	argname		# i: Argument name (used as prompt if not found)
int	index		# i: Index to string within argument list
int	nargs		# i: Number of arguments in list
char	arglist[ARB]	# i: List of arguments, separated by EOS characters
int	defval		# i: Default legal value (or INDEFI)
int	minval		# i: Minimum legal value (or INDEFI)
int	maxval		# i: Maximum legal value (or INDEFI)
int	value		# o: Output value
#--
int	ic, iarg, junk
pointer	sp, valstr, prompt

string	typemsg   "Please enter a number. "
string	rangemsg  "Out of range (%d - %d). "

int	ctoi()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (valstr, SZ_FNAME, TY_CHAR)
	call salloc (prompt, SZ_LINE, TY_CHAR)

	# Get the string representing the value

	if (IS_INDEFI (defval)) {
	    call strcpy (argname, Memc[prompt], SZ_LINE)
	} else {
	    call sprintf (Memc[prompt], SZ_LINE, "%s (%d)")
	    call pargstr (argname)
	    call pargi (defval)
	}

	call getstr_cmd (Memc[prompt], index, nargs, arglist, 
			 Memc[valstr], SZ_FNAME)

	if (Memc[valstr] == EOS) {
	    value = defval
	} else {
	    ic = 1
	    junk = ctoi (Memc[valstr], ic, value)
	    if (Memc[valstr+ic-1] != EOS)
		value = INDEFI
	}

	# Loop until valid value is found

	repeat {
	    if (IS_INDEFI(value)) {
	    	call strcpy (typemsg, Memc[prompt], SZ_LINE)
				 
	    } else if ((value < minval && ! IS_INDEFI (minval)) ||
		(value > maxval && ! IS_INDEFI (maxval))   ) {
		call sprintf (Memc[prompt], SZ_LINE, rangemsg)
		call pargi (minval)
		call pargi (maxval)

	    } else {
		break
	    }

	   # If the string was not valid, get the value interactively

	    iarg = nargs + 1
	    call strcat (argname, Memc[prompt], SZ_LINE)

	    call getstr_cmd (Memc[prompt], iarg, nargs, arglist, 
			     Memc[valstr], SZ_FNAME)

	    ic = 1
	    junk = ctoi (Memc[valstr], ic, value)
	    if (Memc[valstr+ic-1] != EOS)
		value = INDEFI
	}

	call sfree (sp)
end

# GETSTR_CMD -- Get a string from the command argument list

procedure getstr_cmd (argname, index, nargs, arglist, str, maxch)

char	argname		# i: Argument name (used as prompt if not found)
int	index		# i: Index to string within argument list
int	nargs		# i: Number of arguments in list
char	arglist[ARB]	# i: List of arguments, separated by EOS characters
char	str[ARB]	# o: Output string
int	maxch		# i: Maximum length of output string
#--
int	ic, jc, iarg
pointer	sp, prompt

string	nullarg  "getstr_cmd: null argument found in argument list"

begin
	# Allocate dynamic memory for prompt

	call smark (sp)
	call salloc (prompt, SZ_LINE, TY_CHAR)

	# Read the argument interactively if not supplied by the user
	# Otherwise, copy from the argument list string

	if (index > nargs) {
	    call strcpy (argname, Memc[prompt], SZ_LINE)
	    call strcat ("?", Memc[prompt], SZ_LINE)

	    call read_prompt (Memc[prompt], str, maxch)

	} else {
	    # Skip over leading arguments

	    ic = 1
	    for (iarg = 1; iarg < index; iarg = iarg + 1) {
		if (arglist[ic] == EOS)
		    call err1_prompt (nullarg)

		while (arglist[ic] != EOS)
		    ic = ic + 1
		ic = ic + 1
	    }

	    # Copy into output string

	    for (jc = 1; jc <= maxch && arglist[ic] != EOS; jc = jc + 1) {
		str[jc] = arglist[ic]
		ic = ic + 1
	    }
	    str[jc] = EOS
	}

	call sfree (sp)
end

# GOTO_CMD -- Process the goto command

procedure goto_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
int	nrow, row, col
pointer	sp, defcol, tab, cptr

int	tbpsta()

string	notable  "No table associated with this screen"

begin
	# Allocate dynamic memory for column name

	call smark (sp)
	call salloc (defcol, SZ_COLNAME, TY_CHAR)

	# Get number of rows in table

	tab = TED_TABLE(scr)
	if (tab == NULL)
	    call err1_prompt (notable)
	else
	    nrow = tbpsta (TED_TABPTR(tab), TBL_NROWS)

	cptr = TED_COLPTR(tab, TED_CURCOL(scr))
	call tbcigt (cptr, TBL_COL_NAME, Memc[defcol], SZ_COLNAME)

	# Get the row and column numbers

	call getint_cmd ("Go to row", 2, nargs, arglist, 
			 TED_CURROW(scr), 1, nrow, row)
	call getcol_cmd ("Go to column", 3,  nargs, arglist, tab, 
			 Memc[defcol], col)

	# Update screen descriptor

	TED_CURROW(scr) = row
	TED_CURCOL(scr) = col

	call sfree (sp)
end

# HELP_CMD -- Process the help command

procedure help_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--

begin
	call help_screen (TED_WINDOW(scr))

end

# INIT_CMD -- Initialize the global variables used by commands

procedure init_cmd(silent)

bool	silent		# i: do not ring bell when error occurs
#--
include "command.com"

begin
	direction = 1
	search_exp[1] = EOS
	call init_bell (silent)
end

# INSERT_CMD -- Process an insert command

procedure insert_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
int	irow, nrow
pointer	tab, paste

string	nowrite  "Cannot change read only table"
string	nopaste  "Paste buffer is empty"

int	tbpsta()
pointer opn_paste()

begin
	# Check for read only table

	tab = TED_TABLE(scr)
	if (TED_READONLY(tab) == YES) {
	    call warn1_prompt (scr, nowrite)
	    return
	}

	paste = TED_PASTE(scr)
	nrow = tbpsta (TED_TABPTR(tab) ,TBL_NROWS)

	if (paste == NULL) {
	    paste = opn_paste (scr)
	    if (paste == NULL)
		return
	}

	# Get insert command parameters

	call getint_cmd ("Insert after row number", 2, nargs, arglist, 
			 TED_CURROW(scr), 0, nrow, irow)

	# Check to see if there is something to insert

	if (TED_PSTROWS(paste) <= 0) {
	    call warn1_prompt (scr, nopaste)
	    return
	}

	TED_DIRTY(tab) = YES
	if (irow < nrow)
	    call tbrsft (TED_TABPTR(tab), irow+1, TED_PSTROWS(paste))

	call move_paste (TED_PSTPTR(paste), TED_TABPTR(tab), 
			 1, irow+1, TED_PSTROWS(paste))

	if (irow <= TED_HIROW(scr))
	    call move_screen (scr, LEFT, YES)

end

# LOWER_CMD -- Convert a column to lower cse

procedure lower_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
extern	strlwr

begin
	# A common routine handle both the lower and upper case
	# commands, since they are so similar

	call func_cmd (scr, nargs, arglist, strlwr)
end

# NEXT_CMD -- Repeat the search for an expression

procedure next_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
include "command.com"

int	dir, nrow, first, last, row
pointer	tab

string  nofind    "No previous find command"
string	badexpr   "Syntax error"
string	notfound  "No rows matched expression"

int	tbpsta(), option_cmd(), tbl_search()

begin
	tab = TED_TABLE(scr)
	nrow = tbpsta (TED_TABPTR(tab) ,TBL_NROWS)

	# Make sure there was a previous find command

	if (search_exp[1] == EOS) {
	    call warn1_prompt (scr, nofind)
	    return
	}

	# Get the command option

	dir = option_cmd ("|forward|backwards|", nargs, arglist)
	if (dir != 0)
	    direction = dir

	# Set limits for search

	if (direction == 2) {
	    first = max (TED_CURROW(scr)-1, 1)
	    last = 1
	} else {
	    first = min (TED_CURROW(scr)+1, nrow)
	    last = nrow
	}

	# Perform search and report results

	row = tbl_search (TED_TABPTR(tab), search_exp, first, last)

	if (row == ERR) {  # syntax error
	    call warn2_prompt (scr, badexpr, search_exp)
	    search_exp[1] = EOS

	} else if (row == 0) {  # row not found
	    call write_prompt (scr, NO, notfound)

	} else {  # row found, update screen descriptor
	    TED_CURROW(scr) = row
	}

end

# OPTION_CMD -- Get the command option

int procedure option_cmd (optlist, nargs, arglist)

char	optlist[ARB]	# i: List of legal options
int	nargs		# u: Number of command arguments
char	arglist[ARB]	# u: Argument list
#--
int	option, iarg, ic, jc, last[2]
pointer	sp, arg1, arg2

int	strdic()

begin
	# No option if number of arguments < 2
	if (nargs < 2)
	    return (0)

	# Allocate dynamic memory for optional argument

	call smark (sp)
	call salloc (arg1, SZ_LINE, TY_CHAR)
	call salloc (arg2, SZ_LINE, TY_CHAR)

	# Read optional argument, match against list of options

	call getstr_cmd ("Option", 2, nargs, arglist, Memc[arg1], SZ_LINE)
	option = strdic (Memc[arg1], Memc[arg2], SZ_LINE, optlist)

	# If matched, remove option from argument list

	if (option != 0) {
	    ic = 1
	    do iarg = 1, 2 {
		while (arglist[ic] != EOS)
		    ic = ic + 1
		last[iarg] = ic
		ic = ic + 1
	    }

	    ic = last[1]
	    jc = last[2]
	    repeat {
		ic = ic + 1
		jc = jc + 1
		arglist[ic] = arglist[jc]
	    } until (arglist[jc] == EOS && arglist[jc-1] == EOS)

	    nargs = nargs - 1
	}

	call sfree (sp)
	return (option)

end

# PARSE_CMD -- Parse a command string

procedure parse_cmd (command, code, nargs, arglist, maxch)

char	command[ARB]	# i: Command to be parsed
int	code		# o: Command code (0 if unknown command)
int	nargs		# o: Number of arguments (including command name)
char	arglist[ARB]	# o: Array of arguments, packed into one string
int	maxch		# i: Declared length of arglist
#--
int	ic, jc, delim
pointer	sp, temp

string	cmdlist  TED_CMDLIST

int	strdic()

begin
	# Allocate temporary string for full command name

	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)

	# Break command string into arguements
	# Count the number of arguements

	jc = 1
	nargs = 0
	delim = EOS
	for (ic = 1; command[ic] != EOS && jc <= maxch; ic = ic + 1) {
	    if (delim == EOS) {
		if (command[ic] > BLANK) {
		    nargs = nargs + 1
		    if (command[ic] == SQUOTE) {
			delim = SQUOTE
		    } else if (command[ic] == DQUOTE) {
			delim = DQUOTE
		    } else {
			ic = ic - 1	# push back non-blank character
			delim = BLANK
		    }
		}

	    } else if (delim == BLANK) {
		if (command[ic] <= BLANK) {
		    arglist[jc] = EOS
		    jc = jc + 1
		    delim = EOS

		} else {
		    arglist[jc] = command[ic]
		    jc = jc + 1
		}

	    } else {
		if (command[ic] == delim) {
		    arglist[jc] = EOS
		    jc = jc + 1
		    delim = EOS

		} else {
		    arglist[jc] = command[ic]
		    jc = jc + 1
		}
	    }
	}
	arglist[jc] = EOS

	# Get the code which corresponds to the first arguement
	# (the command name)

	if (nargs == 0) {
	    code = 0
	} else {
	   code = strdic (arglist, Memc[temp], SZ_FNAME, cmdlist)
	}

	call sfree (temp)
end

# QUIT_CMD -- Quit the editor without saving files

procedure quit_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
int	iscr
pointer	scr2

int	get_window()

begin
	for (iscr = 1; get_window (iscr, scr2) != EOF; iscr = iscr + 1)
	    call del_screen (scr2, NO)

end

# SET_CMD -- Set a column to the value of an expression

procedure set_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
include "../tabvar.com"

bool	done
int	col, irow, nrow, nbuf, coltype, exptype
pointer	sp, defcol, expr, buffer, tab, cptr, tptr, code

string	nowrite   "Cannot change read only table"
string	syntax    "Syntax error in expression"

extern	tabvar
int	tbpsta()
pointer	vex_compile()

begin
	# Check for read only table

	tab = TED_TABLE(scr)
	if (TED_READONLY(tab) == YES) {
	    call warn1_prompt (scr, nowrite)
	    return
	}

	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (defcol, SZ_COLNAME, TY_CHAR)
	call salloc (expr, SZ_LINE, TY_CHAR)

	# Get name of column

	cptr = TED_COLPTR(tab, TED_CURCOL(scr))
	call tbcigt (cptr, TBL_COL_NAME, Memc[defcol], SZ_COLNAME)

	call getcol_cmd ("Column to change", 2,  nargs, arglist, tab, 
			 Memc[defcol], col)

	call getstr_cmd ("Expression", 3, nargs, arglist, 
			 Memc[expr], SZ_LINE)

	# Get table info

	tptr = TED_TABPTR(tab)
	cptr = TED_COLPTR(tab, col)
	nrow = tbpsta (tptr, TBL_NROWS)

	coltype = TED_COLTYPE(tab,col)
	if (coltype < 0) {
	    # String columns copy the expression verbatim

	    TED_DIRTY(tab) = YES
	    do irow = 1, nrow
		call tbeptt (tptr, cptr, irow, Memc[expr])

	} else {
	    # Numeric columns use the expression evaluator

	    iferr {
		code = vex_compile (Memc[expr])
	    } then {
		call warn2_prompt (scr, syntax, Memc[expr])
		call sfree (sp)
		return
	    }

	    # Initialize common block used by tabvar()

	    tabptr = tptr
	    firstrow = 1
	    lastrow = MAXROWS

	    done = false
	    nullval = HARMLESS

	    repeat {
		if (lastrow >= nrow) {
		    done = true
		    lastrow = nrow
		}

		iferr {
		    call vex_eval (code, tabvar, nullval, exptype)
		} then {
		    call warn2_prompt (scr, syntax, Memc[expr])
		    call sfree (sp)
		    return
		}

		nbuf = (lastrow - firstrow) + 1

		# Copy results to column

		switch (coltype) {
		case TY_BOOL, TY_SHORT, TY_INT, TY_LONG:
		    call malloc (buffer, nbuf, TY_INT)
		    call vex_copyi (code, INDEFI, Memi[buffer], nbuf)
		    call tbcpti (tptr, cptr, Memi[buffer], firstrow, lastrow)
		    call mfree (buffer, TY_INT)
		case TY_REAL:
		    call malloc (buffer, nbuf, TY_REAL)
		    call vex_copyr (code, INDEFR, Memr[buffer], nbuf)
		    call tbcptr (tptr, cptr, Memr[buffer], firstrow, lastrow)
		    call mfree (buffer, TY_REAL)
		case TY_DOUBLE:
		    call malloc (buffer, nbuf, TY_DOUBLE)
		    call vex_copyd (code, INDEFD, Memd[buffer], nbuf)
		    call tbcptd (tptr, cptr, Memd[buffer], firstrow, lastrow)
		    call mfree (buffer, TY_DOUBLE)
		}

		firstrow = firstrow + MAXROWS
		lastrow = lastrow + MAXROWS
	    } until (done)

	    TED_DIRTY(tab) = YES
	    call vex_free (code)
	}

	# Redraw screen if column is displayed

	if (col >= TED_LOCOL(scr) && col <= TED_HICOL(scr))
	    call move_screen (scr, LEFT, YES)

	call sfree (sp)

end

# SUB_CMD --  Substitute strings in a single column

procedure sub_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
int	col, irow, nrow, len
pointer	sp, defcol, from, to, colstr, tab, cptr, tptr

string	nowrite   "Cannot change read only table"
string	numeric   "Cannot change numeric column"

bool	substitute()
int	tbpsta()

begin
	# Check for read only table

	tab = TED_TABLE(scr)
	if (TED_READONLY(tab) == YES) {
	    call warn1_prompt (scr, nowrite)
	    return
	}

	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (defcol, SZ_COLNAME, TY_CHAR)
	call salloc (from, SZ_LINE, TY_CHAR)
	call salloc (to, SZ_LINE, TY_CHAR)

	# Get name of column. Make sure it's a string column

	cptr = TED_COLPTR(tab, TED_CURCOL(scr))
	call tbcigt (cptr, TBL_COL_NAME, Memc[defcol], SZ_COLNAME)

	call getcol_cmd ("Column to change", 2,  nargs, arglist, tab, 
			 Memc[defcol], col)

	if (TED_COLTYPE(tab,col) > 0) {
	    call warn1_prompt (scr, numeric)
	    call sfree (sp)
	    return
	}

	# Get target string

	call getstr_cmd ("Search string", 3, nargs, arglist, 
			 Memc[from], SZ_LINE)

	if (Memc[from] == EOS) {
	    call sfree (sp)
	    return
	}

	# Get replacement string

	call getstr_cmd ("Replacement string", 4, nargs, arglist, 
			 Memc[to], SZ_LINE)

	# Allocate array to hold field

	len = - TED_COLTYPE(tab,col)
	call salloc (colstr, len, TY_CHAR)

	# Get current number of rows in the table

	tptr = TED_TABPTR(tab)
	cptr = TED_COLPTR(tab, col)
	nrow = tbpsta (tptr ,TBL_NROWS)

	# Retrieve each field and perform substitution

	do irow = 1, nrow {
	    call tbegtt (tptr, cptr, irow, Memc[colstr], len)

	    if (substitute (Memc[from], Memc[to], Memc[colstr], len)) {
		TED_DIRTY(tab) = YES
		call tbeptt (tptr, cptr, irow, Memc[colstr])
	    }
	}

	# Redraw screen if column is displayed

	if (col >= TED_LOCOL(scr) && col <= TED_HICOL(scr))
	    call move_screen (scr, LEFT, YES)

	call sfree (sp)

end

# UPPER_CMD -- Convert a column to upper cse

procedure upper_cmd (scr, nargs, arglist)

pointer	scr		# i: Current screen descriptor
int	nargs		# i: Number of arguments
char	arglist[ARB]	# i: Argument list
#--
extern	strupr

begin
	# A common routine handle both the lower and upper case
	# commands, since they are so similar

	call func_cmd (scr, nargs, arglist, strupr)
end

