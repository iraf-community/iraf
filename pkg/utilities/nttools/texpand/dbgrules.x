include	<tbset.h>

define	INT_DEFLEN	10
define	REAL_DEFLEN	14
define	DBL_DEFLEN	24

# DBGRULES -- Write the non-null rows in a table to a debug file
#
# B.Simon	25-Apr-88	Original

procedure dbg_rules (tp, title, row1, row2, dbg)

pointer	tp		# i: Table descriptor
char	title[ARB]	# i: Title to print above table
int	row1		# i: First row to print
int	row2		# i: Last row to print
int	dbg		# i: File descriptor of debug file
#--
bool	nullflg
double	dblval
int	pwidth, ncol, irow, icol, jcol, collen, totlen, intval
pointer	sp, col,strval, colname, colptr, typptr, lenptr
real	realval

int	tbpsta(), tbcnum(), tbcigi(), envgeti(), strlen()

begin
	# First, make sure there is something to print

	if (row2 < row1 || dbg == NULL)
	    return

	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (strval, SZ_LINE, TY_CHAR)
	call salloc (colname, SZ_COLNAME, TY_CHAR)

	# Allocate dynamic memory for column arrays

	ncol = tbpsta (tp, TBL_NCOLS)
	call salloc (typptr, ncol, TY_INT)
	call salloc (colptr, ncol, TY_INT)
	call salloc (lenptr, ncol, TY_INT)

	# Get width of terminal screen

	pwidth = envgeti ("ttyncols")

	# Print title

	call fprintf (dbg, "%s\n")
	    call pargstr (title)

	# Compute width of each column in output

	jcol = 0
	totlen = 0
	do icol = 1, ncol {

	    # Check to see if this column is excluded from the output

	    col = tbcnum (tp, icol)
	    call tbrgtt (tp, col, Memc[strval], nullflg, SZ_LINE, 1, row1)

	    if (row1 != row2 || ! nullflg) {
		jcol = jcol + 1

		call tbcigt (col, TBL_COL_NAME, Memc[colname], SZ_COLNAME)
		Memi[colptr+jcol-1] = col
		Memi[typptr+jcol-1] = tbcigi (col, TBL_COL_DATATYPE)

		# Set column width to default for its type

		switch (Memi[typptr+jcol-1]) {
		case TY_SHORT, TY_INT, TY_LONG:
		    collen = INT_DEFLEN
		case TY_REAL:
		    collen = REAL_DEFLEN
	        case TY_DOUBLE:
		    collen = DBL_DEFLEN
		default:
		    collen = - Memi[typptr+jcol-1]
		}

		# Adjust width to allow room for column titles

		collen = max (collen, strlen (Memc[colname]))
	        totlen = totlen + collen + 1

		# Write the column titles

		if (jcol > 1 && totlen > pwidth)
		    call fprintf (dbg, "\n")

		if (Memi[typptr+jcol-1] > 0) {
		    call fprintf (dbg, " %*s")
		    call pargi (collen)
		} else {
		    call fprintf (dbg, " %*s")
		    call pargi (-collen)
		}
		call strupr (Memc[colname])
		call pargstr (Memc[colname])

		# Set sign to indicate start of new line

		if (jcol > 1 && totlen > pwidth) {
		    totlen = collen + 1
		    Memi[lenptr+jcol-2] = - Memi[lenptr+jcol-2]
		}

		Memi[lenptr+jcol-1] = collen
	    }
	}

	# Recompute number of columns and force newline at end of title row

	ncol = jcol
	if (ncol > 0)
	    Memi[lenptr+ncol-1] = - Memi[lenptr+ncol-1]
		call fprintf (dbg, "\n")

	# Read the data from the database and write the data to STDOUT

	do irow = row1, row2 {
	    do jcol = 1, ncol {

	        col = Memi[colptr+jcol-1]
		collen = abs (Memi[lenptr+jcol-1])

		switch(Memi[typptr+jcol-1]) {
		case TY_SHORT, TY_INT, TY_LONG:
		    call tbegti (tp, col, irow, intval)
		    call fprintf (dbg, " %*d")
			call pargi (collen)
			call pargi (intval)
		case TY_REAL:
		    call tbegtr (tp, col, irow, realval)
		    call fprintf (dbg, " %*.7g")
			call pargi (collen)
			call pargr (realval)
		case TY_DOUBLE:
		    call tbegtd (tp, col, irow, dblval)
		    call fprintf (dbg, " %*.16g")
			call pargi (collen)
			call pargd (dblval)
		default:
		    call tbegtt (tp, col, irow, Memc[strval], SZ_LINE)
		    call fprintf (dbg, " %*s")
			call pargi (-collen)
			call pargstr (Memc[strval])
		}

		if (Memi[lenptr+jcol-1] < 0)
		    call fprintf (dbg, "\n")

	    }
	}

	call fprintf (dbg, "\n\n")
	call sfree (sp)

end
