include <tbset.h>

# GG_RD1COL -- read plot data from a column of a table

int procedure gg_rd1col (table, column, 
	rdmarks, errcol, erraxis, 
	x, y, size)

char	table[SZ_FNAME]		# Table name
char	column[SZ_COLNAME]	# Data column name
bool	rdmarks			# Read errors from file?
char	errcol[SZ_COLNAME]	# Error column name
int	erraxis			# X or Y errors?
pointer	x, y, size		# Pointers to x, y and size vectors

pointer	tdp			# Pointers to table descriptor
pointer	cdp			# Pointers to column descriptors
pointer	null			# Pointer to null
int	numrows			# Number of column rows
int	numcols
int	i
char	errmsg[SZ_LINE]		# Error message

pointer	tbtopn()
int	tbpsta()

errchk	tbtopn

begin
    numcols = 1
    tdp = tbtopn (table, READ_ONLY, 0)

    call tbcfnd (tdp, column, cdp, numcols)
    if (cdp <= 0) {
	call sprintf (errmsg, SZ_LINE, "Cannot find column %s in table %s\n")
	    call pargstr (column)
	    call pargstr (table)
	call error (0, errmsg)
    }

    numrows = tbpsta (tdp, TBL_NROWS)

    # Allocate space for the plot arrays
#    iferr {
	call malloc (x, numrows, TY_REAL)
	call malloc (y, numrows, TY_REAL)
	call malloc (size, numrows, TY_REAL)
	call malloc (null, numrows, TY_BOOL)
#    } then
#	call erract (EA_FATAL)

    # Read the column
    call tbcgtr (tdp, cdp, Memr[y], Memb[null], 1, numrows)

    if (rdmarks)
	call ggrser (tdp, errcol, numrows, 
	    Memr[x], Memr[y], Memr[size], erraxis)

    # Fill in the X values (independent variable)
    do i = 1, numrows
	Memr[x+i-1] = i

    call tbtclo (tdp)
    call mfree (null, TY_BOOL)

    return (numrows)
end
