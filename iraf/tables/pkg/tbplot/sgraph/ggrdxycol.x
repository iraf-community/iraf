include <tbset.h>

# GG_RDXYCOL -- read X and Y plot data from two column of the same table

int procedure gg_rdxycol (table, xcolumn, ycolumn, 
	rdmarks, errcol, erraxis, 
	x, y, size)

char	table[SZ_FNAME]		# Table name
char	xcolumn[SZ_COLNAME], ycolumn[SZ_COLNAME]	# Column names
bool	rdmarks			# Read errors from file?
char	errcol[SZ_COLNAME]	# Error column name
int	erraxis			# X or Y errors?
pointer	x, y, size		# Pointers to x, y and size vectors

pointer	tdp			# Pointer to table descriptor
pointer	xcdp, ycdp		# Pointers to column descriptors
pointer	null			# Pointer to null
int	numrows			# Number of column rows
int	numcols
char	errmsg[SZ_LINE]		# Error message

pointer	tbtopn()
int	tbpsta()

errchk	tbtopn

begin
    numcols = 1
    tdp = tbtopn (table, READ_ONLY, 0)

    numrows = tbpsta (tdp, TBL_NROWS)

    # Allocate space for the plot arrays
#    iferr {
	call malloc (x, numrows, TY_REAL)
	call malloc (y, numrows, TY_REAL)
	call malloc (size, numrows, TY_REAL)
	call malloc (null, numrows, TY_BOOL)
#    } then
#	call erract (EA_FATAL)

    call tbcfnd (tdp, xcolumn, xcdp, numcols)
    if (xcdp <= 0) {
	call sprintf (errmsg, SZ_LINE, "Cannot find column %s in table %s\n")
	    call pargstr (xcolumn)
	    call pargstr (table)
	call error (0, errmsg)
    }

    call tbcgtr (tdp, xcdp, Memr[x], Memb[null], 1, numrows)

    call tbcfnd (tdp, ycolumn, ycdp, numcols)
    if (ycdp <= 0) {
	call sprintf (errmsg, SZ_LINE, "Cannot find column %s in table %s\n")
	    call pargstr (ycolumn)
	    call pargstr (table)
	call error (0, errmsg)
    }

    call tbcgtr (tdp, ycdp, Memr[y], Memb[null], 1, numrows)

    if (rdmarks)
	call ggrser (tdp, errcol, numrows, 
	    Memr[x], Memr[y], Memr[size], erraxis)

    call tbtclo (tdp)
    call mfree (null, TY_BOOL)

    return (numrows)
end
