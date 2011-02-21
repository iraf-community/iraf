include	<error.h>
include <tbset.h>

# GT_RDXYCOL -- read X and Y plot data from two column of the same table

procedure gt_rdxycol (tp, xcolumn, ycolumn, x, y, size, null, numrows, bad_column)

pointer	tp			# Table descriptor
char	xcolumn[SZ_COLNAME], ycolumn[SZ_COLNAME]	# Column names
pointer	x, y, size		# Pointers to x, y and size vectors
int	numrows			# number of pixels or rows in the table
char	bad_column[SZ_COLNAME]	# Return bad column name

pointer	xcdp, ycdp		# Pointers to column descriptors
pointer	null			# Pointer to null
int	numcols
int	i

int	tbpsta()
bool	streq()

begin
    numcols = 1
    numrows = tbpsta (tp, TBL_NROWS)
    call aclrc (bad_column, SZ_COLNAME)

    if (streq (xcolumn, NULL)) {
	do i = 1, numrows 
	    Memr[x + i - 1] = float(i)
    } else {
	call tbcfnd (tp, xcolumn, xcdp, numcols)
        if (xcdp <= 0) {
	    numrows = -1
	    call amovc (xcolumn, bad_column, SZ_COLNAME)
	    return 
        }
        call tbcgtr (tp, xcdp, Memr[x], Memb[null], 1, numrows)
    }

    call tbcfnd (tp, ycolumn, ycdp, numcols)
    if (ycdp <= 0) {
	    numrows = -1
	    call amovc (ycolumn, bad_column, SZ_COLNAME)
	    return 
    }

    call tbcgtr (tp, ycdp, Memr[y], Memb[null], 1, numrows)

    return
end
