include "tbtables.h"

# tbxwnc -- X write new columns
# Write new column descriptors into a row-ordered table.
# The table must have already been reorganized (if necessary) to make
# sufficient space for column descriptors or row length.
#
# Phil Hodge,  1-May-1989  Change calling sequence; don't call tbxsiz.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.
# Phil Hodge, 14-Apr-1998  Change calling sequence of tbcwcd.

procedure tbxwnc (tp, colptr, numcols, old_colused)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: pointers to descriptors for new columns
int	numcols			# i: number of new columns
int	old_colused		# i: previous value of TB_COLUSED
#--
pointer cp			# Pointer to a specific column
int	k			# Loop index
errchk	tbxncn, tbcwcd

begin
	# Assign appropriate indef values in indef record.
	do k = 1, numcols
	    call tbbnll (tp, colptr[k])

	# Write descriptors of new columns to table.
	do k = 1, numcols {
	    cp = colptr[k]
	    call tbcwcd (tp, cp)
	}

	# Assign indef values for each new column in each existing row.
	if (TB_NROWS(tp) > 0)
	    call tbxncn (tp, old_colused, Memc[TB_INDEF(tp)])
end
