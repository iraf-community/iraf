include "tbtables.h"

# tbywnc -- Y write new column
# Write new column descriptors into a column-ordered table.
# The table must have already been reorganized (if necessary) to make
# sufficient space for column descriptors or row length.
#
# Phil Hodge,  1-May-1989  Change calling sequence; don't call tbysiz.
# Phil Hodge, 14-Apr-1998  Change calling sequence of tbcwcd;
#			change LEN_COLSTRUCT to LEN_COLDEF.

procedure tbywnc (tp, colptr, numcols)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: pointers to descriptors for new columns
int	numcols			# i: number of new columns
#--
pointer cp			# Pointer to a specific column
int	k			# Loop index
errchk	tbcwcd, tbyncn

begin
	# Write descriptors of new columns to table.
	do k = 1, numcols {
	    cp = colptr[k]
	    call tbcwcd (tp, cp)
	}

	# Assign indef values for each new column and all allocated rows.
	call tbyncn (tp, colptr, numcols)
end
