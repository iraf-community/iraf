include	<tbset.h>

# REORDER -- Reorder table rows according to an index array
#
# This procedure rearranges the rows of a table according to the contents
# of an index array. The index array is produced by one of the two table
# sort routines, tsort1 or tsortm. The algorithm used is taken from Knuth's
# Sorting and Searching p.595.
#
# B.Simon	17-Sept-87	First Code
# B.Simon	15-Jul-88	Rewritten
# Phil Hodge	12-Sep-88	Don't include tbtables.h

procedure reorder (tp, nindex, index)

pointer	tp		#  i: Table descriptor
int	nindex		#  i: Number of indices
int	index[ARB]	# io: Array of row indices
#--
int	idx, jdx, kdx, ndx
int	tbpsta()

errchk	tbrcpy

begin
	# Use the row after the end of the table for temporary storage

	ndx = tbpsta (tp, TBL_NROWS) + 1

	# Loop over all rows of the table, moving them into their proper
	# order

	do idx = 1, nindex {

	    # The index array forms one or more cycles. Move the first
	    # row in the cycle to the temporary location. Repeatedly
	    # move the remaining rows in the cycle until the final
	    # location of the first row is found. Move the first row
	    # from its temporary location to its final location. Update
	    # the index array to indicate which rows have been moved.

	    if (index[idx] != idx) {
		call tbrcpy (tp, tp, idx, ndx)
		jdx = idx
		while (index[jdx] != idx) {
		    kdx = index[jdx]
		    call tbrcpy (tp, tp, kdx, jdx)
		    index[jdx] = jdx
		    jdx = kdx
		}
		call tbrcpy (tp, tp, ndx, jdx)
		index[jdx] = jdx
	    }

	}

	# Remove the temporary row

	call tbrdel (tp, ndx, ndx)
end
