include <tbset.h>

# This file contains tbcnel and tbcnel1.

# tbcnel -- get the total number of elements for a column
# This function multiplies the number of selected rows by the number of
# elements in one row, for the specified column.  The column may contain
# scalars or arrays.
#
# If the column was listed in a column selector string, and if this
# included an array section, the number of elements for one row will be
# the number in the array section.
#
# Phil Hodge,  5-Mar-1998  Function created.

int procedure tbcnel (tp, cp)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
#--
pointer descrip		# column selector descriptor (ignored)
int	nrows		# number of selected rows
int	nelem		# number of elements in one cell

begin
	call tbcnel1 (tp, cp, descrip, nelem, nrows)

	return (nrows * nelem)
end

procedure tbcnel1 (tp, cp, descrip, nelem, nrows)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
pointer descrip		# o: column selector descriptor
int	nelem		# o: number of elements in one cell
int	nrows		# o: number of selected rows
#--
pointer tbcdes()
int	tcs_totsize()
int	tbpsta(), tbalen()

begin
	descrip = tbcdes (tp, cp)

	if (descrip == NULL)
	    nelem = tbalen (cp)			# cp is not a selected column
	else
	    nelem = tcs_totsize (descrip)

	nrows = tbpsta (tp, TBL_NROWS)		# number of selected rows
end
