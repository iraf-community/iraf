include "tbtables.h"

# tbxudf -- X set to undefined
# "Delete" entries in a table by setting each entry to the
# INDEF value appropriate for its datatype.
# This version is for row-ordered SDAS tables.
#
# Phil Hodge,  7-Mar-1989  Eliminate TB_OFFSET.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.
# Phil Hodge,  3-Mar-1998  Remove call to tbxwsk, use tbxoff instead.

procedure tbxudf (tp, colptr, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to column descriptors
int	numcols			# i: number of columns
int	rownum			# i: row number
#--
int	k			# loop index
int	coloffset		# offset of column within row
long	roffset			# offset to beginning of row
long	offset			# for writing INDEF values in table data file
long	tbxoff()
errchk	seek, write

begin
	# Get the offset to the row in which we are to delete entries.
	roffset = tbxoff (tp, rownum)

	do k = 1, numcols {
	    coloffset = COL_OFFSET(colptr[k])
	    offset = roffset + coloffset
	    call seek (TB_FILE(tp), offset)
	    call write (TB_FILE(tp), Memc[TB_INDEF(tp) + coloffset],
			COL_LEN(colptr[k]))
	}
end
