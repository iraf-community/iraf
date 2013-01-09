include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrcpy -- copy an entire row
# This procedure copies an entire row from one table to another or
# to another location within the same table.
#
# NOTE:  If the input and output tables are not the same file, they
# must have the same column definitions and in the same order.  For
# row-ordered tables the allocated row lengths need not be the same.
# The input and output tables do not have to be both row-ordered or
# both column-ordered, however.
#
# Phil Hodge, 17-Sep-1987  Subroutine created.
# Phil Hodge, 13-Mar-1988  Allow different row lengths for input & output.
# Phil Hodge, 30-Jan-1992  Add option for text table type.
# Phil Hodge,  7-Aug-1992  For row-ordered tables, if the output row length is
#			longer than the input row length, copy indef buffer
#			to the rowbuf buffer.
# Phil Hodge, 30-Mar-1993  TB_INDEF is now TY_CHAR rather than TY_REAL.
# Phil Hodge, 29-Jul-1994  Change calling sequence of tbeoff; include short;
#			rename sbuf to rowbuf.
# Phil Hodge, 31-Aug-1994  Include type short in text table section.
# Phil Hodge,  2-Dec-1994  Call tbalen after calling tbcnum, not before;
#			include test on arrays in text table section.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 20-Jun-1995  Modify for FITS tables.
# Phil Hodge, 11-Dec-1995  Use tbrcsc if not row ordered tables.
# Phil Hodge,  3-Mar-1998  Modify to allow for row and column selectors
#			with the input table.

procedure tbrcpy (itp, otp, iselrow, oselrow)

pointer itp			# i: pointer to descriptor of input table
pointer otp			# i: pointer to descriptor of output table
int	iselrow			# i: row number (selected row) in input table
int	oselrow			# i: row number (selected row) in output table
#--
pointer sp
pointer rowbuf			# scratch for copying entire row
pointer icp, ocp		# pointers to arrays of column descriptors
long	ioffset			# offset in char to element in input table
long	ooffset			# offset in char to element in output table
int	ilen			# if row-ordered, length of input row
int	olen			# if row-ordered, length of output row
int	buflen			# length of buffer if both are row-ordered
int	irownum			# actual row number in input table
int	orownum			# actual row number in output table
int	colnum			# loop index for column number
int	ncols			# number of columns to copy
int	i
pointer tbcnum()
long	tbxoff()
int	read()
int	tbpsta()
errchk	tbrcsc, tbsirow, tbswer1, seek, read, write

begin
	if (TB_READONLY(otp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	call smark (sp)

	# Both tables row ordered, and no column selector specified?
	if (TB_TYPE(itp) == TBL_TYPE_S_ROW &&
	    TB_TYPE(otp) == TBL_TYPE_S_ROW &&
	    TB_COLUMN_SELECT(itp) == NO) {

	    call tbsirow (itp, iselrow, irownum)	# get irownum

	    # Get the actual output row number.  If it's larger than the
	    # current number of rows, add the new rows to the list of
	    # selected rows.

	    # If we're writing at the next row following the last we can
	    # just write the record (that's why we're calling tbswer1
	    # instead of tbswer), but if we're writing beyond that tbswer1
	    # will fill in with indef records.
	    call tbswer1 (otp, oselrow, orownum)

	    # Allocate a buffer large enough for either input or output row.
	    ilen = TB_ROWLEN(itp)		# length in char
	    olen = TB_ROWLEN(otp)
	    buflen = max (ilen, olen)
	    call salloc (rowbuf, buflen, TY_CHAR)

	    # If the output line is longer than the input line, copy that
	    # portion of the INDEF buffer to the buffer for copying the row.
	    do i = ilen, olen-1			# zero indexed
		Memc[rowbuf+i] = Memc[TB_INDEF(otp)+i]

	    ioffset = tbxoff (itp, irownum)	# use actual row number
	    ooffset = tbxoff (otp, orownum)
	    call seek (TB_FILE(itp), ioffset)
	    if (read (TB_FILE(itp), Memc[rowbuf], ilen) < ilen)
		call error (1, "tbrcpy:  could not read row")
	    call seek (TB_FILE(otp), ooffset)
	    call write (TB_FILE(otp), Memc[rowbuf], olen)

	    TB_NROWS(otp) = max (orownum, TB_NROWS(otp))

	} else {

	    # Note that if a column selector is in effect, we're only
	    # copying the selected columns.
	    ncols = tbpsta (itp, TBL_NCOLS)	# can be less than TB_NCOLS
	    call salloc (icp, ncols, TY_POINTER)
	    call salloc (ocp, ncols, TY_POINTER)

	    # This is where we assume that the columns are in the same
	    # order in both tables.
	    do colnum = 1, ncols {
		Memi[icp+colnum-1] = tbcnum (itp, colnum)
		Memi[ocp+colnum-1] = tbcnum (otp, colnum)
	    }

	    # Copy this row.  Some columns may contain arrays.
	    call tbrcsc (itp, otp, Memi[icp], Memi[ocp],
			iselrow, oselrow, ncols)
	}
	TB_MODIFIED(otp) = true

	call sfree (sp)
end
