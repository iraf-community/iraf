include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrcsc -- copy selected columns
# This procedure copies specific columns in a row from one table to another
# or to another row within the same table.  Elements are copied one at a
# time, and the pointers to descriptors of input and output columns are
# passed in the calling sequence, so the restrictions on similarity of
# input and output tables in tbrcpy do not apply to this routine.
#
# For each column to be copied from the input row, the element is read
# using a "get element" routine (tbegt[]), and then the element is put
# in the output row using a "put element" routine (tbept[]).
#
# Phil Hodge,  1-Oct-1987  Subroutine created.
# Phil Hodge, 30-Jan-1992  Use tbegt? instead of tbegp?.
# Phil Hodge,  1-Apr-1993  Include short datatype.
# Phil Hodge, 23-Aug-1994  Also copy array entries.
# Phil Hodge, 30-Nov-1994  When copying arrays of char, copy one at a time.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 11-Dec-1995  Allocate cbuf only if needed.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.
# Phil Hodge, 30-Sep-1997  Delete check on irow being beyond end of file,
#			because it's checked in tbegt[] or tbagt[], and
#			to allow for a row selector.
# Phil Hodge, 18-Jan-1999  Get & put boolean as short, to preserve indef values.
# Phil Hodge,  5-Aug-1999  Use COL_NELEM instead of tbalen to get array length.

procedure tbrcsc (itp, otp, icp, ocp, irow, orow, ncols)

pointer itp		# i: pointer to descriptor of input table
pointer otp		# i: pointer to descriptor of output table
pointer icp[ncols]	# i: array of pointers for input columns
pointer ocp[ncols]	# i: array of pointers for output columns
int	irow		# i: row number in input table
int	orow		# i: row number in output table
int	ncols		# i: number of columns to be copied
#--
pointer sp
int	k		# loop index for column number
int	i		# loop index for array element
int	nget, nput	# number of elements in input & output arrays
int	dtype		# data type of column
# buffers for copying elements of various data types
pointer gbuf		# pointer to array of any data type
pointer cbuf		# for copying character elements
double	dbuf
real	rbuf
int	ibuf
short	sbuf
int	tbagtd(), tbagtr(), tbagti(), tbagts(), tbagtt()
errchk	tbegtd, tbegtr, tbegti, tbegts, tbegtt,
	tbeptd, tbeptr, tbepti, tbepts, tbeptt,
	tbagtd, tbagtr, tbagti, tbagts, tbagtt,
	tbaptd, tbaptr, tbapti, tbapts, tbaptt
string	BAD_DATATYPE	"tbrcsc:  bad data type; table or memory corrupted?"
string	ERR_READ_ARRAY	"tbrcsc:  can't read array entry"

begin
	if (TB_READONLY(otp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	call smark (sp)
	cbuf = NULL				# allocated below

	do k = 1, ncols {
	    dtype = COL_DTYPE(icp[k])
	    nget = COL_NELEM(icp[k])

	    if (nget == 1) {

		# Copy a single element.
		switch (dtype) {
		case TBL_TY_REAL:
		    call tbegtr (itp, icp[k], irow, rbuf)
		    call tbeptr (otp, ocp[k], orow, rbuf)
		case TBL_TY_DOUBLE:
		    call tbegtd (itp, icp[k], irow, dbuf)
		    call tbeptd (otp, ocp[k], orow, dbuf)
		case TBL_TY_INT:
		    call tbegti (itp, icp[k], irow, ibuf)
		    call tbepti (otp, ocp[k], orow, ibuf)
		case TBL_TY_SHORT,TBL_TY_BOOL:
		    call tbegts (itp, icp[k], irow, sbuf)
		    call tbepts (otp, ocp[k], orow, sbuf)
		default:
		    if (dtype < 0 || dtype == TBL_TY_CHAR) {
			if (cbuf == NULL)
			    call salloc (cbuf, SZ_LINE, TY_CHAR)
			call tbegtt (itp, icp[k], irow, Memc[cbuf], SZ_LINE)
			call tbeptt (otp, ocp[k], orow, Memc[cbuf])
		    } else {
			call error (ER_TBCOLBADTYP, BAD_DATATYPE)
		    }
		}

	    } else {			# Copy an array.

		if (TB_TYPE(otp) == TBL_TYPE_TEXT ||
		    TB_TYPE(otp) == TBL_TYPE_S_COL)
		    call error (1,
		"Output table type does not support columns of arrays.")

		nput = COL_NELEM(ocp[k])
		if (nget > nput)
		    call error (1,
		"tbrcsc:  output array is shorter than input array")

		switch (dtype) {
		case TBL_TY_REAL:

		    call malloc (gbuf, max (nget, nput), TY_REAL)
		    do i = nget+1, nput
			Memr[gbuf+i-1] = INDEFR
		    if (tbagtr (itp, icp[k], irow, Memr[gbuf], 1, nget) < nget)
			call error (1, ERR_READ_ARRAY)
		    call tbaptr (otp, ocp[k], orow, Memr[gbuf], 1, nput)
		    call mfree (gbuf, TY_REAL)

		case TBL_TY_DOUBLE:

		    call malloc (gbuf, max (nget, nput), TY_DOUBLE)
		    do i = nget+1, nput
			Memd[gbuf+i-1] = TBL_INDEFD
		    if (tbagtd (itp, icp[k], irow, Memd[gbuf], 1, nget) < nget)
			call error (1, ERR_READ_ARRAY)
		    call tbaptd (otp, ocp[k], orow, Memd[gbuf], 1, nput)
		    call mfree (gbuf, TY_DOUBLE)

		case TBL_TY_INT:

		    call malloc (gbuf, max (nget, nput), TY_INT)
		    do i = nget+1, nput
			Memi[gbuf+i-1] = INDEFI
		    if (tbagti (itp, icp[k], irow, Memi[gbuf], 1, nget) < nget)
			call error (1, ERR_READ_ARRAY)
		    call tbapti (otp, ocp[k], orow, Memi[gbuf], 1, nput)
		    call mfree (gbuf, TY_INT)

		case TBL_TY_SHORT,TBL_TY_BOOL:

		    call malloc (gbuf, max (nget, nput), TY_SHORT)
		    do i = nget+1, nput
			Mems[gbuf+i-1] = INDEFS
		    if (tbagts (itp, icp[k], irow, Mems[gbuf], 1, nget) < nget)
			call error (1, ERR_READ_ARRAY)
		    call tbapts (otp, ocp[k], orow, Mems[gbuf], 1, nput)
		    call mfree (gbuf, TY_SHORT)

		default:
		    if (dtype < 0) {
			if (cbuf == NULL)
			    call salloc (cbuf, SZ_LINE, TY_CHAR)
			do i = 1, nget {
			    if (tbagtt  (itp, icp[k], irow,
				Memc[cbuf], SZ_LINE, i, 1) < 1)
				    call error (1, ERR_READ_ARRAY)
			    call tbaptt (otp, ocp[k], orow,
				Memc[cbuf], SZ_LINE, i, 1)
			}
			do i = nget+1, nput
			    call tbaptt (otp, ocp[k], orow, "", SZ_LINE, i, 1)
		    } else {
			call error (ER_TBCOLBADTYP, BAD_DATATYPE)
		    }
		}
	    }
	}
	TB_MODIFIED(otp) = true

	call sfree (sp)
end
