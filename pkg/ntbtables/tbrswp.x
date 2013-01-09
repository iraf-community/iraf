include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbrswp -- swap two rows
# This procedure interchanges two entire rows within a table.
#
# Phil Hodge, 30-Sep-1987  Subroutine created.
# Phil Hodge, 30-Jan-1992  Add option for text table type.
# Phil Hodge,  1-Apr-1993  Include short datatype.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge,  3-Mar-1998  Modify to allow for row selector.

procedure tbrswp (tp, selrow1, selrow2)

pointer tp		# i: pointer to table descriptor
int	selrow1		# i: first row number (selected row)
int	selrow2		# i: second row number (selected row)
#--
pointer sp
pointer rowbuf1, rowbuf2	# scratch for interchanging entire rows
pointer cptr			# pointer to descriptor for column
long	offset1			# offset in char to first element
long	offset2			# offset in char to second element
int	row1, row2		# actual row numbers corresponding to selrow1,2
int	colnum			# loop index for column number
int	dtype			# data type of column
int	rowlen			# length of row
# buffers for copying single elements
pointer cbuf1, cbuf2		# scratch for character strings
double	dbuf1, dbuf2
real	rbuf1, rbuf2
int	ibuf1, ibuf2
short	sbuf1, sbuf2
bool	bbuf1, bbuf2
pointer tbcnum()
long	tbxoff()
int	read()
errchk	tbegtb, tbegtd, tbegti, tbegts, tbegtr, tbegtt,
	tbeptb, tbeptd, tbepti, tbepts, tbeptr, tbeptt,
	tbswer, seek, read, write

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (row1 == row2)
	    return

	call smark (sp)

	if (TB_TYPE(tp) == TBL_TYPE_S_ROW) {

	    # If either of the rows is beyond the EOF, fill out the table
	    # with indef values to include that row number.
	    if (selrow1 > selrow2) {
		call tbswer (tp, selrow1, row1)
		call tbswer (tp, selrow2, row2)
	    } else {
		call tbswer (tp, selrow2, row2)
		call tbswer (tp, selrow1, row1)
	    }

	    # Read both rows into scratch space, and then write them out again.
	    rowlen = TB_ROWLEN(tp)		# length in char
	    call salloc (rowbuf1, rowlen, TY_CHAR)
	    call salloc (rowbuf2, rowlen, TY_CHAR)

	    # These are the offsets to the beginnings of the rows.
	    offset1 = tbxoff (tp, row1)
	    offset2 = tbxoff (tp, row2)

	    # Read both rows.
	    call seek (TB_FILE(tp), offset1)
	    if (read (TB_FILE(tp), Memc[rowbuf1], rowlen) < rowlen)
		call error (1, "tbrswp:  could not read row")
	    call seek (TB_FILE(tp), offset2)
	    if (read (TB_FILE(tp), Memc[rowbuf2], rowlen) < rowlen)
		call error (1, "tbrswp:  could not read row")

	    # Write both rows.
	    call seek (TB_FILE(tp), offset2)
	    call write (TB_FILE(tp), Memc[rowbuf1], rowlen)
	    call seek (TB_FILE(tp), offset1)
	    call write (TB_FILE(tp), Memc[rowbuf2], rowlen)

	} else {

	    call salloc (cbuf1, SZ_LINE, TY_CHAR)
	    call salloc (cbuf2, SZ_LINE, TY_CHAR)

	    # Copy each element (i.e. each column in the row) one at a time.
	    do colnum = 1, TB_NCOLS(tp) {
		cptr = tbcnum (tp, colnum)

		dtype = COL_DTYPE(cptr)
		switch (dtype) {
		case TBL_TY_REAL:
		    call tbegtr (tp, cptr, selrow1, rbuf1)	# get
		    call tbegtr (tp, cptr, selrow2, rbuf2)	# get
		    call tbeptr (tp, cptr, selrow2, rbuf1)	# put
		    call tbeptr (tp, cptr, selrow1, rbuf2)	# put
		case TBL_TY_DOUBLE:
		    call tbegtd (tp, cptr, selrow1, dbuf1)
		    call tbegtd (tp, cptr, selrow2, dbuf2)
		    call tbeptd (tp, cptr, selrow2, dbuf1)
		    call tbeptd (tp, cptr, selrow1, dbuf2)
		case TBL_TY_INT:
		    call tbegti (tp, cptr, selrow1, ibuf1)
		    call tbegti (tp, cptr, selrow2, ibuf2)
		    call tbepti (tp, cptr, selrow2, ibuf1)
		    call tbepti (tp, cptr, selrow1, ibuf2)
		case TBL_TY_SHORT:
		    call tbegts (tp, cptr, selrow1, sbuf1)
		    call tbegts (tp, cptr, selrow2, sbuf2)
		    call tbepts (tp, cptr, selrow2, sbuf1)
		    call tbepts (tp, cptr, selrow1, sbuf2)
		case TBL_TY_BOOL:
		    call tbegtb (tp, cptr, selrow1, bbuf1)
		    call tbegtb (tp, cptr, selrow2, bbuf2)
		    call tbeptb (tp, cptr, selrow2, bbuf1)
		    call tbeptb (tp, cptr, selrow1, bbuf2)
		default:
		    if (dtype < 0 || dtype == TBL_TY_CHAR) {
			call tbegtt (tp, cptr, selrow1, Memc[cbuf1], SZ_LINE)
			call tbegtt (tp, cptr, selrow2, Memc[cbuf2], SZ_LINE)
			call tbeptt (tp, cptr, selrow2, Memc[cbuf1])
			call tbeptt (tp, cptr, selrow1, Memc[cbuf2])
		    } else {
			call error (ER_TBCOLBADTYP,
			"tbrswp:  bad data type; table or memory corrupted?")
		    }
		}
	    }
	}
	TB_MODIFIED(tp) = true

	call sfree (sp)
end
