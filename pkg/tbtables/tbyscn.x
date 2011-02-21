include "tbtables.h"

# tbyscn -- Y set columns to null
# Write INDEF values for specified columns in a range of rows in a table.
# Note:  This routine assumes that EOS = 0 (or SZB_CHAR=1) because the buffer
# for setting char values to indef is not packed.
#
# Phil Hodge,  1-Apr-1993  Include short datatype.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.

procedure tbyscn (tp, fd, colptr, numcols, firstrow, lastrow)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to descr of new columns
int	fd			# i: identifies the data file for a table
int	numcols			# i: the number of new columns
int	firstrow		# i: the first row to be set to indef
int	lastrow			# i: the last row to be set to indef

pointer sp
pointer charbuf			# Scratch for character string column
long	offset			# Location (chars) for writing in table
int	j, k			# Loop indexes
int	datatype		# Data type of a column
int	dlen			# Number of char in an element of the table
short	sbuf
bool	boolbuf			# Buffer for writing Boolean values
long	tbyoff()
errchk	seek, write

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    if (datatype < 0)
		datatype = TY_CHAR
	    dlen = COL_LEN(colptr[k])
	    offset = tbyoff (tp, colptr[k], firstrow)
	    switch (datatype) {
	    case TY_REAL:
		do j = 1, lastrow-firstrow+1 {
		    call seek (fd, offset)
		    call write (fd, INDEFR, dlen)
		    offset = offset + dlen
		}
	    case TY_DOUBLE:
		do j = 1, lastrow-firstrow+1 {
		    call seek (fd, offset)
		    call write (fd, TBL_INDEFD, dlen)
		    offset = offset + dlen
		}
	    case TY_INT:
		do j = 1, lastrow-firstrow+1 {
		    call seek (fd, offset)
		    call write (fd, INDEFI, dlen)
		    offset = offset + dlen
		}
	    case TY_SHORT:
		# We need this because INDEFS in the call to WRITE would be
		# interpreted as an integer rather than as a short int.
		sbuf = INDEFS
		do j = 1, lastrow-firstrow+1 {
		    call seek (fd, offset)
		    call write (fd, sbuf, dlen)
		    offset = offset + dlen
		}
	    case TY_BOOL:
		boolbuf = false
		do j = 1, lastrow-firstrow+1 {
		    call seek (fd, offset)
		    call write (fd, boolbuf, dlen)
		    offset = offset + dlen
		}
	    case TY_CHAR:
		call smark (sp)
		call salloc (charbuf, dlen, TY_CHAR)
		do j = 1, dlen
		    Memc[charbuf+j-1] = EOS	# N.B. this assumes EOS = 0
		do j = 1, lastrow-firstrow+1 {
		    call seek (fd, offset)
		    call write (fd, Memc[charbuf], dlen)
		    offset = offset + dlen
		}
		call sfree (sp)
	    }
	}
end
