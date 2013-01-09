include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbyudf -- Y set to undefined
# "Delete" entries in a table by setting each entry to the INDEF value
# appropriate for its datatype.
# Note:  This routine assumes that EOS = 0 (or SZB_CHAR = 1) because the
# buffer for setting char values to indef is not packed.
# This version is for column-ordered SDAS tables.
#
# Phil Hodge,  9-Mar-1989  Allow data type to be -n for char.
# Phil Hodge,  1-Apr-1993  Include short datatype.
# Phil Hodge,  2-Jun-1997  Replace INDEFD with TBL_INDEFD.

procedure tbyudf (tp, colptr, numcols, rownum)

pointer tp			# i: pointer to table descriptor
pointer colptr[numcols]		# i: array of pointers to column descriptors
int	numcols			# i: number of columns
int	rownum			# i: row number
#--
pointer sp
pointer charbuf			# Scratch for character string column
long	offset			# Location (chars) for writing in table
int	j, k			# Loop indexes
int	datatype		# Data type of a column
int	dlen			# Number of char in an element of the table
short	sbuf			# buffer for short datatype
bool	boolbuf			# Buffer for writing Boolean values
long	tbyoff()

begin
	do k = 1, numcols {
	    datatype = COL_DTYPE(colptr[k])
	    dlen = COL_LEN(colptr[k])
	    offset = tbyoff (tp, colptr[k], rownum)
	    switch (datatype) {
	    case TY_REAL:
		call seek (TB_FILE(tp), offset)
		call write (TB_FILE(tp), INDEFR, dlen)
	    case TY_DOUBLE:
		call seek (TB_FILE(tp), offset)
		call write (TB_FILE(tp), TBL_INDEFD, dlen)
	    case TY_INT:
		call seek (TB_FILE(tp), offset)
		call write (TB_FILE(tp), INDEFI, dlen)
	    case TY_SHORT:
		sbuf = INDEFS
		call seek (TB_FILE(tp), offset)
		call write (TB_FILE(tp), sbuf, dlen)
	    case TY_BOOL:
		boolbuf = false
		call seek (TB_FILE(tp), offset)
		call write (TB_FILE(tp), boolbuf, dlen)
	    default:
		if (datatype < 0 || datatype == TY_CHAR) {
		    call smark (sp)
		    call salloc (charbuf, dlen, TY_CHAR)
		    do j = 1, dlen
			Memc[charbuf+j-1] = EOS		# this assumes EOS = 0
		    call seek (TB_FILE(tp), offset)
		    call write (TB_FILE(tp), Memc[charbuf], dlen)
		    call sfree (sp)
		} else {
		    call error (ER_TBCOLBADTYP,
			"tbyudf:  invalid datatype; table corrupted?")
		}
	    }
	}
end
