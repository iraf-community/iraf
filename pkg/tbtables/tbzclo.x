include <tbset.h>
include "tbtables.h"

# tbzclo -- do some cleaning up for a text file
# The table data are written back from memory into the text file,
# and the buffers for storing column data and keywords are deallocated.
#
# Phil Hodge, 14-Jan-1992  Subroutine created.
# Phil Hodge,  3-Apr-1995  Check TB_MODIFIED before calling tbzwrt.
# Phil Hodge,  7-Jun-1999  Deallocate keyword list;
#		deallocate comment buffer here instead of in tbtclo.

procedure tbzclo (tp)

pointer tp		# i: pointer to table descriptor
#--
pointer cp		# pointer to column descriptor
int	colnum		# column number
int	key		# keyword number

begin
	# Write the data to the file, and close the file.
	if (TB_MODIFIED(tp) && TB_IOMODE(tp) != READ_ONLY)
	    call tbzwrt (tp)

	# Deallocate memory for column values.
	do colnum = TB_NCOLS(tp), 1, -1 {

	    cp = TB_COLINFO(tp,colnum)

	    # The pointer to the column values is stored in COL_OFFSET(cp).
	    if (COL_OFFSET(cp) != NULL) {

		if (COL_DTYPE(cp) == TY_DOUBLE)
		    call mfree (COL_OFFSET(cp), TY_DOUBLE)

		else if (COL_DTYPE(cp) == TY_INT)
		    call mfree (COL_OFFSET(cp), TY_INT)

		else				# string
		    call mfree (COL_OFFSET(cp), TY_CHAR)
	    }
	}

	# Deallocate comment buffer.
	if (TB_COMMENT(tp) != NULL) {
	    call mfree (TB_COMMENT(tp), TY_CHAR)
	    TB_COMMENT(tp) = NULL
	}

	# Deallocate memory for keywords.
	if (TB_KEYLIST_PTR(tp) != NULL) {
	    do key = TB_NPAR(tp), 1, -1 {
		if (TB_KEYWORD(tp,key) != NULL)
		    call mfree (TB_KEYWORD(tp,key), TY_CHAR)
	    }
	    call mfree (TB_KEYLIST_PTR(tp), TY_POINTER)
	}
end
