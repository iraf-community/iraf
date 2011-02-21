include "tbtables.h"

# tbzsiz -- increase size of internal buffers for text file
# The table must be open when this procedure is called.
# Note that TB_MAXPAR and TB_ALLROWS should be assigned before calling
# this routine.
#
# Phil Hodge, 14-Jan-1992  Subroutine created.
# Phil Hodge,  7-Jun-1999  Add old_maxpar to calling sequence;
#			reallocate TB_KEYLIST_PTR.

procedure tbzsiz (tp, old_maxpar, old_allrows)

pointer tp			# i: pointer to table descriptor
int	old_maxpar		# i: previous value of max number of parameters
int	old_allrows		# i: previous value of allocated number of rows
#--
pointer cp			# pointer to column descriptor
int	dtype			# column data type
int	new_allrows		# new value of allocated number of rows
int	oldsize, newsize	# old & new lengths of char buffer
int	lenstr			# length of each string in string column
int	row_1			# row number minus one
int	colnum			# loop index for column number
int	k			# loop index
errchk	realloc

begin
	# Allocate or reallocate the array of pointers to keywords.
	if (TB_KEYLIST_PTR(tp) == NULL) {
	    call calloc (TB_KEYLIST_PTR(tp), TB_MAXPAR(tp), TY_POINTER)
	} else if (TB_MAXPAR(tp) > old_maxpar) {
	    call realloc (TB_KEYLIST_PTR(tp), TB_MAXPAR(tp), TY_POINTER)
	    do k = old_maxpar + 1, TB_MAXPAR(tp)
		TB_KEYWORD(tp,k) = NULL
	}

	# Check whether we need to do anything further.

	new_allrows = TB_ALLROWS(tp)

	if (new_allrows <= old_allrows)
	    return

	# Reallocate buffers and assign indef values for new rows.
	do colnum = 1, TB_NCOLS(tp) {
	    cp = TB_COLINFO(tp,colnum)

	    dtype = COL_DTYPE(cp)
	    if (dtype == TBL_TY_DOUBLE) {

		call realloc (COL_OFFSET(cp), new_allrows, TY_DOUBLE)
		do row_1 = old_allrows, new_allrows-1	# zero indexed
		    Memd[COL_OFFSET(cp) + row_1] = INDEFD

	    } else if (dtype == TBL_TY_INT) {

		call realloc (COL_OFFSET(cp), new_allrows, TY_INT)
		do row_1 = old_allrows, new_allrows-1
		    Memi[COL_OFFSET(cp) + row_1] = INDEFI

	    } else {					# string

		lenstr = -dtype + 1			# one for EOS
		oldsize = lenstr * old_allrows
		newsize = lenstr * new_allrows
		call realloc (COL_OFFSET(cp), newsize, TY_CHAR)

		do k = oldsize, newsize-1		# zero indexed
		    Memc[COL_OFFSET(cp) + k] = EOS

	    }
	}
end
