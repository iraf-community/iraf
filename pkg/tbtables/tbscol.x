include "tbtables.h"

# If we need to reallocate the space for column selector descriptors,
# this is the amount we will add to the current size.
define	INCR_MAX_SELCOLS	20

# tbscol -- add a new column to the list of selected columns
# If a column selector is in effect, this routine adds one column to the
# list of selected columns.  This would be called primarily when creating
# a new column.
#
# Phil Hodge,  2-Mar-1998  Subroutine created.

procedure tbscol (tp, cp)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
#--
errchk	tcs_addcol

begin
	if (TB_COLUMN_SELECT(tp) == YES) {	# column selection is in effect

	    if (TB_NSEL_COLS(tp) + 1 > TB_MAX_SELCOLS(tp)) {
		TB_MAX_SELCOLS(tp) = TB_NSEL_COLS(tp) + INCR_MAX_SELCOLS
		call realloc (TB_SELCOL_PTR(tp), TB_MAX_SELCOLS(tp), TY_POINTER)
	    }

	    call tcs_addcol (tp, cp,
		TB_SELCOL(tp,1), TB_NSEL_COLS(tp), TB_MAX_SELCOLS(tp))
	}
end
