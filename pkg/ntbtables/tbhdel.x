include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbhdel -- delete Nth parameter
# Delete a header parameter by overwriting with subsequent records and
# decrementing the number of parameter records TB_NPAR(tp) by one.
# The parameter is specified by number rather than by name so a history
# or comment record can be deleted.
# If the parameter number is out of range, this routine simply returns.
#
# Phil Hodge, 16-Mar-1988  Subroutine created.
# Phil Hodge, 14-Feb-1992  Add option for text table type.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge,  3-Oct-1995  Modify for FITS tables.
# Phil Hodge,  7-Jun-1999  Modify for text tables.

procedure tbhdel (tp, parnum)

pointer tp			# i: pointer to table descriptor
int	parnum			# i: number of the parameter to be deleted
#--
pointer sp
pointer str			# scratch for a parameter record
int	k			# loop index for copying keyword
errchk	tbfhdl, tbhrpr, tbhwpr

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "table is readonly")

	TB_MODIFIED(tp) = true

	if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
	    if (TB_KEYLIST_PTR(tp) != NULL &&
			parnum >= 1 && parnum <= TB_NPAR(tp)) {
		call mfree (TB_KEYWORD(tp,parnum), TY_CHAR)
		do k = parnum, TB_NPAR(tp)-1
		    TB_KEYWORD(tp,k) = TB_KEYWORD(tp,k+1)
		k = TB_NPAR(tp)
		TB_KEYWORD(tp,k) = NULL
		TB_NPAR(tp) = TB_NPAR(tp) - 1
	    }
	    return
	}

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhdl (tp, parnum)
	    return
	}

	if (parnum == TB_NPAR(tp)) {

	    TB_NPAR(tp) = TB_NPAR(tp) - 1

	} else if (parnum >= 1 && parnum < TB_NPAR(tp)) {

	    call smark (sp)
	    call salloc (str, SZ_PARREC, TY_CHAR)

	    do k = parnum, TB_NPAR(tp)-1 {
		# Read next parameter record, and overwrite current one.
		call tbhrpr (tp, k+1, Memc[str])
		call tbhwpr (tp, k, Memc[str])
	    }

	    TB_NPAR(tp) = TB_NPAR(tp) - 1
	    call sfree (sp)
	}
end
