include <tbset.h>
include "tbtables.h"
define	SZ_SCRATCH	11

# tbtnam -- get table name
# Get the name of a table which has been opened or at least initialized.
#
# Phil Hodge, 16-Jun-1995  Modify for FITS tables.
# Phil Hodge,  2-Feb-1996  Option to include TB_HDU or TB_EXTVER.
# Phil Hodge,  7-Jun-1999  Replace TB_F_TYPE by TB_TYPE.

procedure tbtnam (tp, tblname, maxch)

pointer tp			# i: pointer to table descriptor
char	tblname[ARB]		# o: the name of the table
int	maxch			# i: maximum number of characters in name
#--
char    scratch[SZ_SCRATCH]     # scratch for appending EXTVER
 
begin
	# Use the source name/url if it is present.
	if (TB_SRC_PTR(tp) != NULL) {
	    call strcpy (TB_SRC(tp), tblname, maxch)
	    return
	}

	call strcpy (TB_NAME(tp), tblname, maxch)
 
	if (TB_TYPE(tp) == TBL_TYPE_FITS || TB_TYPE(tp) == TBL_TYPE_CDF) {
 
	    # This will be non-null only if the user specified a name.
	    if (TB_EXTNAME(tp) != EOS) {

		call strcat ("[", tblname, maxch)
		call strcat (TB_EXTNAME(tp), tblname, maxch)

		if (TB_EXTVER(tp) > 0) {
		    call sprintf (scratch, SZ_SCRATCH, ",%d")
			call pargi (TB_EXTVER(tp))
		    call strcat (scratch, tblname, maxch)
		}
		call strcat ("]", tblname, maxch)

	    } else if (TB_HDU(tp) >= 0) {

		call sprintf (scratch, SZ_SCRATCH, "[%d]")
		    call pargi (TB_HDU(tp))
		call strcat (scratch, tblname, maxch)
	    }
	}
end
