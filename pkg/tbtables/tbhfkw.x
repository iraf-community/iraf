include <tbset.h>
include "tbtables.h"

# tbhfkw -- find keyword for writing
# Find a "header" record for a given keyword.  If the keyword is found
# the number of the parameter in the table will be returned; otherwise,
# the number will be set to zero.  The search begins with the first keyword.
#
# Phil Hodge, 22-Jan-1996  Modify for FITS tables.

procedure tbhfkw (tp, keyword, parnum)

pointer tp			# i: Pointer to table descriptor
char	keyword[SZ_KEYWORD]	# i: Keyword to be found
int	parnum			# o: Parameter number or zero if not found
#--
pointer sp
pointer par			# buffer for header record for parameter
int	k			# loop index
char	uckey[SZ_KEYWORD]	# keyword converted to upper case
bool	tbhkeq()
errchk	tbhrpr

begin
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbffkw (tp, keyword, parnum)
	    return
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call strcpy (keyword, uckey, SZ_KEYWORD)
	call strupr (uckey)

	do k = 1, TB_NPAR(tp) {
	    call tbhrpr (tp, k, Memc[par])		# read parameter record
	    if (tbhkeq (uckey, Memc[par])) {		# keywords equal?
		parnum = k
		call sfree (sp)
		return		# keyword has been found
	    }
	}
	parnum = 0		# keyword not found
	call sfree (sp)
end
