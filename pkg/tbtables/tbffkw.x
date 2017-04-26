include <tbset.h>
include "tbtables.h"
define	SZ_FITS_REC	81	# size of buffer for a FITS header record

# tbffkw -- find keyword number
# This routine finds a header record for a given keyword.  If the keyword
# is found the number of the parameter in the table will be returned;
# otherwise, the number will be set to zero.  The search begins with the
# first keyword and includes special keywords such as NAXIS.
#
# Phil Hodge, 22-Jan-1996  Subroutine created.

procedure tbffkw (tp, keyword, parnum)

pointer tp			# i: Pointer to table descriptor
char	keyword[SZ_KEYWORD]	# i: Keyword to be found
int	parnum			# o: Parameter number or zero if not found
#--
pointer sp
pointer par			# buffer for header record for parameter
int	status			# error return code from fitsio
int	k			# loop index
char	uckey[SZ_KEYWORD]	# keyword converted to upper case
bool	tbhkeq()
errchk	tbferr

begin
	call smark (sp)
	call salloc (par, SZ_FITS_REC, TY_CHAR)

	call strcpy (keyword, uckey, SZ_KEYWORD)
	call strupr (uckey)

	do k = 1, TB_NPAR(tp) {
	    # Read parameter record.
	    call fsgrec (TB_FILE(tp), k, Memc[par], status)
	    if (status != 0)
		call tbferr (status)
	    if (tbhkeq (uckey, Memc[par])) {		# keywords equal?
		parnum = k
		call sfree (sp)
		return		# keyword has been found
	    }
	}
	parnum = 0		# keyword not found
	call sfree (sp)
end
