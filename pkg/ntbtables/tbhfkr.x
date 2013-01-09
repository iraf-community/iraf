include <tbset.h>
include "tbtables.h"

# tbhfkr -- find keyword for reading
# Find a header record for a given keyword.  If the keyword is found
# the string containing the value and the number of the parameter in
# the table will be returned; otherwise, the number will be set to zero.
# The difference between this routine and tbhfkw (find keyword for writing)
# is that the latter does not return the datatype or value string.
# The output string str should be SZ_PARREC in length.
# The keyword search begins with the first keyword.
#
# Phil Hodge,  9-Mar-1989  Change dtype from char to int.
# Phil Hodge,  9-Mar-1995  Ignore comment; trim trailing blanks from value.
# Phil Hodge, 12-May-1995  Check for both ' and " as string delimiter.
# Phil Hodge,  1-Nov-1996  Replace most of code with a call to tbhgnp.

procedure tbhfkr (tp, keyword, dtype, str, parnum)

pointer tp			# i: pointer to table descriptor
char	keyword[SZ_KEYWORD]	# i: keyword to be found
int	dtype			# o: data type (TY_CHAR, etc)
char	str[SZ_PARREC]		# o: the string containing the value
int	parnum			# o: parameter number or zero if not found
#--
pointer sp
pointer par			# buffer for parameter record
char	uckey[SZ_KEYWORD]	# keyword converted to upper case
char	keywordk[SZ_KEYWORD]	# Kth keyword name
int	dtypek			# data type of Kth keyword
int	k			# loop index
bool	streq()
errchk	tbhgnp

begin
	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call strcpy (keyword, uckey, SZ_KEYWORD)
	call strupr (uckey)

	do k = 1, TB_NPAR(tp) {

	    call tbhgnp (tp, k, keywordk, dtypek, Memc[par])

	    if (streq (uckey, keywordk)) {		# keywords equal?

		dtype = dtypek
		call strcpy (Memc[par], str, SZ_PARREC)
		parnum = k

		call sfree (sp)
		return		# keyword has been found
	    }
	}
	parnum = 0		# keyword not found
	call sfree (sp)
end
