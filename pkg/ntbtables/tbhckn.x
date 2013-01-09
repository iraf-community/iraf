include <ctype.h>	# IS_LOWER, TO_UPPER
include <tbset.h>
include "tbtables.h"

# tbhckn -- change keyword name
# This routine changes the name of a keyword without changing either
# the data type, value, or comment.
#
# The current keyword can be specified either by name or number,
# but not both.
#
# Phil Hodge, 22-May-1996  Subroutine created.
# Phil Hodge,  7-Jun-1999  Handle text tables.

procedure tbhckn (tp, oldkey, parnum, newkey)

pointer tp			# i: pointer to table descriptor
char	oldkey[ARB]		# i: current keyword name
int	parnum			# i: number of current keyword
char	newkey[ARB]		# i: new keyword name
#--
pointer sp
pointer par			# buffer for parameter record
char	uc_oldkey[SZ_KEYWORD]	# old keyword converted to upper case
char	uc_newkey[SZ_KEYWORD]
int	i, k
bool	foundit			# true if oldkey found in table
int	len, strlen()
bool	tbhkeq()
errchk	tbhrpr, tbhwpr, tbfckn

begin
	len = strlen (newkey)
	if (len > SZ_KEYWORD)
	    call error (1, "tbhckn:  new keyword name is too long")

	if (oldkey[1] != EOS && parnum > 0)
	    call error (1, "tbhckn:  may not specify both name and number")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfckn (tp, oldkey, parnum, newkey)
	    return
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call strcpy (oldkey, uc_oldkey, SZ_KEYWORD)
	call strcpy (newkey, uc_newkey, SZ_KEYWORD)
	call strupr (uc_oldkey)
	call strupr (uc_newkey)

	if (parnum > 0) {	# current parameter was specified by number

	    k = parnum
	    call tbhrpr (tp, k, Memc[par])		# read parameter record

	} else {		# current parameter was specified by name

	    foundit = false
	    do k = 1, TB_NPAR(tp) {

		call tbhrpr (tp, k, Memc[par])		# read parameter record

		if (tbhkeq (uc_oldkey, Memc[par])) {	# keywords equal?
		    foundit = true
		    break
		}
	    }

	    if (!foundit) {
		call sprintf (Memc[par], SZ_PARREC,
			    "tbhckn:  keyword `%s' not found")
		    call pargstr (oldkey)
		call error (1, Memc[par])
	    }
	}

	do i = 1, len				# replace old keyword
	    Memc[par+i-1] = uc_newkey[i]
	do i = len+1, SZ_KEYWORD
	    Memc[par+i-1] = ' '

	call tbhwpr (tp, k, Memc[par])		# write parameter record

	call sfree (sp)
end
