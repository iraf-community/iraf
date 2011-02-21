include <tbset.h>
include "tbtables.h"

# tbfckn -- change keyword name for FITS table
# This routine changes the name of a keyword without changing either
# the data type, value, or comment.
#
# Phil Hodge, 22-May-1996  Subroutine created.

procedure tbfckn (tp, oldkey, parnum, newkey)

pointer tp			# i: pointer to table descriptor
char	oldkey[ARB]		# i: current keyword name
int	parnum			# i: number of current keyword
char	newkey[ARB]		# i: new keyword name
#--
pointer sp
pointer par			# buffer for parameter record
char	uc_oldkey[SZ_KEYWORD]	# old keyword converted to upper case
char	uc_newkey[SZ_KEYWORD]
int	status
int	k
int	i
int	len, strlen()
errchk	tbffkw, tbferr

begin
	status = 0

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call strcpy (oldkey, uc_oldkey, SZ_KEYWORD)
	call strcpy (newkey, uc_newkey, SZ_KEYWORD)
	call strupr (uc_oldkey)
	call strupr (uc_newkey)

	if (parnum > 0) {	# current parameter was specified by number

	    k = parnum

	} else {		# current parameter was specified by name

	    call tbffkw (tp, uc_oldkey, k)	# find old keyword

	    if (k <= 0) {
		call sprintf (Memc[par], SZ_PARREC,
			    "tbhckn:  keyword `%s' not found")
		    call pargstr (oldkey)
		call error (1, Memc[par])
	    }
	}

	call fsgrec (TB_FILE(tp), k, Memc[par], status)
	if (status != 0)
	    call tbferr (status)

	len = strlen (newkey)
	do i = 1, len				# replace old with new
	    Memc[par+i-1] = uc_newkey[i]
	do i = len+1, SZ_KEYWORD
	    Memc[par+i-1] = ' '

	call fsmrec (TB_FILE(tp), k, Memc[par], status)
	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end
