include "../../lib/astrom.h"

# AT_RCSYM -- Return the symbol for the specified field nymber.

pointer procedure at_rcsym (at, fieldno)

pointer	at		#I the astrometry package descriptor
int	fieldno		#I the region whose symbol is to be locate

pointer	sp, symname, st, sym
pointer	at_statp(), stfind()

begin
	st = at_statp (at, RCST)
	if (st == NULL)
	    return (NULL)

	call smark (sp)
	call salloc (symname, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[symname], SZ_FNAME, "%s%d")
	    call pargstr (DEF_RCST_ROOTNAME)
	    call pargi (fieldno)
	sym = stfind (st, Memc[symname])

	call sfree (sp)

	return (sym)
end
