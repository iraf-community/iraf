include <imio.h>
include "gi.h"

# GI_GSTFVAL -- Get an stf descriptor value
#
# BS Jul 1997: Removed kernel check, replaced with check of stf pointer

int procedure gi_gstfval (im, what)

pointer	im	# image descriptor
char	what[SZ_DATATYPE]
#--
int	stf

int	strcmp()

begin
	stf = IM_KDES(im)
	if (stf == NULL)
	   call error (13, "GI_GSTFVAL: Not a Geis image")

	if (strcmp (what, "GCOUNT") == 0)
	   return (STF_GCOUNT(stf))
	else if (strcmp (what, "PSIZE") == 0)
	   return (STF_PSIZE(stf))
	else if (strcmp (what, "PCOUNT") == 0)
	   return (STF_PCOUNT(stf))
	else if (strcmp (what, "GROUP") == 0)
	   return (STF_GROUP(stf))
	else if (strcmp (what, "BITPIX") == 0)
	   return (STF_BITPIX(stf))
	else if (strcmp (what, "SZGROUP") == 0)
	   return (STF_SZGROUP(stf))
	else
	   call error (13,"gi_gstfval: Item not found")

end
