include <imio.h>
include "gi.h"

# GI_GFITVAL -- Get an fits descriptor value

int procedure gi_gfitval (im, what)

pointer	im	# image descriptor
char	what[SZ_DATATYPE]
#--
int	fit, value

int	strcmp()

begin
	fit = IM_KDES(im)
	if (fit == NULL)
	   call error (13, "GI_GFITVAL: Not a FITS image")

	if (strcmp (what, "INHERIT") == 0) {
	   value = FIT_INHERIT(fit)

	} else if (strcmp (what, "GROUP") == 0) {
	    if (FIT_GROUP(fit) >= 0) {
		value = FIT_GROUP(fit)
	    } else {
		value = IM_CLINDEX (im)
	    }

	} else {
	   call error (13,"gi_gfitval: Item not found")
	}

	return (value)
end
