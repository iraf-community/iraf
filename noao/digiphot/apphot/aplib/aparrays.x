include "../lib/apphotdef.h"
include "../lib/photdef.h"
include "../lib/phot.h"

# APARRAYS -- Procedure to move apphot parameters stored as real arrays
# into a user allocated array.

procedure aparrays (ap, param, array)

pointer	ap		# pointer to apphot structure
int	param		# parameter
real	array[ARB]	# array

pointer	phot

begin
	phot = AP_PPHOT(ap)
	switch (param) {
	case APERTS:
	    call amovr (Memr[AP_APERTS(phot)], array, AP_NAPERTS(phot))
	case MAGS:
	    call amovr (Memr[AP_MAGS(phot)], array, AP_NAPERTS(phot))
	case MAGERRS:
	    call amovr (Memr[AP_MAGERRS(phot)], array, AP_NAPERTS(phot))
	case AREAS:
	    call amovr (Memr[AP_AREA(phot)], array, AP_NAPERTS(phot))
	case SUMS:
	    call amovr (Memr[AP_SUMS(phot)], array, AP_NAPERTS(phot))
	default:
	    call error (0, "APARRAYS: Unknown apphot array")
	}
end
