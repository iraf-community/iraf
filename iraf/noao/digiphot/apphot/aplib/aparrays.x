include "../lib/apphotdef.h"
include "../lib/photdef.h"
include "../lib/phot.h"

# AP_ARRAYR -- Procedure to move apphot parameters stored as real arrays
# into a user allocated array.

procedure ap_arrayr (ap, param, array)

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
	default:
	    call error (0, "AP_ARRAYR: Unknown apphot real array")
	}
end


# AP_ARRAYD -- Procedure to move apphot parameters stored as double arrays
# into a user allocated array.

procedure ap_arrayd (ap, param, array)

pointer	ap		# pointer to apphot structure
int	param		# parameter
double	array[ARB]	# array

pointer	phot

begin
	phot = AP_PPHOT(ap)
	switch (param) {
	case AREAS:
	    call amovd (Memd[AP_AREA(phot)], array, AP_NAPERTS(phot))
	case SUMS:
	    call amovd (Memd[AP_SUMS(phot)], array, AP_NAPERTS(phot))
	default:
	    call error (0, "AP_ARRAYD: Unknown apphot double array")
	}
end
