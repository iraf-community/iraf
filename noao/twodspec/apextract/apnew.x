include	"apertures.h"

# AP_NEW -- Allocate a new aperture and set it to the default aperture.

procedure ap_new (apdef, aps, naps)

pointer	apdef			# Default aperture
pointer	aps[ARB]		# Aperture data
int	naps			# Number of apertures

int	i
pointer	ap

errchk	malloc

begin
	if (naps == AP_MAXAPS)
	    call error (0, "Too many apertures")

	naps = naps + 1
	call ap_alloc (ap)

	AP_ID(ap) = AP_ID(apdef)
	AP_BEAM(ap) = AP_BEAM(apdef)
	AP_AXIS(ap) = AP_AXIS(apdef)
	do i = 1, 2 {
	    AP_CEN(ap, i) = AP_CEN(apdef, i)
	    AP_LOW(ap, i) = AP_LOW(apdef, i)
	    AP_HIGH(ap, i) = AP_HIGH(apdef, i)
	}
	call ap_cvset (apdef, ap)
	call ap_icset (apdef, 0., 0., ap)

	aps[naps] = ap
end
