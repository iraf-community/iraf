include <gset.h>
include	"apertures.h"

# AP_UPDATE -- Update an aperture.

procedure ap_update (gp, ap, line, apid, apbeam, center, low, high)

pointer	gp		# GIO pointer
pointer	ap		# Aperture pointer
int	line		# Dispersion line
int	apid		# New aperture ID
int	apbeam		# New aperture beam
real	center		# New center at dispersion line
real	low		# New lower limit
real	high		# New upper limit

real	cveval(), ic_getr()

begin
	# Erase the current aperture.
	call gseti (gp, G_PLTYPE, 0)
	call ap_gmark (gp, line, ap, 1)

	# Update the aperture.
	AP_ID(ap) = apid
	AP_BEAM(ap) = apbeam
	AP_CEN(ap, AP_AXIS(ap)) = center - cveval (AP_CV(ap), real (line))
        AP_LOW(ap, AP_AXIS(ap)) = min (low, high)
        AP_HIGH(ap, AP_AXIS(ap)) = max (low, high)
	if (AP_IC(ap) != NULL) {
	    call ic_putr (AP_IC(ap), "xmin",
		min (low, high, ic_getr (AP_IC(ap), "xmin")))
	    call ic_putr (AP_IC(ap), "xmax",
		max (low, high, ic_getr (AP_IC(ap), "xmax")))
	}

	# Mark the new aperture.
	call gseti (gp, G_PLTYPE, 1)
	call ap_gmark (gp, line, ap, 1)
end
