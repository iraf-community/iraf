include	"apertures.h"

# AP_VALUES -- Return the values for an aperture

procedure ap_values (current, aps, line, apid, apbeam, center, low, high)

int	current			# Index to current aperture
pointer	aps[ARB]		# Apertures
int	line			# Line
int	apid			# Aperture ID
int	apbeam			# Aperture beam
real	center			# Aperture center
real	low			# Lower limit of aperture
real	high			# Upper limit of aperture

int	apaxis
pointer	ap

real	ap_cveval()

begin
	if (current > 0) {
	    ap = aps[current]
	    apaxis = AP_AXIS(ap)

	    apid = AP_ID(ap)
	    apbeam = AP_BEAM(ap)
	    center = AP_CEN(ap, apaxis) + ap_cveval (AP_CV(ap), real (line))
	    low = AP_LOW(ap, apaxis)
	    high = AP_HIGH(ap, apaxis)
	}
end
