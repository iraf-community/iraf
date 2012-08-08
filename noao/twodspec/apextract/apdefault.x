include	<imhdr.h>
include	"apertures.h"

# AP_DEFAULT -- Create a default aperture.
# The aperture ID, beam, axis, and the aperture center in both dimensions
# are specified.  The aperture limits along the dispersion axis are set to
# the full size of the image while along the dispersion axis they are queried.
# The default offset curve is a constant zero curve.

procedure ap_default (im, apid, apbeam, apaxis, apcenter, dispcenter, ap)

pointer	im		# IMIO pointer
int	apid		# Aperture ID
int	apbeam		# Aperture beam number
int	apaxis		# Aperture axis
real	apcenter	# Center along the aperture axis
real	dispcenter	# Center along the dispersion axis	
pointer	ap		# Aperture pointer

int	dispaxis
real	apgetr()

begin
	dispaxis = mod (apaxis, 2) + 1

	call ap_alloc (ap)
	AP_ID(ap) = apid
	AP_BEAM(ap) = apbeam
	AP_AXIS(ap) = apaxis
	AP_CEN(ap, apaxis) = apcenter
	AP_LOW(ap, apaxis) = apgetr ("lower") 
	if (IS_INDEFR(AP_LOW(ap,apaxis)))
	    call error (1, "INDEF not allowed (lower)")
	AP_HIGH(ap, apaxis) = apgetr ("upper") 
	if (IS_INDEFR(AP_HIGH(ap,apaxis)))
	    call error (1, "INDEF not allowed (upper)")
	AP_CEN(ap, dispaxis) = dispcenter
	AP_LOW(ap, dispaxis) = 1 - AP_CEN(ap, dispaxis)
	AP_HIGH(ap, dispaxis) = IM_LEN(im, dispaxis) - AP_CEN(ap, dispaxis)
	call ap_cvset (NULL, ap)
	call ap_icset (NULL, ap, IM_LEN(im, apaxis))
end
