include	"apertures.h"

# AP_COPY -- Make a copy of an aperture.
# The title is not copied.

procedure ap_copy (apin, apout)

pointer	apin		# Aperture to copy
pointer	apout		# New copy

int	i

begin
	# Allocate memory, transfer the aperture parameters, and call procedures
	# which copy the offset curve and background parameters.
	call ap_alloc (apout)
	AP_ID(apout) = AP_ID(apin)
	AP_BEAM(apout) = AP_BEAM(apin)
	AP_AXIS(apout) = AP_AXIS(apin)
	do i = 1, 2 {
	    AP_CEN(apout, i) = AP_CEN(apin, i)
	    AP_LOW(apout, i) = AP_LOW(apin, i)
	    AP_HIGH(apout, i) = AP_HIGH(apin, i)
	}
	call ap_cvset (apin, apout)
	call ic_open (AP_IC(apout))
	call ic_copy (AP_IC(apin), AP_IC(apout))
end
