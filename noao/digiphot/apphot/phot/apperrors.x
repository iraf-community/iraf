include "../lib/phot.h"

# AP_PERRORS -- Print the photometry task errors.

procedure ap_perrors (ap, cier, sier, pier)

pointer	ap		# apphot structure
int	cier		# centering error code
int	sier		# sky fitting error code
int	pier		# photometry error code

begin
	# Print the centering error message.
	call ap_cerrors (ap, cier)

	# Print the sky fitting error message.
	call ap_serrors (ap, sier)

	# Print the photometry error message.
	switch (pier) {
	case AP_NOAPERT:
	    call printf ("Photometry apertures are outside of the image\n")
	case AP_APERT_OUTOFBOUNDS:
	 call printf ("Photometry apertures are partially outside the image\n")
	case AP_NOSKYMODE:
	    call printf ("The sky value is undefined\n")
	case AP_NEGMAG:
	    call printf ("The total flux inside the aperture is negative\n")
	default:
	    call printf ("")
	}
end
