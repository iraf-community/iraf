include "../lib/phot.h"
include "../lib/radprof.h"

# AP_RFERRORS -- Procedure to print a short form of the output of radprof on
# the standard output.

procedure ap_rferrors (ap, cier, sier, pier, rier)

pointer	ap	# pointer to apphot structure
int	cier	# centering error
int	sier	# sky fitting error
int	pier	# photmetry error
int	rier	# photometric error

begin
	# Print the centering errors.
	call ap_cerrors (ap, cier)

	# Print the sky fitting errors.
	call ap_serrors (ap, sier)

	# Print the photometry errors.
	switch (pier) {
	case AP_NOAPERT:
	    call printf ("Photometry apertures are outside the the image.\n")
	case AP_APERT_OUTOFBOUNDS:
	    call printf ("Photometry apertures are partially outside the image.\n")
	case AP_NOSKYMODE:
	    call printf ("The sky value is undefined.\n")
	case AP_NEGMAG:
	    call printf ("")
	}

	# Print the radial profile fitting errors.
	switch (rier) {
	case AP_NOPROFILE:
	    call printf ("The profile fitting region is outside the image.\n")
	case AP_RPOUTOFBOUNDS:
	    call printf ("The profile fitting region is partially outside the image.\n")
	case AP_NRP_TOO_SMALL:
	    call printf ("There are too few points in the profile fitting region.\n")
	case AP_RP_SINGULAR:
	    call printf ("The profile fit is singular.\n")
	default:
	    call printf ("")
	}
end
