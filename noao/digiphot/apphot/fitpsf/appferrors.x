include "../lib/fitpsf.h"

# AP_PFERRORS -- Print detailed fitpsf error messages on the standard output.

procedure ap_pferrors (ap, ier)

pointer	ap		# pointer to apphot structure (unused)
int	ier		# integer error code

begin
	switch (ier) {
	case AP_NOPSFAREA:
	    call printf (
	        "The psf fitting aperture is outside the image.\n")
	case AP_PSF_OUTOFBOUNDS:
	    call printf (
	        "The psf fitting aperture is partially outside the image.\n")
	case AP_NPSF_TOO_SMALL:
	    call printf (
	        "The number of data points is too few for psf fitting.\n")
	case AP_PSF_SINGULAR:
	    call printf ("The psf fitting solution is singular.\n")
	case AP_PSF_NOCONVERGE:
	    call printf ("The psf fitting solution did not converge.\n")
	}
end
