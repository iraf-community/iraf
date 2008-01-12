include "../lib/center.h"

# AP_CERRORS -- Procedure to print the centering task error messages.

procedure ap_cerrrors (ap, ier)

pointer	ap		# apphot pointer (unused)
int	ier		# centering error code

begin
	switch (ier) {
	case AP_CTR_NOAREA:
	    call printf ("There are no pixels in the centering box.\n")
        case AP_CTR_OUTOFBOUNDS:
	    call printf (
	        "The centering region is partially outside the image.\n")
	case AP_CTR_NTOO_SMALL:
	    call printf ("The centering box has too few points.\n")
	case AP_CTR_SINGULAR:
	    call printf ("The centering solution is singular.\n")
	case AP_CTR_NOCONVERGE:
	    call printf ("The centering algorithm does not converge.\n")
	case AP_CTR_BADSHIFT:
	    call printf ("The center shift is large.\n")
	case AP_CTR_LOWSNRATIO:
	    call printf ("The signal to noise ratio is low.\n")
	case AP_CTR_BADDATA:
	    call printf ("Bad data in the centering subraster.\n")
	default:
	    call printf ("")
	}
end
