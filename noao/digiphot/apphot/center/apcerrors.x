include "../lib/center.h"

# AP_CERRORS -- Procedure to print the centering task error messages.

procedure ap_cerrrors (ap, ier)

pointer	ap		# apphot pointer
int	ier		# error code

begin
	switch (ier) {
	case AP_NOCTRAREA:
	    call printf ("There are no pixels in the centering box.\n")
        case AP_CTR_OUTOFBOUNDS:
	    call printf ("The centering region is partially outside the image.\n")
	case AP_NCTR_TOO_SMALL:
	    call printf ("The centering box has too few points.\n")
	case AP_CTR_SINGULAR:
	    call printf ("The centering solution is singular.\n")
	case AP_CTR_NOCONVERGE:
	    call printf ("The centering algorithm does not converge.\n")
	case AP_BADSHIFT:
	    call printf ("The center shift is large.\n")
	case AP_LOWSNRATIO:
	    call printf ("The signal to noise ratio is low.\n")
	default:
	    call printf ("")
	}
end
