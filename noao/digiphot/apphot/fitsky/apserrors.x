include "../lib/fitsky.h"

# AP_SERRORS -- Program to print out detailed fitsky error messages when the
# program is run in interactive mode.

procedure ap_serrors (ap, ier)

pointer	ap		# pointer to apphot structure (not used)
int	ier		# integer error code

begin
	switch (ier) {
	case AP_NOSKYAREA:
	    call printf ("The are no pixels in the sky annulus.\n")
	case AP_SKY_OUTOFBOUNDS:
	    call printf ("The sky annulus is outside of the image.\n")
	case AP_NOHISTOGRAM:
	    call printf ("The sky histogram has no width.\n")
	case AP_FLAT_HIST:
	    call printf ("The sky histogram is flat or concave.\n")
	case AP_NSKY_TOO_SMALL:
	    call printf ("The number of sky points is too small.\n")
	case AP_SKY_SINGULAR:
	    call printf ("The sky fit is singular.\n")
	case AP_SKY_NOCONVERGE:
	    call printf ("The sky fit did not converge.\n")
	case AP_NOGRAPHICS:
	    call printf ("Interactive graphics are not available.\n")
	case AP_NOSKYFILE:
	    call printf (
	        "The text file containing sky values does not exist.\n")
	case AP_EOFSKYFILE:
	    call printf ("The sky file is at EOF.\n")
	case AP_BADSKYSCAN:
	    call printf (
	    "An error occurred in decoding the current line in the sky file.\n")
	case AP_BADPARAMS:
	    call printf ("Out of range mode or -ve sigma in Gaussian fit.\n")
	default:
	    call printf ("")
	}
end
