include "../lib/polyphot.h"

# AP_PYERRORS -- Procedure to print out polyphot error messages when the
# task is run in interactive mode.

procedure ap_pyerrors (ap, cier, sier, pier)

pointer	ap		# apphot structure
int	cier		# centering error code
int	sier		# sky fitting error code
int	pier		# polyphot error code

begin
	# Print centering errors.
	call ap_cerrors (ap, cier)

	# Print sky fitting errors.
	call ap_serrors (ap, sier)

	# Print the polyphot errors.
	switch (pier) {
	case PY_NOPOLYGON:
	    call printf ("The polygon is undefined or too few vertices.\n")
	case PY_OUTOFBOUNDS:
	    call printf ("The polygon is partially outside the image.\n")
	case PY_NOPIX:
	    call printf ("The effective polygon area is 0.0.\n")
	case PY_NOSKYMODE:
	    call printf ("The sky value is undefined.\n")
	case PY_BADDATA:
	    call printf ("Bad pixels inside the polygon.\n")
	default:
	    call printf ("")
	}
end
