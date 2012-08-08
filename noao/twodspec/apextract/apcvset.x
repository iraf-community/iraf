include	<math/curfit.h>
include	"apertures.h"

# AP_CVSET -- Set the trace curve.
# If the input template aperture is NULL then the output trace curve
# is set to a constant zero otherwise a copy from the input template
# aperture is made.

procedure ap_cvset (apin, apout)

pointer	apin		# Input template aperture
pointer	apout		# Output aperture

int	apaxis, dispaxis, ncoeffs
real	a, b, c[1]
pointer	sp, coeffs

int	cvstati()

begin
	if (AP_CV(apout) != NULL)
	    call cvfree (AP_CV(apout))

	if (apin == NULL) {
	    # Determine the aperture and alternate axes.
	    apaxis = AP_AXIS(apout)
	    dispaxis = mod (apaxis, 2) + 1

	    # Determine the limits over which the curve is defined.
	    a = AP_CEN(apout, dispaxis) + AP_LOW(apout, dispaxis)
	    b = AP_CEN(apout, dispaxis) + AP_HIGH(apout, dispaxis)
	    if (a == b)
		b = b + 1

	    # Set the curve to a legendre polynomial of order 1 and value 0.
	    c[1] = 0.
	    call cvset (AP_CV(apout), LEGENDRE, a, b, c, 1)
	} else {
	    # Use a SAVE and RESTORE to copy the CURFIT data.
	    call smark (sp)
	    ncoeffs = cvstati (AP_CV(apin), CVNSAVE)
	    call salloc (coeffs, ncoeffs, TY_REAL)
	    call cvsave (AP_CV(apin), Memr[coeffs])
	    call cvrestore (AP_CV(apout), Memr[coeffs])
	    call sfree (sp)
	}
end
