include <imhdr.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/find.h"

define	FWHM_TO_SIGMA		0.42467

# AP_BFDFIND -- Find stars in an image using a pattern matching
# technique.

procedure ap_bfdfind (im, cnv, out, ap, boundary, constant, verbose)

pointer	im		# pointer to the input image
pointer	cnv		# pointer to the convolved image
pointer	out		# pointer to the output file
pointer	ap		# pointer to the apphot structure
int	boundary	# type of boundary extension
real	constant	# constant for constant boundary extension
int	verbose		# verbose switch

int	stid, nxk, nyk, nstars
pointer	sp, ker1x, ker1y, ker2d, skip
real	a, b, c, f, relerr

int	apstati(), apfind()
real	ap_egkernel(), apstatr()

begin
	stid = 1

	# Compute the parameters of the Gaussian kernel.
	call ap_egparams (FWHM_TO_SIGMA * apstatr (ap, FWHMPSF) *
	    apstatr (ap, SCALE), apstatr (ap, RATIO), apstatr (ap, THETA),
	    apstatr (ap, NSIGMA), a, b, c, f, nxk, nyk)

	# Allocate working space.
	call smark (sp)
	call salloc (ker1x, nxk, TY_REAL)
	call salloc (ker1y, nyk, TY_REAL)
	call salloc (ker2d, nxk * nyk, TY_REAL)
	call salloc (skip, nxk * nyk, TY_INT)

	# Compute the 1 and 2 D kernels.
	relerr = ap_egkernel (Memr[ker2d], Memi[skip], nxk, nyk, a, b, c, f)
	call ap_gkernel (Memr[ker1x], nxk, apstatr (ap, FWHMPSF) * apstatr (ap,
	    SCALE), apstatr (ap, NSIGMA))
	call ap_gkernel (Memr[ker1y], nyk, apstatr (ap, FWHMPSF) * apstatr (ap,
	    SCALE), apstatr (ap, NSIGMA))

	# Set up the image boundary extension characteristics.
	call ap_imset (im, boundary, max (1 + nxk / 2, 1 + nyk / 2),
	    constant)
	call ap_imset (cnv, boundary, max (1 + nxk / 2, 1 + nyk / 2),
	    constant)

	# Convolve the input image with the Gaussian kernel. The resultant
	# picture constains in each pixel the height of the Gaussian
	# function centered in the subarray which best represents the data
	# within a circle of nsigma * sigma of the Gaussian.

	call ap_convolve (im, cnv, Memr[ker2d], nxk, nyk)

	# Save the task parameters in the database file if the savepars
	# switch is enabled, otherwise a simple list of detected objects
	# is written to the data base file.

	call ap_wfdparam (out, ap)

	# Find all the objects in the input image with the specified image
	# characteristics.

	if (verbose == YES) {
	    call printf ("\nImage: %s  ")
		call pargstr (IM_HDRFILE(im))
	    call printf ("fwhmpsf: %g  ratio: %g  theta: %g  nsigma: %g\n\n")
		call pargr (apstatr (ap, FWHMPSF))
		call pargr (apstatr (ap, RATIO))
		call pargr (apstatr (ap, THETA))
		call pargr (apstatr (ap, NSIGMA))
	}

	nstars = apfind (im, cnv, out, NULL, Memr[ker1x], Memr[ker1y],
	    Memi[skip], nxk, nyk, apstatr (ap, THRESHOLD), apstati (ap,
	    POSITIVE), apstatr (ap, SHARPLO), apstatr (ap, SHARPHI),
	    apstatr (ap, ROUNDLO), apstatr (ap, ROUNDHI), verbose, stid, NO)

	if (verbose == YES) {
	    call printf (
	        "\n%d stars detected threshold: %g  %g <= sharp <= %g  ")
		call pargi (nstars)
		call pargr (apstatr (ap, THRESHOLD))
		call pargr (apstatr (ap, SHARPLO))
		call pargr (apstatr (ap, SHARPHI))
	    call printf ("%g <= round <= %g \n\n")
		call pargr (apstatr (ap, ROUNDLO))
		call pargr (apstatr (ap, ROUNDHI))
	}

	call sfree (sp)
end
