include <imhdr.h>
include <imio.h>
include <mach.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/find.h"

# AP_BFDFIND -- Find stars in an image using a pattern matching
# technique.

procedure ap_bfdfind (im, cnv, sky, out, ap, boundary, constant, verbose)

pointer	im		# pointer to the input image
pointer	cnv		# pointer to the convolved image
pointer	sky		# pointer to the sky image
int	out		# the output file descriptor
pointer	ap		# pointer to the apphot structure
int	boundary	# type of boundary extension
real	constant	# constant for constant boundary extension
int	verbose		# verbose switch

int	norm, nxk, nyk, nstars, stid
pointer	sp, gker2d, ngker2d, dker2d, skip
real	a, b, c, f, gsums[LEN_GAUSS], skysigma, skymode, threshold, relerr
real	dmax, dmin, xsigsq, ysigsq
int	apstati(), ap_find()
real	ap_egkernel(), apstatr()

begin
	# Compute the parameters of the Gaussian kernel.
	call ap_egparams (FWHM_TO_SIGMA * apstatr (ap, FWHMPSF) *
	    apstatr (ap, SCALE), apstatr (ap, RATIO), apstatr (ap, THETA),
	    apstatr (ap, NSIGMA), a, b, c, f, nxk, nyk)

	# Allocate working space.
	call smark (sp)
	call salloc (gker2d, nxk * nyk, TY_REAL)
	call salloc (ngker2d, nxk * nyk, TY_REAL)
	call salloc (dker2d, nxk * nyk, TY_REAL)
	call salloc (skip, nxk * nyk, TY_INT)

	# Compute the 1 and 2 D kernels.
	if (IS_INDEFR(apstatr(ap, DATAMIN)) && IS_INDEFR(apstatr(ap,
	    DATAMAX))) {
	    norm = YES
	    dmin = -MAX_REAL
	    dmax = MAX_REAL
	} else {
	    norm = NO
	    if (IS_INDEFR(apstatr (ap, DATAMIN)))
		dmin = -MAX_REAL
	    else
		dmin = apstatr (ap, DATAMIN)
	    if (IS_INDEFR(apstatr (ap, DATAMAX)))
		dmax = MAX_REAL
	    else
		dmax = apstatr (ap, DATAMAX)
	}
	relerr = ap_egkernel (Memr[gker2d], Memr[ngker2d], Memr[dker2d],
	    Memi[skip], nxk, nyk, gsums, a, b, c, f)

	# Set up the image boundary extension characteristics.
	call ap_imset (im, boundary, max (1 + nxk / 2, 1 + nyk / 2),
	    constant)
	call ap_imset (cnv, boundary, max (1 + nxk / 2, 1 + nyk / 2),
	    constant)

	# Convolve the input image with the Gaussian kernel. The resultant
	# picture constains in each pixel the height of the Gaussian
	# function centered in the subarray which best represents the data
	# within a circle of nsigma * sigma of the Gaussian.

	if (IM_ACMODE(cnv) != READ_ONLY) {
	    if (norm == YES)
	        call ap_fconvolve (im, cnv, sky, Memr[ngker2d], Memr[dker2d],
		    Memi[skip], nxk, nyk, gsums[GAUSS_SGOP])
	    else
	        call ap_gconvolve (im, cnv, sky, Memr[gker2d], Memi[skip],
		    nxk, nyk, gsums, dmin, dmax)
	}

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

	# Get the skymode and threshold.
	skysigma = apstatr (ap, SKYSIGMA)
	if (IS_INDEFR(skysigma)) {
	    skymode = 0.0
	    threshold = 0.0
	} else {
	    skymode = apstatr (ap, EPADU) * (skysigma ** 2 -
	        (apstatr (ap, READNOISE) / apstatr (ap, EPADU)) ** 2)
	    skymode = max (0.0, skymode)
	    threshold = apstatr (ap, THRESHOLD) * skysigma
	}

	# Compute the x and y sigma.
	xsigsq = (apstatr (ap, SCALE) * apstatr (ap, FWHMPSF) / 2.35482) ** 2
	ysigsq = (apstatr (ap, SCALE) * apstatr (ap, RATIO) *
	    apstatr (ap, FWHMPSF) / 2.35482) ** 2

	# Find the stars.
	stid = 1
	nstars = ap_find (ap, im, cnv, out, NULL, Memr[gker2d],
	    Memi[skip], nxk, nyk, skymode, threshold, relerr,
	    apstati (ap,POSITIVE), xsigsq, ysigsq, dmin, dmax,
	    apstatr (ap, SHARPLO), apstatr (ap, SHARPHI), apstatr (ap,
	    ROUNDLO), apstatr (ap, ROUNDHI), verbose, stid, NO)

	if (verbose == YES) {
	    call printf (
	        "\nthreshold: %g relerr: %5.3f  %g <= sharp <= %g  ")
		call pargr (threshold)
		call pargr (relerr)
		call pargr (apstatr (ap, SHARPLO))
		call pargr (apstatr (ap, SHARPHI))
	    call printf ("%g <= round <= %g \n\n")
		call pargr (apstatr (ap, ROUNDLO))
		call pargr (apstatr (ap, ROUNDHI))
	}

	call sfree (sp)
end
