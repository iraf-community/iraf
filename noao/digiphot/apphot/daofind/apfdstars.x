include <imhdr.h>
include <mach.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/find.h"

# AP_FDSTARS -- Find stars in an image using a pattern matching technique.

procedure ap_fdstars (im, ap, cnv, sky, out, id, boundary, constant,
	refit, stid)

pointer	im		# pointer to the input image
pointer	ap		# pointer to the apphot structure
pointer	cnv		# pointer to the convolved image
pointer	sky		# pointer to the sky image
int	out		# the output file descriptor
pointer	id		# pointer to image display stream
int	boundary	# type of boundary extension
real	constant	# constant for constant boundary extension
int	refit		# detect stars again
int	stid		# output file sequence number

int	norm, nxk, nyk, nstars
pointer	sp, str, gker2d, ngker2d, dker2d, skip
real	a, b, c, f, skysigma, skymode, threshold, relerr, gsums[LEN_GAUSS]
real	dmin, dmax, xsigsq, ysigsq
int	apstati(), ap_find()
real	ap_egkernel(), apstatr()
data	gker2d/NULL/, ngker2d/NULL/, dker2d/NULL/ skip /NULL/

define	detect_ 99

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (refit == YES)
	    goto detect_

	# Compute the parameters of the Gaussian kernel.
	call ap_egparams (FWHM_TO_SIGMA * apstatr (ap, FWHMPSF) * apstatr (ap,
	    SCALE), apstatr (ap, RATIO), apstatr (ap, THETA), apstatr (ap,
	    NSIGMA), a, b, c, f, nxk, nyk)

	# Allocate working space.
	if (gker2d != NULL)
	    call mfree (gker2d, TY_REAL)
	call malloc (gker2d, nxk * nyk, TY_REAL)
	if (ngker2d != NULL)
	    call mfree (ngker2d, TY_REAL)
	call malloc (ngker2d, nxk * nyk, TY_REAL)
	if (dker2d != NULL)
	    call mfree (dker2d, TY_REAL)
	call malloc (dker2d, nxk * nyk, TY_REAL)
	if (skip != NULL)
	    call mfree (skip, TY_INT)
	call malloc (skip, nxk * nyk, TY_INT)

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

	if (norm == YES)
	    call ap_fconvolve (im, cnv, sky, Memr[ngker2d], Memr[dker2d],
	        Memi[skip], nxk, nyk, gsums[GAUSS_SGOP])
	else
	    call ap_gconvolve (im, cnv, sky, Memr[gker2d], Memi[skip],
	        nxk, nyk, gsums, dmin, dmax)

detect_

	# Write the output header file.
	if (stid <= 1)
	    call ap_wfdparam (out, ap)

	call printf ("\nImage: %s  ")
	    call pargstr (IM_HDRFILE(im))
	call printf ("fwhmpsf: %g  ratio:  %g  theta:  %g  nsigma: %g\n\n")
	    call pargr (apstatr (ap, FWHMPSF))
	    call pargr (apstatr (ap, RATIO))
	    call pargr (apstatr (ap, THETA))
	    call pargr (apstatr (ap, NSIGMA))

	# Find all the objects in the input image with the specified image
	# characteristics.

	skysigma = apstatr (ap, SKYSIGMA)
	if (IS_INDEFR(skysigma)) {
	    skymode = 0.0
	    threshold = 0.0
	} else {
	    skymode = apstatr (ap, EPADU) * (skysigma ** 2 - (apstatr (ap,
	        READNOISE) / apstatr (ap, EPADU)) ** 2) 
	    skymode = max (0.0, skymode)
	    threshold = apstatr (ap, THRESHOLD) * skysigma
	}
	xsigsq = (apstatr (ap, SCALE) * apstatr (ap, FWHMPSF) / 2.35482) ** 2
	ysigsq = (apstatr (ap, SCALE) * apstatr (ap, RATIO) *
	    apstatr (ap, FWHMPSF) / 2.35482) ** 2

	nstars = ap_find (ap, im, cnv, out, id, Memr[gker2d],
	    Memi[skip], nxk, nyk, skymode, threshold, relerr,
	    apstati (ap, POSITIVE), xsigsq, ysigsq, dmin, dmax,
	    apstatr (ap, SHARPLO), apstatr (ap, SHARPHI), apstatr (ap,
	    ROUNDLO), apstatr (ap, ROUNDHI), YES, stid, apstati (ap,
	    MKDETECTIONS))
	stid = stid + nstars

	call printf ("\nthreshold: %g relerr: %5.3f  %g <= sharp <= %g  ")
	    call pargr (threshold)
	    call pargr (relerr)
	    call pargr (apstatr (ap, SHARPLO))
	    call pargr (apstatr (ap, SHARPHI))
	call printf ("%g <= round <= %g  \n\n")
	    call pargr (apstatr (ap, ROUNDLO))
	    call pargr (apstatr (ap, ROUNDHI))

	call apstats (ap, OUTNAME, Memc[str], SZ_FNAME)
	call printf ("Output file: %s\n\n")
	    call pargstr (Memc[str])

	call sfree (sp)
end
