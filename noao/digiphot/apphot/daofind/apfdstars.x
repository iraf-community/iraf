include <imhdr.h>
include <fset.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/find.h"

define	FWHM_TO_SIGMA		0.42467

# AP_FDSTARS -- Find stars in an image using a pattern matching technique.

procedure ap_fdstars (im, ap, cnv, out, id, boundary, constant, refit, stid)

pointer	im		# pointer to the input image
pointer	ap		# pointer to the apphot structure
pointer	cnv		# pointer to the convolved image
pointer	out		# pointer to the output file
pointer	id		# pointer to image display stream
int	boundary	# type of boundary extension
real	constant	# constant for constant boundary extension
int	refit		# detect stars again
int	stid		# output file sequence number

int	nxk, nyk, nstars
pointer	sp, str, ker1x, ker1y, ker2d, skip
real	a, b, c, f, relerr
int	apstati(), apfind()
real	ap_egkernel(), apstatr()
data	ker1x /NULL/, ker1y/NULL/, ker2d/NULL/, skip /NULL/

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
	if (ker1x != NULL)
	    call mfree (ker1x, TY_REAL)
	call malloc (ker1x, nxk, TY_REAL)
	if (ker1y != NULL)
	    call mfree (ker1y, TY_REAL)
	call malloc (ker1y, nyk, TY_REAL)
	if (ker2d != NULL)
	    call mfree (ker2d, TY_REAL)
	call malloc (ker2d, nxk * nyk, TY_REAL)
	if (skip != NULL)
	    call mfree (skip, TY_INT)
	call malloc (skip, nxk * nyk, TY_INT)

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

detect_
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

	nstars = apfind (im, cnv, out, id, Memr[ker1x], Memr[ker1y], Memi[skip],
	    nxk, nyk, apstatr (ap, THRESHOLD), apstati (ap, POSITIVE),
	    apstatr (ap, SHARPLO), apstatr (ap, SHARPHI), apstatr (ap,
	    ROUNDLO), apstatr (ap, ROUNDHI), YES, stid, apstati (ap,
	    MKDETECTIONS))
	stid = stid + nstars

	call printf ("\n%d  stars detected threshold: %g  %g <= sharp <= %g  ")
	    call pargi (nstars)
	    call pargr (apstatr (ap, THRESHOLD))
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
