include <gset.h>
include <pkg/gtools.h>
include "../lib/fitskydef.h"
include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

define	CRADIUS	3
define	RADIUS	15.0

# AP_YRADSETUP -- Procedure to set up phot interactively using a radial profile
# plot of a bright star.

int procedure ap_yradsetup (ap, im, id, gd, out, stid, x, y, max_nvertices)

pointer	ap			# pointer to apphot structure
pointer	im			# pointero to the IRAF image
pointer	id			# pointer to the image display
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number
real	x[ARB]			# array of x vertices
real	y[ARB]			# array of y vertices
int	max_nvertices		# maximum number of vertices

int	nvertices, ier, nsky, cier, sier, pier
pointer	gt, sp, r, sky, str 
real	rmin, rmax, imin, imax, u1, u2, v1, v2, x1, x2, y1, y2
real	fwhmpsf, capert, annulus, dannulus, sigma3, threshold
real	xcenter, ycenter, radius

int	apskybuf(), nscan(), scan(), ap_ycenter()
int	apfitsky(),  ap_yfit(), apstati(), ap_ymkpoly()
pointer	ap_gtinit()
real	apstatr(), ap_cfwhmpsf(), ap_ccapert(), ap_cannulus(), ap_csigma()
real	ap_cdannulus(), ap_ccthresh()

begin
	# Mark the polygon interactively.
	nvertices = ap_ymkpoly (ap, id, x, y, max_nvertices)
	if (nvertices <= 0)
	    return (nvertices)

	# Check for open display device and graphics stream.
	if (gd == NULL)
	    return (0)
	call greactivate (gd, 0)

	# Get a rough center.
	call ap_ictr (im, apstatr (ap, PYCX), apstatr (ap, PYCY), CRADIUS,
	    apstati (ap, POSITIVE), xcenter, ycenter)

	# Save old sky annulus parameters and set new sky extraction parameters.
	annulus = apstatr (ap, ANNULUS)
	dannulus = apstatr (ap, DANNULUS)
	call apsetr (ap, ANNULUS, 0.0)
	call printf ("Half width of extraction box (%4.1f) pixels:")
	    call pargr (RADIUS)
	call flush (STDOUT)
	if (scan () == EOF)
	    call apsetr (ap, DANNULUS, RADIUS / apstatr (ap, SCALE))
	else {
	    call gargr (radius)
	    if (nscan () < 1)
	        call apsetr (ap, DANNULUS, RADIUS / apstatr (ap, SCALE))
	    else
	        call apsetr (ap, DANNULUS, radius / apstatr (ap, SCALE))
	}
	    
	# Fetch the sky pixels and reset to original sky annuli.
	ier = apskybuf (ap, im, xcenter, ycenter)
	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	if (ier != AP_OK) {
	    call gdeactivate (gd, 0)
	    return (nvertices)
	}

	# Initialize.
	sky = AP_PSKY(ap)
	nsky = AP_NSKYPIX(sky)
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (r, nsky, TY_REAL)

	# Compute the radius values and intensity values.
	call ap_xytor (Memi[AP_COORDS(sky)], Memr[r], nsky, AP_SXC(sky),
	    AP_SYC(sky), AP_SNX(sky))
	call alimr (Memr[r], nsky, rmin, rmax)
	call alimr (Memr[AP_SKYPIX(sky)], nsky, imin, imax)

	# Store the viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Plot the radial profile.
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	gt = ap_gtinit (Memc[str], apstatr (ap, PYCX), apstatr (ap, PYCY))
	call gclear (gd)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[AP_SKYPIX(sky)], nsky, "plus")

	# Mark the FWHM of the PSF on the plot.
	fwhmpsf = ap_cfwhmpsf (ap, gd, rmin, rmax, imin, imax)

	# Mark the centering aperture on the plot.
	capert = ap_ccapert (ap, gd, rmin, rmax, imin, imax)

	# Mark the inner sky radius.
	annulus = ap_cannulus (ap, gd, rmin, rmax, imin, imax)

	# Mark the outer sky radius.
	dannulus = ap_cdannulus (ap, gd, annulus, rmin, rmax, imin, imax)

	# Estimate the mean sky.
	sigma3 = ap_csigma (ap, gd, rmin, rmax, imin, imax)

	# Estimate the minimum (maximum) sky level.
	threshold = ap_ccthresh (ap, gd, rmin, rmax, imin, imax)

	# Interactive setup is complete.
	call printf (
	    "Interactive setup is complete. Type w to save parameters.\n")

	# Update the important parameters.
	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, CAPERT, capert)
	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	call apsetr (ap, CTHRESHOLD, threshold)
	if (! IS_INDEFR(sigma3))
	    sigma3 = sigma3 / 3.0
	call apsetr (ap, SKYSIGMA, sigma3)

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF),
		UN_FWHMPSF, "full width half maximum of the psf")
	    call ap_rparam (out, KY_CAPERT, 2.0 * apstatr (ap, CAPERT),
		UN_CAPERT, "width of the centering box")
	    call ap_rparam (out, KY_ANNULUS, apstatr (ap, ANNULUS),
		UN_ANNULUS, "inner radius of the sky annulus")
	    call ap_rparam (out, KY_DANNULUS, apstatr (ap, DANNULUS),
		UN_DANNULUS, "width of the sky annulus")
	    call ap_rparam (out, KY_CTHRESHOLD, apstatr (ap, CTHRESHOLD),
		UN_CTHRESHOLD, "centering threshold")
	    call ap_rparam (out, KY_SKYSIGMA, apstatr (ap, SKYSIGMA),
		UN_SKYSIGMA, "standard deviation of 1 sky pixel")
	}

	# Restore the viewport and window coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	# Clean up memory space.
	call gdeactivate (gd, 0)
	call ap_gtfree (gt)
	call sfree (sp)

	# Print the answer.
	cier = ap_ycenter (ap, im, xcenter, ycenter, x, y, nvertices + 1)
	sier = apfitsky (ap, im, apstatr (ap, PYCX), apstatr (ap, PYCY), NULL,
	    gd)
	pier = ap_yfit (ap, im, x, y, nvertices + 1, apstatr (ap, SKY_MODE),
	    apstatr (ap, SKY_SIGMA), apstati (ap, NSKY))
	call ap_qyprint (ap, cier, sier, pier)

	return (nvertices)
end
