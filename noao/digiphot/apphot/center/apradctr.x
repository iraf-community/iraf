include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/center.h"

define	CRADIUS	3
define	RADIUS	15.0

# AP_CRADSETUP -- Procedure to set up apphot interactively

procedure ap_cradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointero to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	maxpix, npix, nx, ny, cier
pointer	gt, sp, pix, coords, r, str
real	xcenter, ycenter, xc, yc, radius, rmin, rmax, imin, imax
real	fwhmpsf, capert, skysigma3, threshold
real	u1, u2, v1, v2, x1, x2, y1, y2

int	ap_skypix(), apfitcenter(), nscan(), scan(), apstati()
pointer	ap_gtinit()
real	apstatr(), ap_cfwhmpsf(), ap_ccapert(), ap_csigma(), ap_ccthresh()

begin
	# Check for open graphics stream.
	if (gd == NULL)
	    return
	call greactivate (gd, 0)
	call gclear (gd)

	# Get a rough approximation for the center.
	call ap_ictr (im, wx, wy, CRADIUS, apstati (ap, POSITIVE), xcenter,
	    ycenter)

	# Get the radius of the extraction region.
	call printf ("Half width of extraction box (%4.1f) pixels:")
	    call pargr (RADIUS)
	call flush (STDOUT)
	if (scan () == EOF)
	    radius = RADIUS
	else {
	    call gargr (radius)
	    if (nscan () < 1)
	        radius = RADIUS
	}
	    
	# Allocate temporary space.
	maxpix = (2 * int (radius) + 1) ** 2
	call smark (sp)
	call salloc (coords, maxpix, TY_INT)
	call salloc (pix, maxpix, TY_REAL)
	call salloc (r, maxpix, TY_REAL)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Fetch the radial profile and reset the sky.
	npix = ap_skypix (im, xcenter, ycenter, 0.0, radius, Memr[pix],
	    Memi[coords], xc, yc, nx, ny)
	if (npix == 0) {
	    call gdeactivate (gd, 0)
	    call sfree (sp)
	    return
	}

	# Store old viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Initialize the plot and store the viewport and window limits.
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	gt = ap_gtinit (Memc[str], xcenter, ycenter)

	# Compute the radius values.
	call ap_xytor (Memi[coords], Memr[r], npix, xc, yc, nx)
	call alimr (Memr[r], npix, rmin, rmax)
	call alimr (Memr[pix], npix, imin, imax)

	# Plot radial profiles.
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[pix], npix, "plus")

	# Mark the FWHM of the PSF on the plot.
	fwhmpsf = ap_cfwhmpsf (ap, gd, rmin, rmax, imin, imax)

	# Mark the centering aperture on the plot.
	capert = ap_ccapert (ap, gd, rmin, rmax, imin, imax)

	# Estimate the sky level.
	skysigma3 = ap_csigma (ap, gd, rmin, rmax, imin, imax)

	# Estimate the minimum (maximum) data level.
	threshold = ap_ccthresh (ap, gd, rmin, rmax, imin, imax)

	# Interactive setup is complete.
	call printf (
	    "Interactive setup is complete [Type w to save parameters].\n")

	# Update the important parameters.
	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, CAPERT, capert)
	call apsetr (ap, CTHRESHOLD, threshold)
	if (! IS_INDEFR(skysigma3))
	    skysigma3 = skysigma3 / 3.0
	call apsetr (ap, SKYSIGMA, skysigma3)

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF),
		UN_FWHMPSF, "full width half maximum of the psf")
	    call ap_rparam (out, KY_CAPERT, 2.0 * apstatr (ap, CAPERT),
		UN_CAPERT, "centering box width")
	    call ap_rparam (out, KY_CTHRESHOLD, apstatr (ap, CTHRESHOLD),
		UN_CTHRESHOLD, "threshold for centering ")
	    call ap_rparam (out, KY_SKYSIGMA, apstatr (ap, SKYSIGMA),
		UN_SKYSIGMA, "standard deviation of 1 sky pixel")
	}

	# Restore old window and viewport coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	# Free plots and working space.
	call ap_gtfree (gt)
	call sfree (sp)
	call gdeactivate (gd, 0)

	# Fit the center and print the results under the plot.
	cier = apfitcenter (ap, im, xcenter, ycenter)
	call apcplot (ap, 0, cier, gd, apstati (ap, RADPLOTS))
	call ap_qcenter (ap, cier)
end
