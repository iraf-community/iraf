include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/fitpsf.h"

define	RADIUS	15.0

# AP_PFRADSETUP -- Procedure to set up fitpsf interactively

procedure ap_pfradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointero to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	maxpix, npix, nx, ny, ier
pointer	gt, sp, pix, coords, r, str
real	radius, rmin, rmax, imin, imax, xc, yc, psfapert
real	fwhmpsf, skysigma3, threshold
real	x1, x2, y1, y2, u1, u2, v1, v2

int	ap_skypix(), nscan(), scan(), apsffit()
pointer	ap_gtinit()
real	apstatr(), ap_cfwhmpsf(), ap_cthresh(), ap_csigma(), ap_cpapert()

begin
	if (gd == NULL)
	    return
	call greactivate (gd, 0)

	# Get the half width of the extraction region.
	call printf ("Half width of extraction box in pixels (%4.1f):")
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

	# Fetch the pixels.
	npix = ap_skypix (im, wx, wy, 0.0, radius, Memr[pix], Memi[coords],
	    xc, yc, nx, ny)
	if (npix == 0)
	    return

	# Save old viewport and window coordinates
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Initialize the plot and compute the radius values.
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	gt = ap_gtinit (Memc[str], wx, wy)
	call ap_xytor (Memi[coords], Memr[r], npix, xc, yc, nx)
	call alimr (Memr[r], npix, rmin, rmax)
	call alimr (Memr[pix], npix, imin, imax)

	# Plot the radial profiles.
	call gclear (gd)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[pix], npix, "plus")

	# Mark the FWHM of the PSF on the plot.
	fwhmpsf = ap_cfwhmpsf (ap, gd, rmin, rmax, imin, imax)

	# Mark the fitting box width on the plot.
	psfapert = ap_cpapert (ap, gd, rmin, rmax, imin, imax)

	# Estimate the sky mean.
	skysigma3 = ap_csigma (ap, gd, rmin, rmax, imin, imax)

	# Estimate the minimum (maximum) data value.
	threshold = ap_cthresh (ap, gd, rmin, rmax, imin, imax)

	# Interactive setup is complete.
	call printf (
	    "Interactive setup is complete. Type w to save parameters.\n")

	# Update the important parameters.
	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, PSFAPERT, psfapert)
	call apsetr (ap, THRESHOLD, threshold)
	if (! IS_INDEFR(skysigma3))
	    skysigma3 = skysigma3 / 3.0
	call apsetr (ap, SKYSIGMA, skysigma3)

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF),
		UN_FWHMPSF, "full width half maximum of the psf")
	    call ap_rparam (out, KY_PSFAPERT, apstatr (ap, PSFAPERT),
		UN_PSFAPERT, "width of fitting box")
	    call ap_rparam (out, KY_THRESHOLD, apstatr (ap, THRESHOLD),
		UN_THRESHOLD, "threshold")
	    call ap_rparam (out, KY_SKYSIGMA, apstatr (ap, SKYSIGMA),
		UN_SKYSIGMA, "standard deviation of 1 sky pixel")
	}

	# Restore old viewport and window coords.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	call gdeactivate (gd, 0)
	call ap_gtfree (gt)
	call sfree (sp)

	# Fit the object.
	ier = apsffit (ap, im, wx, wy)
	call ap_qppsf (ap, ier)
end
