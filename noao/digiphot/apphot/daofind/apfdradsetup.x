include <gset.h>
include <pkg/gtools.h>
include <math.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/find.h"

define	CRADIUS	3
define	RADIUS	15.0

# AP_FDRADSETUP -- Procedure to set up daofind interactively using the radial
# profile plot of a bright star.

procedure ap_fdradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number)

int	nx, ny, nsky, lenbuf
pointer	gt, sp, r, skypix, coords, cmd, str
real	radius, rmin, rmax, imin, imax, xcenter, ycenter, xc, yc
real	u1, u2, v1, v2, x1, x2, y1, y2
real	fwhmpsf, threshold

int	ap_skypix(), nscan(), scan(), apstati()
pointer	ap_gtinit()
real	apstatr(), ap_cfwhmpsf(), ap_cthresh()

begin
	# Check for open graphics stream.
	if (gd == NULL)
	    return
	call greactivate (gd, 0)
	call gclear (gd)

	# Center the star.
	call ap_ictr (im, wx, wy, CRADIUS, apstati (ap, POSITIVE), xcenter,
	    ycenter)

	# Save old sky annulus parameters and set new sky extraction parameters.
	call printf ("Radius of extraction box (%4.1f) pixels:")
	    call pargr (RADIUS)
	call flush (STDOUT)
	if (scan () == EOF)
	    radius = RADIUS
	else {
	    call gargr (radius)
	    if (nscan () < 1)
	        radius = RADIUS
	}

	# Initialize.
	lenbuf = PI * radius * (radius + 1.0)
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (r, lenbuf, TY_REAL)
	call salloc (skypix, lenbuf, TY_REAL)
	call salloc (coords, lenbuf, TY_INT)

	# Fetch the sky pixels and reset to original sky annuli.
	nsky = ap_skypix (im, xcenter, ycenter, 0.0, radius, Memr[skypix],
	    Memi[coords], xc, yc, nx, ny)
	if (nsky <= 0) {
	    call sfree (sp)
	    call gdeactivate (gd, 0)
	    return
	}

	# Compute the radius values and intensity values.
	call ap_xytor (Memi[coords], Memr[r], nsky, xc, yc, nx)
	call alimr (Memr[r], nsky, rmin, rmax)
	call alimr (Memr[skypix], nsky, imin, imax)

	# Store the viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Plot the radial profiles.
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	gt = ap_gtinit (Memc[str], xcenter, ycenter)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[skypix], nsky, "plus")

	# Mark the FWHM of the PSF on the plot.
	fwhmpsf = ap_cfwhmpsf (ap, gd, rmin, rmax, imin, imax)

	# Estimate the minimum (maximum) sky level.
	threshold = ap_cthresh (ap, gd, rmin, rmax, imin, imax)

	# Interactive setup is complete.
	call printf (
	    "Interactive setup is complete. Type w to store parameters.\n")

	# Update the important parameters.
	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, THRESHOLD, threshold)

	# Make sure the database file is up to date.
	if (out != NULL && stid > 1) {
	    call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF), 
		UN_FWHMPSF, "full width half maximum of the psf")
	    call ap_rparam (out, KY_THRESHOLD, apstatr (ap, THRESHOLD),
		UN_THRESHOLD, "threshold")
	}

	# Restore the viewport and window coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	call gdeactivate (gd, 0)
	call ap_gtfree (gt)
	call sfree (sp)
end
