include "../lib/apphot.h"

define	CRADIUS	3
define	RADIUS	15.0

# AP_SHOWPLOT -- Plot a radial profile of a star.

int	procedure ap_showplot (ap, im, wx, wy, gd, xcenter, ycenter, rmin,
	rmax, imin, imax)

pointer	ap			# pointer to the apphot structure
pointer	im			# pointer to the image
real	wx, wy			# the cursor coordinates
pointer	gd			# pointer to the graphics stream
real	xcenter, ycenter	# the centered coordinates
real	rmin, rmax		# minimum and maximum radius
real	imin, imax		# minimum and maximum intensity

int	lenbuf, nx, ny, nsky
pointer	sp, r, skypix, coords, str, gt
real	radius, xc, yc
int	ap_gvrad(), apstati(), ap_skypix()
pointer	ap_gtinit()
real	apstatr()

begin
	call gclear (gd)
	call gflush (gd)

	# Set the pixel extraction parameters.
	lenbuf = ap_gvrad (RADIUS, radius)

	# Initialize.
	call smark (sp)
	call salloc (r, lenbuf, TY_REAL)
	call salloc (skypix, lenbuf, TY_REAL)
	call salloc (coords, lenbuf, TY_INT)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Center the star.
	call ap_ictr (im, wx, wy, CRADIUS, apstati (ap, POSITIVE), xcenter,
	    ycenter)

	# Fetch the pixels.
	nsky = ap_skypix (im, xcenter, ycenter, 0.0, radius, Memr[skypix],
	    Memi[coords], xc, yc, nx, ny)
	if (nsky <= 0) {
	    call sfree (sp)
	    return (ERR)
	}

	# Compute the radius and intensity values.
	call ap_xytor (Memi[coords], Memr[r], nsky, xc, yc, nx)
	call alimr (Memr[r], nsky, rmin, rmax)
	call alimr (Memr[skypix], nsky, imin, imax)

	# Plot the radial profiles.
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	gt = ap_gtinit (Memc[str], xcenter, ycenter)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[skypix], nsky, "plus")

	# Cleanup.
	call ap_gtfree (gt)
	call sfree (sp)

	return (OK)
end
