include "../lib/apphot.h"

define	RADIUS	15.0
define	CRADIUS	 5

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

real	radius, xc, yc, xold, yold
pointer	sp, r, skypix, coords, index, str, gt
int	niter, lenbuf, nx, ny, nsky

real	apstatr()
pointer	ap_gtinit()
int	ap_gvrad(), apstati(), ap_skypix()

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
	call salloc (index, lenbuf, TY_INT)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Center the star.
	niter = 0
	xold = wx
	yold = wy
	repeat {
	    call ap_ictr (im, xold, yold, CRADIUS, apstati (ap, POSITIVE),
	        xcenter, ycenter)
	    niter = niter + 1
	    if (abs (xcenter - xold) <= 1.0 && abs (ycenter - yold) <= 1.0)
		break
	    xold = xcenter
	    yold = ycenter
	} until (niter >= 3)

	# Fetch the pixels.
	nsky = ap_skypix (im, xcenter, ycenter, 0.0, radius, Memr[skypix],
	    Memi[coords], xc, yc, nx, ny)
	if (nsky <= 0) {
	    call sfree (sp)
	    return (ERR)
	}
	call ap_index (Memi[index], nsky)

	# Compute the radius and intensity values.
	call ap_xytor (Memi[coords], Memi[index], Memr[r], nsky, xc, yc, nx)
	call alimr (Memr[r], nsky, rmin, rmax)
	call alimr (Memr[skypix], nsky, imin, imax)

	# Plot the radial profiles.
	#call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	call apstats (ap, IMROOT, Memc[str], SZ_FNAME)
	call ap_ltov (im, xcenter, ycenter, xc, yc, 1)
	gt = ap_gtinit (Memc[str], xc, yc)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[skypix], nsky, "plus")

	# Cleanup.
	call ap_gtfree (gt)
	call sfree (sp)

	return (OK)
end
