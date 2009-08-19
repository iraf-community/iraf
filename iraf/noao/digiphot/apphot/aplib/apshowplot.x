include "../lib/apphot.h"

define	RADIUS	15.0
define	CRADIUS	 5

# AP_SHOWPLOT -- Plot a radial profile of a star.

int procedure ap_showplot (ap, im, wx, wy, gd, xcenter, ycenter, rmin,
	rmax, imin, imax)

pointer	ap			# pointer to the apphot structure
pointer	im			# pointer to the image
real	wx, wy			# the cursor coordinates
pointer	gd			# pointer to the graphics stream
real	xcenter, ycenter	# the centered coordinates
real	rmin, rmax		# minimum and maximum radius
real	imin, imax		# minimum and maximum intensity

size_t	sz_val
real	radius, xc, yc, xold, yold
pointer	sp, r, skypix, coords, index, str, gt
int	niter
size_t	lenbuf, nx, ny, nsky
long	l_val

real	apstatr(), aabs()
pointer	ap_gtinit()
int	apstati()
long	ap_skypix(), ap_gvrad()

begin
	call gclear (gd)
	call gflush (gd)

	# Set the pixel extraction parameters.
	lenbuf = ap_gvrad (RADIUS, radius)

	# Initialize.
	call smark (sp)
	call salloc (r, lenbuf, TY_REAL)
	call salloc (skypix, lenbuf, TY_REAL)
	call salloc (coords, lenbuf, TY_LONG)
	call salloc (index, lenbuf, TY_LONG)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)

	# Center the star.
	niter = 0
	xold = wx
	yold = wy
	repeat {
	    l_val = CRADIUS
	    call ap_ictr (im, xold, yold, l_val, apstati (ap, POSITIVE),
	        xcenter, ycenter)
	    niter = niter + 1
	    if (aabs (xcenter - xold) <= 1.0 && aabs (ycenter - yold) <= 1.0)
		break
	    xold = xcenter
	    yold = ycenter
	} until (niter >= 3)

	# Fetch the pixels.
	l_val = ap_skypix (im, xcenter, ycenter, 0.0, radius, Memr[skypix],
	    Meml[coords], xc, yc, nx, ny)
	if (l_val <= 0) {
	    call sfree (sp)
	    return (ERR)
	}
	nsky = l_val
	call ap_index (Meml[index], nsky)

	# Compute the radius and intensity values.
	call ap_xytor (Meml[coords], Meml[index], Memr[r], nsky, xc, yc, nx)
	call alimr (Memr[r], nsky, rmin, rmax)
	call alimr (Memr[skypix], nsky, imin, imax)

	# Plot the radial profiles.
	#call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	call apstats (ap, IMROOT, Memc[str], SZ_FNAME)
	sz_val = 1
	call ap_ltov (im, xcenter, ycenter, xc, yc, sz_val)
	gt = ap_gtinit (Memc[str], xc, yc)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[skypix], nsky, "plus")

	# Cleanup.
	call ap_gtfree (gt)
	call sfree (sp)

	return (OK)
end
