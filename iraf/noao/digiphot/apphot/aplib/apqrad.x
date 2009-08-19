include <mach.h>
include "../lib/apphot.h"
include "../lib/center.h"

define	RADIUS	15.0
define	CRADIUS	5

# AP_QRAD -- Simple radial profile plotter.

procedure ap_qrad (ap, im, wx, wy, gd)

pointer	ap			# pointer to apphot structure
pointer	im			# pointero to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream

size_t	sz_val
real	gwx, gwy, xcenter, ycenter, xc, yc, radius, rmin, rmax, imin, imax
real	u1, u2, v1, v2, x1, x2, y1, y2, xold, yold
pointer	gt, sp, pix, coords, index, r, cmd
int	wcs, key, niter
long	l_val
size_t	maxpix, npix, nx, ny

real	apstatr(), aabs()
pointer	ap_gtinit()
int	clgcur(), apstati()
long	ap_skypix(), lint()
int	nscan(), scan()

begin
	# Check for open graphics stream.
	if (gd == NULL)
	    return
	call greactivate (gd, 0)
	call gclear (gd)
	call gflush (gd)

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
	maxpix = (2 * lint (radius) + 1) ** 2
	    
	# Allocate temporary space.
	call smark (sp)
	call salloc (coords, maxpix, TY_LONG)
	call salloc (index, maxpix, TY_LONG)
	call salloc (pix, maxpix, TY_REAL)
	call salloc (r, maxpix, TY_REAL)
	sz_val = SZ_LINE
	call salloc (cmd, sz_val, TY_CHAR)

	# Fit the center using 3 iterations.
	xold = wx
	yold = wy
	niter = 0
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

	# Fetch the pixels for the radial profile.
	l_val = ap_skypix (im, xcenter, ycenter, 0.0, radius, Memr[pix],
			   Meml[coords], xc, yc, nx, ny)
	if (l_val <= 0) {
	    call gdeactivate (gd, 0)
	    call sfree (sp)
	    return
	}
	npix = l_val
	call ap_index (Meml[index], npix)


	# Store old viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Initialize the plot and store the viewport and window limits.
	#call apstats (ap, IMNAME, Memc[cmd], SZ_FNAME)
	call apstats (ap, IMROOT, Memc[cmd], SZ_FNAME)
	sz_val = 1
	call ap_ltov (im, xcenter, ycenter, xcenter, ycenter, sz_val)
	gt = ap_gtinit (Memc[cmd], xcenter, ycenter)

	# Compute the radius values.
	call ap_xytor (Meml[coords], Meml[index], Memr[r], npix, xc, yc, nx)
	call alimr (Memr[r], npix, rmin, rmax)
	call alimr (Memr[pix], npix, imin, imax)

	# Plot radial profiles.
	call gclear (gd)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[pix], npix, "plus")

	# Go into cursor mode.
	call printf ("Waiting for cursor mode command: [:.help=help,q=quit]")
	while (clgcur ("gcommands", gwx, gwy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {
	    if (key == 'q')
		break
	    call printf (
	        "Waiting for cursor mode command: [:.help=help,q=quit]")
	}

	# Restore old window and viewport coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	# Free plots and working space.
	call ap_gtfree (gt)
	call sfree (sp)
	call gdeactivate (gd, 0)
end
