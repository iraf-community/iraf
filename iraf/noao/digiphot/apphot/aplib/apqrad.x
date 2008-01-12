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

real	gwx, gwy, xcenter, ycenter, xc, yc, radius, rmin, rmax, imin, imax
real	u1, u2, v1, v2, x1, x2, y1, y2, xold, yold
pointer	gt, sp, pix, coords, index, r, cmd
int	maxpix, npix, nx, ny, wcs, key, niter

real	apstatr()
pointer	ap_gtinit()
int	ap_skypix(), clgcur(), apstati()
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
	maxpix = (2 * int (radius) + 1) ** 2
	    
	# Allocate temporary space.
	call smark (sp)
	call salloc (coords, maxpix, TY_INT)
	call salloc (index, maxpix, TY_INT)
	call salloc (pix, maxpix, TY_REAL)
	call salloc (r, maxpix, TY_REAL)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Fit the center using 3 iterations.
	xold = wx
	yold = wy
	niter = 0
	repeat {
	    call ap_ictr (im, xold, yold, CRADIUS, apstati (ap,
	        POSITIVE), xcenter, ycenter)
	    niter = niter + 1
	    if (abs (xcenter - xold) <= 1.0 && abs (ycenter - yold) <= 1.0)
		break
	    xold = xcenter
	    yold = ycenter
	} until (niter >= 3)

	# Fetch the pixels for the radial profile.
	npix = ap_skypix (im, xcenter, ycenter, 0.0, radius, Memr[pix],
	    Memi[coords], xc, yc, nx, ny)
	if (npix <= 0) {
	    call gdeactivate (gd, 0)
	    call sfree (sp)
	    return
	}
	call ap_index (Memi[index], npix)


	# Store old viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Initialize the plot and store the viewport and window limits.
	#call apstats (ap, IMNAME, Memc[cmd], SZ_FNAME)
	call apstats (ap, IMROOT, Memc[cmd], SZ_FNAME)
	call ap_ltov (im, xcenter, ycenter, xcenter, ycenter, 1)
	gt = ap_gtinit (Memc[cmd], xcenter, ycenter)

	# Compute the radius values.
	call ap_xytor (Memi[coords], Memi[index], Memr[r], npix, xc, yc, nx)
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
