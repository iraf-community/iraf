include <pkg/gtools.h>
include <gset.h>
include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/fitskydef.h"
include "../lib/fitsky.h"

# AP_SPLOT -- Procedure to compute radial profile plots for the sky fitting
# routine.

procedure ap_splot (ap, sid, gd, makeplot)

pointer	ap		# pointer to the apphot structure
int	sid		# id number of the star
pointer	gd		# graphics stream
int	makeplot	# make a plot

int	nx, ny, nskypix
pointer	sp, sky, str, r, gt
real	xcenter, ycenter, rmin, rmax, imin, imax
real	u1, u2, v1, v2, x1, x2, y1, y2
int	apstati()
pointer	ap_gtinit()
real	apstatr()

begin
	# Initialize.
	if (gd == NULL || makeplot == NO)
	    return

	# Check for defined center and non-constant algorithm.
	xcenter = apstatr (ap, SXCUR)
	ycenter = apstatr (ap, SYCUR)
	if (IS_INDEFR(xcenter) || IS_INDEFR(ycenter))
	    return
	if (apstati (ap, SKYFUNCTION) == AP_CONSTANT)
	    return

	# Check that a buffer of sky pixels exists.
	sky = AP_PSKY(ap)
	nskypix = AP_NSKYPIX(sky)
	nx = AP_SNX(sky)
	ny = AP_SNY(sky)
	if (nskypix <= 0 || nx <= 0 || ny <= 0)
	    return

	# Allocate working space
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (r, nskypix, TY_REAL)

	# Compute the radii and the plot limits.
	call ap_xytor (Memi[AP_COORDS(sky)], Memi[AP_INDEX(sky)],
	    Memr[r], nskypix, AP_SXC(sky), AP_SYC(sky), nx)
	call alimr (Memr[r], nskypix, rmin, rmax)
	rmin = rmin - 1.0
	rmax = rmax + 1.0
	call alimr (Memr[AP_SKYPIX(sky)], nskypix, imin, imax)

	# Reactivate the work station.
	call greactivate (gd, 0)

	# Store the viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Initialize the plot.
	call apstats (ap, IMROOT, Memc[str], SZ_LINE)
	call sprintf (Memc[str], SZ_LINE, "%s  Star %d")
	    call pargstr (Memc[str])
	    call pargi (sid)
	gt = ap_gtinit (Memc[str], apstatr (ap,OSXCUR), apstatr(ap,OSYCUR))
	
	# Draw the plot.
	call gclear (gd)
	call ap_spset (gd, gt, ap, rmin, rmax, imin, imax)
	call ap_spannotate (gd, ap, rmin, rmax, imin, imax)
	call ap_plotrad (gd, gt, Memr[r], Memr[AP_SKYPIX(sky)], nskypix, "plus")

	# Restore the viewport and window coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	# Free the space.
	call ap_gtfree (gt)
	call gdeactivate (gd, 0)
	call sfree (sp)
end


# AP_SPSET -- Procedure to set up the parameters for the fitsky radial profile
# plot.

procedure ap_spset (gd, gt, ap, xmin, xmax, ymin, ymax)

pointer	gd		# graphics stream
pointer	gt		# gtools pointer
pointer	ap		# apphot pointer
real	xmin, xmax	# minimum and maximum radial distance
real	ymin, ymax	# minimum and maximum of the y axis

int	fd
pointer	sp, str, title
real	aspect, scale, vx1, vx2, vy1, vy2
int	apstati(), stropen()
real	apstatr(), gstatr()

begin
	call smark (sp)
	call salloc (str, 3 * SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)

	# Encode the parameter string.
	fd = stropen (Memc[str], 3 * SZ_LINE, WRITE_ONLY)

	call sysid (Memc[title], SZ_FNAME)
	call fprintf (fd, "%s\n")
	    call pargstr (Memc[title])

	call fprintf (fd,
	    "Sky: value=%0.2f sigma=%0.2f skew=%0.2f nsky=%d nreject=%d\n")
	    call pargr (apstatr (ap, SKY_MODE))
	    call pargr (apstatr (ap, SKY_SIGMA))
	    call pargr (apstatr (ap, SKY_SKEW))
	    call pargi (apstati (ap, NSKY))
	    call pargi (apstati (ap, NSKY_REJECT))

	call gt_gets (gt, GTTITLE, Memc[title], SZ_LINE)
	call fprintf (fd, "%s\n")
	    call pargstr (Memc[title])

	call strclose (fd)

	# Set the aspect ratio.
	scale = apstatr (ap, SCALE)
	aspect = gstatr (gd, G_ASPECT)
	call gsetr (gd, G_ASPECT, 0.75)

	# Set the labels and window.
	call gseti (gd, G_XDRAWAXES, 2)
	call gswind (gd, xmin / scale, xmax / scale, ymin, ymax)
	call glabax (gd, Memc[str], "", "Intensity")
	call gseti (gd, G_YDRAWAXES, 0)
	call gseti (gd, G_XDRAWAXES, 1)
	call ggview (gd, vx1, vx2, vy1, vy2)
	call gsview (gd, vx1, vx2, vy1, vy2)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call glabax (gd, "",
	    "Radial Distance (lower-pixels, upper-scale units)", "")

	call gseti (gd, G_YDRAWAXES, 3)
	call gseti (gd, G_XDRAWAXES, 3)
	call gsetr (gd, G_ASPECT, aspect)
	call gt_sets (gt, GTTYPE, "mark")

	call sfree (sp)
end


# AP_SPANNOTATE -- Procedure to annotate the radial plot in fitsky.

procedure ap_spannotate (gd, ap, xmin, xmax, ymin, ymax)

pointer	gd		# graphics stream
pointer	ap		# apphot structure
real	xmin, xmax	# min and max of x axis
real	ymin, ymax	# min and max of y axis

pointer	sp, str
real	annulus, dannulus, sigma, skyval, skysigma
real	apstatr ()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call gseti (gd, G_PLTYPE, GL_DASHED)

	# Mark the inner sky annulus.
	annulus = apstatr (ap, SCALE) * apstatr (ap, ANNULUS)
	if (annulus >= xmin && annulus <= xmax) {
	    call gamove (gd, annulus, ymin)
	    call gadraw (gd, annulus, ymax)
	    call sprintf (Memc[str], SZ_LINE, "inner sky radius = %0.2f")
	        call pargr (annulus)
	    call gtext (gd, annulus, ymax, Memc[str], "q=h;u=180;v=t;p=r")
	}
 
	# Mark the outer sky annulus.
	dannulus = annulus + apstatr (ap, SCALE) * apstatr (ap, DANNULUS)
	if (dannulus >= xmin && dannulus <= xmax) {
	    call gamove (gd, dannulus, ymin)
	    call gadraw (gd, dannulus, ymax)
	    call sprintf (Memc[str], SZ_LINE, "outer sky radius = %0.2f")
	        call pargr (dannulus)
	    call gtext (gd, dannulus, ymax, Memc[str], "q=h;u=180;v=t;p=r")
	}

	# Mark the sky sigma if defined.
	sigma = apstatr (ap, SKY_SIGMA)
	if (! IS_INDEFR(sigma)) {
	    call gmark (gd, (xmin + xmax) / 2.0, (ymin + ymax) / 2.0, 
		GM_VEBAR, -0.25, -sigma)
	    call sprintf (Memc[str], SZ_LINE, "sigma = %g")
		call pargr (sigma)
	    call gtext (gd, (xmin + xmax) / 2.0, (ymin + ymax + sigma) / 2.0,
		Memc[str], "q=h;h=c")
	}

	# Mark the sky value.
	call gseti (gd, G_PLTYPE, GL_SOLID)
	skyval = apstatr (ap, SKY_MODE)
	if (skyval >= ymin && skyval <= ymax) {
	    call gamove (gd, xmin, skyval)
	    call gadraw (gd, xmax, skyval)
	}

	# Mark the upper sky sigma.
	call gseti (gd, G_PLTYPE, GL_DASHED)
	if (! IS_INDEFR(apstatr (ap, SKY_SIGMA)))
	    skysigma = skyval + apstatr (ap, SHIREJECT) * apstatr (ap,
	        SKY_SIGMA)
	else
	    skysigma = INDEFR
	if (! IS_INDEFR(skysigma) && (skysigma >= ymin) && skysigma <= ymax) {
	    call gamove (gd, xmin, skysigma)
	    call gadraw (gd, xmax, skysigma)
	    #call sprintf (Memc[str], SZ_LINE, "sky sigma= %g")
	        #call pargr (skysigma)
	}

	# Mark the lower sky sigma
	if (! IS_INDEFR(apstatr (ap, SKY_SIGMA)))
	    skysigma = skyval - apstatr (ap, SLOREJECT) * apstatr (ap,
	        SKY_SIGMA)
	else
	    skysigma = INDEFR
	if (! IS_INDEFR(skysigma) && (skysigma >= ymin) && skysigma <= ymax) {
	    call gamove (gd, xmin, skysigma)
	    call gadraw (gd, xmax, skysigma)
	}

	call sfree (sp)
end
