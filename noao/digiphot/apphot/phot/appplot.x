include <pkg/gtools.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_PPLOT -- Procedure to compute radial profile plots for the centering
# routine.

procedure ap_pplot (ap, im, sid, gd, makeplot)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the iraf image
int	sid		# id number of the star
pointer	gd		# graphics stream
int	makeplot	# make a plot ?

int	apert, nx, ny
pointer	buf, sp, str, r, gt
real	xcenter, ycenter, xc, yc, rmin, rmax, imin, imax
real	u1, u2, v1, v2, x1, x2, y1, y2
int	ap_ctrpix()
pointer	ap_gtinit()
real	apstatr()

begin
	# Initialize
	if (gd == NULL || makeplot == NO)
	    return

	# Check for defined center and get the pixels.
	xcenter = apstatr (ap, XCENTER)
	ycenter = apstatr (ap, YCENTER)
	if (IS_INDEFR(xcenter) || IS_INDEFR(ycenter))
	    return

	# Fetch the pixels.
	apert = 2 * int (apstatr (ap, SCALE) * (apstatr (ap, ANNULUS) +
	    apstatr (ap, DANNULUS))) + 1
	buf = ap_ctrpix (im, xcenter, ycenter, apert, xc, yc, nx, ny)
	if (buf == NULL)
	    return

	# Reactivate the work station.
	call greactivate (gd, 0)

	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (r, nx * ny, TY_REAL)

	# Compute the radii and the plot limits.
	call ap_ijtor2 (Memr[r], nx, ny, xc, yc)
	call alimr (Memr[r], nx * ny, rmin, rmax)
	call alimr (Memr[buf], nx * ny, imin, imax)

	# Store the viewport and window coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Initialize the plot.
	call apstats (ap, IMROOT, Memc[str], SZ_LINE)
	call sprintf (Memc[str], SZ_LINE, "%s  Star %d")
	    call pargstr (Memc[str])
	    call pargi (sid)
	gt = ap_gtinit (Memc[str], apstatr (ap, OXINIT), apstatr (ap, OYINIT))
	call gclear (gd)

	# Label and annotate the plot.
	call ap_ppset (gd, gt, ap, rmin, rmax, imin, imax)
	call ap_ppannotate (gd, ap, rmin, rmax, imin, imax)

	# Plot the coordinates.
	call ap_plotrad (gd, gt, Memr[r], Memr[buf], nx * ny, "plus")

	# Store the viewport and window coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	call ap_gtfree (gt)
	call gdeactivate (gd, 0)
	call sfree (sp)
end


# AP_PPSET -- Procedure to set up the parameters for the phot radial profile
# plot.

procedure ap_ppset (gd, gt, ap, xmin, xmax, ymin, ymax)

pointer	gd		# the graphics stream
pointer	gt		# the gtools pointer
pointer	ap		# the apphot pointer
real	xmin, xmax	# the minimum and maximum radial distance
real	ymin, ymax	# the minimum and maximum of the y axes

int	fd, naperts
pointer	sp, str, title, temp
real	aspect, scale, vx1, vx2, vy1, vy2
int	stropen(), apstati()
real	apstatr(), gstatr()

begin
	call smark (sp)
	call salloc (str, 5 * SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	naperts = apstati (ap, NAPERTS)
	call salloc (temp, naperts, TY_REAL)

	fd = stropen (Memc[str], 5 * SZ_LINE, WRITE_ONLY)
	call sysid (Memc[title], SZ_LINE)
	call fprintf (fd, "%s\n")
	    call pargstr (Memc[title])

	# Encode the center parameter string.
	call fprintf (fd,
	    "Center: xc=%0.2f yc=%0.2f xerr=%0.2f yerr=%0.2f\n")
	    call pargr (apstatr (ap, OXCENTER))
	    call pargr (apstatr (ap, OYCENTER))
	    call pargr (apstatr (ap, XERR))
	    call pargr (apstatr (ap, YERR))

	# Encode the sky fitting parameter string
	call fprintf (fd,
	    "Sky: value=%0.2f sigma=%0.2f skew=%0.2f nsky=%d nrej=%d\n")
	    call pargr (apstatr (ap, SKY_MODE))
	    call pargr (apstatr (ap, SKY_SIGMA))
	    call pargr (apstatr (ap, SKY_SKEW))
	    call pargi (apstati (ap, NSKY))
	    call pargi (apstati (ap, NSKY_REJECT))

	# Encode the apertures and magnitudes.
	call ap_arrayr (ap, APERTS, Memr[temp])
	call amulkr (Memr[temp], apstatr (ap, SCALE), Memr[temp], naperts)
	call fprintf (fd, "Photometry: maxapert=")
	call fprintf (fd, "%0.2f  mag=")
	    call pargr (Memr[temp+naperts-1])
	call ap_arrayr (ap, MAGS, Memr[temp])
	call fprintf (fd, "%0.3f  merr=")
	    call pargr (Memr[temp+naperts-1])
	call ap_arrayr (ap, MAGERRS, Memr[temp])
	call fprintf (fd, "%0.3f\n")
	    call pargr (Memr[temp+naperts-1])

	# Encode the title.
	call gt_gets (gt, GTTITLE, Memc[title], SZ_LINE)
	call fprintf (fd, "%s\n")
	    call pargstr (Memc[title])

	call strclose (fd)

	aspect = gstatr (gd, G_ASPECT)
	call gsetr (gd, G_ASPECT, 0.70)
	scale = apstatr (ap, SCALE)

	# Draw three axes.
	call gseti (gd, G_XDRAWAXES, 2)
	call gswind (gd, xmin / scale, xmax / scale, ymin, ymax)
	call glabax (gd, Memc[str], "", "Intensity")

	# Draw the bottom x axis.
	call gseti (gd, G_YDRAWAXES, 0)
	call gseti (gd, G_XDRAWAXES, 1)
	call ggview (gd, vx1, vx2, vy1, vy2)
	call gsview (gd, vx1, vx2, vy1, vy2)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call glabax (gd,
	    "","Radial Distance (lower-pixels, upper-scale units)", "")

	# Restore the default draw axis parameters.
	call gseti (gd, G_YDRAWAXES, 3)
	call gseti (gd, G_XDRAWAXES, 3)
	call gsetr (gd, G_ASPECT, aspect)

	# Set the mark type.
	call gt_sets (gt, GTTYPE, "mark")

	call sfree (sp)
end


# AP_PPANNOTATE -- Procedure to annotate the radial plot in phot.

procedure ap_ppannotate (gd, ap, xmin, xmax, ymin, ymax)

pointer	gd		# graphics stream
pointer	ap		# apphot structure
real	xmin, xmax	# minimum and maximum of the x axis
real	ymin, ymax	# minimum and maximum of the y axis

int	i, naperts
pointer	sp, str, temp
real	annulus, dannulus, skyval, skysigma
int	apstati()
real	apstatr ()

begin
	naperts = apstati (ap, NAPERTS)
	call smark (sp)
	call salloc (str, SZ_LINE + 1, TY_CHAR)
	call salloc (temp, naperts, TY_REAL)

	# Define some temporary variables
	annulus = apstatr (ap, SCALE) * apstatr (ap, ANNULUS)
	dannulus = annulus + apstatr (ap, SCALE) * apstatr (ap, DANNULUS)

	# Mark the inner sky annulus.
	if (annulus >= xmin && annulus <= xmax) {
	    call gamove (gd, annulus, ymin)
	    call gadraw (gd, annulus, ymax)
	    call sprintf (Memc[str], SZ_LINE, "inner sky radius = %0.2f")
		call pargr (annulus)
	    call gtext (gd, annulus, ymax, Memc[str], "q=h;u=180;v=t;p=r")
	}
     
	# Mark the outer sky annulus.
	if (dannulus >= xmin && dannulus <= xmax) {
	    call gamove (gd, dannulus, ymin)
	    call gadraw (gd, dannulus, ymax)
	    call sprintf (Memc[str], SZ_LINE, "outer sky radius = %0.2f")
	        call pargr (dannulus)
	    call gtext (gd, dannulus, ymax, Memc[str], "q=h;u=180;v=t;p=r")
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

	# Mark the appertures.
	call gseti (gd, G_PLTYPE, GL_SOLID)
	call ap_arrayr (ap, APERTS, Memr[temp])
	call amulkr (Memr[temp], apstatr (ap, SCALE), Memr[temp], naperts)
	do i = 1, naperts {
	    call gamove (gd, Memr[temp+i-1], ymin)
	    call gadraw (gd, Memr[temp+i-1], ymax)
	    call sprintf (Memc[str], SZ_LINE, "apert[%d] = %0.2f")
		call pargi (i)
		call pargr (Memr[temp+i-1])
	    call gtext (gd, Memr[temp+i-1], ymax, Memc[str],
	        "q=h;u=180;v=t;p=r")
	}

	call sfree (sp)
end
