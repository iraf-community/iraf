include <gset.h>
include <pkg/gtools.h>
include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/photdef.h"
include "../lib/phot.h"
include "../lib/radprofdef.h"
include "../lib/radprof.h"

define	IMIN	-0.1		# Minimum intensity value for plot
define	IMAX	1.1		# Maximum intensity value for plot

# AP_RPPLOT -- Procedure to plot the radial profile.

procedure ap_rpplot (ap, sid, gd, makeplots)

pointer	ap		# pointer to the apphot structure
int	sid		# output file id number (not used)
pointer	gd		# pointer to the plot stream
int	makeplots	# make plots on the screen ?

int	nxpts, nypts, nrpts
pointer	rprof, gt
real	rmin, rmax, inorm, x1, x2, y1, y2, u1, u2, v1, v2
int	apstati()
pointer	ap_gtinit()
real	apstatr()

begin
	# Return if no graphics stream.
	if (gd == NULL || makeplots == NO)
	    return

	# Return if the center or sky is undefined.
	if (IS_INDEFR(apstatr (ap, XCENTER)) || IS_INDEFR(apstatr (ap,
	    YCENTER)) || IS_INDEFR (apstatr (ap, SKY_MODE)))
	    return

	# Return if there are no pixels.
	rprof = AP_RPROF(ap)
	if (AP_RPIX(rprof) == NULL)
	    return

	# Set up some useful constants.
	rmin = 0.0
	rmax = apstatr (ap, SCALE) * apstatr (ap, RPRADIUS)
	nxpts = AP_RPNX(rprof)
	nypts = AP_RPNY(rprof)
	nrpts = apstati (ap, RPNPTS)
	inorm = apstatr (ap, INORM)

	# Reopen the work station.
	call greactivate (gd, 0)

	# Save old viewport and coordinates.
	call ggwind (gd, x1, x2, y1, y2)
	call ggview (gd, u1, u2, v1, v2)

	# Set up the labels and annotate the plot.
	gt = ap_gtinit (AP_IMROOT(ap), apstatr (ap, OXINIT), apstatr (ap,
	    OYINIT))
	call gclear (gd)
	call ap_rpset (gd, gt, ap, rmin, rmax, IMIN, IMAX)
	call ap_rpannotate (gd, ap, rmin, rmax, IMIN, IMAX)

	# Plot the intensity and total intensity.
	call ap_plothist (gd, gt, Memr[AP_RPDIST(rprof)],
	    Memr[AP_INTENSITY(rprof)], nrpts, "line", GL_SOLID)
	call ap_plothist (gd, gt, Memr[AP_RPDIST(rprof)],
	    Memr[AP_TINTENSITY(rprof)], nrpts, "line", GL_DOTDASH)

	# Plot the points.
	call gswind (gd, rmin, rmax, (IMIN * inorm), (IMAX * inorm))
	call rp_ptsplot (gd, gt, Memr[AP_RPIX(rprof)], nxpts, nypts,
	    AP_RPXC(rprof), AP_RPYC(rprof))

	# Restore old viewport and world coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	call ap_gtfree (gt)
	call gdeactivate (gd, 0)
end


# AP_RPSET -- Procedure to set up the parameters for the radial profile
# plot.

procedure ap_rpset (gd, gt, ap, xmin, xmax, ymin, ymax)

pointer	gd		# graphics stream
pointer	gt		# gtools pointer
pointer	ap		# apphot pointer
real	xmin, xmax	# minimum and maximum radial distance
real	ymin, ymax	# minimum and maximum of the y axis

int	fd, naperts
pointer	sp, str, tstr, temp
real	scale, aspect, vx1, vx2, vy1, vy2
int	stropen(), apstati()
real	apstatr(), gstatr()

begin
	call smark (sp)
	call salloc (str, 6 * SZ_LINE, TY_CHAR)
	call salloc (tstr, SZ_LINE, TY_CHAR)
	naperts = apstati (ap, NAPERTS)
	call salloc (temp, naperts, TY_REAL)

	# Open the title string. 
	fd = stropen (Memc[str], 6 * SZ_LINE, WRITE_ONLY)

	# Encode the sysid.
	call sysid (Memc[tstr], SZ_LINE)
	call fprintf (fd, "%s\n")
	    call pargstr (Memc[tstr])

	# Encode the center string
	call fprintf (fd,
	    "Center: xc=%0.2f yc=%0.2f xerr=%0.2f yerr=%0.2f\n")
	    call pargr (apstatr (ap, OXCENTER))
	    call pargr (apstatr (ap, OYCENTER))
	    call pargr (apstatr (ap, XERR))
	    call pargr (apstatr (ap, YERR))

	# Encode the sky string
	call fprintf (fd,
	    "Sky: value=%0.2f sigma=%0.2f skew=%0.2f nsky=%d nreject=%d\n")
	    call pargr (apstatr (ap, SKY_MODE))
	    call pargr (apstatr (ap, SKY_SIGMA))
	    call pargr (apstatr (ap, SKY_SKEW))
	    call pargi (apstati (ap, NSKY))
	    call pargi (apstati (ap, NSKY_REJECT))

	# Encode the value of the magnitude at the maximum aperture.
	call fprintf (fd, "Photometry: fwhmpsf=%0.3f  maxapert=")
	    call pargr (apstatr (ap, RPFWHM))
	call ap_arrayr (ap, APERTS, Memr[temp])
	call amulkr (Memr[temp], apstatr (ap, SCALE), Memr[temp], naperts)
	call fprintf (fd, "%0.2f  mags=")
	    call pargr (Memr[temp+naperts-1])
	call ap_arrayr (ap, MAGS, Memr[temp])
	call fprintf (fd, "%0.3f\n")
	    call pargr (Memr[temp+naperts-1])

	# Encode the parameter string.
	call fprintf (fd,
	    "Fit: spline3 order=%d krej=%0.1f sigma np=%d nprej=%d\n")
	    call pargi (apstati (ap, RPORDER))
	    call pargr (apstatr (ap, RPKSIGMA))
	    call pargi (apstati (ap, RPNDATA))
	    call pargi (apstati (ap, RPNDATAREJ))

	# Encode the title.
	call gt_gets (gt, GTTITLE, Memc[tstr], SZ_LINE)
	call fprintf (fd, "%s\n\n")
	    call pargstr (Memc[tstr])

	call strclose (fd)

	aspect = gstatr (gd, G_ASPECT)
	scale = apstatr (ap, SCALE)
	call gsetr (gd, G_ASPECT, 0.70)
	call gseti (gd, G_WCS, 2)

	# Draw and label the axes.
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

	# Reset the axes parameters.
	call gseti (gd, G_YDRAWAXES, 3)
	call gseti (gd, G_XDRAWAXES, 3)
	call gsetr (gd, G_ASPECT, aspect)

	# Set the plot type.
	call gt_sets (gt, GTTYPE, "line")

	call sfree (sp)
end


# AP_RPANNOTATE -- Procedure to annotate the radial plot in radprof.

procedure ap_rpannotate (gd, ap, xmin, xmax, ymin, ymax)

pointer	gd		# graphics stream
pointer	ap		# apphot structure
real	xmin, xmax	# min and max of x axis
real	ymin, ymax	# min and max of y axis

int	naperts
pointer	sp, str, temp
real	scale, rpfwhm, annulus, dannulus, inorm, tinorm
int	apstati()
real	apstatr()

begin
	# Allocate working space.
	naperts = apstati (ap, NAPERTS)
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (temp, naperts, TY_REAL)
	call gseti (gd, G_PLTYPE, GL_DASHED)

	# Draw the zero level line.
	call gamove (gd, xmin, 0.0)
	call gadraw (gd, xmax, 0.0)

	# Draw the half power point.
	call gamove (gd, xmin, 0.5)
	call gadraw (gd, xmax, 0.5)

	# Draw the unit normalization value.
	call gamove (gd, xmin, 1.0)
	call gadraw (gd, xmax, 1.0)

	# Plot the full width half maximum of the radial profile.
	scale = apstatr (ap, SCALE)
	rpfwhm = apstatr (ap, RPFWHM) / 2.0
	if (rpfwhm >= xmin && rpfwhm <= xmax) {
	    call gamove (gd, rpfwhm, ymin)
	    call gadraw (gd, rpfwhm, ymax)
	    call sprintf (Memc[str], SZ_LINE, "hwhm = %0.2f")
	        call pargr (rpfwhm)
	    call gtext (gd, rpfwhm, 0.0, Memc[str], "q=h;u=180;p=r")
	}

	# Mark the sky annuli.
	annulus = scale * apstatr (ap, ANNULUS)
	dannulus = scale * (apstatr (ap, ANNULUS) + apstatr (ap, DANNULUS))
	if (annulus >= xmin && annulus <= xmax) {
	    call gamove (gd, annulus, ymin)
	    call gadraw (gd, annulus, ymax)
	    call sprintf (Memc[str], SZ_LINE, "inner sky radius = %0.2f")
	        call pargr (annulus)
	    call gtext (gd, annulus, 0.0, Memc[str], "q=h;u=180;p=r")
	}
	if (dannulus >= xmin && dannulus <= xmax) {
	    call gamove (gd, dannulus, ymin)
	    call gadraw (gd, dannulus, ymax)
	    call sprintf (Memc[str], SZ_LINE, "outer sky radius = %0.2f")
	        call pargr (dannulus)
	    call gtext (gd, dannulus, 0.0, Memc[str], "q=h;u=180;p=r")
	}

	# Plot the aperture value.
	call ap_arrayr (ap, APERTS, Memr[temp])
	call amulkr (Memr[temp], scale, Memr[temp], naperts)
	call gseti (gd, G_PLTYPE, GL_SOLID)
	if (Memr[temp+naperts-1] >= xmin && Memr[temp+naperts-1] <= xmax) {
    	    call gamove (gd, Memr[temp+naperts-1], ymin)
	    call gadraw (gd, Memr[temp+naperts-1], ymax)
	    call sprintf (Memc[str], SZ_LINE, "maxapert = %0.2f")
	        call pargr (Memr[temp+naperts-1])
	    call gtext (gd, Memr[temp+naperts-1], 0.0, Memc[str],
	        "q=h;u=180;p=r")
	}
	call gseti (gd, G_PLTYPE, GL_DASHED)

	# Plot the inorm value.
	inorm = apstatr (ap, INORM)
	call sprintf (Memc[str], SZ_LINE, "inorm = %0.2f")
	    call pargr (inorm)
	call gtext (gd, 0.0, 1.0, Memc[str], "q=h")

	# Plot the tinorm value.
	tinorm = apstatr (ap, TNORM)
	call sprintf (Memc[str], SZ_LINE, "tinorm = %0.2f")
	    call pargr (tinorm)
	call gtext (gd, xmax, 1.0, Memc[str], "q=h;h=r")

	call sfree (sp)
end


# RP_PTSPLOT -- Plot the radial profile plots.

procedure rp_ptsplot (gd, gt, pixels, nx, ny, wx, wy)

pointer	gd		# pointer to the graphics stream
pointer	gt		# pointer to the gtools structure
real	pixels[nx,ARB]	# subraster of pixel values
int	nx, ny		# dimensions of the pixel subraster
real	wx, wy		# x and y coordinates of the center

int	i
pointer	sp, rtemp

begin
	call smark (sp)
	call salloc (rtemp, nx, TY_REAL)
	do i = 1, ny {
	    call ap_ijtor (Memr[rtemp], nx, i, wx, wy)
	    call ap_plotrad (gd, gt, Memr[rtemp], pixels[1,i], nx, "plus")
	}
	call sfree (sp)
end
