include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/fitskydef.h"
include "../lib/fitsky.h"

define	CRADIUS	3
define	RADIUS	15.0

# AP_SRADSETUP -- Procedure to set up the sky fitting interactively using
# radial profile plot around the given coordinates.

procedure ap_sradsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointero to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	ier, nsky
pointer	gt, sp, r, sky, str
real	xcenter, ycenter, radius, rmin, rmax, imin, imax
real	annulus, dannulus, sigma3
real	u1, u2, v1, v2, x1, x2, y1, y2

int	apstati(), apskybuf(), apfitsky(), nscan(), scan()
pointer	ap_gtinit()
real	apstatr(), ap_cannulus(), ap_cdannulus(), ap_csigma()

begin
	if (gd == NULL)
	    return
	call greactivate (gd, 0)

	# Get a rough value for the center.
	call ap_ictr (im, wx, wy, CRADIUS, apstati (ap, POSITIVE), xcenter,
	    ycenter)

	# Save the old sky annulus parameters.
	annulus = apstatr (ap, ANNULUS)
	dannulus = apstatr (ap, DANNULUS)

	# Set the new sky extraction parameters.
	call apsetr (ap, ANNULUS, 0.0)
	call printf ("Radius of extraction box (%4.1f) pixels:")
	    call pargr (RADIUS)
	call flush (STDOUT)
	if (scan () == EOF)
	    call apsetr (ap, DANNULUS, RADIUS / apstatr (ap, SCALE))
	else {
	    call gargr (radius)
	    if (nscan () < 1)
	        call apsetr (ap, DANNULUS, RADIUS / apstatr (ap, SCALE))
	    else
	        call apsetr (ap, DANNULUS, radius / apstatr (ap, SCALE))
	}
	    
	# Fetch the sky pixels. Reset the original sky annuli.
	ier = apskybuf (ap, im, xcenter, ycenter)
	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	if (ier != AP_OK) {
	    call gdeactivate (gd, 0)
	    return
	}

	# Initialize.
	sky = AP_PSKY(ap)
	nsky = AP_NSKYPIX(sky)
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (r, nsky, TY_REAL)

	# Store the old window and viewport coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Compute the radial profile.
	call ap_xytor (Memi[AP_COORDS(sky)], Memr[r], nsky, AP_SXC(sky),
	    AP_SYC(sky), AP_SNX(sky))
	call alimr (Memr[r], nsky, rmin, rmax)
	call alimr (Memr[AP_SKYPIX(sky)], nsky, imin, imax)

	# Initialize the radial profile plot.
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	gt = ap_gtinit (Memc[str], xcenter, ycenter)

	# Make the radial profile plot.
	call gclear (gd)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[AP_SKYPIX(sky)], nsky, "plus")

	# Mark the inner sky radius.
	annulus = ap_cannulus (ap, gd, rmin, rmax, imin, imax)

	# Mark the outer sky radius.
	dannulus = ap_cdannulus (ap, gd, annulus, rmin, rmax, imin, imax)

	# Estimate the sky mean.
	sigma3 = ap_csigma (ap, gd, rmin, rmax, imin, imax)

	# interactive setup is complete.
	call printf (
	    "Interactive setup is complete. Type w to store parameters.\n")

	# Update the important parameters.
	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	if (! IS_INDEFR(sigma3))
	    sigma3 = sigma3 / 3.0
	call apsetr (ap, SKYSIGMA, sigma3)

	# Update the date base file.
	if (out != NULL && stid > 1) {
	    call ap_rparam (out, KY_ANNULUS, apstatr (ap, ANNULUS),
		UN_ANNULUS, "inner radius of sky annulus")
	    call ap_rparam (out, KY_DANNULUS, apstatr (ap, DANNULUS),
		UN_DANNULUS, "width of the sky annulus")
	    call ap_rparam (out, KY_SKYSIGMA, apstatr (ap, SKYSIGMA),
		UN_SKYSIGMA, "standard deviation of 1 sky pixel") 
	}

	# Store the old window and viewport coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	# Free the plotting space.
	call ap_gtfree (gt)
	call sfree (sp)
	call gdeactivate (gd, 0)

	# Fit the new sky value and print it on the standard output.
	ier = apfitsky (ap, im, xcenter, ycenter, NULL, gd)
	call apsplot (ap, 0, ier, gd, apstati (ap, RADPLOTS))
	call ap_qspsky (ap, ier)
end
