include "../lib/fitskydef.h"
include "../lib/apphotdef.h"
include "../lib/photdef.h"
include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/phot.h"
include "../lib/radprof.h"

define	CRADIUS	3
define	RADIUS	15.0

# AP_PROFSETUP -- Procedure to set up radprof interactively using a radial
# profile plot.

procedure ap_profsetup (ap, im, wx, wy, gd, out, stid)

pointer	ap			# pointer to apphot structure
pointer	im			# pointer to the IRAF image
real	wx, wy			# cursor coordinates
pointer	gd			# pointer to graphics stream
int	out			# output file descriptor
int	stid			# output file sequence number

int	ier, nsky, cier, sier, pier, rier
pointer	gt, sp, r, sky, phot, str
real	fwhmpsf, capert, annulus, dannulus, threshold, sigma3
real	xcenter, ycenter, radius, step, rmin, rmax, imin, imax
real	u1, u2, v1, v2, x1, x2, y1, y2

int	apskybuf(), nscan(), scan(), apfitcenter()
int	apfitsky(), apfrprof(), apstati()
pointer	ap_gtinit()
real	apstatr(), ap_cfwhmpsf(), ap_ccapert(), ap_cannulus(), ap_cdannulus()
real	ap_csigma(), ap_ccthresh(), ap_crprof(), ap_crpstep()

begin
	if (gd == NULL)
	    return
	call greactivate (gd, 0)

	# Estimate a rough center.
	call ap_ictr (im, wx, wy, CRADIUS, apstati (ap, POSITIVE), xcenter,
	    ycenter)

	# Save old sky annulus parameters and set new sky extraction parameters.
	annulus = apstatr (ap, ANNULUS)
	dannulus = apstatr (ap, DANNULUS)
	call apsetr (ap, ANNULUS, 0.0)
	call printf ("Half width of extraction box (%4.1f) pixels:")
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
	    
	# Fetch the sky pixels and restore the original sky annuli.
	ier = apskybuf (ap, im, xcenter, ycenter)
	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	if (ier != AP_OK) {
	    call gdeactivate (gd, 0)
	    return
	}

	# Allocate temporary space for the radial profile.
	phot = AP_PPHOT(ap)
	sky = AP_PSKY(ap)
	nsky = AP_NSKYPIX(sky)
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (r, nsky, TY_REAL)

	# Initialize the plot and compute the radius values.
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	gt = ap_gtinit (Memc[str], xcenter, ycenter)
	call ap_xytor (Memi[AP_COORDS(sky)], Memr[r], nsky, AP_SXC(sky),
	    AP_SYC(sky), AP_SNX(sky))
	call alimr (Memr[r], nsky, rmin, rmax)
	call alimr (Memr[AP_SKYPIX(sky)], nsky, imin, imax)

	# Save old viewport and window
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Plot the radial profiles.
	call gclear (gd)
	call ap_rset (gd, gt, 0.0, rmax, imin, imax, apstatr (ap, SCALE))
	call ap_plotrad (gd, gt, Memr[r], Memr[AP_SKYPIX(sky)], nsky, "plus")

	# Mark the FWHM of the PSF on the plot.
	fwhmpsf = ap_cfwhmpsf (ap, gd, rmin, rmax, imin, imax)

	# Mark the centering aperture on the plot.
	capert = ap_ccapert (ap, gd, rmin, imin, imax)

	# Mark the inner sky radius.
	annulus = ap_cannulus (ap, gd, rmin, rmax, imin, imax)

	# Mark the outer sky radius.
	dannulus = ap_cdannulus (ap, gd, annulus, rmin, rmax, imin,imax)

	# Estimate the sky sigma.
	sigma3 = ap_csigma (ap, gd, rmin, rmax, imin, imax)

	# Estimate the minimum (maximum) data level.
	threshold = ap_ccthresh (ap, gd, rmin, rmax, imin, imax)

	# Get the apertures.
	call ap_caper (ap, gd, Memc[str], rmin, rmax, imin, imax)

	# Mark maximum radius of the radial profile.
	radius = ap_crprof (ap, gd, rmin, rmax, imin, imax)

	# Mark the radial profile step size.
	step = ap_crpstep (ap, gd, rmin, rmax, imin, imax)

	# Interactive setup is complete.
	call printf ("Interactive setup is complete\n")

	# Update the important parameters.
	call apsetr (ap, FWHMPSF, fwhmpsf)
	call apsetr (ap, CAPERT, capert)
	call apsetr (ap, ANNULUS, annulus)
	call apsetr (ap, DANNULUS, dannulus)
	call apsetr (ap, CTHRESHOLD, threshold)
	if (! IS_INDEFR(sigma3))
	    sigma3 = sigma3 / 3.0
	call apsetr (ap, SKYSIGMA, sigma3)
	if (Memc[str] != EOS)
	    call apsets (ap, APERTS, Memc[str])
	call apsetr (ap, RPRADIUS, radius)
	call apsetr (ap, RPSTEP, step)

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF),
		UN_FWHMPSF, "full width half maximum of the psf")
	    call ap_rparam (out, KY_CAPERT, 2.0 * apstatr (ap, CAPERT),
		UN_CAPERT, "centering box width")
	    call ap_rparam (out, KY_ANNULUS, apstatr (ap, ANNULUS),
		UN_ANNULUS, "inner radius of the sky annulus")
	    call ap_rparam (out, KY_DANNULUS, apstatr (ap, DANNULUS),
		UN_DANNULUS, "width of the sky annulus")
	    call ap_rparam (out, KY_CTHRESHOLD, apstatr (ap, CTHRESHOLD),
		UN_CTHRESHOLD, "centering threshold")
	    call ap_rparam (out, KY_SKYSIGMA, apstatr (ap, SKYSIGMA),
		UN_SKYSIGMA, "standard deviation of 1 sky pixel")
	    call apstats (ap, APERTS, Memc[str], SZ_LINE)
	    call ap_sparam (out, KY_APERTS, Memc[str], UN_APERTS,
		"list of apertures")
	    call ap_rparam (out, KY_RPRADIUS, apstatr (ap, RPRADIUS),
		UN_RPRADIUS, "fitting radius")
	    call ap_rparam (out, KY_RPSTEP, apstatr (ap, RPSTEP),
		UN_RPSTEP, "step size in radius")
	}

	# Restore old view port and window
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)
	call ap_gtfree (gt)
	call sfree (sp)

	# Compute the answer.
	cier = apfitcenter (ap, im, xcenter, ycenter)
	sier = apfitsky (ap, im, apstatr (ap, XCENTER), apstatr (ap, YCENTER),
	    NULL, gd)
	rier = apfrprof (ap, im, apstatr (ap, XCENTER), apstatr (ap, YCENTER),
	    apstatr (ap, SKY_MODE), apstatr (ap, SKY_SIGMA), apstati (ap,
	    NSKY), pier)
	call ap_rpplot (ap, 0, cier, sier, pier, rier, gd, apstati (ap,
	    RADPLOTS))
	call ap_qprprof (ap, cier, sier, pier, rier)
end
