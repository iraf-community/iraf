include <gset.h>
include <math/curfit.h>
include <math/iminterp.h>
include "../lib/apphotdef.h"
include "../lib/photdef.h"
include "../lib/radprofdef.h"
include "../lib/noise.h"
include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/phot.h"
include "../lib/radprof.h"

# AP_FRPROF -- Procedure to compute the radial profile of an object.

int procedure ap_frprof (ap, im, wx, wy, sky_mode, sky_sig, nsky, pier)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# object coordinates
real	sky_mode	# sky value
real	sky_sig		# sky sigma
int	nsky		# number of sky pixels
int	pier		# photometry error

int	i, ier, fier, nxpts, nypts, nrpts, order
pointer	rprof, phot, sp, aperts, cv, asi
real	xc, yc, step, rmin, rmax, inorm, tinorm, zmag
int	apstati(), ap_rpbuf(), ap_rpmeasure(), ap_rpiter()
real	apstatr(), asigrl(), cveval(), ap_rphalf()

errchk	cvinit, cvfit, cvvector, cveval, cvfree
errchk	asinit, asifit, asigrl, asifree
errchk	ap_rpmeasure, ap_rpiter

begin
	# Initialize.
	call apsetr (ap, RPXCUR, wx)
	call apsetr (ap, RPYCUR, wy)
	call ap_rpindef (ap)

	# Check for defined center.
	if (IS_INDEFR(wx) || IS_INDEFR(wy)) {
	    pier = AP_NOAPERT
	    return (AP_NOPROFILE)
	}

	# Return if the sky value is undefined.
	if (IS_INDEFR(sky_mode)) {
	    pier = AP_NOSKYMODE
	    return (AP_RPNOSKYMODE)
	}

	# Get the pixels.
	ier = ap_rpbuf (ap, im, wx, wy)
	if (ier == AP_NOPROFILE) {
	    pier = AP_NOAPERT
	    return (AP_NOPROFILE)
	}
	call ap_maxap (ap, pier)

	# Allocate working space and intialize.
	call smark (sp)
	call salloc (aperts, apstati (ap, NAPERTS), TY_REAL)
	rprof = AP_RPROF(ap)
	phot = AP_PPHOT(ap)
	xc = AP_RPXC(rprof)
	yc = AP_RPYC(rprof)
	nxpts = AP_RPNX(rprof)
	nypts = AP_RPNY(rprof)

	# Perform the photometry.
	call aparrays (ap, APERTS, Memr[aperts])
	call amulkr (Memr[aperts], apstatr (ap, SCALE), Memr[aperts], 
	    apstati (ap, NAPERTS))
	call ap_rmmeasure (Memr[AP_RPIX(rprof)], nxpts, nypts, xc, yc,
	    Memr[aperts], Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)],
	    AP_NMAXAP(phot))
	zmag = apstatr (ap, ZMAG) + 2.5 * log10 (apstatr (ap, ITIME))
	if (apstati (ap, POSITIVE) == YES) {
	    call apcopmags (Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)],
	        Memr[AP_MAGS(phot)], Memr[AP_MAGERRS(phot)], AP_NMAXAP(phot),
		sky_mode, sky_sig, nsky, zmag, apstati (ap, NOISEFUNCTION),
		apstatr (ap, EPADU))
	} else {
	    call apconmags (Memr[AP_SUMS(phot)], Memr[AP_AREA(phot)],
	        Memr[AP_MAGS(phot)], Memr[AP_MAGERRS(phot)], AP_NMAXAP(phot),
		sky_mode, sky_sig, nsky, zmag, apstati (ap, NOISEFUNCTION),
		apstatr (ap, EPADU), apstatr (ap, READNOISE))
	}

	# Initialize the radial profile curve fitting.
	nrpts = AP_RPNPTS(rprof)
	step = apstatr (ap, SCALE) * apstatr (ap, RPSTEP)
	order = max (1, min (AP_RPORDER(rprof), nxpts * nypts - 3))
	rmin = 0.0
	rmax = (nrpts - 1) * step
	call cvinit (cv, SPLINE3, order, rmin, rmax) 

	# Fit the curve.
	call asubkr (Memr[AP_RPIX(rprof)], sky_mode, Memr[AP_RPIX(rprof)],
	    nxpts * nypts)
	fier = ap_rpmeasure (cv, Memr[AP_RPIX(rprof)], nxpts, nypts,
	    xc, yc, rmax, AP_RPNDATA(rprof))

	# Perform the rejection cycle.
	if (fier != NO_DEG_FREEDOM && apstati (ap, RPNREJECT) > 0 &&
	    apstatr (ap, RPKSIGMA) > 0.0) {
	    AP_RPNDATAREJ(rprof) = ap_rpiter (cv, Memr[AP_RPIX(rprof)], nxpts,
	        nypts, xc, yc, rmax, apstati (ap, RPNREJECT), apstatr (ap,
		RPKSIGMA), fier)
	}

	# Evaluate the fit.
	if (fier != NO_DEG_FREEDOM) {

	    # Evaluate the profile.
	    do i = 1, nrpts
	        Memr[AP_RPDIST(rprof)+i-1] = (i - 1) * step
	    call cvvector (cv, Memr[AP_RPDIST(rprof)],
	        Memr[AP_INTENSITY(rprof)], nrpts)

	    # Evaluate the integral.
	    call asiinit (asi, II_SPLINE3)
	    call asifit (asi, Memr[AP_INTENSITY(rprof)], nrpts)
	    Memr[AP_TINTENSITY(rprof)] = 0.0
	    do i = 2, nrpts
	        Memr[AP_TINTENSITY(rprof)+i-1] = Memr[AP_TINTENSITY(rprof)+
		    i-2] + asigrl (asi, real (i - 1), real (i))
	    call asifree (asi)

	    # Normalize the radial profile.
	    inorm = cveval (cv, 0.0)
	    call adivkr (Memr[AP_INTENSITY(rprof)], inorm,
	        Memr[AP_INTENSITY(rprof)], nrpts)
	    call apsetr (ap, INORM, inorm)

	    # Normalize the total intensity.
	    tinorm = Memr[AP_TINTENSITY(rprof)+AP_RPNPTS(rprof)-1]
	    call adivkr (Memr[AP_TINTENSITY(rprof)], tinorm,
	        Memr[AP_TINTENSITY(rprof)], nrpts)
	    call apsetr (ap, TNORM, tinorm)

	    # Compute the FWHMPSF.
	    call apsetr (ap, RPFWHM, 2.0 * ap_rphalf (Memr[AP_RPDIST(rprof)],
	        Memr[AP_INTENSITY(rprof)], nrpts))
	}

	# Set the error code and return.
	call cvfree (cv)
	if (fier == NO_DEG_FREEDOM)
	    ier = AP_NRP_TOO_SMALL
	else if (fier == SINGULAR)
	    ier = AP_RP_SINGULAR

	# Free space.
	call sfree (sp)
	return (ier)
end


# AP_RPITER -- Procedure to reject pixels from the fit.

int procedure ap_rpiter (cv, pixels, nx, ny, wx, wy, rmax, niter, ksigma, fier)

pointer	cv		# pointer to the curfit structure
real	pixels[nx,ARB]	# pixel values
int	nx, ny		# dimensions of image subraster
real	wx, wy		# x and y coordinates of the center
real	rmax		# maximum radius value
int	niter		# maximum number of rejection cycles
real	ksigma		# ksigma rejection limit
int	fier		# fitting error code

int	i, j, k, npts, nreject
pointer	sp, rtemp, w, ptr
real	chisqr, diff, locut, hicut
real	cveval()
errchk	cveval, cvrject, cvsolve

begin
	# Allocate the necessary space.
	call smark (sp)
	call salloc (rtemp, nx, TY_REAL)
	call salloc (w, nx * ny, TY_REAL)
	call amovkr (1.0, Memr[w], nx * ny)

	do i = 1, niter { 

	    # Compute the chisqr.
	    chisqr = 0.0
	    npts = 0
	    ptr = w
	    do j = 1, ny {
	    	call ap_ijtor (Memr[rtemp], nx, j, wx, wy)
		do k = 1, nx {
		    if (Memr[rtemp+k-1] > rmax || Memr[ptr+k-1] <= 0.0)
			next
		    chisqr = chisqr + (pixels[k,j] - cveval (cv,
		        Memr[rtemp+k-1])) ** 2
		    npts = npts + 1
		}
		ptr = ptr + nx
	    }

	    # Compute the new limits.
	    if (npts > 1)
	        chisqr = sqrt (chisqr / (npts - 1))
	    else
		chisqr = 0.0
	    locut = - ksigma * chisqr
	    hicut = ksigma * chisqr

	    # Reject pixels from the fit.
	    nreject = 0
	    ptr = w
	    do j = 1, ny {
	    	call ap_ijtor (Memr[rtemp], nx, j, wx, wy)
		do k = 1, nx {
		    if (Memr[rtemp+k-1] > rmax || Memr[ptr+k-1] <= 0.0)
			next
		    diff = pixels[k,j] - cveval (cv, Memr[rtemp+k-1])
		    if (diff >= locut && diff <= hicut)
			next
		    call cvrject (cv, Memr[rtemp+k-1], pixels[k,j], 1.0,
			WTS_UNIFORM)
		    nreject = nreject + 1
		    Memr[ptr+k-1] = 0.0
		}
		ptr = ptr + nx
	    }
	    if (nreject == 0)
	        break

	    # Recompute the fit.
	    call cvsolve (cv, fier)
	    if (fier == NO_DEG_FREEDOM)
		break
	}

	call sfree (sp)
	return (nreject)
end


# AP_RPHALF -- Compute the FWHM of the PSF.

real procedure ap_rphalf (radius, intensity, npts)

real	radius[ARB]		# radius in pixels
real	intensity[ARB]		# profile intensity
int	npts			# number of points

int	i
real	halfp

begin
	# Seach for the appropriate interval.
	do i = 1, npts
	    if (intensity[i] < 0.5)
		break

	# Compute the full width half maximum.
	if (i == 1)
	    halfp = radius[1]
	else if (i == npts && intensity[npts] >= 0.5)
	    halfp = radius[npts]
	else
	    halfp = (radius[i] * (0.5 - intensity[i-1]) + radius[i-1] *
		(intensity[i] - 0.5)) / (intensity[i] - intensity[i-1])

	return (halfp)
end
