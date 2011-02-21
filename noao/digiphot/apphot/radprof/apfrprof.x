include <mach.h>
include <gset.h>
include <math.h>

include <math/curfit.h>
include <math/iminterp.h>
include "../lib/apphotdef.h"
include "../lib/noisedef.h"
include "../lib/fitskydef.h"
include "../lib/photdef.h"
include "../lib/radprofdef.h"
include "../lib/apphot.h"
include "../lib/phot.h"
include "../lib/radprof.h"

# AP_FRPROF -- Compute the radial profile of an object.

int procedure ap_frprof (ap, im, wx, wy, pier)

pointer	ap		# pointer to the apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# object coordinates
int	pier		# photometry error

int	i, ier, fier, nxpts, nypts, nrpts, order
pointer	sky, rprof, cv, asi
real	datamin, datamax, step, rmin, rmax, inorm, tinorm
int	ap_rpbuf(), ap_rmag(), ap_rpmeasure(), ap_rpiter()
real	asigrl(), cveval(), ap_rphalf()

errchk	cvinit(), cvfit(), cvvector(), cveval(), cvfree()
errchk	asinit(), asifit(), asigrl(), asifree()
errchk	ap_rpmeasure(), ap_rpiter()

begin
	# Set up some apphot pointers.
	sky = AP_PSKY(ap)
	rprof = AP_RPROF(ap)

	# Initialize.
         AP_RPXCUR(rprof) = wx
         AP_RPYCUR(rprof) = wy
	call ap_rpindef (ap)
        if (IS_INDEFR(wx) || IS_INDEFR(wy)) {
            AP_ORPXCUR(rprof) = INDEFR
            AP_ORPYCUR(rprof) = INDEFR
        } else {
            switch (AP_WCSOUT(ap)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_ltoo (ap, wx, wy, AP_ORPXCUR(rprof),
		    AP_ORPYCUR(rprof), 1)
            case WCS_TV:
                call ap_ltov (im, wx, wy, AP_ORPXCUR(rprof),
		    AP_ORPYCUR(rprof), 1)
            default:
                AP_ORPXCUR(rprof) = wx
                AP_ORPYCUR(rprof) = wy
            }
        }

	# Get the pixels and check for error conditions.
	if (IS_INDEFR(wx) || IS_INDEFR(wy)) {
	    pier = AP_APERT_NOAPERT
	    return (AP_RP_NOPROFILE)
	} else if (IS_INDEFR(AP_SKY_MODE(sky))) {
	    pier = AP_APERT_NOSKYMODE
	    return (AP_RP_NOSKYMODE)
	} else if (ap_rpbuf (ap, im, wx, wy) == AP_RP_NOPROFILE) {
	    pier = AP_APERT_NOAPERT
	    return (AP_RP_NOPROFILE)
	}

	# Do the photometry.
	pier = ap_rmag (ap)

	# Initialize some common variables.
	nxpts = AP_RPNX(rprof)
	nypts = AP_RPNY(rprof)
	nrpts = AP_RPNPTS(rprof)

	# Initialize the radial profile curve fitting.
	step = AP_SCALE(ap) * AP_RPSTEP(rprof)
	order = max (1, min (AP_RPORDER(rprof), nxpts * nypts - 3))
	rmin = 0.0
	rmax = (nrpts - 1) * step
	if (IS_INDEFR(AP_DATAMIN(ap)))
	    datamin = -MAX_REAL
	else
	    datamin = AP_DATAMIN(ap) - AP_SKY_MODE(sky)
	if (IS_INDEFR(AP_DATAMAX(ap)))
	    datamax = MAX_REAL
	else
	    datamax = AP_DATAMAX(ap) - AP_SKY_MODE(sky)

	# Fit the curve.
	call cvinit (cv, SPLINE3, order, rmin, rmax) 
	call asubkr (Memr[AP_RPIX(rprof)], AP_SKY_MODE(sky),
	    Memr[AP_RPIX(rprof)], nxpts * nypts)
	fier = ap_rpmeasure (cv, Memr[AP_RPIX(rprof)], nxpts, nypts,
	    AP_RPXC(rprof), AP_RPYC(rprof), datamin, datamax, rmax,
	    AP_RPNDATA(rprof), AP_RPNBAD(rprof))

	# Perform the rejection cycle.
	if (fier != NO_DEG_FREEDOM && AP_RPNREJECT(rprof) > 0 &&
	    AP_RPKSIGMA(rprof) > 0.0)
	    AP_RPNDATAREJ(rprof) = ap_rpiter (cv, Memr[AP_RPIX(rprof)], nxpts,
	        nypts, AP_RPXC(rprof), AP_RPYC(rprof), rmax, datamin, datamax,
		AP_RPNREJECT(rprof), AP_RPKSIGMA(rprof), fier)
	else
	    AP_RPNDATAREJ(rprof) = 0

	AP_RPNDATA(rprof) = AP_RPNDATA(rprof) - AP_RPNDATAREJ(rprof)
	AP_RPNDATAREJ(rprof) = AP_RPNDATAREJ(rprof) + AP_RPNBAD(rprof)

	# Evaluate the fit.
	if (fier != NO_DEG_FREEDOM) {

	    # Evaluate the profile.
	    do i = 1, nrpts
	        Memr[AP_RPDIST(rprof)+i-1] = (i - 1) * step
	    call cvvector (cv, Memr[AP_RPDIST(rprof)],
	        Memr[AP_INTENSITY(rprof)], nrpts)

	    # Evaluate the integral.
	    call asiinit (asi, II_SPLINE3)
	    call amulr (Memr[AP_RPDIST(rprof)], Memr[AP_INTENSITY(rprof)],
	        Memr[AP_TINTENSITY(rprof)], nrpts)
	    call asifit (asi, Memr[AP_TINTENSITY(rprof)], nrpts)
	    Memr[AP_TINTENSITY(rprof)] = 0.0
	    do i = 2, nrpts
	        Memr[AP_TINTENSITY(rprof)+i-1] = Memr[AP_TINTENSITY(rprof)+
		    i-2] + asigrl (asi, real (i - 1), real (i))
	    call amulkr (Memr[AP_TINTENSITY(rprof)], real (TWOPI) * step,
	        Memr[AP_TINTENSITY(rprof)], nrpts)
	    call asifree (asi)

	    # Normalize the radial profile.
	    inorm = cveval (cv, 0.0)
	    if (inorm != 0.0)
	        call adivkr (Memr[AP_INTENSITY(rprof)], inorm,
	            Memr[AP_INTENSITY(rprof)], nrpts)
	    call apsetr (ap, INORM, inorm)

	    # Normalize the total intensity.
	    tinorm = Memr[AP_TINTENSITY(rprof)+AP_RPNPTS(rprof)-1]
	    if (tinorm != 0.0)
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
	    ier = AP_RP_NPTS_TOO_SMALL
	else if (fier == SINGULAR)
	    ier = AP_RP_SINGULAR

	# Free space.
	return (ier)
end


# AP_RMAG -- Compute the magnitudes for the radial profile

int procedure ap_rmag (ap)

pointer	ap		# pointer to the apphot structure

int	pier, nap
pointer	sp, nse, sky, phot, rprof, aperts
real	datamin, datamax, zmag

begin
	# Initialize some apphot pointers.
	nse = AP_NOISE(ap)
	sky = AP_PSKY(ap)
	phot = AP_PPHOT(ap)
	rprof = AP_RPROF(ap)

	# Allocate working space.
	call smark (sp)
	call salloc (aperts, AP_NAPERTS(phot), TY_REAL)

	# Check for out of bounds apertures.
	call ap_maxap (ap, pier)

	# Define the good data minimum and maximum for photometry.
	if (IS_INDEFR(AP_DATAMIN(ap)))
	    datamin = -MAX_REAL
	else
	    datamin = AP_DATAMIN(ap)
	if (IS_INDEFR(AP_DATAMAX(ap)))
	    datamax = MAX_REAL
	else
	    datamax = AP_DATAMAX(ap)

	# Compute the sums.
	call ap_arrayr (ap, APERTS, Memr[aperts])
	call amulkr (Memr[aperts], AP_SCALE(ap), Memr[aperts], AP_NAPERTS(phot))
	if (IS_INDEFR(AP_DATAMIN(ap)) && IS_INDEFR(AP_DATAMAX(ap))) {
	    call ap_rmmeasure (Memr[AP_RPIX(rprof)], AP_RPNX(rprof),
	        AP_RPNY(rprof), AP_RPXC(rprof), AP_RPYC(rprof), Memr[aperts],
		Memd[AP_SUMS(phot)], Memd[AP_AREA(phot)], AP_NMAXAP(phot))
	    AP_NMINAP(phot) = AP_NMAXAP(phot) + 1
	} else
	    call ap_brmmeasure (Memr[AP_RPIX(rprof)], AP_RPNX(rprof),
	        AP_RPNY(rprof), AP_RPXC(rprof), AP_RPYC(rprof), datamin,
		datamax, Memr[aperts], Memd[AP_SUMS(phot)],
		Memd[AP_AREA(phot)], AP_NMAXAP(phot), AP_NMINAP(phot))

	# Check for bad pixels.
	if ((pier == AP_OK) && (AP_NMINAP(phot) <= AP_NMAXAP(phot)))
	    pier = AP_APERT_BADDATA
	nap = min (AP_NMINAP(phot) - 1, AP_NMAXAP(phot))

	# Compute the magnitudes.
	zmag = AP_ZMAG(phot) + 2.5 * log10 (AP_ITIME(ap))
	if (AP_POSITIVE(ap) == YES)
	    call apcopmags (Memd[AP_SUMS(phot)], Memd[AP_AREA(phot)],
	        Memr[AP_MAGS(phot)], Memr[AP_MAGERRS(phot)], nap,
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_NSKY(sky), zmag,
		AP_NOISEFUNCTION(nse), AP_EPADU(nse))
	else
	    call apconmags (Memd[AP_SUMS(phot)], Memd[AP_AREA(phot)],
	        Memr[AP_MAGS(phot)], Memr[AP_MAGERRS(phot)], nap,
		AP_SKY_MODE(sky), AP_SKY_SIG(sky), AP_NSKY(sky), zmag,
		AP_NOISEFUNCTION(nse), AP_EPADU(nse), AP_READNOISE(nse))

	call sfree (sp)

	return (pier)
end


# AP_RPMEASURE -- Procedure to measure the flux and effective area in a set of
# apertures and accumulate the fit to the radial profile.

int procedure ap_rpmeasure (cv, pixels, nx, ny, wx, wy, datamin, datamax,
	rmax, ndata, nbad)

pointer	cv			# pointer to curfit structure
real	pixels[nx,ARB]		# subraster pixel values
int	nx, ny			# dimensions of the subraster
real	wx, wy			# center of subraster
real	datamin			# minimum good data value
real	datamax			# maximum good data value
real	rmax			# the maximum radius
int	ndata			# the number of ok data points
int	nbad			# the number of bad data points

int	i, j, ier
real	weight, dy2, r2, rcv

begin
	# Initialize.
	ndata = 0
	nbad = 0
	call cvzero (cv)

	# Loop over the pixels.
	do j = 1, ny {
	    dy2 = (j - wy) ** 2
	    do i = 1, nx {
		r2 = (i - wx) ** 2 + dy2
		rcv = sqrt (r2)
		if (rcv > rmax)
		    next
		if (pixels[i,j] < datamin || pixels[i,j] > datamax) {
		    nbad = nbad + 1
		} else {
		    call cvaccum (cv, rcv, pixels[i,j], weight, WTS_UNIFORM)
		    ndata = ndata + 1
		}
	    }
	}

	call cvsolve (cv, ier)
	return (ier)
end


# AP_RPITER -- Procedure to reject pixels from the fit.

int procedure ap_rpiter (cv, pixels, nx, ny, wx, wy, rmax, datamin, datamax,
	niter, ksigma, fier)

pointer	cv		# pointer to the curfit structure
real	pixels[nx,ARB]	# pixel values
int	nx, ny		# dimensions of image subraster
real	wx, wy		# x and y coordinates of the center
real	rmax		# maximum radius value
real	datamin		# minimum good data value
real	datamax		# maximum good data value
int	niter		# maximum number of rejection cycles
real	ksigma		# ksigma rejection limit
int	fier		# fitting error code

int	i, j, k, npts, ntreject, nreject
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

	# Set the weights of out of range and bad data points to 0.0.
	ptr = w
	do j = 1, ny {
	    call ap_ijtor (Memr[rtemp], nx, j, wx, wy)
	    do k = 1, nx {
		if (Memr[rtemp+k-1] > rmax || pixels[k,j] < datamin ||
		    pixels[k,j] > datamax)
		    Memr[ptr+k-1] = 0.0
	    }
	    ptr = ptr + nx
	}

	ntreject = 0
	do i = 1, niter { 

	    # Compute the chisqr.
	    chisqr = 0.0
	    npts = 0
	    ptr = w
	    do j = 1, ny {
	    	call ap_ijtor (Memr[rtemp], nx, j, wx, wy)
		do k = 1, nx {
		    if (Memr[ptr+k-1] <= 0.0)
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
		    if (Memr[ptr+k-1] <= 0.0)
			next
		    diff = pixels[k,j] - cveval (cv, Memr[rtemp+k-1])
		    if (diff >= locut && diff <= hicut)
			next
		    call cvrject (cv, Memr[rtemp+k-1], pixels[k,j], 1.0)
		    nreject = nreject + 1
		    Memr[ptr+k-1] = 0.0
		}
		ptr = ptr + nx
	    }

	    if (nreject == 0)
	        break
	    ntreject = ntreject + nreject

	    # Recompute the fit.
	    call cvsolve (cv, fier)
	    if (fier == NO_DEG_FREEDOM)
		break
	}


	call sfree (sp)

	return (ntreject)
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
