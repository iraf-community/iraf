include "../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_FITPEAK -- Procedure to fit the peak of the star

int procedure dp_fitpeak (dao, subrast, xdim, ydim, oxstar, oystar,
	xstar, ystar, rel_bright, sky)

pointer	dao				# pointer to the daophot structure
real	subrast[xdim,ydim]		# image subraster
int	xdim, ydim			# dimensions of the image subraster
real	oxstar, oystar			# image position of star
real	xstar, ystar			# position of the star
real	rel_bright			# relative brightness of the star
real	sky				# real sky value

int	niter
pointer	psf, psffit
real	magnitude, errmag, chi, sharp

begin
	# Define some daophot pointers.
	psf = DP_PSF(dao)
	psffit = DP_PSFFIT(dao)

	# Estimate the position and brightness of this star.
	oxstar = DP_CUR_PSFX(psf)
	oystar = DP_CUR_PSFY(psf)
	xstar = DP_SZLOOKUP(psf) / 2 + oxstar - int (oxstar)
	ystar = DP_SZLOOKUP(psf) / 2 + oystar - int (oystar)
	magnitude = DP_CUR_PSFMAG(psf)
	rel_bright = DAO_RELBRIGHT (psffit, magnitude)
	sky = DP_CUR_PSFSKY(psf)

	# Normalize the lookup table.
	call adivkr (Memr[DP_PSFLUT(psffit)], Memr[DP_PSFMATRIX(psf)],
	    Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit) * DP_PSFSIZE(psffit))

	# Fit the peak.
	call dp_ipkfit ()
	call dp_pkfit (dao, subrast, xdim, ydim, NO, xstar, ystar, 0.0, 0.0,
	    rel_bright, sky, errmag, chi, sharp, niter)
	call dp_fpkfit()
	if (niter < 0)
	    return (ERR)

	# Normalize the lookup table.
	call amulkr (Memr[DP_PSFLUT(psffit)], Memr[DP_PSFMATRIX(psf)],
	    Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit) * DP_PSFSIZE(psffit))

	return (OK)
end
