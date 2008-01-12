include <mach.h>
include "../lib/center.h"
include "../lib/noise.h"

# AP_CSNRATIO -- Procedure to estimate the signal to noise ratio for centering.

real procedure ap_csnratio (ap, array, nx, ny, threshold)

pointer	ap		# apphot structure
real	array[nx,ny]	# data array
int	nx, ny		# subraster dimensions
real	threshold	# threshold value for snr computation

int	apstati()
real	apstatr(), ap_cratio(), ap_pratio()

begin
	switch (apstati (ap, NOISEFUNCTION)) {
	case AP_NCONSTANT:
	    return (ap_cratio (array, nx, ny, threshold, apstatr (ap,
	        SKYSIGMA)))
	case AP_NPOISSON:
	    return (ap_pratio (array, nx, ny, threshold, apstatr (ap,
	        SKYSIGMA), apstatr (ap, EPADU)))
	default:
	    return (MAX_REAL)
	}
end


# AP_CRATIO -- Procedure to estimate the signal to noise ratio in the
# centering aperture assuming that the noise is due to a constant sky
# sigma. These computations are approximate only.

real procedure ap_cratio (array, nx, ny, sky, noise)

real	array[nx,ny]		# centering buffer
int	nx, ny			# dimensions of the centering buffer
real	sky			# mean sky value of the data in ADU
real	noise			# estimate of sky noise in ADU

int	npts
real	signal, tnoise
real	asumr()

begin
	npts = nx * ny
	signal = asumr (array, npts) - npts * sky
	if (IS_INDEFR(noise))
	    tnoise = 0.0
	else
	    tnoise = sqrt (npts * noise ** 2)
	if (signal <= 0.0)
	    return (0.0)
	else if (tnoise <= 0.0)
	    return (MAX_REAL)
	else
	    return (signal / tnoise)
end


# AP_PRATIO -- Procedure to estimate the signal to noise ratio in the
# centering aperture assuming the noise is due to a constant sky sigma
# and poisson statistics in the image. These computations are
# approximate only.

real procedure ap_pratio (array, nx, ny, sky, noise, padu)

real	array[nx,ny]		# centering buffer
int	nx, ny			# dimensions of the centering buffer
real	sky			# mean sky value of the data in ADU
real	noise			# estimate of sky noise in ADU
real	padu			# photons per adu

int	npts
real	signal, tnoise
real	asumr()

begin
	npts = nx * ny
	signal = asumr (array, npts) - npts * sky
	if (IS_INDEFR(noise))
	    tnoise = sqrt (abs (signal / padu))
	else
	    tnoise = sqrt (abs (signal / padu) + npts * noise ** 2)
	if (signal <= 0.0)
	    return (0.0)
	else if (tnoise <= 0.0)
	    return (MAX_REAL)
	else
	    return (signal / tnoise)
end
