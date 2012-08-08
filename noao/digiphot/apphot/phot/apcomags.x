include <mach.h>
include "../lib/noise.h"

# APCOPMAGS -- Procedure to compute the magnitudes from the aperture sums,
# areas and sky values.

procedure apcopmags (sums, areas, mags, magerrs, naperts, sky, sigma, nsky,
    zmag, noise, padu)

double	sums[ARB]		# aperture sums
double	areas[ARB]		# aperture areas
real	mags[ARB]		# output magnitudes
real	magerrs[ARB]		# errors in the magnitudes
int	naperts			# number of apertures
real	sky			# sky value
real	sigma			# sigma of the sky values
int	nsky			# number of sky pixels
real	zmag			# sky value, sigma and zero point magnitudes
int	noise			# noise model
real	padu			# photons per adu

int	i
real	err1, err2, err3, err

begin
	# Compute the magnitudes and errors
	do i = 1, naperts {
	    mags[i] = sums[i] - areas[i] * sky
	    if (mags[i] <= 0.0)
		mags[i] = INDEFR
	    else {
		if (IS_INDEFR(sigma))
		    err1 = 0.0
		else
		    err1 = areas[i] * sigma ** 2
		switch (noise) {
		case AP_NCONSTANT:
		    err2 = 0.0
		case AP_NPOISSON:
		    err2 = mags[i] / padu
		default:
		    err2 = 0.0
		}
		if (nsky <= 0)
		    err3 = 0.0
		else if (IS_INDEFR(sigma))
		    err3 = 0.0
		else
		    err3 = sigma ** 2 * areas[i] ** 2 / nsky
		err = err1 + err2 + err3
		if (err <= 0.0)
		    magerrs[i] = 0.0
		else {
		    magerrs[i] = 1.0857 * sqrt (err) / mags[i]
		}
		mags[i] = zmag - 2.5 * log10 (mags[i])
	    }
	}
end


# APCONMAGS -- Procedure to compute the magnitudes from the aperture sums,
# areas and sky values.

procedure apconmags (sums, areas, mags, magerrs, naperts, sky, sigma, nsky,
    zmag, noise, padu, readnoise)

double	sums[ARB]		# aperture sums
double	areas[ARB]		# aperture areas
real	mags[ARB]		# output magnitudes
real	magerrs[ARB]		# errors in the magnitudes
int	naperts			# number of apertures
real	sky			# sky value
real	readnoise		# readout noise in electrons
real	sigma			# sigma of the sky values
int	nsky			# number of sky pixels
real	zmag			# sky value, sigma and zero point magnitudes
int	noise			# noise model
real	padu			# photons per adu

int	i
real	err1, err2, err3, err

begin
	# Compute the magnitudes and errors
	do i = 1, naperts {
	    mags[i] = areas[i] * sky - sums[i]
	    if (mags[i] <= 0.0)
		mags[i] = INDEFR
	    else {
		if (IS_INDEFR(readnoise))
		    err1 = 0.0
		else
		    err1 = areas[i] * (readnoise / padu) ** 2
		switch (noise) {
		case AP_NCONSTANT:
		    err2 = 0.0
		case AP_NPOISSON:
		    err2 = abs (sums[i]) / padu
		default:
		    err2 = 0.0
		}
		if (nsky <= 0)
		    err3 = 0.0
		else if (IS_INDEFR(sigma))
		    err3 = 0.0
		else
		    err3 = sigma ** 2 * areas[i] ** 2 / nsky
		err = err1 + err2 + err3
		if (err <= 0.0)
		    magerrs[i] = 0.0
		else {
		    magerrs[i] = 1.0857 * sqrt (err) / abs (sums[i])
		}
		mags[i] = zmag - 2.5 * log10 (mags[i])
	    }
	}
end
