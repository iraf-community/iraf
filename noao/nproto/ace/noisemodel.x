# NOISEMODEL -- Compute noise model.
#
# var = (var(sky) + (image-sky)/gain) / sqrt (exposure)
#
# What is actually returned is the square root of the variance.
# The variance of the sky and the effective gain are for a unit
# exposure in the exposure map.

procedure noisemodel (image, sky, sig, gain, exp, sigma, npix)

real	image[npix]		#I Image
real	sky[npix]		#I Sky
real	sig[npix]		#I Sky sigma
real	gain[npix]		#I Gain
real	exp[npix]		#I Exposure
real	sigma[npix]		#O Sigma
int	npix			#I Number of pixels

int	i
real	e, elast, sqrte

begin
	if (IS_INDEFR(exp[1])) {
	    if (IS_INDEFR(gain[1]))
		call amovr (sig, sigma, npix)
	    else {
		do i = 1, npix
		    sigma[i] = sqrt (sig[i] * sig[1] +
			(image[i] - sky[i]) / gain[i])
	    }
	} else if (IS_INDEFR(gain[1])) {
	    elast = INDEFR
	    do i = 1, npix {
		e = exp[i]
		if (e == 0.) {
		    sigma[i] = sig[i]
		    next
		}
		if (e != elast) {
		    sqrte = sqrt (e)
		    elast = e
		}
		sigma[i] = sig[i] / sqrte
	    }
	} else {
	    do i = 1, npix {
		e = exp[i]
		if (e == 0.) {
		    sigma[i] = sqrt (sig[i] * sig[i] +
			(image[i] - sky[i]) / gain[i])
		    next
		}
		sigma[i] = sqrt ((sig[i] * sig[i] +
		    (image[i] - sky[i]) / gain[i]) / e)
	    }
	}
end


# EXPSIGMA -- Apply exposure map to correct sky sigma.
# Assume the exposure map has region of contiguous constant values so
# that the number of square roots can be minimized.  An exposure map
# value of zero leaves the sigma unchanged.

procedure expsigma (sigma, expmap, npix, mode)

real	sigma[npix]		#U Sigma values
real	expmap[npix]		#I Exposure map values
int	npix			#I Number of pixels
int	mode			#I 0=divide, 1=multiply

int	i
real	exp, lastexp, scale

begin
	switch (mode) {
	case 0:
	    lastexp = INDEFR
	    do i = 1, npix {
		exp = expmap[i]
		if (exp == 0.)
		    next
		if (exp != lastexp) {
		    scale = sqrt (exp)
		    lastexp = exp
		}
		sigma[i] = sigma[i] / scale
	    }
	case 1:
	    lastexp = INDEFR
	    do i = 1, npix {
		exp = expmap[i]
		if (exp == 0.)
		    next
		if (exp != lastexp) {
		    scale = sqrt (exp)
		    lastexp = exp
		}
		sigma[i] = sigma[i] * scale
	    }
	}
end
