# EP_NOISE -- Add noise.
# If the sigma is zero add no noise.  If a nonzero sigma is given then
# add gaussian random noise.  If the sigma is INDEF then use histogram
# sampling from the background.  The background histogram is corrected
# for a background function.  The histogram is sampled by sorting the
# background values and selecting uniformly from the central 80%.

procedure ep_noise (sigma, data, mask, x, y, npts, gs)

real	sigma			# Noise sigma
real	data[npts]		# Image data
int	mask[npts]		# Mask (1=object, 2=background)
real	x[npts], y[npts]	# Coordinates
int	npts			# Number of pixels in subraster
pointer	gs			# Background surface

int	i, j, nbg
real	a, b, urand(), gseval(), ep_gauss()
pointer	bg

long	seed
data	seed /1/

begin
	# Add gaussian random noise.
	if (!IS_INDEF (sigma)) {
	    if (sigma <= 0.)
		return
	    do i = 1, npts {
	        if (mask[i] == 1)
	            data[i] = data[i] + sigma * ep_gauss (seed)
	    }
	    return
	}
		
	# Add background sampling with background slope correction.

	if (gs == NULL)
	    return

	call malloc (bg, npts, TY_REAL)

	nbg = 0
	do i = 1, npts {
	    if (mask[i] == 2) {
	        Memr[bg+nbg] = data[i] - gseval (gs, x[i], y[i])
	        nbg = nbg + 1
	    }
	}
	if (nbg < 10) {
	    call mfree (bg, TY_REAL)
	    return
	}

	call asrtr (Memr[bg], Memr[bg], nbg)
	a = .1 * nbg - 1
	b = .8 * nbg

	do i = 1, npts
	    if (mask[i] == 1) {
		j = a + b * urand (seed)
	        data[i] = data[i] + Memr[bg + j]
	    }

	call mfree (bg, TY_REAL)
end


# EP_GAUSS -- Gaussian random number generator based on uniform random number
# generator.

real procedure ep_gauss (seed)

long	seed		# Random number seed

real	a, b, c, d, urand()
int	flag
data	flag/NO/

begin
	if (flag == NO) {
	    repeat {
	        a = 2. * urand (seed) - 1.
	        b = 2. * urand (seed) - 1.
	        c = a * a + b * b
	    } until (c <= 1.)

	    d = sqrt (-2. * log (c) / c)
	    flag = YES
	    return (a * d)
	} else {
	    flag = NO
	    return (b * d)
	}
end
