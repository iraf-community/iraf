int	nxc, nyc		# Number of PSF centers
int	nxssub, nyssub		# Number of star subsamples
int	nxgsub, nygsub		# Number of galaxy subsamples
real	dynrange		# Profile intensity dynamic range
real	psfrange		# PSF convolution dynamic range
pointer	stp, star, see

common	/mktcom/ dynrange, psfrange, nxc, nyc, nxssub, nyssub, nxgsub, nygsub,
		stp, star, see
