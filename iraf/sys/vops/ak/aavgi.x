# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AAVG -- Compute the mean and standard deviation (sigma) of a sample.
# All pixels are used.

procedure aavgi (a, npix, mean, sigma)

int	a[ARB]
size_t	npix
real	mean, sigma, lcut, hcut
size_t	junk
size_t	awvgi()
data	lcut /0./, hcut /0./

begin
	junk = awvgi (a, npix, mean, sigma, lcut, hcut)
end
