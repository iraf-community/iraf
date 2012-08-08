# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AAVG -- Compute the mean and standard deviation (sigma) of a sample.
# All pixels are used.

procedure aavgd (a, npix, mean, sigma)

double	a[ARB]
int	npix
double	mean, sigma, lcut, hcut
int	junk, awvgd()
data	lcut /0./, hcut /0./

begin
	junk = awvgd (a, npix, mean, sigma, lcut, hcut)
end
