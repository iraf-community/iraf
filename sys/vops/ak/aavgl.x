# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# AAVG -- Compute the mean and standard deviation (sigma) of a sample.
# All pixels are used.

procedure aavgl (a, npix, mean, sigma)

long	a[ARB]
int	npix
double	mean, sigma, lcut, hcut
int	junk, awvgl()
data	lcut /0./, hcut /0./

begin
	junk = awvgl (a, npix, mean, sigma, lcut, hcut)
end
