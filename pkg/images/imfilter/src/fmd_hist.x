# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FMD_ASHGMI -- Accumulate the histogram of the input vector.  The output vector
# HGM (the histogram) should be cleared prior to the first call.

procedure fmd_ashgmi (data, npix, hgm, nbins, z1, z2)

int 	data[ARB]		#I data vector
int	npix			#I number of pixels
int	hgm[ARB]		#U output histogram
int	nbins			#I number of bins in histogram
int	z1, z2			#I greyscale values of first and last bins

real	dz
int	bin, i

begin
	if (nbins < 2)
	    return
	dz = real (nbins - 1) / real (z2 - z1)

	do i = 1, npix {
	    bin = int ((data[i] - z1) * dz) + 1
	    if (bin <= 0 || bin > nbins)
		next
	    hgm[bin] = hgm[bin] + 1
	}
end
