include	<mach.h>
include "pexamine.h"

# PT_AHGMR -- Accumulate the histogram of the input vector.  The output vector
# HGM (the histogram) should be cleared prior to the first call. Delete
# points or points marked for deletion are not included in the plot.

procedure pt_ahgmr (data, delete, npix, hgm, nbins, z1, z2)

real 	data[ARB]		# data vector
int	delete[ARB]		# deletions array
int	npix			# number of pixels
int	hgm[ARB]		# output histogram
int	nbins			# number of bins in histogram
real	z1, z2			# greyscale values of first and last bins

real	z
real	dz
int	bin, i

begin
	dz = real (nbins - 1) / real (z2 - z1)
	if (abs (dz - 1.0) < (EPSILONR * 2.0)) {
	    do i = 1, npix {
		if ((delete[i] == PX_DELETE) || (delete[i] == PX_MARK))
		    next
		z = data[i]
		if (z >= z1 && z <= z2) {
		    bin = int (z - z1) + 1
		    hgm[bin] = hgm[bin] + 1
		}
	    }
	} else {
	    do i = 1, npix {
		if ((delete[i] == PX_DELETE) || (delete[i] == PX_MARK))
		    next
		z = data[i]
		if (z >= z1 && z <= z2) {
		    bin = int ((z - z1) * dz) + 1
		    hgm[bin] = hgm[bin] + 1
		}
	    }
	}
end
