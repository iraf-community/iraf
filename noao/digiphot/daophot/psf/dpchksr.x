include <mach.h>
include	"../lib/daophotdef.h"
include	"../lib/psfdef.h"

# DP_CHKSR -- Check that the input subraster does not have out of 
# bounds pixels.

int procedure dp_chksr (dao, sr, xdim, ydim)

pointer	dao			# pointer to the daophot structure
real	sr[xdim,ydim]		# the data subraster
int	xdim, ydim		# the dimensions of the subraster

int	i,j, npix
real	lowbad, highbad
pointer	psf

begin
	# Some daophot definitions.
	psf = DP_PSF (dao)

	# Define the minimum and maximum good data values.
	if (IS_INDEFR(DP_MINGDATA(dao)))
	    lowbad = -MAX_REAL
	else
	    lowbad = DP_MINGDATA(dao)
	if (IS_INDEFR(DP_MAXGDATA(dao)))
	    highbad = MAX_REAL
	else
	    highbad = DP_MAXGDATA(dao)

	# Loop through the pixels looking for bad values.
	do i = 1, xdim {
	    do j = 1, ydim {
		if (sr[i,j] < lowbad || sr[i,j] > highbad)
		    return (ERR)
	    }
	}

	# Compute the minimum and maximum data values in the PSF subraster.
	npix = xdim * ydim
	call alimr (sr, npix, DP_PSFMIN(psf), DP_PSFMAX(psf))

	return (OK)
end
