include <math/curfit.h>

# AP_RPMEASURE -- Procedure to measure the flux and effective area in a set of
# apertures and accumulate the fit to the radial profile.

int procedure ap_rpmeasure (cv, pixels, nx, ny, wx, wy, rmax, ndata)

pointer	cv			# pointer to curfit structure
real	pixels[nx,ARB]		# subraster pixel values
int	nx, ny			# dimensions of the subraster
real	wx, wy			# center of subraster
real	rmax			# the maximum radius
int	ndata			# the number of data points

int	i, j, ier
real	weight, dy2, r2, rcv

begin
	# Initialize.
	ndata = 0
	call cvzero (cv)

	# Loop over the pixels.
	do j = 1, ny {
	    dy2 = (j - wy) ** 2
	    do i = 1, nx {
		r2 = (i - wx) ** 2 + dy2
		rcv = sqrt (r2)
		if (rcv <= rmax) {
		    call cvaccum (cv, rcv, pixels[i,j], weight, WTS_UNIFORM)
		    ndata = ndata + 1
		}
	    }
	}

	call cvsolve (cv, ier)
	return (ier)
end
