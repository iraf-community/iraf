include "sensfunc.h"


# SF_WEIGHTS -- Change weights for point, star, or wavelength.
# The original input weight is permanently lost.

procedure sf_weights (stds, nstds, key, istd, ipt, weight)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
int	key			# Delete point, star, or wavelength
int	istd			# Index of standard star
int	ipt			# Index of point
real	weight			# New weight

int	i, j, n
real	wave
pointer	z, w, iw

begin
	switch (key) {
	case 'p':
	    Memr[STD_WTS(stds[istd])+ipt-1] = weight
	    Memr[STD_IWTS(stds[istd])+ipt-1] = weight
	case 's':
	    n = STD_NWAVES(stds[istd])
	    w = STD_WTS(stds[istd])
	    iw = STD_IWTS(stds[istd])
	    call amovkr (weight, Memr[w], n)
	    call amovkr (weight, Memr[iw], n)
	case 'w':
	    wave = Memr[STD_WAVES(stds[istd])+ipt-1]
	    do i = 1, nstds {
	        if (STD_FLAG(stds[i]) != SF_INCLUDE)
		    next
	        n = STD_NWAVES(stds[i])
		z = STD_WAVES(stds[i])
	        w = STD_WTS(stds[i])
	        iw = STD_IWTS(stds[i])
		do j = 1, n {
		    if (Memr[z] == wave) {
	                Memr[w] = weight
	                Memr[iw] = weight
		    }
		    w = w + 1
		    iw = iw + 1
		    z = z + 1
		}
	    }
	}
end
