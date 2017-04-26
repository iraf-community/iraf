include	"sensfunc.h"


# SF_RMS -- Compute the RMS of the sensitivity function fit.

procedure sf_rms (stds, nstds, rms, npts)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
real	rms			# RMS about fit (returned)
int	npts			# Number of points in fit (excluding zero wts.)

int	i, j, f, n
pointer	x, y, w

begin
	npts = 0
	rms = 0.
	do i = 1, nstds {
	    if (STD_FLAG(stds[i]) != SF_INCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    x = STD_WAVES(stds[i])
	    y = STD_SENS(stds[i])
	    f = STD_FIT(stds[i])
	    w = STD_WTS(stds[i])
	    do j = 1, n {
		if (Memr[w] != 0.) {
		    rms = rms + (Memr[y] - Memr[f]) ** 2
		    npts = npts + 1
		}
		x = x + 1
		y = y + 1
		f = f + 1
		w = w + 1
	    }
	}

	if (npts > 1)
	    rms = sqrt (rms / (npts - 1))
	else
	    rms = INDEF
end
