include	<mach.h>

# ECF_REJECT -- Reject points with large residuals from the fit.

procedure ecf_reject (ecf, x, y, z, w, r, npts, fixedorder)

pointer	ecf			# GSURFIT pointer
double	x[npts]			# X points
double	y[npts]			# Y points
double	z[npts]			# Z points
double	w[npts]			# Weights
double	r[npts]			# Residuals
int	npts			# Number of points
int	fixedorder		# Fixed order?

int	i, j, newreject
double	low_cut, high_cut
include	"ecffit.com"

begin
	# Return if rejection is not desired.
	nreject = 0
	if (niterate == 0 || (low == 0. && high == 0.))
	    return

	# Reject points.
	do i = 1, niterate {
	    if (low > 0.)
		low_cut = -low * rms
	    else
		low_cut = -MAX_REAL
	    if (high > 0.)
		high_cut = high * rms
	    else
		high_cut = MAX_REAL

	    newreject = 0
	    do j = 1, npts {
		if (w[j] == 0.)
		    next
		if ((r[j] > high_cut) || (r[j] < low_cut)) {
		    w[j] = 0.
		    newreject = newreject + 1
		}
	    }

	    if (newreject == 0)
		break

	    call ecf_solve (ecf, x, y, z, w, r, npts, fixedorder)
	    nreject = nreject + newreject
	}
end
