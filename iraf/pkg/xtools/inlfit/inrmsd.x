# IN_RMS -- Compute rms of points which have a non-zero weight.

double procedure in_rmsd (y, fit, wts, npts)

double	y[npts]		# function
double	fit[npts]	# fit
double	wts[npts]	# weights
int	npts		# number of points

int	i, ndata
double	resid, rms

begin
	rms = double (0.0)
	ndata = 0

	do i = 1, npts {
	    if (wts[i] == double (0.0))
		next
	    resid = y[i] - fit[i]
	    rms = rms + resid * resid
	    ndata = ndata + 1
	}

	if (ndata > 0)
	    rms = sqrt (rms / ndata)
	else
	    rms = double (0.0)

	return (rms)
end
