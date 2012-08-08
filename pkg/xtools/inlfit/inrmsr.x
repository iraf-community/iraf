# IN_RMS -- Compute rms of points which have a non-zero weight.

real procedure in_rmsr (y, fit, wts, npts)

real	y[npts]		# function
real	fit[npts]	# fit
real	wts[npts]	# weights
int	npts		# number of points

int	i, ndata
real	resid, rms

begin
	rms = real (0.0)
	ndata = 0

	do i = 1, npts {
	    if (wts[i] == real (0.0))
		next
	    resid = y[i] - fit[i]
	    rms = rms + resid * resid
	    ndata = ndata + 1
	}

	if (ndata > 0)
	    rms = sqrt (rms / ndata)
	else
	    rms = real (0.0)

	return (rms)
end
