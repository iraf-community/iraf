# ECF_RMS -- Compute the rms with deleted points ignored.

double procedure ecf_rms (r, w, npts)

double	r[npts]			# Residuals
double	w[npts]			# Weights
int	npts			# Number of points

int	i, n
double	rms

begin
	n = 0
	rms = 0.
	do i = 1, npts {
	    if (w[i] == 0.)
		next
	    n = n + 1
	    rms = rms + r[i] * r[i]
	}
	if (n > 0)
	    rms = sqrt (rms / n)
	else
	    rms = INDEFD
	return (rms)
end
