# AP_WLIMR -- Compute the data limits and set the weights.

procedure ap_wlimr (pix, w, npts, datamin, datamax, dmin, dmax)

real	pix[ARB]		# input pixel array
real	w[ARB]			# weight array
int	npts			# number of points
real	datamin			# minimum good data point
real	datamax			# maximum good data point
real	dmin			# output data minimum
real	dmax			# output data maximum

int	i
real	value

begin
	dmin = datamax
	dmax = datamin
	do i = 1, npts {
	    value = pix[i]
	    if (value < datamin || value > datamax) {
		w[i] = 0.0
		next
	    }
	    if (value  < dmin)
		dmin = value
	    if (value  > dmax)
		dmax = value
	}
end
