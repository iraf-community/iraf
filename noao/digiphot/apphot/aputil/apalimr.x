# AP_ALIMR -- Procedure to compute the maximum and minimum data values and
# indices of a 1D array.

procedure ap_alimr (data, npts, mindat, maxdat, imin, imax)

real	data[npts]	# data array
int	npts		# number of points
real	mindat, maxdat	# min and max data value
int	imin, imax	# indices of min and max element

int	i

begin
	imin = 1
	imax = 1
	mindat = data[1]
	maxdat = data[1]
	do i = 2, npts {
	    if (data[i] > maxdat) {
		imax = i
		maxdat = data[i]
	    } else if (data[i] < mindat) {
		imin = i
		mindat = data[i]
	    }
	}
end
