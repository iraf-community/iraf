# AP_AMAXEL -- Procedure to find the maximum element of a 1D array.

procedure ap_amaxel (a, npts, max, el)

real	a[ARB]		# array
int	npts		# number of points
real	max		# maximum value
int	el		# array element

int	i

begin
	max = a[1]
	el = 1
	do i = 2, npts {
	    if (a[i] > max) {
		max = a[i]
		el = i
	    }
	}
end
