# AP_2DALIMR -- Procedure to compute min and max a of 2D array along with
# the indices of the minimum and maximum pixel.

procedure ap_2dalimr (data, nx, ny, mindat, maxdat, imin, jmin, imax, jmax)

real	data[nx, ny]	# data
int	nx, ny		# array dimensions
real	mindat, maxdat	# min, max data values
int	imin, jmin	# indices of min element
int	imax, jmax	# indices of max element

int	i, j

begin
	imin = 1
	jmin = 1
	imax = 1
	jmax = 1
	mindat = data[1,1]
	maxdat = data[1,1]

	do j = 2, ny {
	    do i = 1, nx {
		if (data[i,j] < mindat) {
		    imin = i
		    jmin = j
		    mindat = data[i,j]
		} else if (data[i,j] > maxdat) {
		    imax = i
		    jmax = j
		    maxdat = data[i,j]
		}
	    }
	}
end
