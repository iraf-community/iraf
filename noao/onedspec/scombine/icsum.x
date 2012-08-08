# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"icombine.h"


# IC_SUM -- Compute the summed image line.

procedure ic_sumr (d, n, npts, sum)

pointer	d[ARB]			# Data pointers
int	n[npts]			# Number of points
int	npts			# Number of output points per line
real	sum[npts]		# Average (returned)

int	i, j, k
real	s

include	"icombine.com"

begin
	# If no data has been excluded do the sum without checking the
	# number of points.  If all the data has been excluded set the
	# sum to the blank value.

	if (dflag == D_ALL) {
	    do i = 1, npts {
		k = i - 1
		s = Memr[d[1]+k]
		do j = 2, n[i]
		    s = s + Memr[d[j]+k]
		sum[i] = s
	    }
	} else if (dflag == D_NONE) {
	    do i = 1, npts
		sum[i] = blank
	} else {
	    do i = 1, npts {
		if (n[i] > 0) {
		    k = i - 1
		    s = Memr[d[1]+k]
		    do j = 2, n[i]
			s = s + Memr[d[j]+k]
		    sum[i] = s
		} else
		    sum[i] = blank
	    }
	}
end
