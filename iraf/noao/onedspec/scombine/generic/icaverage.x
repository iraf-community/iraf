# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"../icombine.h"


# IC_AVERAGE -- Compute the average image line.
# Options include a weight average.

procedure ic_averager (d, m, n, wts, npts, average)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image ID pointers
int	n[npts]			# Number of points
real	wts[ARB]		# Weights
int	npts			# Number of output points per line
real	average[npts]		# Average (returned)

int	i, j, k
real	sumwt, wt
real	sum

include	"../icombine.com"

begin
	# If no data has been excluded do the average without checking the
	# number of points and using the fact that the weights are normalized.
	# If all the data has been excluded set the average to the blank value.

	if (dflag == D_ALL) {
	    if (dowts) {
		do i = 1, npts {
		    k = i - 1
		    wt = wts[Memi[m[1]+k]]
		    sum = Memr[d[1]+k] * wt
		    do j = 2, n[i] {
			wt = wts[Memi[m[j]+k]]
			sum = sum + Memr[d[j]+k] * wt
		    }
		    average[i] = sum
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    sum = Memr[d[1]+k]
		    do j = 2, n[i]
			sum = sum + Memr[d[j]+k]
		    average[i] = sum / n[i]
		}
	    }
	} else if (dflag == D_NONE) {
	    do i = 1, npts
		average[i] = blank
	} else {
	    if (dowts) {
		do i = 1, npts {
		    if (n[i] > 0) {
			k = i - 1
			wt = wts[Memi[m[1]+k]]
			sum = Memr[d[1]+k] * wt
			sumwt = wt
			do j = 2, n[i] {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + Memr[d[j]+k] * wt
			    sumwt = sumwt + wt
			}
			average[i] = sum / sumwt
		    } else
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    if (n[i] > 0) {
			k = i - 1
			sum = Memr[d[1]+k]
			do j = 2, n[i]
			    sum = sum + Memr[d[j]+k]
			average[i] = sum / n[i]
		    } else
			average[i] = blank
		}
	    }
	}
end
