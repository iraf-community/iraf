# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	"../icombine.h"
include	"../icmask.h"


# IC_AVERAGE -- Compute the average (or summed) image line.
# Options include a weighted average/sum.

procedure ic_averages (d, m, n, wts, nimages, npts, doblank, doaverage,
	average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image ID pointers
int	n[npts]			# Number of points
real	wts[nimages]		# Weights
int	nimages			# Number of images
int	npts			# Number of output points per line
int	doblank			# Set blank values?
int	doaverage		# Do average?
real	average[npts]		# Average (returned)

int	i, j, k, n1
real	sumwt, wt
real	sum

include	"../icombine.com"

begin
	# If no data has been excluded do the average/sum without checking
	# the number of points and using the fact that the weights are
	# normalized.  If all the data has been excluded set the average/sum
	# to the blank value if requested.

	if (dflag == D_ALL) {
	    if (dowts && doaverage == YES) {
		do i = 1, npts {
		    k = i - 1
		    wt = wts[Memi[m[1]+k]]
		    sum = Mems[d[1]+k] * wt
		    do j = 2, n[i] {
			wt = wts[Memi[m[j]+k]]
			sum = sum + Mems[d[j]+k] * wt
		    }
		    average[i] = sum
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    sum = Mems[d[1]+k]
		    do j = 2, n[i]
			sum = sum + Mems[d[j]+k]
		    if (doaverage == YES)
			average[i] = sum / n[i]
		    else
			average[i] = sum
		}
	    }
	} else if (dflag == D_NONE) {
	    if (doblank == YES) {
		do i = 1, npts
		    average[i] = blank
	    }
	} else {
	    if (dowts && doaverage == YES) {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			wt = wts[Memi[m[1]+k]]
			sum = Mems[d[1]+k] * wt
			sumwt = wt
			do j = 2, n1 {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + Mems[d[j]+k] * wt
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sum / sumwt
			    else {
				sum = Mems[d[1]+k]
				do j = 2, n1
				    sum = sum + Mems[d[j]+k]
				average[i] = sum / n1
			    }
			} else
			    average[i] = sum
		    } else if (doblank == YES)
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			sum = Mems[d[1]+k]
			do j = 2, n1
			    sum = sum + Mems[d[j]+k]
			if (doaverage == YES)
			    average[i] = sum / n1
			else
			    average[i] = sum
		    } else if (doblank == YES)
			average[i] = blank
		}
	    }
	}
end

# IC_AVERAGE -- Compute the average (or summed) image line.
# Options include a weighted average/sum.

procedure ic_averagei (d, m, n, wts, nimages, npts, doblank, doaverage,
	average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image ID pointers
int	n[npts]			# Number of points
real	wts[nimages]		# Weights
int	nimages			# Number of images
int	npts			# Number of output points per line
int	doblank			# Set blank values?
int	doaverage		# Do average?
real	average[npts]		# Average (returned)

int	i, j, k, n1
real	sumwt, wt
real	sum

include	"../icombine.com"

begin
	# If no data has been excluded do the average/sum without checking
	# the number of points and using the fact that the weights are
	# normalized.  If all the data has been excluded set the average/sum
	# to the blank value if requested.

	if (dflag == D_ALL) {
	    if (dowts && doaverage == YES) {
		do i = 1, npts {
		    k = i - 1
		    wt = wts[Memi[m[1]+k]]
		    sum = Memi[d[1]+k] * wt
		    do j = 2, n[i] {
			wt = wts[Memi[m[j]+k]]
			sum = sum + Memi[d[j]+k] * wt
		    }
		    average[i] = sum
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    sum = Memi[d[1]+k]
		    do j = 2, n[i]
			sum = sum + Memi[d[j]+k]
		    if (doaverage == YES)
			average[i] = sum / n[i]
		    else
			average[i] = sum
		}
	    }
	} else if (dflag == D_NONE) {
	    if (doblank == YES) {
		do i = 1, npts
		    average[i] = blank
	    }
	} else {
	    if (dowts && doaverage == YES) {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			wt = wts[Memi[m[1]+k]]
			sum = Memi[d[1]+k] * wt
			sumwt = wt
			do j = 2, n1 {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + Memi[d[j]+k] * wt
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sum / sumwt
			    else {
				sum = Memi[d[1]+k]
				do j = 2, n1
				    sum = sum + Memi[d[j]+k]
				average[i] = sum / n1
			    }
			} else
			    average[i] = sum
		    } else if (doblank == YES)
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			sum = Memi[d[1]+k]
			do j = 2, n1
			    sum = sum + Memi[d[j]+k]
			if (doaverage == YES)
			    average[i] = sum / n1
			else
			    average[i] = sum
		    } else if (doblank == YES)
			average[i] = blank
		}
	    }
	}
end

# IC_AVERAGE -- Compute the average (or summed) image line.
# Options include a weighted average/sum.

procedure ic_averager (d, m, n, wts, nimages, npts, doblank, doaverage,
	average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image ID pointers
int	n[npts]			# Number of points
real	wts[nimages]		# Weights
int	nimages			# Number of images
int	npts			# Number of output points per line
int	doblank			# Set blank values?
int	doaverage		# Do average?
real	average[npts]		# Average (returned)

int	i, j, k, n1
real	sumwt, wt
real	sum

include	"../icombine.com"

begin
	# If no data has been excluded do the average/sum without checking
	# the number of points and using the fact that the weights are
	# normalized.  If all the data has been excluded set the average/sum
	# to the blank value if requested.

	if (dflag == D_ALL) {
	    if (dowts && doaverage == YES) {
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
		    if (doaverage == YES)
			average[i] = sum / n[i]
		    else
			average[i] = sum
		}
	    }
	} else if (dflag == D_NONE) {
	    if (doblank == YES) {
		do i = 1, npts
		    average[i] = blank
	    }
	} else {
	    if (dowts && doaverage == YES) {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			wt = wts[Memi[m[1]+k]]
			sum = Memr[d[1]+k] * wt
			sumwt = wt
			do j = 2, n1 {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + Memr[d[j]+k] * wt
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sum / sumwt
			    else {
				sum = Memr[d[1]+k]
				do j = 2, n1
				    sum = sum + Memr[d[j]+k]
				average[i] = sum / n1
			    }
			} else
			    average[i] = sum
		    } else if (doblank == YES)
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			sum = Memr[d[1]+k]
			do j = 2, n1
			    sum = sum + Memr[d[j]+k]
			if (doaverage == YES)
			    average[i] = sum / n1
			else
			    average[i] = sum
		    } else if (doblank == YES)
			average[i] = blank
		}
	    }
	}
end

# IC_AVERAGE -- Compute the average (or summed) image line.
# Options include a weighted average/sum.

procedure ic_averaged (d, m, n, wts, nimages, npts, doblank, doaverage,
	average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image ID pointers
int	n[npts]			# Number of points
real	wts[nimages]		# Weights
int	nimages			# Number of images
int	npts			# Number of output points per line
int	doblank			# Set blank values?
int	doaverage		# Do average?
double	average[npts]		# Average (returned)

int	i, j, k, n1
real	sumwt, wt
double	sum

include	"../icombine.com"

begin
	# If no data has been excluded do the average/sum without checking
	# the number of points and using the fact that the weights are
	# normalized.  If all the data has been excluded set the average/sum
	# to the blank value if requested.

	if (dflag == D_ALL) {
	    if (dowts && doaverage == YES) {
		do i = 1, npts {
		    k = i - 1
		    wt = wts[Memi[m[1]+k]]
		    sum = Memd[d[1]+k] * wt
		    do j = 2, n[i] {
			wt = wts[Memi[m[j]+k]]
			sum = sum + Memd[d[j]+k] * wt
		    }
		    average[i] = sum
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    sum = Memd[d[1]+k]
		    do j = 2, n[i]
			sum = sum + Memd[d[j]+k]
		    if (doaverage == YES)
			average[i] = sum / n[i]
		    else
			average[i] = sum
		}
	    }
	} else if (dflag == D_NONE) {
	    if (doblank == YES) {
		do i = 1, npts
		    average[i] = blank
	    }
	} else {
	    if (dowts && doaverage == YES) {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			wt = wts[Memi[m[1]+k]]
			sum = Memd[d[1]+k] * wt
			sumwt = wt
			do j = 2, n1 {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + Memd[d[j]+k] * wt
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sum / sumwt
			    else {
				sum = Memd[d[1]+k]
				do j = 2, n1
				    sum = sum + Memd[d[j]+k]
				average[i] = sum / n1
			    }
			} else
			    average[i] = sum
		    } else if (doblank == YES)
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			sum = Memd[d[1]+k]
			do j = 2, n1
			    sum = sum + Memd[d[j]+k]
			if (doaverage == YES)
			    average[i] = sum / n1
			else
			    average[i] = sum
		    } else if (doblank == YES)
			average[i] = blank
		}
	    }
	}
end

