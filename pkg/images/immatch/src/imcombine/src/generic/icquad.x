# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	"../icombine.h"
include	"../icmask.h"


# IC_QUAD -- Compute the quadrature average (or summed) image line.
# Options include a weighted average/sum.

procedure ic_quads (d, m, n, wts, nimages, npts, doblank, doaverage,
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
real	val, wt, sumwt
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
		    val = Mems[d[1]+k]
		    wt = wts[Memi[m[1]+k]]
		    sum = (val * wt) ** 2
		    do j = 2, n[i] {
			val = Mems[d[j]+k]
			wt = wts[Memi[m[j]+k]]
			sum = sum + (val * wt) ** 2
		    }
		    average[i] = sqrt(sum)
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    val = Mems[d[1]+k]
		    sum = val**2
		    do j = 2, n[i] {
			val = Mems[d[j]+k]
			sum = sum + val**2
		    }
		    if (doaverage == YES)
			average[i] = sqrt(sum) / n[i]
		    else
			average[i] = sqrt(sum)
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
			val = Mems[d[1]+k]
			wt = wts[Memi[m[1]+k]]
			sum = (val * wt) ** 2
			sumwt = wt
			do j = 2, n1 {
			    val = Mems[d[j]+k]
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + (val* wt) ** 2
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sqrt(sum) / sumwt
			    else {
				val = Mems[d[1]+k]
				sum = val**2
				do j = 2, n1 {
				    val = Mems[d[j]+k]
				    sum = sum + val**2
				}
				average[i] = sqrt(sum) / n1
			    }
			} else
			    average[i] = sqrt(sum)
		    } else if (doblank == YES)
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			val = Mems[d[1]+k]
			sum = val**2
			do j = 2, n1 {
			    val = Mems[d[j]+k]
			    sum = sum + val**2
			}
			if (doaverage == YES)
			    average[i] = sqrt(sum) / n1
			else
			    average[i] = sqrt(sum)
		    } else if (doblank == YES)
			average[i] = blank
		}
	    }
	}
end

# IC_QUAD -- Compute the quadrature average (or summed) image line.
# Options include a weighted average/sum.

procedure ic_quadi (d, m, n, wts, nimages, npts, doblank, doaverage,
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
real	val, wt, sumwt
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
		    val = Memi[d[1]+k]
		    wt = wts[Memi[m[1]+k]]
		    sum = (val * wt) ** 2
		    do j = 2, n[i] {
			val = Memi[d[j]+k]
			wt = wts[Memi[m[j]+k]]
			sum = sum + (val * wt) ** 2
		    }
		    average[i] = sqrt(sum)
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    val = Memi[d[1]+k]
		    sum = val**2
		    do j = 2, n[i] {
			val = Memi[d[j]+k]
			sum = sum + val**2
		    }
		    if (doaverage == YES)
			average[i] = sqrt(sum) / n[i]
		    else
			average[i] = sqrt(sum)
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
			val = Memi[d[1]+k]
			wt = wts[Memi[m[1]+k]]
			sum = (val * wt) ** 2
			sumwt = wt
			do j = 2, n1 {
			    val = Memi[d[j]+k]
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + (val* wt) ** 2
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sqrt(sum) / sumwt
			    else {
				val = Memi[d[1]+k]
				sum = val**2
				do j = 2, n1 {
				    val = Memi[d[j]+k]
				    sum = sum + val**2
				}
				average[i] = sqrt(sum) / n1
			    }
			} else
			    average[i] = sqrt(sum)
		    } else if (doblank == YES)
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			val = Memi[d[1]+k]
			sum = val**2
			do j = 2, n1 {
			    val = Memi[d[j]+k]
			    sum = sum + val**2
			}
			if (doaverage == YES)
			    average[i] = sqrt(sum) / n1
			else
			    average[i] = sqrt(sum)
		    } else if (doblank == YES)
			average[i] = blank
		}
	    }
	}
end

# IC_QUAD -- Compute the quadrature average (or summed) image line.
# Options include a weighted average/sum.

procedure ic_quadr (d, m, n, wts, nimages, npts, doblank, doaverage,
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
real	val, wt, sumwt
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
		    val = Memr[d[1]+k]
		    wt = wts[Memi[m[1]+k]]
		    sum = (val * wt) ** 2
		    do j = 2, n[i] {
			val = Memr[d[j]+k]
			wt = wts[Memi[m[j]+k]]
			sum = sum + (val * wt) ** 2
		    }
		    average[i] = sqrt(sum)
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    val = Memr[d[1]+k]
		    sum = val**2
		    do j = 2, n[i] {
			val = Memr[d[j]+k]
			sum = sum + val**2
		    }
		    if (doaverage == YES)
			average[i] = sqrt(sum) / n[i]
		    else
			average[i] = sqrt(sum)
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
			val = Memr[d[1]+k]
			wt = wts[Memi[m[1]+k]]
			sum = (val * wt) ** 2
			sumwt = wt
			do j = 2, n1 {
			    val = Memr[d[j]+k]
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + (val* wt) ** 2
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sqrt(sum) / sumwt
			    else {
				val = Memr[d[1]+k]
				sum = val**2
				do j = 2, n1 {
				    val = Memr[d[j]+k]
				    sum = sum + val**2
				}
				average[i] = sqrt(sum) / n1
			    }
			} else
			    average[i] = sqrt(sum)
		    } else if (doblank == YES)
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			val = Memr[d[1]+k]
			sum = val**2
			do j = 2, n1 {
			    val = Memr[d[j]+k]
			    sum = sum + val**2
			}
			if (doaverage == YES)
			    average[i] = sqrt(sum) / n1
			else
			    average[i] = sqrt(sum)
		    } else if (doblank == YES)
			average[i] = blank
		}
	    }
	}
end

# IC_QUAD -- Compute the quadrature average (or summed) image line.
# Options include a weighted average/sum.

procedure ic_quadd (d, m, n, wts, nimages, npts, doblank, doaverage,
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
real	val, wt, sumwt
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
		    val = Memd[d[1]+k]
		    wt = wts[Memi[m[1]+k]]
		    sum = (val * wt) ** 2
		    do j = 2, n[i] {
			val = Memd[d[j]+k]
			wt = wts[Memi[m[j]+k]]
			sum = sum + (val * wt) ** 2
		    }
		    average[i] = sqrt(sum)
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    val = Memd[d[1]+k]
		    sum = val**2
		    do j = 2, n[i] {
			val = Memd[d[j]+k]
			sum = sum + val**2
		    }
		    if (doaverage == YES)
			average[i] = sqrt(sum) / n[i]
		    else
			average[i] = sqrt(sum)
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
			val = Memd[d[1]+k]
			wt = wts[Memi[m[1]+k]]
			sum = (val * wt) ** 2
			sumwt = wt
			do j = 2, n1 {
			    val = Memd[d[j]+k]
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + (val* wt) ** 2
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sqrt(sum) / sumwt
			    else {
				val = Memd[d[1]+k]
				sum = val**2
				do j = 2, n1 {
				    val = Memd[d[j]+k]
				    sum = sum + val**2
				}
				average[i] = sqrt(sum) / n1
			    }
			} else
			    average[i] = sqrt(sum)
		    } else if (doblank == YES)
			average[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = abs(n[i])
		    if (n1 > 0) {
			k = i - 1
			val = Memd[d[1]+k]
			sum = val**2
			do j = 2, n1 {
			    val = Memd[d[j]+k]
			    sum = sum + val**2
			}
			if (doaverage == YES)
			    average[i] = sqrt(sum) / n1
			else
			    average[i] = sqrt(sum)
		    } else if (doblank == YES)
			average[i] = blank
		}
	    }
	}
end

