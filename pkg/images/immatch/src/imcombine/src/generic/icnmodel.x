# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	"../icombine.h"
include	"../icmask.h"


# IC_NMODEL -- Compute the quadrature average (or summed) noise model.
# Options include a weighted average/sum.

procedure ic_nmodels (d, m, n, nm, wts, nimages, npts, doblank, doaverage,
	average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image ID pointers
int	n[npts]			# Number of points
real	nm[3,nimages]		# Noise model parameters
real	wts[nimages]		# Weights
int	nimages			# Number of images
int	npts			# Number of output points per line
int	doblank			# Set blank values?
int	doaverage		# Do average?
real	average[npts]		# Average (returned)

int	i, j, k, n1
real	val, wt, sumwt
real	sum, zero
data	zero /0.0/

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
		    val = max (zero, Mems[d[1]+k])
		    val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
		    wt = wts[Memi[m[1]+k]]
		    sum = val * wt**2
		    do j = 2, n[i] {
			val = max (zero, Mems[d[j]+k])
			val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			wt = wts[Memi[m[j]+k]]
			sum = sum + val * wt**2
		    }
		    average[i] = sqrt(sum)
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    val = max (zero, Mems[d[1]+k])
		    val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
		    sum = val
		    do j = 2, n[i] {
			val = max (zero, Mems[d[j]+k])
			val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			sum = sum + val
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
			val = max (zero, Mems[d[1]+k])
			val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
			wt = wts[Memi[m[1]+k]]
			sum = val * wt**2
			sumwt = wt
			do j = 2, n1 {
			    val = max (zero, Mems[d[j]+k])
			    val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + val * wt**2
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sqrt(sum) / sumwt
			    else {
				val = max (zero, Mems[d[1]+k])
				val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
				sum = Mems[d[1]+k]**2
				do j = 2, n1 {
				    val = max (zero, Mems[d[j]+k])
				    val = nm[1,j] + val/nm[2,j] +
				        (val*nm[3,j])**2
				    sum = sum + val
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
			val = max (zero, Mems[d[1]+k])
			val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
			sum = val
			do j = 2, n1 {
			    val = max (zero, Mems[d[j]+k])
			    val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			    sum = sum + val
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

# IC_NMODEL -- Compute the quadrature average (or summed) noise model.
# Options include a weighted average/sum.

procedure ic_nmodeli (d, m, n, nm, wts, nimages, npts, doblank, doaverage,
	average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image ID pointers
int	n[npts]			# Number of points
real	nm[3,nimages]		# Noise model parameters
real	wts[nimages]		# Weights
int	nimages			# Number of images
int	npts			# Number of output points per line
int	doblank			# Set blank values?
int	doaverage		# Do average?
real	average[npts]		# Average (returned)

int	i, j, k, n1
real	val, wt, sumwt
real	sum, zero
data	zero /0.0/

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
		    val = max (zero, Memi[d[1]+k])
		    val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
		    wt = wts[Memi[m[1]+k]]
		    sum = val * wt**2
		    do j = 2, n[i] {
			val = max (zero, Memi[d[j]+k])
			val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			wt = wts[Memi[m[j]+k]]
			sum = sum + val * wt**2
		    }
		    average[i] = sqrt(sum)
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    val = max (zero, Memi[d[1]+k])
		    val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
		    sum = val
		    do j = 2, n[i] {
			val = max (zero, Memi[d[j]+k])
			val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			sum = sum + val
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
			val = max (zero, Memi[d[1]+k])
			val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
			wt = wts[Memi[m[1]+k]]
			sum = val * wt**2
			sumwt = wt
			do j = 2, n1 {
			    val = max (zero, Memi[d[j]+k])
			    val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + val * wt**2
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sqrt(sum) / sumwt
			    else {
				val = max (zero, Memi[d[1]+k])
				val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
				sum = Memi[d[1]+k]**2
				do j = 2, n1 {
				    val = max (zero, Memi[d[j]+k])
				    val = nm[1,j] + val/nm[2,j] +
				        (val*nm[3,j])**2
				    sum = sum + val
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
			val = max (zero, Memi[d[1]+k])
			val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
			sum = val
			do j = 2, n1 {
			    val = max (zero, Memi[d[j]+k])
			    val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			    sum = sum + val
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

# IC_NMODEL -- Compute the quadrature average (or summed) noise model.
# Options include a weighted average/sum.

procedure ic_nmodelr (d, m, n, nm, wts, nimages, npts, doblank, doaverage,
	average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image ID pointers
int	n[npts]			# Number of points
real	nm[3,nimages]		# Noise model parameters
real	wts[nimages]		# Weights
int	nimages			# Number of images
int	npts			# Number of output points per line
int	doblank			# Set blank values?
int	doaverage		# Do average?
real	average[npts]		# Average (returned)

int	i, j, k, n1
real	val, wt, sumwt
real	sum, zero
data	zero /0.0/

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
		    val = max (zero, Memr[d[1]+k])
		    val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
		    wt = wts[Memi[m[1]+k]]
		    sum = val * wt**2
		    do j = 2, n[i] {
			val = max (zero, Memr[d[j]+k])
			val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			wt = wts[Memi[m[j]+k]]
			sum = sum + val * wt**2
		    }
		    average[i] = sqrt(sum)
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    val = max (zero, Memr[d[1]+k])
		    val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
		    sum = val
		    do j = 2, n[i] {
			val = max (zero, Memr[d[j]+k])
			val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			sum = sum + val
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
			val = max (zero, Memr[d[1]+k])
			val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
			wt = wts[Memi[m[1]+k]]
			sum = val * wt**2
			sumwt = wt
			do j = 2, n1 {
			    val = max (zero, Memr[d[j]+k])
			    val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + val * wt**2
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sqrt(sum) / sumwt
			    else {
				val = max (zero, Memr[d[1]+k])
				val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
				sum = Memr[d[1]+k]**2
				do j = 2, n1 {
				    val = max (zero, Memr[d[j]+k])
				    val = nm[1,j] + val/nm[2,j] +
				        (val*nm[3,j])**2
				    sum = sum + val
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
			val = max (zero, Memr[d[1]+k])
			val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
			sum = val
			do j = 2, n1 {
			    val = max (zero, Memr[d[j]+k])
			    val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			    sum = sum + val
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

# IC_NMODEL -- Compute the quadrature average (or summed) noise model.
# Options include a weighted average/sum.

procedure ic_nmodeld (d, m, n, nm, wts, nimages, npts, doblank, doaverage,
	average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image ID pointers
int	n[npts]			# Number of points
real	nm[3,nimages]		# Noise model parameters
real	wts[nimages]		# Weights
int	nimages			# Number of images
int	npts			# Number of output points per line
int	doblank			# Set blank values?
int	doaverage		# Do average?
double	average[npts]		# Average (returned)

int	i, j, k, n1
real	val, wt, sumwt
double	sum, zero
data	zero /0.0D0/

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
		    val = max (zero, Memd[d[1]+k])
		    val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
		    wt = wts[Memi[m[1]+k]]
		    sum = val * wt**2
		    do j = 2, n[i] {
			val = max (zero, Memd[d[j]+k])
			val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			wt = wts[Memi[m[j]+k]]
			sum = sum + val * wt**2
		    }
		    average[i] = sqrt(sum)
		}
	    } else {
		do i = 1, npts {
		    k = i - 1
		    val = max (zero, Memd[d[1]+k])
		    val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
		    sum = val
		    do j = 2, n[i] {
			val = max (zero, Memd[d[j]+k])
			val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			sum = sum + val
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
			val = max (zero, Memd[d[1]+k])
			val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
			wt = wts[Memi[m[1]+k]]
			sum = val * wt**2
			sumwt = wt
			do j = 2, n1 {
			    val = max (zero, Memd[d[j]+k])
			    val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + val * wt**2
			    sumwt = sumwt + wt
			}
			if (doaverage == YES) {
			    if (sumwt > 0)
				average[i] = sqrt(sum) / sumwt
			    else {
				val = max (zero, Memd[d[1]+k])
				val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
				sum = Memd[d[1]+k]**2
				do j = 2, n1 {
				    val = max (zero, Memd[d[j]+k])
				    val = nm[1,j] + val/nm[2,j] +
				        (val*nm[3,j])**2
				    sum = sum + val
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
			val = max (zero, Memd[d[1]+k])
			val = nm[1,1] + val/nm[2,1] + (val*nm[3,1])**2
			sum = val
			do j = 2, n1 {
			    val = max (zero, Memd[d[j]+k])
			    val = nm[1,j] + val/nm[2,j] + (val*nm[3,j])**2
			    sum = sum + val
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

