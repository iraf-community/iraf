# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"../icombine.h"


# IC_SIGMA -- Compute the sigma image line.
# The estimated sigma includes a correction for the finite population.
# Weights are used if desired.

procedure ic_sigmas (d, m, n, wts, npts, average, sigma)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image ID pointers
int	n[npts]			# Number of points
real	wts[ARB]		# Weights
int	npts			# Number of output points per line
real	average[npts]		# Average
real	sigma[npts]		# Sigma line (returned)

int	i, j, k, n1
real	wt, sigcor, sumwt
real	a, sum

include	"../icombine.com"

begin
	if (dflag == D_ALL) {
	    n1 = n[1]
	    if (dowts) {
		if (n1 > 1)
		    sigcor = real (n1) / real (n1 - 1)
		else
		    sigcor = 1.
		do i = 1, npts {
		    k = i - 1
		    a = average[i]
		    wt = wts[Memi[m[1]+k]]
		    sum = (Mems[d[1]+k] - a) ** 2 * wt
		    do j = 2, n1 {
			wt = wts[Memi[m[j]+k]]
			sum = sum + (Mems[d[j]+k] - a) ** 2 * wt
		    }
		    sigma[i] = sqrt (sum * sigcor)
		}
	    } else {
		if (n1 > 1)
		    sigcor = 1. / real (n1 - 1)
		else
		    sigcor = 1.
		do i = 1, npts {
		    k = i - 1
		    a = average[i]
		    sum = (Mems[d[1]+k] - a) ** 2
		    do j = 2, n1
			sum = sum + (Mems[d[j]+k] - a) ** 2
		    sigma[i] = sqrt (sum * sigcor)
		}
	    }
	} else if (dflag == D_NONE) {
	    do i = 1, npts
		sigma[i] = blank
	} else {
	    if (dowts) {
		do i = 1, npts {
		    n1 = n[i]
		    if (n1 > 0) {
			k = i - 1
			if (n1 > 1)
			    sigcor = real (n1) / real (n1 -1)
			else
			    sigcor = 1
			a = average[i]
			wt = wts[Memi[m[1]+k]]
			sum = (Mems[d[1]+k] - a) ** 2 * wt
			sumwt = wt
			do j = 2, n1 {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + (Mems[d[j]+k] - a) ** 2 * wt
			    sumwt = sumwt + wt
			}
			if (sumwt > 0)
			    sigma[i] = sqrt (sum / sumwt * sigcor)
			else {
			    sum = (Mems[d[1]+k] - a) ** 2
			    do j = 2, n1
				sum = sum + (Mems[d[j]+k] - a) ** 2
			    sigma[i] = sqrt (sum / n1 * sigcor)
			}
		    } else
			sigma[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = n[i]
		    if (n1 > 0) {
			k = i - 1
			if (n1 > 1)
			    sigcor = 1. / real (n1 - 1)
			else
			    sigcor = 1.
			a = average[i]
			sum = (Mems[d[1]+k] - a) ** 2
			do j = 2, n1
			    sum = sum + (Mems[d[j]+k] - a) ** 2
			sigma[i] = sqrt (sum * sigcor)
		    } else
			sigma[i] = blank
		}
	    }
	}
end

# IC_SIGMA -- Compute the sigma image line.
# The estimated sigma includes a correction for the finite population.
# Weights are used if desired.

procedure ic_sigmai (d, m, n, wts, npts, average, sigma)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image ID pointers
int	n[npts]			# Number of points
real	wts[ARB]		# Weights
int	npts			# Number of output points per line
real	average[npts]		# Average
real	sigma[npts]		# Sigma line (returned)

int	i, j, k, n1
real	wt, sigcor, sumwt
real	a, sum

include	"../icombine.com"

begin
	if (dflag == D_ALL) {
	    n1 = n[1]
	    if (dowts) {
		if (n1 > 1)
		    sigcor = real (n1) / real (n1 - 1)
		else
		    sigcor = 1.
		do i = 1, npts {
		    k = i - 1
		    a = average[i]
		    wt = wts[Memi[m[1]+k]]
		    sum = (Memi[d[1]+k] - a) ** 2 * wt
		    do j = 2, n1 {
			wt = wts[Memi[m[j]+k]]
			sum = sum + (Memi[d[j]+k] - a) ** 2 * wt
		    }
		    sigma[i] = sqrt (sum * sigcor)
		}
	    } else {
		if (n1 > 1)
		    sigcor = 1. / real (n1 - 1)
		else
		    sigcor = 1.
		do i = 1, npts {
		    k = i - 1
		    a = average[i]
		    sum = (Memi[d[1]+k] - a) ** 2
		    do j = 2, n1
			sum = sum + (Memi[d[j]+k] - a) ** 2
		    sigma[i] = sqrt (sum * sigcor)
		}
	    }
	} else if (dflag == D_NONE) {
	    do i = 1, npts
		sigma[i] = blank
	} else {
	    if (dowts) {
		do i = 1, npts {
		    n1 = n[i]
		    if (n1 > 0) {
			k = i - 1
			if (n1 > 1)
			    sigcor = real (n1) / real (n1 -1)
			else
			    sigcor = 1
			a = average[i]
			wt = wts[Memi[m[1]+k]]
			sum = (Memi[d[1]+k] - a) ** 2 * wt
			sumwt = wt
			do j = 2, n1 {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + (Memi[d[j]+k] - a) ** 2 * wt
			    sumwt = sumwt + wt
			}
			if (sumwt > 0)
			    sigma[i] = sqrt (sum / sumwt * sigcor)
			else {
			    sum = (Memi[d[1]+k] - a) ** 2
			    do j = 2, n1
				sum = sum + (Memi[d[j]+k] - a) ** 2
			    sigma[i] = sqrt (sum / n1 * sigcor)
			}
		    } else
			sigma[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = n[i]
		    if (n1 > 0) {
			k = i - 1
			if (n1 > 1)
			    sigcor = 1. / real (n1 - 1)
			else
			    sigcor = 1.
			a = average[i]
			sum = (Memi[d[1]+k] - a) ** 2
			do j = 2, n1
			    sum = sum + (Memi[d[j]+k] - a) ** 2
			sigma[i] = sqrt (sum * sigcor)
		    } else
			sigma[i] = blank
		}
	    }
	}
end

# IC_SIGMA -- Compute the sigma image line.
# The estimated sigma includes a correction for the finite population.
# Weights are used if desired.

procedure ic_sigmar (d, m, n, wts, npts, average, sigma)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image ID pointers
int	n[npts]			# Number of points
real	wts[ARB]		# Weights
int	npts			# Number of output points per line
real	average[npts]		# Average
real	sigma[npts]		# Sigma line (returned)

int	i, j, k, n1
real	wt, sigcor, sumwt
real	a, sum

include	"../icombine.com"

begin
	if (dflag == D_ALL) {
	    n1 = n[1]
	    if (dowts) {
		if (n1 > 1)
		    sigcor = real (n1) / real (n1 - 1)
		else
		    sigcor = 1.
		do i = 1, npts {
		    k = i - 1
		    a = average[i]
		    wt = wts[Memi[m[1]+k]]
		    sum = (Memr[d[1]+k] - a) ** 2 * wt
		    do j = 2, n1 {
			wt = wts[Memi[m[j]+k]]
			sum = sum + (Memr[d[j]+k] - a) ** 2 * wt
		    }
		    sigma[i] = sqrt (sum * sigcor)
		}
	    } else {
		if (n1 > 1)
		    sigcor = 1. / real (n1 - 1)
		else
		    sigcor = 1.
		do i = 1, npts {
		    k = i - 1
		    a = average[i]
		    sum = (Memr[d[1]+k] - a) ** 2
		    do j = 2, n1
			sum = sum + (Memr[d[j]+k] - a) ** 2
		    sigma[i] = sqrt (sum * sigcor)
		}
	    }
	} else if (dflag == D_NONE) {
	    do i = 1, npts
		sigma[i] = blank
	} else {
	    if (dowts) {
		do i = 1, npts {
		    n1 = n[i]
		    if (n1 > 0) {
			k = i - 1
			if (n1 > 1)
			    sigcor = real (n1) / real (n1 -1)
			else
			    sigcor = 1
			a = average[i]
			wt = wts[Memi[m[1]+k]]
			sum = (Memr[d[1]+k] - a) ** 2 * wt
			sumwt = wt
			do j = 2, n1 {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + (Memr[d[j]+k] - a) ** 2 * wt
			    sumwt = sumwt + wt
			}
			if (sumwt > 0)
			    sigma[i] = sqrt (sum / sumwt * sigcor)
			else {
			    sum = (Memr[d[1]+k] - a) ** 2
			    do j = 2, n1
				sum = sum + (Memr[d[j]+k] - a) ** 2
			    sigma[i] = sqrt (sum / n1 * sigcor)
			}
		    } else
			sigma[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = n[i]
		    if (n1 > 0) {
			k = i - 1
			if (n1 > 1)
			    sigcor = 1. / real (n1 - 1)
			else
			    sigcor = 1.
			a = average[i]
			sum = (Memr[d[1]+k] - a) ** 2
			do j = 2, n1
			    sum = sum + (Memr[d[j]+k] - a) ** 2
			sigma[i] = sqrt (sum * sigcor)
		    } else
			sigma[i] = blank
		}
	    }
	}
end

# IC_SIGMA -- Compute the sigma image line.
# The estimated sigma includes a correction for the finite population.
# Weights are used if desired.

procedure ic_sigmad (d, m, n, wts, npts, average, sigma)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image ID pointers
int	n[npts]			# Number of points
real	wts[ARB]		# Weights
int	npts			# Number of output points per line
double	average[npts]		# Average
double	sigma[npts]		# Sigma line (returned)

int	i, j, k, n1
real	wt, sigcor, sumwt
double	a, sum

include	"../icombine.com"

begin
	if (dflag == D_ALL) {
	    n1 = n[1]
	    if (dowts) {
		if (n1 > 1)
		    sigcor = real (n1) / real (n1 - 1)
		else
		    sigcor = 1.
		do i = 1, npts {
		    k = i - 1
		    a = average[i]
		    wt = wts[Memi[m[1]+k]]
		    sum = (Memd[d[1]+k] - a) ** 2 * wt
		    do j = 2, n1 {
			wt = wts[Memi[m[j]+k]]
			sum = sum + (Memd[d[j]+k] - a) ** 2 * wt
		    }
		    sigma[i] = sqrt (sum * sigcor)
		}
	    } else {
		if (n1 > 1)
		    sigcor = 1. / real (n1 - 1)
		else
		    sigcor = 1.
		do i = 1, npts {
		    k = i - 1
		    a = average[i]
		    sum = (Memd[d[1]+k] - a) ** 2
		    do j = 2, n1
			sum = sum + (Memd[d[j]+k] - a) ** 2
		    sigma[i] = sqrt (sum * sigcor)
		}
	    }
	} else if (dflag == D_NONE) {
	    do i = 1, npts
		sigma[i] = blank
	} else {
	    if (dowts) {
		do i = 1, npts {
		    n1 = n[i]
		    if (n1 > 0) {
			k = i - 1
			if (n1 > 1)
			    sigcor = real (n1) / real (n1 -1)
			else
			    sigcor = 1
			a = average[i]
			wt = wts[Memi[m[1]+k]]
			sum = (Memd[d[1]+k] - a) ** 2 * wt
			sumwt = wt
			do j = 2, n1 {
			    wt = wts[Memi[m[j]+k]]
			    sum = sum + (Memd[d[j]+k] - a) ** 2 * wt
			    sumwt = sumwt + wt
			}
			if (sumwt > 0)
			    sigma[i] = sqrt (sum / sumwt * sigcor)
			else {
			    sum = (Memd[d[1]+k] - a) ** 2
			    do j = 2, n1
				sum = sum + (Memd[d[j]+k] - a) ** 2
			    sigma[i] = sqrt (sum / n1 * sigcor)
			}
		    } else
			sigma[i] = blank
		}
	    } else {
		do i = 1, npts {
		    n1 = n[i]
		    if (n1 > 0) {
			k = i - 1
			if (n1 > 1)
			    sigcor = 1. / real (n1 - 1)
			else
			    sigcor = 1.
			a = average[i]
			sum = (Memd[d[1]+k] - a) ** 2
			do j = 2, n1
			    sum = sum + (Memd[d[j]+k] - a) ** 2
			sigma[i] = sqrt (sum * sigcor)
		    } else
			sigma[i] = blank
		}
	    }
	}
end

