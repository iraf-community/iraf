# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"

define	MINCLIP		3	# Mininum number of images for algorithm


# IC_ACCDCLIP -- Reject pixels using CCD noise parameters about the average

procedure ic_accdclipr (d, m, n, scales, zeros, rn, g, npts, average)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[ARB]		# Scales
real	zeros[ARB]		# Zeros
real	rn[ARB]			# Read noise squared in DN
real	g[ARB]			# Gain
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, n1, n2
real	d1, low, high, sum, a, s, r
pointer	dp, mp1, mp2

include	"../icombine.com"

begin
	# If there are no pixels go on to the combining.  Since the unweighted
	# average is computed here possibly skip the combining later.

	if (dflag == D_NONE) {
	    docombine = true
	    return
	} else if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	# There must be at least two pixels for rejection.  The initial
	# average is the low/high rejected average except in the case of
	# just two pixels.  The rejections are iterated and the average
	# is recomputed.  Corrections for scaling may be performed.
	# Depending on other flags the image IDs may also need to be adjusted.

	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (n1 < 2) {
		if (!docombine) {
		    if (n1 == 1)
			average[i] = Memr[d[1]+k]
		    else
			average[i] = blank
		}
		next
	    }

	    repeat {
		if (n1 == 2) {
		    sum = (Memr[d[1]+k] + Memr[d[2]+k])
		    a = sum / 2
		} else {
		    low = Memr[d[1]+k]
		    high = Memr[d[2]+k]
		    if (low > high) {
			d1 = low
			low = high
			high = d1
		    }
		    sum = 0.
		    do j = 3, n1 {
		       d1 = Memr[d[j]+k]
		       if (d1 < low) {
			   sum = sum + low
			   low = d1
			} else if (d1 > high) {
			    sum = sum + high
			    high = d1
			} else
			    sum = sum + d1
		    }
		    a = sum / (n1 - 2)
		    sum = sum + low + high
		}
		n2 = n1
		if (doscale1) {
		    for (j=1; j<=n1; j=j+1) {
			dp = d[j] + k
			mp1 = m[j] + k

			l = Memi[mp1]
			s = scales[l]
			d1 = max (0., s * (a - zeros[l]))
			s = sqrt (rn[l] + d1 / g[l]) / s

			d1 = Memr[dp]
			r = (d1 - a) / s
			if (r < -lsigma || r > hsigma) {
			    if (j < n1) {
				Memr[dp] = Memr[d[n1]+k]
				if (grow > 0) {
				    mp2 = m[n1] + k
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = l
				} else
				    Memi[mp1] = Memi[m[n1]+k]
				j = j - 1
			    }
			    sum = sum - d1
			    n1 = n1 - 1
			}
		    }
		} else {
		    if (!keepids)
			s = sqrt (rn[1] + max (0., a) / g[1])
		    for (j=1; j<=n1; j=j+1) {
			if (keepids) {
			    l = Memi[m[j]+k]
			    s = sqrt (rn[l] + max (0., a) / g[l])
			}
			dp = d[j] + k
			d1 = Memr[dp]
			r = (d1 - a) / s
			if (r < -lsigma || r > hsigma) {
			    if (j < n1) {
				Memr[dp] = Memr[d[n1]+k]
				if (keepids) {
				    if (grow > 0) {
					mp1 = m[j] + k
					mp2 = m[n1] + k
					l = Memi[mp1]
					Memi[mp1] = Memi[mp2]
					Memi[mp2] = l
				    } else
					Memi[m[j]+k] = Memi[m[n1]+k]
				}
				j = j - 1
			    }
			    sum = sum - d1
			    n1 = n1 - 1
			}
		    }
		}
	    } until (n1 == n2 || n1 < 3)

	    n[i] = n1
	    if (!docombine)
		if (n1 > 0)
		    average[i] = sum / n1
		else
		    average[i] = blank
	}

	# Check if the data flag has to be reset for rejected pixels
	if (dflag == D_ALL) {
	    n1 = n[1]
	    do i = 1, npts {
		if (n[i] != n1) {
		    dflag = D_MIX
		    break
		}
	    }
	}
end


# IC_CCDCLIP -- Reject pixels using CCD noise parameters about the median

procedure ic_mccdclipr (d, m, n, scales, zeros, rn, g, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	rn[nimages]		# Read noise squared in DN
real	g[nimages]		# Gain
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh
real	r, s
pointer	mp1, mp2
real	med

include	"../icombine.com"

begin
	# There must be at  least MINCLIP pixels.
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Compute median and sigma and iteratively clip.
	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    nl = 1
	    nh = n1

	    repeat {
		n2 = n1
		n3 = nl + n1 / 2

		if (n1 == 0)
		    med = blank
		else if (mod (n1, 2) == 0)
		    med = (Memr[d[n3-1]+k] + Memr[d[n3]+k]) / 2.
		else
		    med = Memr[d[n3]+k]

		if (n1 >= MINCLIP) {
		    if (doscale1) {
			for (; nl <= n2; nl = nl + 1) {
			    l = Memi[m[nl]+k]
			    s = scales[l]
			    r = max (0., s * (med - zeros[l]))
			    s = sqrt (rn[l] + r / g[l]) / s
			    r = (med - Memr[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    n1 = n1 - 1
			}
			for (; nh >= 1; nh = nh - 1) {
			    l = Memi[m[nl]+k]
			    s = scales[l]
			    r = max (0., s * (med - zeros[l]))
			    s = sqrt (rn[l] + r / g[l]) / s
			    r = (Memr[d[nh]+k] - med) / s
			    if (r <= hsigma)
				break
			    n1 = n1 - 1
			}
		    } else {
			if (!keepids)
			    s = sqrt (rn[1] + max (0., med) / g[1])
			for (; nl <= n2; nl = nl + 1) {
			    if (keepids) {
				l = Memi[m[nl]+k]
				s = sqrt (rn[l] + max (0., med) / g[l])
			    }
			    r = (med - Memr[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    n1 = n1 - 1
			}
			for (; nh >= 1; nh = nh - 1) {
			    if (keepids) {
				l = Memi[m[nh]+k]
				s = sqrt (rn[l] + max (0., med) / g[l])
			    }
			    r = (Memr[d[nh]+k] -  med) / s
			    if (r <= hsigma)
				break
			    n1 = n1 - 1
			}
		    }
		}
	    } until (n1 == n2 || n1 < MINCLIP)

	    # Only set median and reorder if needed
	    n[i] = n1
	    if (combine == MEDIAN)
		median[i] = med
	    if (nl > 1 && (combine != MEDIAN || grow > 0)) {
		j = n1 + 1
		if (keepids) {
		    do l = 1, nl-1 {
			Memr[d[l]+k] = Memr[d[j]+k]
			if (grow > 0) {
			    mp1 = m[l] + k
			    mp2 = m[j] + k
			    id = Memi[mp1]
			    Memi[mp1] = Memi[mp2]
			    Memi[mp2] = id
			} else
			    Memi[m[l]+k] = Memi[m[j]+k]
			j = j + 1
		    }
		} else {
		    do l = 1, nl - 1 {
			Memr[d[l]+k] = Memr[d[j]+k]
			j = j + 1
		    }
		}
	    }
	}

	# Check if data flag needs to be reset for rejected pixels
	if (dflag == D_ALL) {
	    n1 = n[1]
	    do i = 1, npts {
		if (n[i] != n1) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	# Signal that the median is computed here
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true
end
