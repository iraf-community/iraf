# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"

define	MINCLIP		3	# Minimum number of images for this algorithm


# IC_AAVSIGCLIP -- Reject pixels using an average sigma about the average
# The average sigma is normalized by the expected poisson sigma.

procedure ic_aavsigclipr (d, m, n, scales, zeros, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
real	d1, low, high, sum, a, s, s1, r, one
data	one /1.0/
pointer	sp, sums, resid, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining.
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	}

	call smark (sp)
	call salloc (sums, npts, TY_REAL)
	call salloc (resid, nimages+1, TY_REAL)

	# Since the unweighted average is computed here possibly skip combining
	if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	# Compute the unweighted average with the high and low rejected and
	# the poisson scaled average sigma.  There must be at least three
	# pixels at each point to define the average and contributions to
	# the mean sigma.  Corrections for differences in the image
	# scale factors are selected by the doscale1 flag.

	nin = n[1]
	s = 0.
	n2 = 0
	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (n1 < 3)
		next

	    # Unweighted average with the high and low rejected
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

	    # Poisson scaled sigma accumulation
	    if (doscale1) {
		do j = 1, n1 {
		    dp1 = d[j] + k
		    mp1 = m[j] + k

		    d1 = Memr[dp1]
		    l = Memi[mp1]
		    s1 = max (one, (a + zeros[l]) /  scales[l])
		    s = s + (d1 - a) ** 2 / s1
		}
	    } else {
		s1 = max (one, a)
		do j = 1, n1
		    s = s + (Memr[d[j]+k] - a) ** 2 / s1
	    }
	    n2 = n2 + n1

	    # Save the average and sum for later.
	    average[i] = a
	    Memr[sums+k] = sum
	}

	# Here is the final sigma.
	if (n2 > 1)
	    s = sqrt (s / (n2 - 1))

	# Reject pixels and compute the final average (if needed).
	# There must be at least three pixels at each point for rejection.
	# Iteratively scale the mean sigma and reject pixels
	# Compact the data and keep track of the image IDs if needed.

	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    if (n1 <= max (2, maxkeep)) {
		if (!docombine) {
		    if (n1 == 0)
			average[i] = blank
		    else {
			sum = Memr[d[1]+k]
			do j = 2, n1
			    sum = sum + Memr[d[j]+k]
			average[i] = sum / n1
		    }
		}
		next
	    }

	    a = average[i]
	    sum = Memr[sums+k]

	    repeat {
		n2 = n1
		if (s > 0.) {
		    if (doscale1) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    mp1 = m[j] + k

			    d1 = Memr[dp1]
			    l = Memi[mp1]
			    s1 = s * sqrt (max (one, (a+zeros[l]) /  scales[l]))
			    r = (d1 - a) / s1
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs(r)
				if (j < n1) {
				    dp2 = d[n1] + k
				    Memr[dp1] = Memr[dp2]
				    Memr[dp2] = d1
				    mp2 = m[n1] + k
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = l
				    j = j - 1
				}
				sum = sum - d1
				n1 = n1 - 1
			    }
			}
		    } else {
			s1 = s * sqrt (max (one, a))
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    d1 = Memr[dp1]
			    r = (d1 - a) / s1
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs(r)
				if (j < n1) {
				    dp2 = d[n1] + k
				    Memr[dp1] = Memr[dp2]
				    Memr[dp2] = d1
				    if (keepids) {
					mp1 = m[j] + k
					mp2 = m[n1] + k
					l = Memi[mp1]
					Memi[mp1] = Memi[mp2]
					Memi[mp2] = l
				    }
				    j = j - 1
				}
				sum = sum - d1
				n1 = n1 - 1
			    }
			}
		    }
		}
		if (n1 > 1)
		    a = sum / n1
	    } until (n1 == n2 || n1 <= max (2, maxkeep))

	    # If too many are rejected add some back in.
	    # Pixels with equal residuals are added together.
	    if (n1 < maxkeep) {
		nk = maxkeep
		if (doscale1) {
		    for (j=n1+1; j<=nk; j=j+1) {
			dp1 = d[j] + k
			mp1 = m[j] + k
			r = Memr[resid+j]
			jj = 0
			do l = j+1, n2 {
			    s = Memr[resid+l]
			    if (s < r + TOL) {
				if (s > r - TOL)
				    jj = jj + 1
				else {
				    jj = 0
				    Memr[resid+l] = r
				    r = s
				    dp2 = d[l] + k
				    d1 = Memr[dp1]
				    Memr[dp1] = Memr[dp2]
				    Memr[dp2] = d1
				    mp2 = m[l] + k
				    s = Memi[mp1]
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = s
				}
			    }
			}
			sum = sum + Memr[dp1]
			n1 = n1 + 1
			nk = max (nk, j+jj)
		    }
		} else {
		    for (j=n1+1; j<=nk; j=j+1) {
			dp1 = d[j] + k
			r = Memr[resid+j]
			jj = 0
			do l = j+1, n2 {
			    s = Memr[resid+l]
			    if (s < r + TOL) {
				if (s > r - TOL)
				    jj = jj + 1
				else {
				    jj = 0
				    Memr[resid+l] = r
				    r = s
				    dp2 = d[l] + k
				    d1 = Memr[dp1]
				    Memr[dp1] = Memr[dp2]
				    Memr[dp2] = d1
				    if (keepids) {
					mp1 = m[j] + k
					mp2 = m[l] + k
					s = Memi[mp1]
					Memi[mp1] = Memi[mp2]
					Memi[mp2] = s
				    }
				}
			    }
			}
			sum = sum + Memr[dp1]
			n1 = n1 + 1
			nk = max (nk, j+jj)
		    }
		}
		if (n1 > 1)
		    a = sum / n1
	    }

	    # Save the average if needed.
	    n[i] = n1
	    if (!docombine) {
		if (n1 > 0)
		    average[i] = a
		else
		    average[i] = blank
	    }
	}

	# Check if the data flag has to be reset for rejected pixels
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (n[i] != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_MAVSIGCLIP -- Reject pixels using an average sigma about the median
# The average sigma is normalized by the expected poisson sigma.

procedure ic_mavsigclipr (d, m, n, scales, zeros, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
pointer	sp, resid, mp1, mp2
real	med, low, high, r, s, s1, one
data	one /1.0/

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining.
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	}

	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)

	# Compute the poisson scaled average sigma about the median.
	# There must be at least three pixels at each point to define
	# the mean sigma.  Corrections for differences in the image
	# scale factors are selected by the doscale1 flag.

	s = 0.
	n2 = 0
	nin = n[1]
	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (n1 < 3) {
		if (n1 == 0)
		    median[i] = blank
		else if (n1 == 1)
		    median[i] = Memr[d[1]+k]
		else {
		    low = Memr[d[1]+k]
		    high = Memr[d[2]+k]
		    median[i] = (low + high) / 2.
		}
		next
	    }

	    # Median
	    n3 = 1 + n1 / 2
	    if (mod (n1, 2) == 0) {
		low = Memr[d[n3-1]+k]
		high = Memr[d[n3]+k]
		med = (low + high) / 2.
	    } else
		med = Memr[d[n3]+k]

	    # Poisson scaled sigma accumulation
	    if (doscale1) {
		do j = 1, n1 {
		    l = Memi[m[j]+k]
		    s1 = max (one, (med + zeros[l]) /  scales[l])
		    s = s + (Memr[d[j]+k] - med) ** 2 / s1
		}
	    } else {
		s1 = max (one, med)
		do j = 1, n1
		    s = s + (Memr[d[j]+k] - med) ** 2 / s1
	    }
	    n2 = n2 + n1

	    # Save the median for later.
	    median[i] = med
	}

	# Here is the final sigma.
	if (n2 > 1)
	    s = sqrt (s / (n2 - 1))
	else
	    return

	# Compute individual sigmas and iteratively clip.
	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    if (n1 < max (3, maxkeep+1))
		next
	    nl = 1
	    nh = n1
	    med = median[i]

	    repeat {
		n2 = n1
		n3 = nl + n1 / 2

		if (n1 >= max (MINCLIP, maxkeep+1) && s > 0.) {
		    if (doscale1) {
			for (; nl <= n2; nl = nl + 1) {
			    l = Memi[m[nl]+k]
			    s1 = s * sqrt (max (one, (med+zeros[l])/scales[l]))
			    r = (med - Memr[d[nl]+k]) / s1
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    l = Memi[m[nh]+k]
			    s1 = s * sqrt (max (one, (med+zeros[l])/scales[l]))
			    r = (Memr[d[nh]+k] - med) / s1
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    } else {
			s1 = s * sqrt (max (one, med))
			for (; nl <= n2; nl = nl + 1) {
			    r = (med - Memr[d[nl]+k]) / s1
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    r = (Memr[d[nh]+k] - med) / s1
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    }

		    # Recompute median
		    if (n1 < n2) {
			if (n1 > 0) {
			    n3 = nl + n1 / 2
			    if (mod (n1, 2) == 0) {
				low = Memr[d[n3-1]+k]
				high = Memr[d[n3]+k]
				med = (low + high) / 2.
			    } else
				med = Memr[d[n3]+k]
			} else
			    med = blank
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    # If too many are rejected add some back in.
	    # Pixels with equal residuals are added together.
	    while (n1 < maxkeep) {
		if (nl == 1)
		    nh = nh + 1
		else if (nh == n[i])
		    nl = nl - 1
		else {
		    r = Memr[resid+nl-1]
		    s = Memr[resid+nh+1]
		    if (r < s) {
			nl = nl - 1
			r = r + TOL
			if (s <= r)
			    nh = nh + 1
			if (nl > 1) {
			    if (Memr[resid+nl-1] <= r)
				nl = nl - 1
			}
		    } else {
			nh = nh + 1
			s = s + TOL
			if (r <= s)
			    nl = nl - 1
			if (nh < n2) {
			    if (Memr[resid+nh+1] <= s)
				nh = nh + 1
			}
		    }
		}
		n1 = nh - nl + 1

		# Recompute median
		if (n1 < n2) {
		    if (n1 > 0) {
			n3 = nl + n1 / 2
			if (mod (n1, 2) == 0) {
			    low = Memr[d[n3-1]+k]
			    high = Memr[d[n3]+k]
			    med = (low + high) / 2.
			} else
			    med = Memr[d[n3]+k]
		    } else
			med = blank
		}
	    }

	    # Only set median and reorder if needed
	    n[i] = n1
	    if (n1 > 0 && nl > 1 && (combine != MEDIAN || grow > 0)) {
		j = max (nl, n1 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
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
		    do l = 1, min (n1, nl - 1) {
			Memr[d[l]+k] = Memr[d[j]+k]
			j = j + 1
		    }
		}
	    }

	    if (combine == MEDIAN)
		median[i] = med
	}

	# Check if data flag needs to be reset for rejected pixels
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (n[i] != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	# Flag that the median is computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

