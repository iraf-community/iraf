# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"

define	MINCLIP		3	# Minimum number of images for this algorithm


# IC_AAVSIGCLIP -- Reject pixels using an average sigma about the average
# The average sigma is normalized by the expected poisson sigma.

procedure ic_aavsigclips (d, m, n, scales, zeros, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, n1, n2
real	d1, low, high, sum, a, s, s1, r
pointer	sp, sums, dp, mp1, mp2

include	"../icombine.com"

begin
	# If there are no pixels go on to the combining
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	call smark (sp)
	call salloc (sums, npts, TY_REAL)

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

	s = 0.
	n2 = 0
	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (n1 < 3)
		next

	    # Unweighted average with the high and low rejected
	    low = Mems[d[1]+k]
	    high = Mems[d[2]+k]
	    if (low > high) {
		d1 = low
		low = high
		high = d1
	    }
	    sum = 0.
	    do j = 3, n1 {
	       d1 = Mems[d[j]+k]
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
		    dp = d[j] + k
		    mp1 = m[j] + k

		    d1 = Mems[dp]
		    l = Memi[mp1]
		    s1 = max (1., (a - zeros[l]) /  scales[l])
		    s = s + (d1 - a) ** 2 / s1
		}
	    } else {
		s1 = max (1., a)
		do j = 1, n1
		    s = s + (Mems[d[j]+k] - a) ** 2 / s1
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
	    if (n1 < 3) {
		if (!docombine) {
		    if (n1 == 2) {
			low = Mems[d[1]+k]
			high = Mems[d[2]+k]
		        average[i] = (low + high) / 2
		    } else if (n1 == 1)
			average[i] = Mems[d[1]+k]
		    else
			average[i] = blank
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
			    dp = d[j] + k
			    mp1 = m[j] + k

			    d1 = Mems[dp]
			    l = Memi[mp1]
			    s1 = s * sqrt (max (1., (a - zeros[l]) /  scales[l]))
			    r = (d1 - a) / s1
			    if (r < -lsigma || r > hsigma) {
				if (j < n1) {
				    Mems[dp] = Mems[d[n1]+k]
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
			s1 = s * sqrt (max (1., a))
			for (j=1; j<=n1; j=j+1) {
			    dp = d[j] + k
			    d1 = Mems[dp]
			    r = (d1 - a) / s1
			    if (r < -lsigma || r > hsigma) {
				if (j < n1) {
				    Mems[dp] = Mems[d[n1]+k]
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
		}
		if (n1 > 1)
		    a = sum / n1
	    } until (n1 == n2 || n1 < 3)

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
	    n1 = n[1]
	    do i = 1, npts {
		if (n[i] != n1) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_MAVSIGCLIP -- Reject pixels using an average sigma about the median
# The average sigma is normalized by the expected poisson sigma.

procedure ic_mavsigclips (d, m, n, scales, zeros, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh
pointer	mp1, mp2
real	med, low, high, r, s, s1

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Compute the poisson scaled average sigma about the median.
	# There must be at least three pixels at each point to define
	# the mean sigma.  Corrections for differences in the image
	# scale factors are selected by the doscale1 flag.

	s = 0.
	n2 = 0
	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (n1 < 3) {
		if (n1 == 0)
		    median[i] = blank
		else if (n1 == 1)
		    median[i] = Mems[d[1]+k]
		else {
		    low = Mems[d[1]+k]
		    high = Mems[d[2]+k]
		    median[i] = (low + high) / 2.
		}
		next
	    }

	    # Median
	    n3 = 1 + n1 / 2
	    if (mod (n1, 2) == 0) {
		low = Mems[d[n3-1]+k]
		high = Mems[d[n3]+k]
		med = (low + high) / 2.
	    } else
		med = Mems[d[n3]+k]

	    # Poisson scaled sigma accumulation
	    if (doscale1) {
		do j = 1, n1 {
		    l = Memi[m[j]+k]
		    s1 = max (1., (med - zeros[l]) /  scales[l])
		    s = s + (Mems[d[j]+k] - med) ** 2 / s1
		}
	    } else {
		s1 = max (1., med)
		do j = 1, n1
		    s = s + (Mems[d[j]+k] - med) ** 2 / s1
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

	# Compute sigma and iteratively clip.
	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (n1 < 3)
		next
	    nl = 1
	    nh = n1
	    med = median[i]

	    repeat {
		n2 = n1
		n3 = nl + n1 / 2

		if (n1 >= MINCLIP && s > 0.) {
		    if (doscale1) {
			for (; nl <= n2; nl = nl + 1) {
			    l = Memi[m[nl]+k]
			    s1 = s * sqrt (max (1., (med-zeros[l])/scales[l]))
			    r = (med - Mems[d[nl]+k]) / s1
			    if (r <= lsigma)
				break
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    l = Memi[m[nh]+k]
			    s1 = s * sqrt (max (1., (med-zeros[l])/scales[l]))
			    r = (Mems[d[nh]+k] - med) / s1
			    if (r <= hsigma)
				break
			    n1 = n1 - 1
			}
		    } else {
			s1 = s * sqrt (max (1., med))
			for (; nl <= n2; nl = nl + 1) {
			    r = (med - Mems[d[nl]+k]) / s1
			    if (r <= lsigma)
				break
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    r = (Mems[d[nh]+k] - med) / s1
			    if (r <= hsigma)
				break
			    n1 = n1 - 1
			}
		    }
		}

		if (n1 < n2) {
		    if (n1 > 0) {
			n3 = nl + n1 / 2
			if (mod (n1, 2) == 0) {
			    low = Mems[d[n3-1]+k]
			    high = Mems[d[n3]+k]
			    med = (low + high) / 2.
			} else
			    med = Mems[d[n3]+k]
		    } else
			med = blank
		}
	    } until (n1 == n2 || n1 < MINCLIP)

	    # Only set median and reorder if needed
	    n[i] = n1
	    if (combine == MEDIAN)
		median[i] = med
	    if (n1 > 0 && nl > 1 && (combine != MEDIAN || grow > 0)) {
		j = n1 + 1
		if (keepids) {
		    do l = 1, nl-1 {
			Mems[d[l]+k] = Mems[d[j]+k]
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
			Mems[d[l]+k] = Mems[d[j]+k]
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

int	i, j, k, l, n1, n2
real	d1, low, high, sum, a, s, s1, r
pointer	sp, sums, dp, mp1, mp2

include	"../icombine.com"

begin
	# If there are no pixels go on to the combining
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	call smark (sp)
	call salloc (sums, npts, TY_REAL)

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
		    dp = d[j] + k
		    mp1 = m[j] + k

		    d1 = Memr[dp]
		    l = Memi[mp1]
		    s1 = max (1., (a - zeros[l]) /  scales[l])
		    s = s + (d1 - a) ** 2 / s1
		}
	    } else {
		s1 = max (1., a)
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
	    if (n1 < 3) {
		if (!docombine) {
		    if (n1 == 2) {
			low = Memr[d[1]+k]
			high = Memr[d[2]+k]
		        average[i] = (low + high) / 2
		    } else if (n1 == 1)
			average[i] = Memr[d[1]+k]
		    else
			average[i] = blank
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
			    dp = d[j] + k
			    mp1 = m[j] + k

			    d1 = Memr[dp]
			    l = Memi[mp1]
			    s1 = s * sqrt (max (1., (a - zeros[l]) /  scales[l]))
			    r = (d1 - a) / s1
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
			s1 = s * sqrt (max (1., a))
			for (j=1; j<=n1; j=j+1) {
			    dp = d[j] + k
			    d1 = Memr[dp]
			    r = (d1 - a) / s1
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
		}
		if (n1 > 1)
		    a = sum / n1
	    } until (n1 == n2 || n1 < 3)

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
	    n1 = n[1]
	    do i = 1, npts {
		if (n[i] != n1) {
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

int	i, j, k, l, id, n1, n2, n3, nl, nh
pointer	mp1, mp2
real	med, low, high, r, s, s1

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Compute the poisson scaled average sigma about the median.
	# There must be at least three pixels at each point to define
	# the mean sigma.  Corrections for differences in the image
	# scale factors are selected by the doscale1 flag.

	s = 0.
	n2 = 0
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
		    s1 = max (1., (med - zeros[l]) /  scales[l])
		    s = s + (Memr[d[j]+k] - med) ** 2 / s1
		}
	    } else {
		s1 = max (1., med)
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

	# Compute sigma and iteratively clip.
	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (n1 < 3)
		next
	    nl = 1
	    nh = n1
	    med = median[i]

	    repeat {
		n2 = n1
		n3 = nl + n1 / 2

		if (n1 >= MINCLIP && s > 0.) {
		    if (doscale1) {
			for (; nl <= n2; nl = nl + 1) {
			    l = Memi[m[nl]+k]
			    s1 = s * sqrt (max (1., (med-zeros[l])/scales[l]))
			    r = (med - Memr[d[nl]+k]) / s1
			    if (r <= lsigma)
				break
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    l = Memi[m[nh]+k]
			    s1 = s * sqrt (max (1., (med-zeros[l])/scales[l]))
			    r = (Memr[d[nh]+k] - med) / s1
			    if (r <= hsigma)
				break
			    n1 = n1 - 1
			}
		    } else {
			s1 = s * sqrt (max (1., med))
			for (; nl <= n2; nl = nl + 1) {
			    r = (med - Memr[d[nl]+k]) / s1
			    if (r <= lsigma)
				break
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    r = (Memr[d[nh]+k] - med) / s1
			    if (r <= hsigma)
				break
			    n1 = n1 - 1
			}
		    }
		}

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
	    } until (n1 == n2 || n1 < MINCLIP)

	    # Only set median and reorder if needed
	    n[i] = n1
	    if (combine == MEDIAN)
		median[i] = med
	    if (n1 > 0 && nl > 1 && (combine != MEDIAN || grow > 0)) {
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

