# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"

define	MINCLIP		2	# Mininum number of images for algorithm


# IC_ACCDCLIP -- Reject pixels using CCD noise parameters about the average

procedure ic_accdclips (d, m, n, scales, zeros, nm, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	nm[3,nimages]		# Noise model parameters
int	nimages			# Number of images
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
real	d1, low, high, sum, a, s, r, zero
data	zero /0.0/
pointer	sp, resid, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are no pixels go on to the combining.  Since the unweighted
	# average is computed here possibly skip the combining later.

	# There must be at least max (1, nkeep) pixels.
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	} else if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)

	# There must be at least two pixels for rejection.  The initial
	# average is the low/high rejected average except in the case of
	# just two pixels.  The rejections are iterated and the average
	# is recomputed.  Corrections for scaling may be performed.
	# Depending on other flags the image IDs may also need to be adjusted.

	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    if (n1 <= max (MINCLIP-1, maxkeep)) {
		if (!docombine) {
		    if (n1 == 0)
			average[i] = blank
		    else {
			sum = Mems[d[1]+k]
			do j = 2, n1
			    sum = sum + Mems[d[j]+k]
			average[i] = sum / n1
		    }
		}
		next
	    }

	    repeat {
		if (n1 == 2) {
		    sum = Mems[d[1]+k]
		    sum = sum + Mems[d[2]+k]
		    a = sum / 2
		} else {
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
		}
		n2 = n1
		if (doscale1) {
		    for (j=1; j<=n1; j=j+1) {
			dp1 = d[j] + k
			mp1 = m[j] + k

			l = Memi[mp1]
			s = scales[l]
			d1 = max (zero, s * (a + zeros[l]))
			s = sqrt (nm[1,l] + d1/nm[2,l] + (d1*nm[3,l])**2) / s

			d1 = Mems[dp1]
			r = (d1 - a) / s
			if (r < -lsigma || r > hsigma) {
			    Memr[resid+n1] = abs(r)
			    if (j < n1) {
				dp2 = d[n1] + k
				Mems[dp1] = Mems[dp2]
				Mems[dp2] = d1
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
		    if (!keepids) {
			s = max (zero, a)
			s = sqrt (nm[1,1] + s/nm[2,1] + (s*nm[3,1])**2)
		    }
		    for (j=1; j<=n1; j=j+1) {
			if (keepids) {
			    l = Memi[m[j]+k]
			    s = max (zero, a)
			    s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			}
			dp1 = d[j] + k
			d1 = Mems[dp1]
			r = (d1 - a) / s
			if (r < -lsigma || r > hsigma) {
			    Memr[resid+n1] = abs(r)
			    if (j < n1) {
				dp2 = d[n1] + k
				Mems[dp1] = Mems[dp2]
				Mems[dp2] = d1
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
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

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
				    d1 = Mems[dp1]
				    Mems[dp1] = Mems[dp2]
				    Mems[dp2] = d1
				    mp2 = m[l] + k
				    s = Memi[mp1]
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = s
				}
			    }
			}
			sum = sum + Mems[dp1]
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
				    d1 = Mems[dp1]
				    Mems[dp1] = Mems[dp2]
				    Mems[dp2] = d1
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
			sum = sum + Mems[dp1]
			n1 = n1 + 1
			nk = max (nk, j+jj)
		    }
		}
	    }

	    n[i] = n1
	    if (!docombine)
		if (n1 > 0)
		    average[i] = sum / n1
		else
		    average[i] = blank
	}

	# Check if the data flag has to be reset for rejected pixels
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_CCDCLIP -- Reject pixels using CCD noise parameters about the median

procedure ic_mccdclips (d, m, n, scales, zeros, nm, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	nm[3,nimages]		# Noise model
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
real	r, s
pointer	sp, resid, mp1, mp2
real	med, zero
data	zero /0.0/

include	"../icombine.com"

begin
	# There must be at least max (MINCLIP, nkeep+1) pixels.
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

	# Compute median and sigma and iteratively clip.
	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    nl = 1
	    nh = n1

	    repeat {
		n2 = n1
		n3 = nl + n1 / 2

		if (n1 == 0)
		    med = blank
		else if (mod (n1, 2) == 0) {
		    med = Mems[d[n3-1]+k]
		    med = (med + Mems[d[n3]+k]) / 2.
		} else
		    med = Mems[d[n3]+k]

		if (n1 >= max (MINCLIP, maxkeep+1)) {
		    if (doscale1) {
			for (; nl <= nh; nl = nl + 1) {
			    l = Memi[m[nl]+k]
			    s = scales[l]
			    r = max (zero, s * (med + zeros[l]))
			    s = sqrt (nm[1,l] + r/nm[2,l] + (r*nm[3,l])**2) / s
			    r = (med - Mems[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    l = Memi[m[nh]+k]
			    s = scales[l]
			    r = max (zero, s * (med + zeros[l]))
			    s = sqrt (nm[1,l] + r/nm[2,l] + (r*nm[3,l])**2) / s
			    r = (Mems[d[nh]+k] - med) / s
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    } else {
			if (!keepids) {
			    s = max (zero, med)
			    s = sqrt (nm[1,1] + s/nm[2,1] + (s*nm[3,1])**2)
			}
			for (; nl <= nh; nl = nl + 1) {
			    if (keepids) {
				l = Memi[m[nl]+k]
				s = max (zero, med)
				s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			    }
			    r = (med - Mems[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    if (keepids) {
				l = Memi[m[nh]+k]
				s = max (zero, med)
				s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			    }
			    r = (Mems[d[nh]+k] -  med) / s
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    while (n1 < maxkeep) {
		if (nl == 1)
		    nh = nh + 1
		else if (nh == max (0, n[i]))
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
	    }

	    # Only set median and reorder if needed
	    n[i] = n1
	    if (n1 > 0 && nl > 1 && (combine != MEDIAN || grow >= 1.)) {
		j = max (nl, n1 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
			Mems[d[l]+k] = Mems[d[j]+k]
			if (grow >= 1.) {
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
			Mems[d[l]+k] = Mems[d[j]+k]
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
		if (max (0, n[i]) != nin) {
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

# IC_ACCDCLIP -- Reject pixels using CCD noise parameters about the average

procedure ic_accdclipi (d, m, n, scales, zeros, nm, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	nm[3,nimages]		# Noise model parameters
int	nimages			# Number of images
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
real	d1, low, high, sum, a, s, r, zero
data	zero /0.0/
pointer	sp, resid, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are no pixels go on to the combining.  Since the unweighted
	# average is computed here possibly skip the combining later.

	# There must be at least max (1, nkeep) pixels.
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	} else if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)

	# There must be at least two pixels for rejection.  The initial
	# average is the low/high rejected average except in the case of
	# just two pixels.  The rejections are iterated and the average
	# is recomputed.  Corrections for scaling may be performed.
	# Depending on other flags the image IDs may also need to be adjusted.

	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    if (n1 <= max (MINCLIP-1, maxkeep)) {
		if (!docombine) {
		    if (n1 == 0)
			average[i] = blank
		    else {
			sum = Memi[d[1]+k]
			do j = 2, n1
			    sum = sum + Memi[d[j]+k]
			average[i] = sum / n1
		    }
		}
		next
	    }

	    repeat {
		if (n1 == 2) {
		    sum = Memi[d[1]+k]
		    sum = sum + Memi[d[2]+k]
		    a = sum / 2
		} else {
		    low = Memi[d[1]+k]
		    high = Memi[d[2]+k]
		    if (low > high) {
			d1 = low
			low = high
			high = d1
		    }
		    sum = 0.
		    do j = 3, n1 {
		       d1 = Memi[d[j]+k]
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
			dp1 = d[j] + k
			mp1 = m[j] + k

			l = Memi[mp1]
			s = scales[l]
			d1 = max (zero, s * (a + zeros[l]))
			s = sqrt (nm[1,l] + d1/nm[2,l] + (d1*nm[3,l])**2) / s

			d1 = Memi[dp1]
			r = (d1 - a) / s
			if (r < -lsigma || r > hsigma) {
			    Memr[resid+n1] = abs(r)
			    if (j < n1) {
				dp2 = d[n1] + k
				Memi[dp1] = Memi[dp2]
				Memi[dp2] = d1
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
		    if (!keepids) {
			s = max (zero, a)
			s = sqrt (nm[1,1] + s/nm[2,1] + (s*nm[3,1])**2)
		    }
		    for (j=1; j<=n1; j=j+1) {
			if (keepids) {
			    l = Memi[m[j]+k]
			    s = max (zero, a)
			    s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			}
			dp1 = d[j] + k
			d1 = Memi[dp1]
			r = (d1 - a) / s
			if (r < -lsigma || r > hsigma) {
			    Memr[resid+n1] = abs(r)
			    if (j < n1) {
				dp2 = d[n1] + k
				Memi[dp1] = Memi[dp2]
				Memi[dp2] = d1
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
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

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
				    d1 = Memi[dp1]
				    Memi[dp1] = Memi[dp2]
				    Memi[dp2] = d1
				    mp2 = m[l] + k
				    s = Memi[mp1]
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = s
				}
			    }
			}
			sum = sum + Memi[dp1]
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
				    d1 = Memi[dp1]
				    Memi[dp1] = Memi[dp2]
				    Memi[dp2] = d1
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
			sum = sum + Memi[dp1]
			n1 = n1 + 1
			nk = max (nk, j+jj)
		    }
		}
	    }

	    n[i] = n1
	    if (!docombine)
		if (n1 > 0)
		    average[i] = sum / n1
		else
		    average[i] = blank
	}

	# Check if the data flag has to be reset for rejected pixels
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_CCDCLIP -- Reject pixels using CCD noise parameters about the median

procedure ic_mccdclipi (d, m, n, scales, zeros, nm, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	nm[3,nimages]		# Noise model
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
real	r, s
pointer	sp, resid, mp1, mp2
real	med, zero
data	zero /0.0/

include	"../icombine.com"

begin
	# There must be at least max (MINCLIP, nkeep+1) pixels.
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

	# Compute median and sigma and iteratively clip.
	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    nl = 1
	    nh = n1

	    repeat {
		n2 = n1
		n3 = nl + n1 / 2

		if (n1 == 0)
		    med = blank
		else if (mod (n1, 2) == 0) {
		    med = Memi[d[n3-1]+k]
		    med = (med + Memi[d[n3]+k]) / 2.
		} else
		    med = Memi[d[n3]+k]

		if (n1 >= max (MINCLIP, maxkeep+1)) {
		    if (doscale1) {
			for (; nl <= nh; nl = nl + 1) {
			    l = Memi[m[nl]+k]
			    s = scales[l]
			    r = max (zero, s * (med + zeros[l]))
			    s = sqrt (nm[1,l] + r/nm[2,l] + (r*nm[3,l])**2) / s
			    r = (med - Memi[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    l = Memi[m[nh]+k]
			    s = scales[l]
			    r = max (zero, s * (med + zeros[l]))
			    s = sqrt (nm[1,l] + r/nm[2,l] + (r*nm[3,l])**2) / s
			    r = (Memi[d[nh]+k] - med) / s
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    } else {
			if (!keepids) {
			    s = max (zero, med)
			    s = sqrt (nm[1,1] + s/nm[2,1] + (s*nm[3,1])**2)
			}
			for (; nl <= nh; nl = nl + 1) {
			    if (keepids) {
				l = Memi[m[nl]+k]
				s = max (zero, med)
				s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			    }
			    r = (med - Memi[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    if (keepids) {
				l = Memi[m[nh]+k]
				s = max (zero, med)
				s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			    }
			    r = (Memi[d[nh]+k] -  med) / s
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    while (n1 < maxkeep) {
		if (nl == 1)
		    nh = nh + 1
		else if (nh == max (0, n[i]))
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
	    }

	    # Only set median and reorder if needed
	    n[i] = n1
	    if (n1 > 0 && nl > 1 && (combine != MEDIAN || grow >= 1.)) {
		j = max (nl, n1 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
			Memi[d[l]+k] = Memi[d[j]+k]
			if (grow >= 1.) {
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
			Memi[d[l]+k] = Memi[d[j]+k]
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
		if (max (0, n[i]) != nin) {
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

# IC_ACCDCLIP -- Reject pixels using CCD noise parameters about the average

procedure ic_accdclipr (d, m, n, scales, zeros, nm, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	nm[3,nimages]		# Noise model parameters
int	nimages			# Number of images
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
real	d1, low, high, sum, a, s, r, zero
data	zero /0.0/
pointer	sp, resid, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are no pixels go on to the combining.  Since the unweighted
	# average is computed here possibly skip the combining later.

	# There must be at least max (1, nkeep) pixels.
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	} else if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)

	# There must be at least two pixels for rejection.  The initial
	# average is the low/high rejected average except in the case of
	# just two pixels.  The rejections are iterated and the average
	# is recomputed.  Corrections for scaling may be performed.
	# Depending on other flags the image IDs may also need to be adjusted.

	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    if (n1 <= max (MINCLIP-1, maxkeep)) {
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

	    repeat {
		if (n1 == 2) {
		    sum = Memr[d[1]+k]
		    sum = sum + Memr[d[2]+k]
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
			dp1 = d[j] + k
			mp1 = m[j] + k

			l = Memi[mp1]
			s = scales[l]
			d1 = max (zero, s * (a + zeros[l]))
			s = sqrt (nm[1,l] + d1/nm[2,l] + (d1*nm[3,l])**2) / s

			d1 = Memr[dp1]
			r = (d1 - a) / s
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
		    if (!keepids) {
			s = max (zero, a)
			s = sqrt (nm[1,1] + s/nm[2,1] + (s*nm[3,1])**2)
		    }
		    for (j=1; j<=n1; j=j+1) {
			if (keepids) {
			    l = Memi[m[j]+k]
			    s = max (zero, a)
			    s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			}
			dp1 = d[j] + k
			d1 = Memr[dp1]
			r = (d1 - a) / s
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
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

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
	    }

	    n[i] = n1
	    if (!docombine)
		if (n1 > 0)
		    average[i] = sum / n1
		else
		    average[i] = blank
	}

	# Check if the data flag has to be reset for rejected pixels
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_CCDCLIP -- Reject pixels using CCD noise parameters about the median

procedure ic_mccdclipr (d, m, n, scales, zeros, nm, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	nm[3,nimages]		# Noise model
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
real	r, s
pointer	sp, resid, mp1, mp2
real	med, zero
data	zero /0.0/

include	"../icombine.com"

begin
	# There must be at least max (MINCLIP, nkeep+1) pixels.
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

	# Compute median and sigma and iteratively clip.
	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    nl = 1
	    nh = n1

	    repeat {
		n2 = n1
		n3 = nl + n1 / 2

		if (n1 == 0)
		    med = blank
		else if (mod (n1, 2) == 0) {
		    med = Memr[d[n3-1]+k]
		    med = (med + Memr[d[n3]+k]) / 2.
		} else
		    med = Memr[d[n3]+k]

		if (n1 >= max (MINCLIP, maxkeep+1)) {
		    if (doscale1) {
			for (; nl <= nh; nl = nl + 1) {
			    l = Memi[m[nl]+k]
			    s = scales[l]
			    r = max (zero, s * (med + zeros[l]))
			    s = sqrt (nm[1,l] + r/nm[2,l] + (r*nm[3,l])**2) / s
			    r = (med - Memr[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    l = Memi[m[nh]+k]
			    s = scales[l]
			    r = max (zero, s * (med + zeros[l]))
			    s = sqrt (nm[1,l] + r/nm[2,l] + (r*nm[3,l])**2) / s
			    r = (Memr[d[nh]+k] - med) / s
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    } else {
			if (!keepids) {
			    s = max (zero, med)
			    s = sqrt (nm[1,1] + s/nm[2,1] + (s*nm[3,1])**2)
			}
			for (; nl <= nh; nl = nl + 1) {
			    if (keepids) {
				l = Memi[m[nl]+k]
				s = max (zero, med)
				s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			    }
			    r = (med - Memr[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    if (keepids) {
				l = Memi[m[nh]+k]
				s = max (zero, med)
				s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			    }
			    r = (Memr[d[nh]+k] -  med) / s
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    while (n1 < maxkeep) {
		if (nl == 1)
		    nh = nh + 1
		else if (nh == max (0, n[i]))
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
	    }

	    # Only set median and reorder if needed
	    n[i] = n1
	    if (n1 > 0 && nl > 1 && (combine != MEDIAN || grow >= 1.)) {
		j = max (nl, n1 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
			Memr[d[l]+k] = Memr[d[j]+k]
			if (grow >= 1.) {
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
		if (max (0, n[i]) != nin) {
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

# IC_ACCDCLIP -- Reject pixels using CCD noise parameters about the average

procedure ic_accdclipd (d, m, n, scales, zeros, nm, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	nm[3,nimages]		# Noise model parameters
int	nimages			# Number of images
int	npts			# Number of output points per line
double	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
double	d1, low, high, sum, a, s, r, zero
data	zero /0.0D0/
pointer	sp, resid, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are no pixels go on to the combining.  Since the unweighted
	# average is computed here possibly skip the combining later.

	# There must be at least max (1, nkeep) pixels.
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	} else if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)

	# There must be at least two pixels for rejection.  The initial
	# average is the low/high rejected average except in the case of
	# just two pixels.  The rejections are iterated and the average
	# is recomputed.  Corrections for scaling may be performed.
	# Depending on other flags the image IDs may also need to be adjusted.

	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    if (n1 <= max (MINCLIP-1, maxkeep)) {
		if (!docombine) {
		    if (n1 == 0)
			average[i] = blank
		    else {
			sum = Memd[d[1]+k]
			do j = 2, n1
			    sum = sum + Memd[d[j]+k]
			average[i] = sum / n1
		    }
		}
		next
	    }

	    repeat {
		if (n1 == 2) {
		    sum = Memd[d[1]+k]
		    sum = sum + Memd[d[2]+k]
		    a = sum / 2
		} else {
		    low = Memd[d[1]+k]
		    high = Memd[d[2]+k]
		    if (low > high) {
			d1 = low
			low = high
			high = d1
		    }
		    sum = 0.
		    do j = 3, n1 {
		       d1 = Memd[d[j]+k]
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
			dp1 = d[j] + k
			mp1 = m[j] + k

			l = Memi[mp1]
			s = scales[l]
			d1 = max (zero, s * (a + zeros[l]))
			s = sqrt (nm[1,l] + d1/nm[2,l] + (d1*nm[3,l])**2) / s

			d1 = Memd[dp1]
			r = (d1 - a) / s
			if (r < -lsigma || r > hsigma) {
			    Memr[resid+n1] = abs(r)
			    if (j < n1) {
				dp2 = d[n1] + k
				Memd[dp1] = Memd[dp2]
				Memd[dp2] = d1
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
		    if (!keepids) {
			s = max (zero, a)
			s = sqrt (nm[1,1] + s/nm[2,1] + (s*nm[3,1])**2)
		    }
		    for (j=1; j<=n1; j=j+1) {
			if (keepids) {
			    l = Memi[m[j]+k]
			    s = max (zero, a)
			    s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			}
			dp1 = d[j] + k
			d1 = Memd[dp1]
			r = (d1 - a) / s
			if (r < -lsigma || r > hsigma) {
			    Memr[resid+n1] = abs(r)
			    if (j < n1) {
				dp2 = d[n1] + k
				Memd[dp1] = Memd[dp2]
				Memd[dp2] = d1
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
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

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
				    d1 = Memd[dp1]
				    Memd[dp1] = Memd[dp2]
				    Memd[dp2] = d1
				    mp2 = m[l] + k
				    s = Memi[mp1]
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = s
				}
			    }
			}
			sum = sum + Memd[dp1]
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
				    d1 = Memd[dp1]
				    Memd[dp1] = Memd[dp2]
				    Memd[dp2] = d1
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
			sum = sum + Memd[dp1]
			n1 = n1 + 1
			nk = max (nk, j+jj)
		    }
		}
	    }

	    n[i] = n1
	    if (!docombine)
		if (n1 > 0)
		    average[i] = sum / n1
		else
		    average[i] = blank
	}

	# Check if the data flag has to be reset for rejected pixels
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_CCDCLIP -- Reject pixels using CCD noise parameters about the median

procedure ic_mccdclipd (d, m, n, scales, zeros, nm, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	nm[3,nimages]		# Noise model
int	nimages			# Number of images
int	npts			# Number of output points per line
double	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
real	r, s
pointer	sp, resid, mp1, mp2
double	med, zero
data	zero /0.0D0/

include	"../icombine.com"

begin
	# There must be at least max (MINCLIP, nkeep+1) pixels.
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

	# Compute median and sigma and iteratively clip.
	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    nl = 1
	    nh = n1

	    repeat {
		n2 = n1
		n3 = nl + n1 / 2

		if (n1 == 0)
		    med = blank
		else if (mod (n1, 2) == 0) {
		    med = Memd[d[n3-1]+k]
		    med = (med + Memd[d[n3]+k]) / 2.
		} else
		    med = Memd[d[n3]+k]

		if (n1 >= max (MINCLIP, maxkeep+1)) {
		    if (doscale1) {
			for (; nl <= nh; nl = nl + 1) {
			    l = Memi[m[nl]+k]
			    s = scales[l]
			    r = max (zero, s * (med + zeros[l]))
			    s = sqrt (nm[1,l] + r/nm[2,l] + (r*nm[3,l])**2) / s
			    r = (med - Memd[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    l = Memi[m[nh]+k]
			    s = scales[l]
			    r = max (zero, s * (med + zeros[l]))
			    s = sqrt (nm[1,l] + r/nm[2,l] + (r*nm[3,l])**2) / s
			    r = (Memd[d[nh]+k] - med) / s
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    } else {
			if (!keepids) {
			    s = max (zero, med)
			    s = sqrt (nm[1,1] + s/nm[2,1] + (s*nm[3,1])**2)
			}
			for (; nl <= nh; nl = nl + 1) {
			    if (keepids) {
				l = Memi[m[nl]+k]
				s = max (zero, med)
				s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			    }
			    r = (med - Memd[d[nl]+k]) / s
			    if (r <= lsigma)
				break
			    Memr[resid+nl] = r
			    n1 = n1 - 1
			}
			for (; nh >= nl; nh = nh - 1) {
			    if (keepids) {
				l = Memi[m[nh]+k]
				s = max (zero, med)
				s = sqrt (nm[1,l] + s/nm[2,l] + (s*nm[3,l])**2)
			    }
			    r = (Memd[d[nh]+k] -  med) / s
			    if (r <= hsigma)
				break
			    Memr[resid+nh] = r
			    n1 = n1 - 1
			}
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    while (n1 < maxkeep) {
		if (nl == 1)
		    nh = nh + 1
		else if (nh == max (0, n[i]))
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
	    }

	    # Only set median and reorder if needed
	    n[i] = n1
	    if (n1 > 0 && nl > 1 && (combine != MEDIAN || grow >= 1.)) {
		j = max (nl, n1 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
			Memd[d[l]+k] = Memd[d[j]+k]
			if (grow >= 1.) {
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
			Memd[d[l]+k] = Memd[d[j]+k]
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
		if (max (0, n[i]) != nin) {
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

