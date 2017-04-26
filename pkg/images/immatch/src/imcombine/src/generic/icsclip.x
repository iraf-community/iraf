# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"

define	MINCLIP		3	# Mininum number of images for algorithm


# IC_ASIGCLIP -- Reject pixels using sigma clipping about the average
# The initial average rejects the high and low pixels.  A correction for
# different scalings of the images may be made.  Weights are not used.

procedure ic_asigclips (d, m, n, scales, zeros, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
real	d1, low, high, sum, a, s, r, one
data	one /1.0/
pointer	sp, resid, w, wp, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	} 
	
	# Flag whether returned average needs to be recomputed.
	if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	# Save the residuals and the sigma scaling corrections if needed.
	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)
	if (doscale1)
	    call salloc (w, nimages, TY_REAL)

	# Do sigma clipping.
	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)

	    # If there are not enough pixels simply compute the average.
	    if (n1 < max (3, maxkeep)) {
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

	    # Compute average with the high and low rejected.
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

	    # Iteratively reject pixels and compute the final average if needed.
	    # Compact the data and keep track of the image IDs if needed.

	    repeat {
		n2 = n1
		if (doscale1) {
		    # Compute sigma corrected for scaling.
		    s = 0.
		    wp = w - 1
		    do j = 1, n1 {
			dp1 = d[j] + k
			mp1 = m[j] + k
			wp = wp + 1

			d1 = Mems[dp1]
			l = Memi[mp1]
			r = sqrt (max (one, (a + zeros[l]) / scales[l]))
			s = s + ((d1 - a) / r) ** 2
			Memr[wp] = r
		    }
		    s = sqrt (s / (n1 - 1))

		    # Reject pixels.  Save the residuals and data values.
		    wp = w - 1
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    mp1 = m[j] + k
			    wp = wp + 1

			    d1 = Mems[dp1]
			    r = (d1 - a) / (s * Memr[wp])
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs (r)
				if (j < n1) {
				    dp2 = d[n1] + k
				    Mems[dp1] = Mems[dp2]
				    Mems[dp2] = d1
				    Memr[wp] = Memr[w+n1-1]
				    mp2 = m[n1] + k
				    l = Memi[mp1]
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = l
				    j = j - 1
				}
				sum = sum - d1
				n1 = n1 - 1
			    }
			}
		    }
		} else {
		    # Compute the sigma without scale correction.
		    s = 0.
		    do j = 1, n1
			s = s + (Mems[d[j]+k] - a) ** 2
		    s = sqrt (s / (n1 - 1))

		    # Reject pixels.  Save the residuals and data values.
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    d1 = Mems[dp1]
			    r = (d1 - a) / s
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs (r)
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
		}

		# Recompute the average.
		if (n1 > 1)
		    a = sum / n1
	    } until (n1 == n2 || n1 <= max (2, maxkeep))

	    # If too many pixels are rejected add some back.
	    # All pixels with equal residuals are added back.
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

		# Recompute the average.
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
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_MSIGCLIP -- Reject pixels using sigma clipping about the median

procedure ic_msigclips (d, m, n, scales, zeros, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
real	r, s
pointer	sp, resid, w, mp1, mp2
real	med, one
data	one /1.0/

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Save the residuals and sigma scaling corrections if needed.
	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)
	if (doscale1)
	    call salloc (w, nimages, TY_REAL)

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
		else if (mod (n1, 2) == 0)
		    med = (Mems[d[n3-1]+k] + Mems[d[n3]+k]) / 2.
		else
		    med = Mems[d[n3]+k]

		if (n1 >= max (MINCLIP, maxkeep+1)) {
		    if (doscale1) {
			# Compute the sigma with scaling correction.
			s = 0.
			do j = nl, nh {
			    l = Memi[m[j]+k]
			    r = sqrt (max (one, (med + zeros[l]) / scales[l]))
			    s = s + ((Mems[d[j]+k] - med) / r) ** 2
			    Memr[w+j-1] = r
			}
			s = sqrt (s / (n1 - 1))

			# Reject pixels and save the residuals.
			if (s > 0.) {
			    for (; nl <= nh; nl = nl + 1) {
				r = (med - Mems[d[nl]+k]) / (s * Memr[w+nl-1])
				if (r <= lsigma)
				    break
				Memr[resid+nl] = r
				n1 = n1 - 1
			    }
			    for (; nh >= nl; nh = nh - 1) {
				r = (Mems[d[nh]+k] - med) / (s * Memr[w+nh-1])
				if (r <= hsigma)
				    break
				Memr[resid+nh] = r
				n1 = n1 - 1
			    }
			}
		    } else {
			# Compute the sigma without scaling correction.
			s = 0.
			do j = nl, nh
			    s = s + (Mems[d[j]+k] - med) ** 2
			s = sqrt (s / (n1 - 1))

			# Reject pixels and save the residuals.
			if (s > 0.) {
			    for (; nl <= nh; nl = nl + 1) {
				r = (med - Mems[d[nl]+k]) / s
				if (r <= lsigma)
				    break
				Memr[resid+nl] = r
				n1 = n1 - 1
			    }
			    for (; nh >= nl; nh = nh - 1) {
				r = (Mems[d[nh]+k] -  med) / s
				if (r <= hsigma)
				    break
				Memr[resid+nh] = r
				n1 = n1 - 1
			    }
			}
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    # If too many pixels are rejected add some back.
	    # All pixels with equal residuals are added back.
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

	# Flag that the median has been computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

# IC_ASIGCLIP -- Reject pixels using sigma clipping about the average
# The initial average rejects the high and low pixels.  A correction for
# different scalings of the images may be made.  Weights are not used.

procedure ic_asigclipi (d, m, n, scales, zeros, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
real	d1, low, high, sum, a, s, r, one
data	one /1.0/
pointer	sp, resid, w, wp, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	} 
	
	# Flag whether returned average needs to be recomputed.
	if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	# Save the residuals and the sigma scaling corrections if needed.
	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)
	if (doscale1)
	    call salloc (w, nimages, TY_REAL)

	# Do sigma clipping.
	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)

	    # If there are not enough pixels simply compute the average.
	    if (n1 < max (3, maxkeep)) {
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

	    # Compute average with the high and low rejected.
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

	    # Iteratively reject pixels and compute the final average if needed.
	    # Compact the data and keep track of the image IDs if needed.

	    repeat {
		n2 = n1
		if (doscale1) {
		    # Compute sigma corrected for scaling.
		    s = 0.
		    wp = w - 1
		    do j = 1, n1 {
			dp1 = d[j] + k
			mp1 = m[j] + k
			wp = wp + 1

			d1 = Memi[dp1]
			l = Memi[mp1]
			r = sqrt (max (one, (a + zeros[l]) / scales[l]))
			s = s + ((d1 - a) / r) ** 2
			Memr[wp] = r
		    }
		    s = sqrt (s / (n1 - 1))

		    # Reject pixels.  Save the residuals and data values.
		    wp = w - 1
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    mp1 = m[j] + k
			    wp = wp + 1

			    d1 = Memi[dp1]
			    r = (d1 - a) / (s * Memr[wp])
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs (r)
				if (j < n1) {
				    dp2 = d[n1] + k
				    Memi[dp1] = Memi[dp2]
				    Memi[dp2] = d1
				    Memr[wp] = Memr[w+n1-1]
				    mp2 = m[n1] + k
				    l = Memi[mp1]
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = l
				    j = j - 1
				}
				sum = sum - d1
				n1 = n1 - 1
			    }
			}
		    }
		} else {
		    # Compute the sigma without scale correction.
		    s = 0.
		    do j = 1, n1
			s = s + (Memi[d[j]+k] - a) ** 2
		    s = sqrt (s / (n1 - 1))

		    # Reject pixels.  Save the residuals and data values.
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    d1 = Memi[dp1]
			    r = (d1 - a) / s
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs (r)
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
		}

		# Recompute the average.
		if (n1 > 1)
		    a = sum / n1
	    } until (n1 == n2 || n1 <= max (2, maxkeep))

	    # If too many pixels are rejected add some back.
	    # All pixels with equal residuals are added back.
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

		# Recompute the average.
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
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_MSIGCLIP -- Reject pixels using sigma clipping about the median

procedure ic_msigclipi (d, m, n, scales, zeros, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
real	r, s
pointer	sp, resid, w, mp1, mp2
real	med, one
data	one /1.0/

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Save the residuals and sigma scaling corrections if needed.
	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)
	if (doscale1)
	    call salloc (w, nimages, TY_REAL)

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
		else if (mod (n1, 2) == 0)
		    med = (Memi[d[n3-1]+k] + Memi[d[n3]+k]) / 2.
		else
		    med = Memi[d[n3]+k]

		if (n1 >= max (MINCLIP, maxkeep+1)) {
		    if (doscale1) {
			# Compute the sigma with scaling correction.
			s = 0.
			do j = nl, nh {
			    l = Memi[m[j]+k]
			    r = sqrt (max (one, (med + zeros[l]) / scales[l]))
			    s = s + ((Memi[d[j]+k] - med) / r) ** 2
			    Memr[w+j-1] = r
			}
			s = sqrt (s / (n1 - 1))

			# Reject pixels and save the residuals.
			if (s > 0.) {
			    for (; nl <= nh; nl = nl + 1) {
				r = (med - Memi[d[nl]+k]) / (s * Memr[w+nl-1])
				if (r <= lsigma)
				    break
				Memr[resid+nl] = r
				n1 = n1 - 1
			    }
			    for (; nh >= nl; nh = nh - 1) {
				r = (Memi[d[nh]+k] - med) / (s * Memr[w+nh-1])
				if (r <= hsigma)
				    break
				Memr[resid+nh] = r
				n1 = n1 - 1
			    }
			}
		    } else {
			# Compute the sigma without scaling correction.
			s = 0.
			do j = nl, nh
			    s = s + (Memi[d[j]+k] - med) ** 2
			s = sqrt (s / (n1 - 1))

			# Reject pixels and save the residuals.
			if (s > 0.) {
			    for (; nl <= nh; nl = nl + 1) {
				r = (med - Memi[d[nl]+k]) / s
				if (r <= lsigma)
				    break
				Memr[resid+nl] = r
				n1 = n1 - 1
			    }
			    for (; nh >= nl; nh = nh - 1) {
				r = (Memi[d[nh]+k] -  med) / s
				if (r <= hsigma)
				    break
				Memr[resid+nh] = r
				n1 = n1 - 1
			    }
			}
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    # If too many pixels are rejected add some back.
	    # All pixels with equal residuals are added back.
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

	# Flag that the median has been computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

# IC_ASIGCLIP -- Reject pixels using sigma clipping about the average
# The initial average rejects the high and low pixels.  A correction for
# different scalings of the images may be made.  Weights are not used.

procedure ic_asigclipr (d, m, n, scales, zeros, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
real	d1, low, high, sum, a, s, r, one
data	one /1.0/
pointer	sp, resid, w, wp, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	} 
	
	# Flag whether returned average needs to be recomputed.
	if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	# Save the residuals and the sigma scaling corrections if needed.
	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)
	if (doscale1)
	    call salloc (w, nimages, TY_REAL)

	# Do sigma clipping.
	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)

	    # If there are not enough pixels simply compute the average.
	    if (n1 < max (3, maxkeep)) {
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

	    # Compute average with the high and low rejected.
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

	    # Iteratively reject pixels and compute the final average if needed.
	    # Compact the data and keep track of the image IDs if needed.

	    repeat {
		n2 = n1
		if (doscale1) {
		    # Compute sigma corrected for scaling.
		    s = 0.
		    wp = w - 1
		    do j = 1, n1 {
			dp1 = d[j] + k
			mp1 = m[j] + k
			wp = wp + 1

			d1 = Memr[dp1]
			l = Memi[mp1]
			r = sqrt (max (one, (a + zeros[l]) / scales[l]))
			s = s + ((d1 - a) / r) ** 2
			Memr[wp] = r
		    }
		    s = sqrt (s / (n1 - 1))

		    # Reject pixels.  Save the residuals and data values.
		    wp = w - 1
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    mp1 = m[j] + k
			    wp = wp + 1

			    d1 = Memr[dp1]
			    r = (d1 - a) / (s * Memr[wp])
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs (r)
				if (j < n1) {
				    dp2 = d[n1] + k
				    Memr[dp1] = Memr[dp2]
				    Memr[dp2] = d1
				    Memr[wp] = Memr[w+n1-1]
				    mp2 = m[n1] + k
				    l = Memi[mp1]
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = l
				    j = j - 1
				}
				sum = sum - d1
				n1 = n1 - 1
			    }
			}
		    }
		} else {
		    # Compute the sigma without scale correction.
		    s = 0.
		    do j = 1, n1
			s = s + (Memr[d[j]+k] - a) ** 2
		    s = sqrt (s / (n1 - 1))

		    # Reject pixels.  Save the residuals and data values.
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    d1 = Memr[dp1]
			    r = (d1 - a) / s
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs (r)
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

		# Recompute the average.
		if (n1 > 1)
		    a = sum / n1
	    } until (n1 == n2 || n1 <= max (2, maxkeep))

	    # If too many pixels are rejected add some back.
	    # All pixels with equal residuals are added back.
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

		# Recompute the average.
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
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_MSIGCLIP -- Reject pixels using sigma clipping about the median

procedure ic_msigclipr (d, m, n, scales, zeros, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
real	r, s
pointer	sp, resid, w, mp1, mp2
real	med, one
data	one /1.0/

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Save the residuals and sigma scaling corrections if needed.
	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)
	if (doscale1)
	    call salloc (w, nimages, TY_REAL)

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
		else if (mod (n1, 2) == 0)
		    med = (Memr[d[n3-1]+k] + Memr[d[n3]+k]) / 2.
		else
		    med = Memr[d[n3]+k]

		if (n1 >= max (MINCLIP, maxkeep+1)) {
		    if (doscale1) {
			# Compute the sigma with scaling correction.
			s = 0.
			do j = nl, nh {
			    l = Memi[m[j]+k]
			    r = sqrt (max (one, (med + zeros[l]) / scales[l]))
			    s = s + ((Memr[d[j]+k] - med) / r) ** 2
			    Memr[w+j-1] = r
			}
			s = sqrt (s / (n1 - 1))

			# Reject pixels and save the residuals.
			if (s > 0.) {
			    for (; nl <= nh; nl = nl + 1) {
				r = (med - Memr[d[nl]+k]) / (s * Memr[w+nl-1])
				if (r <= lsigma)
				    break
				Memr[resid+nl] = r
				n1 = n1 - 1
			    }
			    for (; nh >= nl; nh = nh - 1) {
				r = (Memr[d[nh]+k] - med) / (s * Memr[w+nh-1])
				if (r <= hsigma)
				    break
				Memr[resid+nh] = r
				n1 = n1 - 1
			    }
			}
		    } else {
			# Compute the sigma without scaling correction.
			s = 0.
			do j = nl, nh
			    s = s + (Memr[d[j]+k] - med) ** 2
			s = sqrt (s / (n1 - 1))

			# Reject pixels and save the residuals.
			if (s > 0.) {
			    for (; nl <= nh; nl = nl + 1) {
				r = (med - Memr[d[nl]+k]) / s
				if (r <= lsigma)
				    break
				Memr[resid+nl] = r
				n1 = n1 - 1
			    }
			    for (; nh >= nl; nh = nh - 1) {
				r = (Memr[d[nh]+k] -  med) / s
				if (r <= hsigma)
				    break
				Memr[resid+nh] = r
				n1 = n1 - 1
			    }
			}
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    # If too many pixels are rejected add some back.
	    # All pixels with equal residuals are added back.
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

	# Flag that the median has been computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

# IC_ASIGCLIP -- Reject pixels using sigma clipping about the average
# The initial average rejects the high and low pixels.  A correction for
# different scalings of the images may be made.  Weights are not used.

procedure ic_asigclipd (d, m, n, scales, zeros, nimages, npts, average)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
double	average[npts]		# Average

int	i, j, k, l, jj, n1, n2, nin, nk, maxkeep
double	d1, low, high, sum, a, s, r, one
data	one /1.0D0/
pointer	sp, resid, w, wp, dp1, dp2, mp1, mp2

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	} 
	
	# Flag whether returned average needs to be recomputed.
	if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	# Save the residuals and the sigma scaling corrections if needed.
	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)
	if (doscale1)
	    call salloc (w, nimages, TY_REAL)

	# Do sigma clipping.
	nin = max (0, n[1])
	do i = 1, npts {
	    k = i - 1
	    n1 = max (0, n[i])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)

	    # If there are not enough pixels simply compute the average.
	    if (n1 < max (3, maxkeep)) {
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

	    # Compute average with the high and low rejected.
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

	    # Iteratively reject pixels and compute the final average if needed.
	    # Compact the data and keep track of the image IDs if needed.

	    repeat {
		n2 = n1
		if (doscale1) {
		    # Compute sigma corrected for scaling.
		    s = 0.
		    wp = w - 1
		    do j = 1, n1 {
			dp1 = d[j] + k
			mp1 = m[j] + k
			wp = wp + 1

			d1 = Memd[dp1]
			l = Memi[mp1]
			r = sqrt (max (one, (a + zeros[l]) / scales[l]))
			s = s + ((d1 - a) / r) ** 2
			Memr[wp] = r
		    }
		    s = sqrt (s / (n1 - 1))

		    # Reject pixels.  Save the residuals and data values.
		    wp = w - 1
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    mp1 = m[j] + k
			    wp = wp + 1

			    d1 = Memd[dp1]
			    r = (d1 - a) / (s * Memr[wp])
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs (r)
				if (j < n1) {
				    dp2 = d[n1] + k
				    Memd[dp1] = Memd[dp2]
				    Memd[dp2] = d1
				    Memr[wp] = Memr[w+n1-1]
				    mp2 = m[n1] + k
				    l = Memi[mp1]
				    Memi[mp1] = Memi[mp2]
				    Memi[mp2] = l
				    j = j - 1
				}
				sum = sum - d1
				n1 = n1 - 1
			    }
			}
		    }
		} else {
		    # Compute the sigma without scale correction.
		    s = 0.
		    do j = 1, n1
			s = s + (Memd[d[j]+k] - a) ** 2
		    s = sqrt (s / (n1 - 1))

		    # Reject pixels.  Save the residuals and data values.
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp1 = d[j] + k
			    d1 = Memd[dp1]
			    r = (d1 - a) / s
			    if (r < -lsigma || r > hsigma) {
				Memr[resid+n1] = abs (r)
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
		}

		# Recompute the average.
		if (n1 > 1)
		    a = sum / n1
	    } until (n1 == n2 || n1 <= max (2, maxkeep))

	    # If too many pixels are rejected add some back.
	    # All pixels with equal residuals are added back.
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

		# Recompute the average.
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
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	call sfree (sp)
end


# IC_MSIGCLIP -- Reject pixels using sigma clipping about the median

procedure ic_msigclipd (d, m, n, scales, zeros, nimages, npts, median)

pointer	d[nimages]		# Data pointers
pointer	m[nimages]		# Image id pointers
int	n[npts]			# Number of good pixels
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
int	nimages			# Number of images
int	npts			# Number of output points per line
double	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, nl, nh, nin, maxkeep
real	r, s
pointer	sp, resid, w, mp1, mp2
double	med, one
data	one /1.0D0/

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nkeep < 0)
	    maxkeep = max (0, nimages + nkeep)
	else
	    maxkeep = min (nimages, nkeep)
	if (nimages < max (MINCLIP, maxkeep+1) || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Save the residuals and sigma scaling corrections if needed.
	call smark (sp)
	call salloc (resid, nimages+1, TY_REAL)
	if (doscale1)
	    call salloc (w, nimages, TY_REAL)

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
		else if (mod (n1, 2) == 0)
		    med = (Memd[d[n3-1]+k] + Memd[d[n3]+k]) / 2.
		else
		    med = Memd[d[n3]+k]

		if (n1 >= max (MINCLIP, maxkeep+1)) {
		    if (doscale1) {
			# Compute the sigma with scaling correction.
			s = 0.
			do j = nl, nh {
			    l = Memi[m[j]+k]
			    r = sqrt (max (one, (med + zeros[l]) / scales[l]))
			    s = s + ((Memd[d[j]+k] - med) / r) ** 2
			    Memr[w+j-1] = r
			}
			s = sqrt (s / (n1 - 1))

			# Reject pixels and save the residuals.
			if (s > 0.) {
			    for (; nl <= nh; nl = nl + 1) {
				r = (med - Memd[d[nl]+k]) / (s * Memr[w+nl-1])
				if (r <= lsigma)
				    break
				Memr[resid+nl] = r
				n1 = n1 - 1
			    }
			    for (; nh >= nl; nh = nh - 1) {
				r = (Memd[d[nh]+k] - med) / (s * Memr[w+nh-1])
				if (r <= hsigma)
				    break
				Memr[resid+nh] = r
				n1 = n1 - 1
			    }
			}
		    } else {
			# Compute the sigma without scaling correction.
			s = 0.
			do j = nl, nh
			    s = s + (Memd[d[j]+k] - med) ** 2
			s = sqrt (s / (n1 - 1))

			# Reject pixels and save the residuals.
			if (s > 0.) {
			    for (; nl <= nh; nl = nl + 1) {
				r = (med - Memd[d[nl]+k]) / s
				if (r <= lsigma)
				    break
				Memr[resid+nl] = r
				n1 = n1 - 1
			    }
			    for (; nh >= nl; nh = nh - 1) {
				r = (Memd[d[nh]+k] -  med) / s
				if (r <= hsigma)
				    break
				Memr[resid+nh] = r
				n1 = n1 - 1
			    }
			}
		    }
		}
	    } until (n1 == n2 || n1 < max (MINCLIP, maxkeep+1))

	    # If too many pixels are rejected add some back.
	    # All pixels with equal residuals are added back.
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

	# Flag that the median has been computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

