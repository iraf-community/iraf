# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"

define	MINCLIP		3	# Mininum number of images for algorithm


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

int	i, j, k, l, n1, n2
real	d1, low, high, sum, a, s, r
pointer	w, dp, wp, mp1, mp2

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	} 
	
	if (dowts || combine != AVERAGE)
	    docombine = true
	else
	    docombine = false

	# Save the sigma scaling corrections if needed.
	if (doscale1)
	    call malloc (w, nimages, TY_REAL)

	do i = 1, npts {
	    k = i - 1
	    n1 = n[i]
	    if (n1 < 3) {
		if (!docombine) {
		    if (n1 == 2)
		        average[i] = (Memr[d[1]+k] + Memr[d[2]+k]) / 2
		    else if (n1 == 1)
			average[i] = Memr[d[1]+k]
		    else
			average[i] = blank
		}
		next
	    }

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

	    # Iteratively reject pixels and compute the final average if needed.
	    # Compact the data and keep track of the image IDs if needed.

	    repeat {
		n2 = n1
		if (doscale1) {
		    s = 0.
		    wp = w - 1
		    do j = 1, n1 {
			dp = d[j] + k
			mp1 = m[j] + k
			wp = wp + 1

			d1 = Memr[dp]
			l = Memi[mp1]
			r = sqrt (max (1., (a - zeros[l]) / scales[l]))
			s = s + ((d1 - a) / r) ** 2
			Memr[wp] = r
		    }
		    s = sqrt (s / (n1 - 1))

		    wp = w - 1
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
			    dp = d[j] + k
			    mp1 = m[j] + k
			    wp = wp + 1

			    d1 = Memr[dp]
			    r = (d1 - a) / (s * Memr[wp])
			    if (r < -lsigma || r > hsigma) {
				if (j < n1) {
				    Memr[dp] = Memr[d[n1]+k]
				    Memr[wp] = Memr[w+n1-1]
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
		    }
		} else {
		    s = 0.
		    do j = 1, n1
			s = s + (Memr[d[j]+k] - a) ** 2
		    s = sqrt (s / (n1 - 1))
		    if (s > 0.) {
			for (j=1; j<=n1; j=j+1) {
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
		}
		if (n1 > 1)
		    a = sum / n1
	    } until (n1 == n2 || n1 < 3)

	    n[i] = n1
	    if (!docombine)
		if (n1 > 0)
		    average[i] = a
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

	if (doscale1)
	    call mfree (w, TY_REAL)
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

int	i, j, k, l, id, n1, n2, n3, nl, nh
real	r, s
pointer	w, mp1, mp2
real	med

include	"../icombine.com"

begin
	# If there are insufficient pixels go on to the combining
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Save the sigma scaling corrections if needed.
	if (doscale1)
	    call malloc (w, nimages, TY_REAL)

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
			s = 0.
			do j = nl, nh {
			    l = Memi[m[j]+k]
			    r = sqrt (max (1., (med - zeros[l]) / scales[l]))
			    s = s + ((Memr[d[j]+k] - med) / r) ** 2
			    Memr[w+j-1] = r
			}
			s = sqrt (s / (n1 - 1))

			if (s > 0.) {
			    for (; nl <= n2; nl = nl + 1) {
				r = (med - Memr[d[nl]+k]) / (s * Memr[w+nl-1])
				if (r <= lsigma)
				    break
				n1 = n1 - 1
			    }
			    for (; nh >= 1; nh = nh - 1) {
				r = (Memr[d[nh]+k] - med) / (s * Memr[w+nl-1])
				if (r <= hsigma)
				    break
				n1 = n1 - 1
			    }
			}
		    } else {
			s = 0.
			do j = nl, nh
			    s = s + (Memr[d[j]+k] - med) ** 2
			s = sqrt (s / (n1 - 1))

			if (s > 0.) {
			    for (; nl <= n2; nl = nl + 1) {
				r = (med - Memr[d[nl]+k]) / s
				if (r <= lsigma)
				    break
				n1 = n1 - 1
			    }
			    for (; nh >= 1; nh = nh - 1) {
				r = (Memr[d[nh]+k] -  med) / s
				if (r <= hsigma)
				    break
				n1 = n1 - 1
			    }
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

	if (doscale1)
	    call mfree (w, TY_REAL)
end
