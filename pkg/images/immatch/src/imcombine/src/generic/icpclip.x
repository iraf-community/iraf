# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"

define	MINCLIP		3	# Minimum number for clipping


# IC_PCLIP -- Percentile clip
#
# 1) Find the median
# 2) Find the pixel which is the specified order index away
# 3) Use the data value difference as a sigma and apply clipping
# 4) Since the median is known return it so it does not have to be recomputed

procedure ic_pclips (d, m, n, nimages, npts, median)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
int	nimages			# Number of input images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, n4, n5, nl, nh, nin, maxkeep
bool	even, fp_equalr()
real	sigma, r, s, t
pointer	sp, resid, mp1, mp2
real	med

include	"../icombine.com"

begin
	# There must be at least MINCLIP and more than nkeep pixels.
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

	# Set sign of pclip parameter
	if (pclip < 0)
	    t = -1.
	else
	    t = 1.

	# If there are no rejected pixels compute certain parameters once.
	if (dflag == D_ALL) {
	    n1 = max (0, n[1])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    n2 = 1 + n1 / 2
	    even = (mod (n1, 2) == 0)
	    if (pclip < 0.) {
		if (even)
		    n3 = max (1, nint (n2 - 1 + pclip))
		else
		    n3 = max (1, nint (n2 + pclip))
	    } else
		n3 = min (n1, nint (n2 + pclip))
	    nin = n1
	}

	# Now apply clipping.
	do i = 1, npts {
	    # Compute median.
	    if (dflag == D_MIX) {
		n1 = max (0, n[i])
		if (nkeep < 0)
		    maxkeep = max (0, n1 + nkeep)
		else
		    maxkeep = min (n1, nkeep)
		if (n1 == 0) {
		    if (combine == MEDIAN)
			median[i] = blank
		    next
		}
		n2 = 1 + n1 / 2
		even = (mod (n1, 2) == 0)
		if (pclip < 0) {
		    if (even)
			n3 = max (1, nint (n2 - 1 + pclip))
		    else
			n3 = max (1, nint (n2 + pclip))
		} else
		    n3 = min (n1, nint (n2 + pclip))
	    }

	    j = i - 1 
	    if (even) {
		med = Mems[d[n2-1]+j]
		med = (med + Mems[d[n2]+j]) / 2.
	    } else
		med = Mems[d[n2]+j]

	    if (n1 < max (MINCLIP, maxkeep+1)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Define sigma for clipping
	    sigma = t * (Mems[d[n3]+j] - med)
	    if (fp_equalr (sigma, 0.)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Reject pixels and save residuals.
	    # Check if any pixels are clipped.
	    # If so recompute the median and reset the number of good pixels.
	    # Only reorder if needed.

	    for (nl=1; nl<=n1; nl=nl+1) {
		r = (med - Mems[d[nl]+j]) / sigma
		if (r < lsigma)
		    break
		Memr[resid+nl] = r
	    }
	    for (nh=n1; nh>=1; nh=nh-1) {
		r = (Mems[d[nh]+j] -  med) / sigma
		if (r < hsigma)
		    break
		Memr[resid+nh] = r
	    }
	    n4 = nh - nl + 1

	    # If too many pixels are rejected add some back in.
	    # All pixels with the same residual are added.
	    while (n4 < maxkeep) {
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
		n4 = nh - nl + 1
	    }

	    # If any pixels are rejected recompute the median.
	    if (nl > 1 || nh < n1) {
		n5 = nl + n4 / 2
		if (mod (n4, 2) == 0) {
		    med = Mems[d[n5-1]+j]
		    med = (med + Mems[d[n5]+j]) / 2.
		} else
		    med = Mems[d[n5]+j]
		n[i] = n4
	    }
	    if (combine == MEDIAN)
		median[i] = med

	    # Reorder if pixels only if necessary.
	    if (nl > 1 && (combine != MEDIAN || grow >= 1.)) {
		k = max (nl, n4 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
			Mems[d[l]+j] = Mems[d[k]+j]
			if (grow >= 1.) {
			    mp1 = m[l] + j
			    mp2 = m[k] + j
			    id = Memi[mp1]
			    Memi[mp1] = Memi[mp2]
			    Memi[mp2] = id
			} else
			    Memi[m[l]+j] = Memi[m[k]+j]
			k = k + 1
		    }
		} else {
		    do l = 1, min (n1, nl - 1) {
			Mems[d[l]+j] = Mems[d[k]+j]
			k = k + 1
		    }
		}
	    }
	}

	# Check if data flag needs to be reset for rejected pixels.
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	# Flag whether the median has been computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

# IC_PCLIP -- Percentile clip
#
# 1) Find the median
# 2) Find the pixel which is the specified order index away
# 3) Use the data value difference as a sigma and apply clipping
# 4) Since the median is known return it so it does not have to be recomputed

procedure ic_pclipi (d, m, n, nimages, npts, median)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
int	nimages			# Number of input images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, n4, n5, nl, nh, nin, maxkeep
bool	even, fp_equalr()
real	sigma, r, s, t
pointer	sp, resid, mp1, mp2
real	med

include	"../icombine.com"

begin
	# There must be at least MINCLIP and more than nkeep pixels.
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

	# Set sign of pclip parameter
	if (pclip < 0)
	    t = -1.
	else
	    t = 1.

	# If there are no rejected pixels compute certain parameters once.
	if (dflag == D_ALL) {
	    n1 = max (0, n[1])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    n2 = 1 + n1 / 2
	    even = (mod (n1, 2) == 0)
	    if (pclip < 0.) {
		if (even)
		    n3 = max (1, nint (n2 - 1 + pclip))
		else
		    n3 = max (1, nint (n2 + pclip))
	    } else
		n3 = min (n1, nint (n2 + pclip))
	    nin = n1
	}

	# Now apply clipping.
	do i = 1, npts {
	    # Compute median.
	    if (dflag == D_MIX) {
		n1 = max (0, n[i])
		if (nkeep < 0)
		    maxkeep = max (0, n1 + nkeep)
		else
		    maxkeep = min (n1, nkeep)
		if (n1 == 0) {
		    if (combine == MEDIAN)
			median[i] = blank
		    next
		}
		n2 = 1 + n1 / 2
		even = (mod (n1, 2) == 0)
		if (pclip < 0) {
		    if (even)
			n3 = max (1, nint (n2 - 1 + pclip))
		    else
			n3 = max (1, nint (n2 + pclip))
		} else
		    n3 = min (n1, nint (n2 + pclip))
	    }

	    j = i - 1 
	    if (even) {
		med = Memi[d[n2-1]+j]
		med = (med + Memi[d[n2]+j]) / 2.
	    } else
		med = Memi[d[n2]+j]

	    if (n1 < max (MINCLIP, maxkeep+1)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Define sigma for clipping
	    sigma = t * (Memi[d[n3]+j] - med)
	    if (fp_equalr (sigma, 0.)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Reject pixels and save residuals.
	    # Check if any pixels are clipped.
	    # If so recompute the median and reset the number of good pixels.
	    # Only reorder if needed.

	    for (nl=1; nl<=n1; nl=nl+1) {
		r = (med - Memi[d[nl]+j]) / sigma
		if (r < lsigma)
		    break
		Memr[resid+nl] = r
	    }
	    for (nh=n1; nh>=1; nh=nh-1) {
		r = (Memi[d[nh]+j] -  med) / sigma
		if (r < hsigma)
		    break
		Memr[resid+nh] = r
	    }
	    n4 = nh - nl + 1

	    # If too many pixels are rejected add some back in.
	    # All pixels with the same residual are added.
	    while (n4 < maxkeep) {
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
		n4 = nh - nl + 1
	    }

	    # If any pixels are rejected recompute the median.
	    if (nl > 1 || nh < n1) {
		n5 = nl + n4 / 2
		if (mod (n4, 2) == 0) {
		    med = Memi[d[n5-1]+j]
		    med = (med + Memi[d[n5]+j]) / 2.
		} else
		    med = Memi[d[n5]+j]
		n[i] = n4
	    }
	    if (combine == MEDIAN)
		median[i] = med

	    # Reorder if pixels only if necessary.
	    if (nl > 1 && (combine != MEDIAN || grow >= 1.)) {
		k = max (nl, n4 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
			Memi[d[l]+j] = Memi[d[k]+j]
			if (grow >= 1.) {
			    mp1 = m[l] + j
			    mp2 = m[k] + j
			    id = Memi[mp1]
			    Memi[mp1] = Memi[mp2]
			    Memi[mp2] = id
			} else
			    Memi[m[l]+j] = Memi[m[k]+j]
			k = k + 1
		    }
		} else {
		    do l = 1, min (n1, nl - 1) {
			Memi[d[l]+j] = Memi[d[k]+j]
			k = k + 1
		    }
		}
	    }
	}

	# Check if data flag needs to be reset for rejected pixels.
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	# Flag whether the median has been computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

# IC_PCLIP -- Percentile clip
#
# 1) Find the median
# 2) Find the pixel which is the specified order index away
# 3) Use the data value difference as a sigma and apply clipping
# 4) Since the median is known return it so it does not have to be recomputed

procedure ic_pclipr (d, m, n, nimages, npts, median)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
int	nimages			# Number of input images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, n4, n5, nl, nh, nin, maxkeep
bool	even, fp_equalr()
real	sigma, r, s, t
pointer	sp, resid, mp1, mp2
real	med

include	"../icombine.com"

begin
	# There must be at least MINCLIP and more than nkeep pixels.
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

	# Set sign of pclip parameter
	if (pclip < 0)
	    t = -1.
	else
	    t = 1.

	# If there are no rejected pixels compute certain parameters once.
	if (dflag == D_ALL) {
	    n1 = max (0, n[1])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    n2 = 1 + n1 / 2
	    even = (mod (n1, 2) == 0)
	    if (pclip < 0.) {
		if (even)
		    n3 = max (1, nint (n2 - 1 + pclip))
		else
		    n3 = max (1, nint (n2 + pclip))
	    } else
		n3 = min (n1, nint (n2 + pclip))
	    nin = n1
	}

	# Now apply clipping.
	do i = 1, npts {
	    # Compute median.
	    if (dflag == D_MIX) {
		n1 = max (0, n[i])
		if (nkeep < 0)
		    maxkeep = max (0, n1 + nkeep)
		else
		    maxkeep = min (n1, nkeep)
		if (n1 == 0) {
		    if (combine == MEDIAN)
			median[i] = blank
		    next
		}
		n2 = 1 + n1 / 2
		even = (mod (n1, 2) == 0)
		if (pclip < 0) {
		    if (even)
			n3 = max (1, nint (n2 - 1 + pclip))
		    else
			n3 = max (1, nint (n2 + pclip))
		} else
		    n3 = min (n1, nint (n2 + pclip))
	    }

	    j = i - 1 
	    if (even) {
		med = Memr[d[n2-1]+j]
		med = (med + Memr[d[n2]+j]) / 2.
	    } else
		med = Memr[d[n2]+j]

	    if (n1 < max (MINCLIP, maxkeep+1)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Define sigma for clipping
	    sigma = t * (Memr[d[n3]+j] - med)
	    if (fp_equalr (sigma, 0.)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Reject pixels and save residuals.
	    # Check if any pixels are clipped.
	    # If so recompute the median and reset the number of good pixels.
	    # Only reorder if needed.

	    for (nl=1; nl<=n1; nl=nl+1) {
		r = (med - Memr[d[nl]+j]) / sigma
		if (r < lsigma)
		    break
		Memr[resid+nl] = r
	    }
	    for (nh=n1; nh>=1; nh=nh-1) {
		r = (Memr[d[nh]+j] -  med) / sigma
		if (r < hsigma)
		    break
		Memr[resid+nh] = r
	    }
	    n4 = nh - nl + 1

	    # If too many pixels are rejected add some back in.
	    # All pixels with the same residual are added.
	    while (n4 < maxkeep) {
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
		n4 = nh - nl + 1
	    }

	    # If any pixels are rejected recompute the median.
	    if (nl > 1 || nh < n1) {
		n5 = nl + n4 / 2
		if (mod (n4, 2) == 0) {
		    med = Memr[d[n5-1]+j]
		    med = (med + Memr[d[n5]+j]) / 2.
		} else
		    med = Memr[d[n5]+j]
		n[i] = n4
	    }
	    if (combine == MEDIAN)
		median[i] = med

	    # Reorder if pixels only if necessary.
	    if (nl > 1 && (combine != MEDIAN || grow >= 1.)) {
		k = max (nl, n4 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
			Memr[d[l]+j] = Memr[d[k]+j]
			if (grow >= 1.) {
			    mp1 = m[l] + j
			    mp2 = m[k] + j
			    id = Memi[mp1]
			    Memi[mp1] = Memi[mp2]
			    Memi[mp2] = id
			} else
			    Memi[m[l]+j] = Memi[m[k]+j]
			k = k + 1
		    }
		} else {
		    do l = 1, min (n1, nl - 1) {
			Memr[d[l]+j] = Memr[d[k]+j]
			k = k + 1
		    }
		}
	    }
	}

	# Check if data flag needs to be reset for rejected pixels.
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	# Flag whether the median has been computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

# IC_PCLIP -- Percentile clip
#
# 1) Find the median
# 2) Find the pixel which is the specified order index away
# 3) Use the data value difference as a sigma and apply clipping
# 4) Since the median is known return it so it does not have to be recomputed

procedure ic_pclipd (d, m, n, nimages, npts, median)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
int	nimages			# Number of input images
int	npts			# Number of output points per line
double	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, n4, n5, nl, nh, nin, maxkeep
bool	even, fp_equalr()
real	sigma, r, s, t
pointer	sp, resid, mp1, mp2
double	med

include	"../icombine.com"

begin
	# There must be at least MINCLIP and more than nkeep pixels.
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

	# Set sign of pclip parameter
	if (pclip < 0)
	    t = -1.
	else
	    t = 1.

	# If there are no rejected pixels compute certain parameters once.
	if (dflag == D_ALL) {
	    n1 = max (0, n[1])
	    if (nkeep < 0)
		maxkeep = max (0, n1 + nkeep)
	    else
		maxkeep = min (n1, nkeep)
	    n2 = 1 + n1 / 2
	    even = (mod (n1, 2) == 0)
	    if (pclip < 0.) {
		if (even)
		    n3 = max (1, nint (n2 - 1 + pclip))
		else
		    n3 = max (1, nint (n2 + pclip))
	    } else
		n3 = min (n1, nint (n2 + pclip))
	    nin = n1
	}

	# Now apply clipping.
	do i = 1, npts {
	    # Compute median.
	    if (dflag == D_MIX) {
		n1 = max (0, n[i])
		if (nkeep < 0)
		    maxkeep = max (0, n1 + nkeep)
		else
		    maxkeep = min (n1, nkeep)
		if (n1 == 0) {
		    if (combine == MEDIAN)
			median[i] = blank
		    next
		}
		n2 = 1 + n1 / 2
		even = (mod (n1, 2) == 0)
		if (pclip < 0) {
		    if (even)
			n3 = max (1, nint (n2 - 1 + pclip))
		    else
			n3 = max (1, nint (n2 + pclip))
		} else
		    n3 = min (n1, nint (n2 + pclip))
	    }

	    j = i - 1 
	    if (even) {
		med = Memd[d[n2-1]+j]
		med = (med + Memd[d[n2]+j]) / 2.
	    } else
		med = Memd[d[n2]+j]

	    if (n1 < max (MINCLIP, maxkeep+1)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Define sigma for clipping
	    sigma = t * (Memd[d[n3]+j] - med)
	    if (fp_equalr (sigma, 0.)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Reject pixels and save residuals.
	    # Check if any pixels are clipped.
	    # If so recompute the median and reset the number of good pixels.
	    # Only reorder if needed.

	    for (nl=1; nl<=n1; nl=nl+1) {
		r = (med - Memd[d[nl]+j]) / sigma
		if (r < lsigma)
		    break
		Memr[resid+nl] = r
	    }
	    for (nh=n1; nh>=1; nh=nh-1) {
		r = (Memd[d[nh]+j] -  med) / sigma
		if (r < hsigma)
		    break
		Memr[resid+nh] = r
	    }
	    n4 = nh - nl + 1

	    # If too many pixels are rejected add some back in.
	    # All pixels with the same residual are added.
	    while (n4 < maxkeep) {
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
		n4 = nh - nl + 1
	    }

	    # If any pixels are rejected recompute the median.
	    if (nl > 1 || nh < n1) {
		n5 = nl + n4 / 2
		if (mod (n4, 2) == 0) {
		    med = Memd[d[n5-1]+j]
		    med = (med + Memd[d[n5]+j]) / 2.
		} else
		    med = Memd[d[n5]+j]
		n[i] = n4
	    }
	    if (combine == MEDIAN)
		median[i] = med

	    # Reorder if pixels only if necessary.
	    if (nl > 1 && (combine != MEDIAN || grow >= 1.)) {
		k = max (nl, n4 + 1)
		if (keepids) {
		    do l = 1, min (n1, nl-1) {
			Memd[d[l]+j] = Memd[d[k]+j]
			if (grow >= 1.) {
			    mp1 = m[l] + j
			    mp2 = m[k] + j
			    id = Memi[mp1]
			    Memi[mp1] = Memi[mp2]
			    Memi[mp2] = id
			} else
			    Memi[m[l]+j] = Memi[m[k]+j]
			k = k + 1
		    }
		} else {
		    do l = 1, min (n1, nl - 1) {
			Memd[d[l]+j] = Memd[d[k]+j]
			k = k + 1
		    }
		}
	    }
	}

	# Check if data flag needs to be reset for rejected pixels.
	if (dflag == D_ALL) {
	    do i = 1, npts {
		if (max (0, n[i]) != nin) {
		    dflag = D_MIX
		    break
		}
	    }
	}

	# Flag whether the median has been computed.
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true

	call sfree (sp)
end

