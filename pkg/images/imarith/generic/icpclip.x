# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../icombine.h"

define	MINCLIP		3	# Minimum number for clipping


# IC_PCLIP -- Percentile clip
#
# 1) Sort the pixels
# 2) Find the median
# 3) Find the pixel which is the specified order index away
# 4) Use the data value difference as a sigma and apply clipping
# 5) Since the median is known return it so it does not have to computed again

procedure ic_pclips (d, m, n, nimages, npts, median)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
int	nimages			# Number of input images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, n4, n5, nl, nh
bool	even, fp_equalr()
real	sigma, r, s
pointer	mp1, mp2
real	med

include	"../icombine.com"

begin
	# There must be at  least MINCLIP pixels.
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Set sign of pclip parameter
	if (pclip < 0)
	    s = -1.
	else
	    s = 1.

	# If there are no rejected pixels compute certain parameters once.
	if (dflag == D_ALL) {
	    n1 = n[1]
	    n2 = 1 + n1 / 2
	    even = (mod (n1, 2) == 0)
	    if (pclip < 0.) {
		if (even)
		    n3 = max (1, nint (n2 - 1 + pclip))
		else
		    n3 = max (1, nint (n2 + pclip))
	    } else
		n3 = min (n1, nint (n2 + pclip))
	}

	do i = 1, npts {
	    if (dflag == D_MIX) {
		n1 = n[i]
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

	    if (n1 < MINCLIP) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    sigma = s * (Mems[d[n3]+j] - med)
	    if (fp_equalr (sigma, 0.)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Check if pixels are clipped
	    # If  so recompute the median and rest the number of good pixels
	    # Only reorder if needed

	    do nl = 1, n1 {
		r = (med - Mems[d[nl]+j]) / sigma
		if (r < lsigma)
		    break
	    }
	    do nh = n1, 1, -1 {
		r = (Mems[d[nh]+j] -  med) / sigma
		if (r < hsigma)
		    break
	    }

	    if (nl > 1 || nh < n1) {
		n4 = nh - nl + 1
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
	    if (nl > 1 && (combine != MEDIAN || grow > 0)) {
		k = n4 + 1
		if (keepids) {
		    do l = 1, nl-1 {
			Mems[d[l]+j] = Mems[d[k]+j]
			if (grow > 0) {
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
		    do l = 1, nl - 1 {
			Mems[d[l]+j] = Mems[d[k]+j]
			k = k + 1
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

	# Signal that the median has been computed
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true
end

# IC_PCLIP -- Percentile clip
#
# 1) Sort the pixels
# 2) Find the median
# 3) Find the pixel which is the specified order index away
# 4) Use the data value difference as a sigma and apply clipping
# 5) Since the median is known return it so it does not have to computed again

procedure ic_pclipi (d, m, n, nimages, npts, median)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
int	nimages			# Number of input images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, n4, n5, nl, nh
bool	even, fp_equalr()
real	sigma, r, s
pointer	mp1, mp2
real	med

include	"../icombine.com"

begin
	# There must be at  least MINCLIP pixels.
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Set sign of pclip parameter
	if (pclip < 0)
	    s = -1.
	else
	    s = 1.

	# If there are no rejected pixels compute certain parameters once.
	if (dflag == D_ALL) {
	    n1 = n[1]
	    n2 = 1 + n1 / 2
	    even = (mod (n1, 2) == 0)
	    if (pclip < 0.) {
		if (even)
		    n3 = max (1, nint (n2 - 1 + pclip))
		else
		    n3 = max (1, nint (n2 + pclip))
	    } else
		n3 = min (n1, nint (n2 + pclip))
	}

	do i = 1, npts {
	    if (dflag == D_MIX) {
		n1 = n[i]
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

	    if (n1 < MINCLIP) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    sigma = s * (Memi[d[n3]+j] - med)
	    if (fp_equalr (sigma, 0.)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Check if pixels are clipped
	    # If  so recompute the median and rest the number of good pixels
	    # Only reorder if needed

	    do nl = 1, n1 {
		r = (med - Memi[d[nl]+j]) / sigma
		if (r < lsigma)
		    break
	    }
	    do nh = n1, 1, -1 {
		r = (Memi[d[nh]+j] -  med) / sigma
		if (r < hsigma)
		    break
	    }

	    if (nl > 1 || nh < n1) {
		n4 = nh - nl + 1
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
	    if (nl > 1 && (combine != MEDIAN || grow > 0)) {
		k = n4 + 1
		if (keepids) {
		    do l = 1, nl-1 {
			Memi[d[l]+j] = Memi[d[k]+j]
			if (grow > 0) {
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
		    do l = 1, nl - 1 {
			Memi[d[l]+j] = Memi[d[k]+j]
			k = k + 1
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

	# Signal that the median has been computed
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true
end

# IC_PCLIP -- Percentile clip
#
# 1) Sort the pixels
# 2) Find the median
# 3) Find the pixel which is the specified order index away
# 4) Use the data value difference as a sigma and apply clipping
# 5) Since the median is known return it so it does not have to computed again

procedure ic_pclipr (d, m, n, nimages, npts, median)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
int	nimages			# Number of input images
int	npts			# Number of output points per line
real	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, n4, n5, nl, nh
bool	even, fp_equalr()
real	sigma, r, s
pointer	mp1, mp2
real	med

include	"../icombine.com"

begin
	# There must be at  least MINCLIP pixels.
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Set sign of pclip parameter
	if (pclip < 0)
	    s = -1.
	else
	    s = 1.

	# If there are no rejected pixels compute certain parameters once.
	if (dflag == D_ALL) {
	    n1 = n[1]
	    n2 = 1 + n1 / 2
	    even = (mod (n1, 2) == 0)
	    if (pclip < 0.) {
		if (even)
		    n3 = max (1, nint (n2 - 1 + pclip))
		else
		    n3 = max (1, nint (n2 + pclip))
	    } else
		n3 = min (n1, nint (n2 + pclip))
	}

	do i = 1, npts {
	    if (dflag == D_MIX) {
		n1 = n[i]
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

	    if (n1 < MINCLIP) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    sigma = s * (Memr[d[n3]+j] - med)
	    if (fp_equalr (sigma, 0.)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Check if pixels are clipped
	    # If  so recompute the median and rest the number of good pixels
	    # Only reorder if needed

	    do nl = 1, n1 {
		r = (med - Memr[d[nl]+j]) / sigma
		if (r < lsigma)
		    break
	    }
	    do nh = n1, 1, -1 {
		r = (Memr[d[nh]+j] -  med) / sigma
		if (r < hsigma)
		    break
	    }

	    if (nl > 1 || nh < n1) {
		n4 = nh - nl + 1
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
	    if (nl > 1 && (combine != MEDIAN || grow > 0)) {
		k = n4 + 1
		if (keepids) {
		    do l = 1, nl-1 {
			Memr[d[l]+j] = Memr[d[k]+j]
			if (grow > 0) {
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
		    do l = 1, nl - 1 {
			Memr[d[l]+j] = Memr[d[k]+j]
			k = k + 1
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

	# Signal that the median has been computed
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true
end

# IC_PCLIP -- Percentile clip
#
# 1) Sort the pixels
# 2) Find the median
# 3) Find the pixel which is the specified order index away
# 4) Use the data value difference as a sigma and apply clipping
# 5) Since the median is known return it so it does not have to computed again

procedure ic_pclipd (d, m, n, nimages, npts, median)

pointer	d[ARB]			# Data pointers
pointer	m[ARB]			# Image id pointers
int	n[npts]			# Number of good pixels
int	nimages			# Number of input images
int	npts			# Number of output points per line
double	median[npts]		# Median

int	i, j, k, l, id, n1, n2, n3, n4, n5, nl, nh
bool	even, fp_equalr()
real	sigma, r, s
pointer	mp1, mp2
double	med

include	"../icombine.com"

begin
	# There must be at  least MINCLIP pixels.
	if (nimages < MINCLIP || dflag == D_NONE) {
	    docombine = true
	    return
	}

	# Set sign of pclip parameter
	if (pclip < 0)
	    s = -1.
	else
	    s = 1.

	# If there are no rejected pixels compute certain parameters once.
	if (dflag == D_ALL) {
	    n1 = n[1]
	    n2 = 1 + n1 / 2
	    even = (mod (n1, 2) == 0)
	    if (pclip < 0.) {
		if (even)
		    n3 = max (1, nint (n2 - 1 + pclip))
		else
		    n3 = max (1, nint (n2 + pclip))
	    } else
		n3 = min (n1, nint (n2 + pclip))
	}

	do i = 1, npts {
	    if (dflag == D_MIX) {
		n1 = n[i]
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

	    if (n1 < MINCLIP) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    sigma = s * (Memd[d[n3]+j] - med)
	    if (fp_equalr (sigma, 0.)) {
		if (combine == MEDIAN)
		    median[i] = med
		next
	    }

	    # Check if pixels are clipped
	    # If  so recompute the median and rest the number of good pixels
	    # Only reorder if needed

	    do nl = 1, n1 {
		r = (med - Memd[d[nl]+j]) / sigma
		if (r < lsigma)
		    break
	    }
	    do nh = n1, 1, -1 {
		r = (Memd[d[nh]+j] -  med) / sigma
		if (r < hsigma)
		    break
	    }

	    if (nl > 1 || nh < n1) {
		n4 = nh - nl + 1
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
	    if (nl > 1 && (combine != MEDIAN || grow > 0)) {
		k = n4 + 1
		if (keepids) {
		    do l = 1, nl-1 {
			Memd[d[l]+j] = Memd[d[k]+j]
			if (grow > 0) {
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
		    do l = 1, nl - 1 {
			Memd[d[l]+j] = Memd[d[k]+j]
			k = k + 1
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

	# Signal that the median has been computed
	if (combine == MEDIAN)
	    docombine = false
	else
	    docombine = true
end

