# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	"../icombine.h"


# IC_GDATA -- Get line of image and mask data and apply threshold and scaling.
# Entirely empty lines are excluded.  The data are compacted within the
# input data buffers.  If it is required, the connection to the original
# image index is kept in the returned m data pointers.

procedure ic_gdatas (in, out, dbuf, d, id, n, m, lflag, offsets, scales,
	zeros, nimages, npts, v1, v2)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
pointer	dbuf[nimages]		# Data buffers
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Empty mask flags
int	offsets[nimages,ARB]	# Image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
int	nimages			# Number of input images
int	npts			# NUmber of output points per line
long	v1[ARB], v2[ARB]	# Line vectors

int	i, j, k, l, ndim, nused
real	a, b
pointer	buf, dp, ip, mp, imgnls()

include	"../icombine.com"

begin
	# Get masks and return if there is no data
	call ic_mget (in, out, offsets, v1, v2, m, lflag, nimages)
	if (dflag == D_NONE) {
	    call aclri (n, npts)
	    return
	}

	# Get data and fill data buffers.  Correct for offsets if needed.
	ndim = IM_NDIM(out[1])
	do i = 1, nimages {
	    if (lflag[i] == D_NONE)
		next
	    if (dbuf[i] == NULL) {
		call amovl (v1, v2, IM_MAXDIM)
		if (project)
		    v2[ndim+1] = i
		j = imgnls (in[i], d[i], v2)
	    } else {
		v2[1] = v1[1]
		do j = 2, ndim
		    v2[j] = v1[j] - offsets[i,j]
		if (project)
		    v2[ndim+1] = i
		j = imgnls (in[i], buf, v2)
		call amovs (Mems[buf], Mems[dbuf[i]+offsets[i,1]],
		    IM_LEN(in[i],1))
		d[i] = dbuf[i]
	    }
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		dp = d[i]
		if (lflag[i] == D_ALL) {
		    do j = 1, npts {
			a = Mems[dp]
			if (a < lthresh || a > hthresh) {
			    Memi[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
		    }
		} else if (lflag[i] == D_MIX) {
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    a = Mems[dp]
			    if (a < lthresh || a > hthresh) {
				Memi[m[i]+j-1] = 1
				dflag = D_MIX
			    }
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}

		# Check for completely empty lines
		if (lflag[i] == D_MIX) {
		    lflag[i] = D_NONE
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    lflag[i] = D_MIX
			    break
			}
			mp = mp + 1
		    }
		}
	    }
	}

	# Apply scaling (avoiding masked pixels which might overflow?)
	if (doscale) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    do j = 1, npts {
			Mems[dp] = Mems[dp] / a + b
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			do j = 1, npts {
			    Mems[dp] = Mems[dp] / a + b
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			mp = m[i]
			do j = 1, npts {
			    if (Memi[mp] == 0)
				Mems[dp] = Mems[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
		    }
		}
	    }
	}

	# Sort pointers to exclude unused images.
	# Use the lflag array to keep track of the image index.

	if (dflag == D_ALL)
	    nused = nimages
	else {
	    nused = 0
	    do i = 1, nimages
		if (lflag[i] != D_NONE) {
		    nused = nused + 1
		    d[nused] = d[i]
		    m[nused] = m[i]
		    lflag[nused] = i
		}
	    if (nused == 0)
		dflag = D_NONE
	}

	# Compact data to remove bad pixels
	# Keep track of the image indices if needed
	# If growing mark the end of the included image indices with zero

	if (dflag == D_ALL) {
	    call amovki (nused, n, npts)
	    if (keepids)
		do i = 1, nimages
		    call amovki (i, Memi[id[i]], npts)
	} else if (dflag == D_NONE)
	    call aclri (n, npts)
	else {
	    call aclri (n, npts)
	    if (keepids) {
		do i = 1, nused {
		    l = lflag[i]
		    dp = d[i]
		    ip = id[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i) {
				Mems[d[k]+j-1] = Mems[dp]
				Memi[id[k]+j-1] = l
			    } else
				Memi[ip] = l
			}
			dp = dp + 1
			ip = ip + 1
			mp = mp + 1
		    }
		}
		if (grow >= 1.) {
		    do j = 1, npts {
			do i = n[j]+1, nimages
			    Memi[id[i]+j-1] = 0
		    }
		}
	    } else {
		do i = 1, nused {
		    dp = d[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i)
				Mems[d[k]+j-1] = Mems[dp]
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}
	    }
	}

	# Sort the pixels and IDs if needed
	if (mclip) {
	    call malloc (dp, nimages, TY_SHORT)
	    if (keepids) {
		call malloc (ip, nimages, TY_INT)
		call ic_2sorts (d, Mems[dp], id, Memi[ip], n, npts)
		call mfree (ip, TY_INT)
	    } else
		call ic_sorts (d, Mems[dp], n, npts)
	    call mfree (dp, TY_SHORT)
	}
end

# IC_GDATA -- Get line of image and mask data and apply threshold and scaling.
# Entirely empty lines are excluded.  The data are compacted within the
# input data buffers.  If it is required, the connection to the original
# image index is kept in the returned m data pointers.

procedure ic_gdatai (in, out, dbuf, d, id, n, m, lflag, offsets, scales,
	zeros, nimages, npts, v1, v2)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
pointer	dbuf[nimages]		# Data buffers
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Empty mask flags
int	offsets[nimages,ARB]	# Image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
int	nimages			# Number of input images
int	npts			# NUmber of output points per line
long	v1[ARB], v2[ARB]	# Line vectors

int	i, j, k, l, ndim, nused
real	a, b
pointer	buf, dp, ip, mp, imgnli()

include	"../icombine.com"

begin
	# Get masks and return if there is no data
	call ic_mget (in, out, offsets, v1, v2, m, lflag, nimages)
	if (dflag == D_NONE) {
	    call aclri (n, npts)
	    return
	}

	# Get data and fill data buffers.  Correct for offsets if needed.
	ndim = IM_NDIM(out[1])
	do i = 1, nimages {
	    if (lflag[i] == D_NONE)
		next
	    if (dbuf[i] == NULL) {
		call amovl (v1, v2, IM_MAXDIM)
		if (project)
		    v2[ndim+1] = i
		j = imgnli (in[i], d[i], v2)
	    } else {
		v2[1] = v1[1]
		do j = 2, ndim
		    v2[j] = v1[j] - offsets[i,j]
		if (project)
		    v2[ndim+1] = i
		j = imgnli (in[i], buf, v2)
		call amovi (Memi[buf], Memi[dbuf[i]+offsets[i,1]],
		    IM_LEN(in[i],1))
		d[i] = dbuf[i]
	    }
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		dp = d[i]
		if (lflag[i] == D_ALL) {
		    do j = 1, npts {
			a = Memi[dp]
			if (a < lthresh || a > hthresh) {
			    Memi[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
		    }
		} else if (lflag[i] == D_MIX) {
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    a = Memi[dp]
			    if (a < lthresh || a > hthresh) {
				Memi[m[i]+j-1] = 1
				dflag = D_MIX
			    }
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}

		# Check for completely empty lines
		if (lflag[i] == D_MIX) {
		    lflag[i] = D_NONE
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    lflag[i] = D_MIX
			    break
			}
			mp = mp + 1
		    }
		}
	    }
	}

	# Apply scaling (avoiding masked pixels which might overflow?)
	if (doscale) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    do j = 1, npts {
			Memi[dp] = Memi[dp] / a + b
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			do j = 1, npts {
			    Memi[dp] = Memi[dp] / a + b
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			mp = m[i]
			do j = 1, npts {
			    if (Memi[mp] == 0)
				Memi[dp] = Memi[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
		    }
		}
	    }
	}

	# Sort pointers to exclude unused images.
	# Use the lflag array to keep track of the image index.

	if (dflag == D_ALL)
	    nused = nimages
	else {
	    nused = 0
	    do i = 1, nimages
		if (lflag[i] != D_NONE) {
		    nused = nused + 1
		    d[nused] = d[i]
		    m[nused] = m[i]
		    lflag[nused] = i
		}
	    if (nused == 0)
		dflag = D_NONE
	}

	# Compact data to remove bad pixels
	# Keep track of the image indices if needed
	# If growing mark the end of the included image indices with zero

	if (dflag == D_ALL) {
	    call amovki (nused, n, npts)
	    if (keepids)
		do i = 1, nimages
		    call amovki (i, Memi[id[i]], npts)
	} else if (dflag == D_NONE)
	    call aclri (n, npts)
	else {
	    call aclri (n, npts)
	    if (keepids) {
		do i = 1, nused {
		    l = lflag[i]
		    dp = d[i]
		    ip = id[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i) {
				Memi[d[k]+j-1] = Memi[dp]
				Memi[id[k]+j-1] = l
			    } else
				Memi[ip] = l
			}
			dp = dp + 1
			ip = ip + 1
			mp = mp + 1
		    }
		}
		if (grow >= 1.) {
		    do j = 1, npts {
			do i = n[j]+1, nimages
			    Memi[id[i]+j-1] = 0
		    }
		}
	    } else {
		do i = 1, nused {
		    dp = d[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i)
				Memi[d[k]+j-1] = Memi[dp]
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}
	    }
	}

	# Sort the pixels and IDs if needed
	if (mclip) {
	    call malloc (dp, nimages, TY_INT)
	    if (keepids) {
		call malloc (ip, nimages, TY_INT)
		call ic_2sorti (d, Memi[dp], id, Memi[ip], n, npts)
		call mfree (ip, TY_INT)
	    } else
		call ic_sorti (d, Memi[dp], n, npts)
	    call mfree (dp, TY_INT)
	}
end

# IC_GDATA -- Get line of image and mask data and apply threshold and scaling.
# Entirely empty lines are excluded.  The data are compacted within the
# input data buffers.  If it is required, the connection to the original
# image index is kept in the returned m data pointers.

procedure ic_gdatar (in, out, dbuf, d, id, n, m, lflag, offsets, scales,
	zeros, nimages, npts, v1, v2)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
pointer	dbuf[nimages]		# Data buffers
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Empty mask flags
int	offsets[nimages,ARB]	# Image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
int	nimages			# Number of input images
int	npts			# NUmber of output points per line
long	v1[ARB], v2[ARB]	# Line vectors

int	i, j, k, l, ndim, nused
real	a, b
pointer	buf, dp, ip, mp, imgnlr()

include	"../icombine.com"

begin
	# Get masks and return if there is no data
	call ic_mget (in, out, offsets, v1, v2, m, lflag, nimages)
	if (dflag == D_NONE) {
	    call aclri (n, npts)
	    return
	}

	# Get data and fill data buffers.  Correct for offsets if needed.
	ndim = IM_NDIM(out[1])
	do i = 1, nimages {
	    if (lflag[i] == D_NONE)
		next
	    if (dbuf[i] == NULL) {
		call amovl (v1, v2, IM_MAXDIM)
		if (project)
		    v2[ndim+1] = i
		j = imgnlr (in[i], d[i], v2)
	    } else {
		v2[1] = v1[1]
		do j = 2, ndim
		    v2[j] = v1[j] - offsets[i,j]
		if (project)
		    v2[ndim+1] = i
		j = imgnlr (in[i], buf, v2)
		call amovr (Memr[buf], Memr[dbuf[i]+offsets[i,1]],
		    IM_LEN(in[i],1))
		d[i] = dbuf[i]
	    }
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		dp = d[i]
		if (lflag[i] == D_ALL) {
		    do j = 1, npts {
			a = Memr[dp]
			if (a < lthresh || a > hthresh) {
			    Memi[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
		    }
		} else if (lflag[i] == D_MIX) {
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    a = Memr[dp]
			    if (a < lthresh || a > hthresh) {
				Memi[m[i]+j-1] = 1
				dflag = D_MIX
			    }
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}

		# Check for completely empty lines
		if (lflag[i] == D_MIX) {
		    lflag[i] = D_NONE
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    lflag[i] = D_MIX
			    break
			}
			mp = mp + 1
		    }
		}
	    }
	}

	# Apply scaling (avoiding masked pixels which might overflow?)
	if (doscale) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    do j = 1, npts {
			Memr[dp] = Memr[dp] / a + b
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			do j = 1, npts {
			    Memr[dp] = Memr[dp] / a + b
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			mp = m[i]
			do j = 1, npts {
			    if (Memi[mp] == 0)
				Memr[dp] = Memr[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
		    }
		}
	    }
	}

	# Sort pointers to exclude unused images.
	# Use the lflag array to keep track of the image index.

	if (dflag == D_ALL)
	    nused = nimages
	else {
	    nused = 0
	    do i = 1, nimages
		if (lflag[i] != D_NONE) {
		    nused = nused + 1
		    d[nused] = d[i]
		    m[nused] = m[i]
		    lflag[nused] = i
		}
	    if (nused == 0)
		dflag = D_NONE
	}

	# Compact data to remove bad pixels
	# Keep track of the image indices if needed
	# If growing mark the end of the included image indices with zero

	if (dflag == D_ALL) {
	    call amovki (nused, n, npts)
	    if (keepids)
		do i = 1, nimages
		    call amovki (i, Memi[id[i]], npts)
	} else if (dflag == D_NONE)
	    call aclri (n, npts)
	else {
	    call aclri (n, npts)
	    if (keepids) {
		do i = 1, nused {
		    l = lflag[i]
		    dp = d[i]
		    ip = id[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i) {
				Memr[d[k]+j-1] = Memr[dp]
				Memi[id[k]+j-1] = l
			    } else
				Memi[ip] = l
			}
			dp = dp + 1
			ip = ip + 1
			mp = mp + 1
		    }
		}
		if (grow >= 1.) {
		    do j = 1, npts {
			do i = n[j]+1, nimages
			    Memi[id[i]+j-1] = 0
		    }
		}
	    } else {
		do i = 1, nused {
		    dp = d[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i)
				Memr[d[k]+j-1] = Memr[dp]
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}
	    }
	}

	# Sort the pixels and IDs if needed
	if (mclip) {
	    call malloc (dp, nimages, TY_REAL)
	    if (keepids) {
		call malloc (ip, nimages, TY_INT)
		call ic_2sortr (d, Memr[dp], id, Memi[ip], n, npts)
		call mfree (ip, TY_INT)
	    } else
		call ic_sortr (d, Memr[dp], n, npts)
	    call mfree (dp, TY_REAL)
	}
end

# IC_GDATA -- Get line of image and mask data and apply threshold and scaling.
# Entirely empty lines are excluded.  The data are compacted within the
# input data buffers.  If it is required, the connection to the original
# image index is kept in the returned m data pointers.

procedure ic_gdatad (in, out, dbuf, d, id, n, m, lflag, offsets, scales,
	zeros, nimages, npts, v1, v2)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
pointer	dbuf[nimages]		# Data buffers
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Empty mask flags
int	offsets[nimages,ARB]	# Image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
int	nimages			# Number of input images
int	npts			# NUmber of output points per line
long	v1[ARB], v2[ARB]	# Line vectors

int	i, j, k, l, ndim, nused
real	a, b
pointer	buf, dp, ip, mp, imgnld()

include	"../icombine.com"

begin
	# Get masks and return if there is no data
	call ic_mget (in, out, offsets, v1, v2, m, lflag, nimages)
	if (dflag == D_NONE) {
	    call aclri (n, npts)
	    return
	}

	# Get data and fill data buffers.  Correct for offsets if needed.
	ndim = IM_NDIM(out[1])
	do i = 1, nimages {
	    if (lflag[i] == D_NONE)
		next
	    if (dbuf[i] == NULL) {
		call amovl (v1, v2, IM_MAXDIM)
		if (project)
		    v2[ndim+1] = i
		j = imgnld (in[i], d[i], v2)
	    } else {
		v2[1] = v1[1]
		do j = 2, ndim
		    v2[j] = v1[j] - offsets[i,j]
		if (project)
		    v2[ndim+1] = i
		j = imgnld (in[i], buf, v2)
		call amovd (Memd[buf], Memd[dbuf[i]+offsets[i,1]],
		    IM_LEN(in[i],1))
		d[i] = dbuf[i]
	    }
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		dp = d[i]
		if (lflag[i] == D_ALL) {
		    do j = 1, npts {
			a = Memd[dp]
			if (a < lthresh || a > hthresh) {
			    Memi[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
		    }
		} else if (lflag[i] == D_MIX) {
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    a = Memd[dp]
			    if (a < lthresh || a > hthresh) {
				Memi[m[i]+j-1] = 1
				dflag = D_MIX
			    }
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}

		# Check for completely empty lines
		if (lflag[i] == D_MIX) {
		    lflag[i] = D_NONE
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    lflag[i] = D_MIX
			    break
			}
			mp = mp + 1
		    }
		}
	    }
	}

	# Apply scaling (avoiding masked pixels which might overflow?)
	if (doscale) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    do j = 1, npts {
			Memd[dp] = Memd[dp] / a + b
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    dp = d[i]
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			do j = 1, npts {
			    Memd[dp] = Memd[dp] / a + b
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			mp = m[i]
			do j = 1, npts {
			    if (Memi[mp] == 0)
				Memd[dp] = Memd[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
		    }
		}
	    }
	}

	# Sort pointers to exclude unused images.
	# Use the lflag array to keep track of the image index.

	if (dflag == D_ALL)
	    nused = nimages
	else {
	    nused = 0
	    do i = 1, nimages
		if (lflag[i] != D_NONE) {
		    nused = nused + 1
		    d[nused] = d[i]
		    m[nused] = m[i]
		    lflag[nused] = i
		}
	    if (nused == 0)
		dflag = D_NONE
	}

	# Compact data to remove bad pixels
	# Keep track of the image indices if needed
	# If growing mark the end of the included image indices with zero

	if (dflag == D_ALL) {
	    call amovki (nused, n, npts)
	    if (keepids)
		do i = 1, nimages
		    call amovki (i, Memi[id[i]], npts)
	} else if (dflag == D_NONE)
	    call aclri (n, npts)
	else {
	    call aclri (n, npts)
	    if (keepids) {
		do i = 1, nused {
		    l = lflag[i]
		    dp = d[i]
		    ip = id[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i) {
				Memd[d[k]+j-1] = Memd[dp]
				Memi[id[k]+j-1] = l
			    } else
				Memi[ip] = l
			}
			dp = dp + 1
			ip = ip + 1
			mp = mp + 1
		    }
		}
		if (grow >= 1.) {
		    do j = 1, npts {
			do i = n[j]+1, nimages
			    Memi[id[i]+j-1] = 0
		    }
		}
	    } else {
		do i = 1, nused {
		    dp = d[i]
		    mp = m[i]
		    do j = 1, npts {
			if (Memi[mp] == 0) {
			    n[j] = n[j] + 1
			    k = n[j]
			    if (k < i)
				Memd[d[k]+j-1] = Memd[dp]
			}
			dp = dp + 1
			mp = mp + 1
		    }
		}
	    }
	}

	# Sort the pixels and IDs if needed
	if (mclip) {
	    call malloc (dp, nimages, TY_DOUBLE)
	    if (keepids) {
		call malloc (ip, nimages, TY_INT)
		call ic_2sortd (d, Memd[dp], id, Memi[ip], n, npts)
		call mfree (ip, TY_INT)
	    } else
		call ic_sortd (d, Memd[dp], n, npts)
	    call mfree (dp, TY_DOUBLE)
	}
end
