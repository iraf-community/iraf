# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	"../icombine.h"


# IC_GDATA -- Get line of image and mask data and apply threshold and scaling.
# Entirely empty lines are excluded.  The data are compacted within the
# input data buffers.  If it is required, the connection to the original
# image index is kept in the returned m data pointers.

procedure ic_gdatas (in, out, dbuf, d, wtp, wbuf, w, id, n, m, lflag, offsets,
	scales, zeros, nimages, npts, v1, v2)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
pointer	dbuf[nimages]		# Data buffers
pointer	d[nimages]		# Data pointers
pointer	wtp[nimages]		# Weight images
pointer	wbuf[nimages]		# Weight buffers
pointer	w[nimages]		# Weight pointers
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

int	i, j, k, l, n1, n2, npix, nin, nout, ndim, nused, xt_imgnls()
int	xt_imgnlr()
long	v3[IM_MAXDIM]
real	a, b
pointer	buf, dp, ip, mp
errchk	xt_cpix, xt_imgnls

include	"../icombine.com"

begin
	# Get masks and return if there is no data
	call ic_mget (in, out, offsets, v1, v2, m, lflag, nimages)
	if (dflag == D_NONE) {
	    call aclri (n, npts)
	    return
	}

	# Close images which are not needed.
	nout = IM_LEN(out[1],1)
	ndim = IM_NDIM(out[1])
	if (!project) {
	    do i = 1, nimages {
		nin = IM_LEN(in[i],1)
		j = max (0, offsets[i,1])
		k = min (nout, nin + offsets[i,1])
		npix = k - j
		if (npix < 1) {
		    call xt_cpix (i)
		    call xt_cpix (nimages+i)
		}
		if (ndim > 1) {
		    j = v1[2] - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2)) {
			call xt_cpix (i)
			call xt_cpix (nimages+i)
		    }
		}
	    }
	}

	# Get data and fill data buffers.  Correct for offsets if needed.
	do i = 1, nimages {
	    if (lflag[i] == D_NONE)
		next
	    if (dbuf[i] == NULL) {
		call amovl (v1, v2, IM_MAXDIM)
		if (project)
		    v2[ndim+1] = i
		if (wtp[i] != NULL) {
		    call amovl (v2, v3, IM_MAXDIM)
		    j = xt_imgnlr (wtp[i], nimages+i, w[i], v3, v1[2])
		}
		j = xt_imgnls (in[i], i, d[i], v2, v1[2])
	    } else {
		nin = IM_LEN(in[i],1)
		j = max (0, offsets[i,1])
		k = min (nout, nin + offsets[i,1])
		npix = k - j
		if (npix < 1) {
		    lflag[i] = D_NONE
		    next
		}
		k = 1 + j - offsets[i,1]
		v2[1] = k
		do l = 2, ndim {
		    v2[l] = v1[l] - offsets[i,l]
		    if (v2[l] < 1 || v2[l] > IM_LEN(in[i],l)) {
			lflag[i] = D_NONE
			break
		    }
		}
		if (lflag[i] == D_NONE)
		    next
		if (project)
		    v2[ndim+1] = i
		if (wtp[i] != NULL) {
		    call amovl (v2, v3, IM_MAXDIM)
		    l = xt_imgnlr (wtp[i], nimages+i, buf, v3, v1[2])
		    call amovr (Memr[buf+k-1], Memr[wbuf[i]+j], npix)
		    w[i] = wbuf[i]
		}
		l = xt_imgnls (in[i], i, buf, v2, v1[2])
		call amovs (Mems[buf+k-1], Mems[dbuf[i]+j], npix)
		d[i] = dbuf[i]
	    }
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		if (lflag[i] == D_ALL) {
		    dp = d[i]
		    do j = 1, npts {
			a = Mems[dp]
			if (a < lthresh || a > hthresh) {
			    Memi[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
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
		} else if (lflag[i] == D_MIX) {
		    nin = IM_LEN(in[i],1)
		    j = max (0, offsets[i,1])
		    k = min (nout, nin + offsets[i,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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

		    # Check for completely empty lines
		    lflag[i] = D_NONE
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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
		    if (wtype == S_SIGMAP) {
			dp = w[i]
			do j = 1, npts {
			    Memr[dp] = Memr[dp] / a
			    dp = dp + 1
			}
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			dp = d[i]
			do j = 1, npts {
			    Mems[dp] = Mems[dp] / a + b
			    dp = dp + 1
			}
			if (wtype == S_SIGMAP) {
			    dp = w[i]
			    do j = 1, npts {
				Memr[dp] = Memr[dp] / a
				dp = dp + 1
			    }
			}
		    } else if (lflag[i] == D_MIX) {
			nin = IM_LEN(in[i],1)
			j = max (0, offsets[i,1])
			k = min (nout, nin + offsets[i,1])
			npix = k - j
			n1 = 1 + j
			n2 = n1 + npix - 1
			dp = d[i] + n1 - 1
			mp = m[i] + n1 - 1
			do j = n1, n2 {
			    if (Memi[mp] == 0)
				Mems[dp] = Mems[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
			if (wtype == S_SIGMAP) {
			    dp = w[i] + n1 - 1
			    mp = m[i] + n1 - 1
			    do j = n1, n2 {
				if (Memi[mp] == 0)
				    Memr[dp] = Memr[dp] / a
				dp = dp + 1
				mp = mp + 1
			    }
			}
		    }
		}
	    }
	}

	# Compute weights from sigmas.
	if (wtype == S_SIGMAP) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = w[i]
		    do j = 1, npts {
			a = Memr[dp]
			if (a > 0.)
			    Memr[dp] = 1. / (a**2)
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    if (lflag[i] == D_ALL) {
			dp = w[i]
			do j = 1, npts {
			    a = Memr[dp]
			    if (a > 0.)
				Memr[dp] = 1. / (a**2)
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			nin = IM_LEN(in[i],1)
			j = max (0, offsets[i,1])
			k = min (nout, nin + offsets[i,1])
			npix = k - j
			n1 = 1 + j
			n2 = n1 + npix - 1
			dp = w[i] + n1 - 1
			mp = m[i] + n1 - 1
			do j = n1, n2 {
			    if (Memi[mp] == 0) {
				a = Memr[dp]
				if (a > 0.)
				    Memr[dp] = 1. / (a**2)
			    }
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
		    nin = IM_LEN(in[l],1)
		    j = max (0, offsets[l,1])
		    k = min (nout, nin + offsets[l,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    ip = id[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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
		    l = lflag[i]
		    nin = IM_LEN(in[l],1)
		    j = max (0, offsets[l,1])
		    k = min (nout, nin + offsets[l,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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

procedure ic_gdatai (in, out, dbuf, d, wtp, wbuf, w, id, n, m, lflag, offsets,
	scales, zeros, nimages, npts, v1, v2)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
pointer	dbuf[nimages]		# Data buffers
pointer	d[nimages]		# Data pointers
pointer	wtp[nimages]		# Weight images
pointer	wbuf[nimages]		# Weight buffers
pointer	w[nimages]		# Weight pointers
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

int	i, j, k, l, n1, n2, npix, nin, nout, ndim, nused, xt_imgnli()
int	xt_imgnlr()
long	v3[IM_MAXDIM]
real	a, b
pointer	buf, dp, ip, mp
errchk	xt_cpix, xt_imgnli

include	"../icombine.com"

begin
	# Get masks and return if there is no data
	call ic_mget (in, out, offsets, v1, v2, m, lflag, nimages)
	if (dflag == D_NONE) {
	    call aclri (n, npts)
	    return
	}

	# Close images which are not needed.
	nout = IM_LEN(out[1],1)
	ndim = IM_NDIM(out[1])
	if (!project) {
	    do i = 1, nimages {
		nin = IM_LEN(in[i],1)
		j = max (0, offsets[i,1])
		k = min (nout, nin + offsets[i,1])
		npix = k - j
		if (npix < 1) {
		    call xt_cpix (i)
		    call xt_cpix (nimages+i)
		}
		if (ndim > 1) {
		    j = v1[2] - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2)) {
			call xt_cpix (i)
			call xt_cpix (nimages+i)
		    }
		}
	    }
	}

	# Get data and fill data buffers.  Correct for offsets if needed.
	do i = 1, nimages {
	    if (lflag[i] == D_NONE)
		next
	    if (dbuf[i] == NULL) {
		call amovl (v1, v2, IM_MAXDIM)
		if (project)
		    v2[ndim+1] = i
		if (wtp[i] != NULL) {
		    call amovl (v2, v3, IM_MAXDIM)
		    j = xt_imgnlr (wtp[i], nimages+i, w[i], v3, v1[2])
		}
		j = xt_imgnli (in[i], i, d[i], v2, v1[2])
	    } else {
		nin = IM_LEN(in[i],1)
		j = max (0, offsets[i,1])
		k = min (nout, nin + offsets[i,1])
		npix = k - j
		if (npix < 1) {
		    lflag[i] = D_NONE
		    next
		}
		k = 1 + j - offsets[i,1]
		v2[1] = k
		do l = 2, ndim {
		    v2[l] = v1[l] - offsets[i,l]
		    if (v2[l] < 1 || v2[l] > IM_LEN(in[i],l)) {
			lflag[i] = D_NONE
			break
		    }
		}
		if (lflag[i] == D_NONE)
		    next
		if (project)
		    v2[ndim+1] = i
		if (wtp[i] != NULL) {
		    call amovl (v2, v3, IM_MAXDIM)
		    l = xt_imgnlr (wtp[i], nimages+i, buf, v3, v1[2])
		    call amovr (Memr[buf+k-1], Memr[wbuf[i]+j], npix)
		    w[i] = wbuf[i]
		}
		l = xt_imgnli (in[i], i, buf, v2, v1[2])
		call amovi (Memi[buf+k-1], Memi[dbuf[i]+j], npix)
		d[i] = dbuf[i]
	    }
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		if (lflag[i] == D_ALL) {
		    dp = d[i]
		    do j = 1, npts {
			a = Memi[dp]
			if (a < lthresh || a > hthresh) {
			    Memi[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
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
		} else if (lflag[i] == D_MIX) {
		    nin = IM_LEN(in[i],1)
		    j = max (0, offsets[i,1])
		    k = min (nout, nin + offsets[i,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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

		    # Check for completely empty lines
		    lflag[i] = D_NONE
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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
		    if (wtype == S_SIGMAP) {
			dp = w[i]
			do j = 1, npts {
			    Memr[dp] = Memr[dp] / a
			    dp = dp + 1
			}
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			dp = d[i]
			do j = 1, npts {
			    Memi[dp] = Memi[dp] / a + b
			    dp = dp + 1
			}
			if (wtype == S_SIGMAP) {
			    dp = w[i]
			    do j = 1, npts {
				Memr[dp] = Memr[dp] / a
				dp = dp + 1
			    }
			}
		    } else if (lflag[i] == D_MIX) {
			nin = IM_LEN(in[i],1)
			j = max (0, offsets[i,1])
			k = min (nout, nin + offsets[i,1])
			npix = k - j
			n1 = 1 + j
			n2 = n1 + npix - 1
			dp = d[i] + n1 - 1
			mp = m[i] + n1 - 1
			do j = n1, n2 {
			    if (Memi[mp] == 0)
				Memi[dp] = Memi[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
			if (wtype == S_SIGMAP) {
			    dp = w[i] + n1 - 1
			    mp = m[i] + n1 - 1
			    do j = n1, n2 {
				if (Memi[mp] == 0)
				    Memr[dp] = Memr[dp] / a
				dp = dp + 1
				mp = mp + 1
			    }
			}
		    }
		}
	    }
	}

	# Compute weights from sigmas.
	if (wtype == S_SIGMAP) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = w[i]
		    do j = 1, npts {
			a = Memr[dp]
			if (a > 0.)
			    Memr[dp] = 1. / (a**2)
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    if (lflag[i] == D_ALL) {
			dp = w[i]
			do j = 1, npts {
			    a = Memr[dp]
			    if (a > 0.)
				Memr[dp] = 1. / (a**2)
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			nin = IM_LEN(in[i],1)
			j = max (0, offsets[i,1])
			k = min (nout, nin + offsets[i,1])
			npix = k - j
			n1 = 1 + j
			n2 = n1 + npix - 1
			dp = w[i] + n1 - 1
			mp = m[i] + n1 - 1
			do j = n1, n2 {
			    if (Memi[mp] == 0) {
				a = Memr[dp]
				if (a > 0.)
				    Memr[dp] = 1. / (a**2)
			    }
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
		    nin = IM_LEN(in[l],1)
		    j = max (0, offsets[l,1])
		    k = min (nout, nin + offsets[l,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    ip = id[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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
		    l = lflag[i]
		    nin = IM_LEN(in[l],1)
		    j = max (0, offsets[l,1])
		    k = min (nout, nin + offsets[l,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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

procedure ic_gdatar (in, out, dbuf, d, wtp, wbuf, w, id, n, m, lflag, offsets,
	scales, zeros, nimages, npts, v1, v2)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
pointer	dbuf[nimages]		# Data buffers
pointer	d[nimages]		# Data pointers
pointer	wtp[nimages]		# Weight images
pointer	wbuf[nimages]		# Weight buffers
pointer	w[nimages]		# Weight pointers
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

int	i, j, k, l, n1, n2, npix, nin, nout, ndim, nused, xt_imgnlr()
long	v3[IM_MAXDIM]
real	a, b
pointer	buf, dp, ip, mp
errchk	xt_cpix, xt_imgnlr

include	"../icombine.com"

begin
	# Get masks and return if there is no data
	call ic_mget (in, out, offsets, v1, v2, m, lflag, nimages)
	if (dflag == D_NONE) {
	    call aclri (n, npts)
	    return
	}

	# Close images which are not needed.
	nout = IM_LEN(out[1],1)
	ndim = IM_NDIM(out[1])
	if (!project) {
	    do i = 1, nimages {
		nin = IM_LEN(in[i],1)
		j = max (0, offsets[i,1])
		k = min (nout, nin + offsets[i,1])
		npix = k - j
		if (npix < 1) {
		    call xt_cpix (i)
		    call xt_cpix (nimages+i)
		}
		if (ndim > 1) {
		    j = v1[2] - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2)) {
			call xt_cpix (i)
			call xt_cpix (nimages+i)
		    }
		}
	    }
	}

	# Get data and fill data buffers.  Correct for offsets if needed.
	do i = 1, nimages {
	    if (lflag[i] == D_NONE)
		next
	    if (dbuf[i] == NULL) {
		call amovl (v1, v2, IM_MAXDIM)
		if (project)
		    v2[ndim+1] = i
		if (wtp[i] != NULL) {
		    call amovl (v2, v3, IM_MAXDIM)
		    j = xt_imgnlr (wtp[i], nimages+i, w[i], v3, v1[2])
		}
		j = xt_imgnlr (in[i], i, d[i], v2, v1[2])
	    } else {
		nin = IM_LEN(in[i],1)
		j = max (0, offsets[i,1])
		k = min (nout, nin + offsets[i,1])
		npix = k - j
		if (npix < 1) {
		    lflag[i] = D_NONE
		    next
		}
		k = 1 + j - offsets[i,1]
		v2[1] = k
		do l = 2, ndim {
		    v2[l] = v1[l] - offsets[i,l]
		    if (v2[l] < 1 || v2[l] > IM_LEN(in[i],l)) {
			lflag[i] = D_NONE
			break
		    }
		}
		if (lflag[i] == D_NONE)
		    next
		if (project)
		    v2[ndim+1] = i
		if (wtp[i] != NULL) {
		    call amovl (v2, v3, IM_MAXDIM)
		    l = xt_imgnlr (wtp[i], nimages+i, buf, v3, v1[2])
		    call amovr (Memr[buf+k-1], Memr[wbuf[i]+j], npix)
		    w[i] = wbuf[i]
		}
		l = xt_imgnlr (in[i], i, buf, v2, v1[2])
		call amovr (Memr[buf+k-1], Memr[dbuf[i]+j], npix)
		d[i] = dbuf[i]
	    }
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		if (lflag[i] == D_ALL) {
		    dp = d[i]
		    do j = 1, npts {
			a = Memr[dp]
			if (a < lthresh || a > hthresh) {
			    Memi[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
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
		} else if (lflag[i] == D_MIX) {
		    nin = IM_LEN(in[i],1)
		    j = max (0, offsets[i,1])
		    k = min (nout, nin + offsets[i,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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

		    # Check for completely empty lines
		    lflag[i] = D_NONE
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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
		    if (wtype == S_SIGMAP) {
			dp = w[i]
			do j = 1, npts {
			    Memr[dp] = Memr[dp] / a
			    dp = dp + 1
			}
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			dp = d[i]
			do j = 1, npts {
			    Memr[dp] = Memr[dp] / a + b
			    dp = dp + 1
			}
			if (wtype == S_SIGMAP) {
			    dp = w[i]
			    do j = 1, npts {
				Memr[dp] = Memr[dp] / a
				dp = dp + 1
			    }
			}
		    } else if (lflag[i] == D_MIX) {
			nin = IM_LEN(in[i],1)
			j = max (0, offsets[i,1])
			k = min (nout, nin + offsets[i,1])
			npix = k - j
			n1 = 1 + j
			n2 = n1 + npix - 1
			dp = d[i] + n1 - 1
			mp = m[i] + n1 - 1
			do j = n1, n2 {
			    if (Memi[mp] == 0)
				Memr[dp] = Memr[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
			if (wtype == S_SIGMAP) {
			    dp = w[i] + n1 - 1
			    mp = m[i] + n1 - 1
			    do j = n1, n2 {
				if (Memi[mp] == 0)
				    Memr[dp] = Memr[dp] / a
				dp = dp + 1
				mp = mp + 1
			    }
			}
		    }
		}
	    }
	}

	# Compute weights from sigmas.
	if (wtype == S_SIGMAP) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = w[i]
		    do j = 1, npts {
			a = Memr[dp]
			if (a > 0.)
			    Memr[dp] = 1. / (a**2)
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    if (lflag[i] == D_ALL) {
			dp = w[i]
			do j = 1, npts {
			    a = Memr[dp]
			    if (a > 0.)
				Memr[dp] = 1. / (a**2)
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			nin = IM_LEN(in[i],1)
			j = max (0, offsets[i,1])
			k = min (nout, nin + offsets[i,1])
			npix = k - j
			n1 = 1 + j
			n2 = n1 + npix - 1
			dp = w[i] + n1 - 1
			mp = m[i] + n1 - 1
			do j = n1, n2 {
			    if (Memi[mp] == 0) {
				a = Memr[dp]
				if (a > 0.)
				    Memr[dp] = 1. / (a**2)
			    }
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
		    nin = IM_LEN(in[l],1)
		    j = max (0, offsets[l,1])
		    k = min (nout, nin + offsets[l,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    ip = id[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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
		    l = lflag[i]
		    nin = IM_LEN(in[l],1)
		    j = max (0, offsets[l,1])
		    k = min (nout, nin + offsets[l,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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

procedure ic_gdatad (in, out, dbuf, d, wtp, wbuf, w, id, n, m, lflag, offsets,
	scales, zeros, nimages, npts, v1, v2)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
pointer	dbuf[nimages]		# Data buffers
pointer	d[nimages]		# Data pointers
pointer	wtp[nimages]		# Weight images
pointer	wbuf[nimages]		# Weight buffers
pointer	w[nimages]		# Weight pointers
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

int	i, j, k, l, n1, n2, npix, nin, nout, ndim, nused, xt_imgnld()
int	xt_imgnlr()
long	v3[IM_MAXDIM]
real	a, b
pointer	buf, dp, ip, mp
errchk	xt_cpix, xt_imgnld

include	"../icombine.com"

begin
	# Get masks and return if there is no data
	call ic_mget (in, out, offsets, v1, v2, m, lflag, nimages)
	if (dflag == D_NONE) {
	    call aclri (n, npts)
	    return
	}

	# Close images which are not needed.
	nout = IM_LEN(out[1],1)
	ndim = IM_NDIM(out[1])
	if (!project) {
	    do i = 1, nimages {
		nin = IM_LEN(in[i],1)
		j = max (0, offsets[i,1])
		k = min (nout, nin + offsets[i,1])
		npix = k - j
		if (npix < 1) {
		    call xt_cpix (i)
		    call xt_cpix (nimages+i)
		}
		if (ndim > 1) {
		    j = v1[2] - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2)) {
			call xt_cpix (i)
			call xt_cpix (nimages+i)
		    }
		}
	    }
	}

	# Get data and fill data buffers.  Correct for offsets if needed.
	do i = 1, nimages {
	    if (lflag[i] == D_NONE)
		next
	    if (dbuf[i] == NULL) {
		call amovl (v1, v2, IM_MAXDIM)
		if (project)
		    v2[ndim+1] = i
		if (wtp[i] != NULL) {
		    call amovl (v2, v3, IM_MAXDIM)
		    j = xt_imgnlr (wtp[i], nimages+i, w[i], v3, v1[2])
		}
		j = xt_imgnld (in[i], i, d[i], v2, v1[2])
	    } else {
		nin = IM_LEN(in[i],1)
		j = max (0, offsets[i,1])
		k = min (nout, nin + offsets[i,1])
		npix = k - j
		if (npix < 1) {
		    lflag[i] = D_NONE
		    next
		}
		k = 1 + j - offsets[i,1]
		v2[1] = k
		do l = 2, ndim {
		    v2[l] = v1[l] - offsets[i,l]
		    if (v2[l] < 1 || v2[l] > IM_LEN(in[i],l)) {
			lflag[i] = D_NONE
			break
		    }
		}
		if (lflag[i] == D_NONE)
		    next
		if (project)
		    v2[ndim+1] = i
		if (wtp[i] != NULL) {
		    call amovl (v2, v3, IM_MAXDIM)
		    l = xt_imgnlr (wtp[i], nimages+i, buf, v3, v1[2])
		    call amovr (Memr[buf+k-1], Memr[wbuf[i]+j], npix)
		    w[i] = wbuf[i]
		}
		l = xt_imgnld (in[i], i, buf, v2, v1[2])
		call amovd (Memd[buf+k-1], Memd[dbuf[i]+j], npix)
		d[i] = dbuf[i]
	    }
	}

	# Apply threshold if needed
	if (dothresh) {
	    do i = 1, nimages {
		if (lflag[i] == D_ALL) {
		    dp = d[i]
		    do j = 1, npts {
			a = Memd[dp]
			if (a < lthresh || a > hthresh) {
			    Memi[m[i]+j-1] = 1
			    lflag[i] = D_MIX
			    dflag = D_MIX
			}
			dp = dp + 1
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
		} else if (lflag[i] == D_MIX) {
		    nin = IM_LEN(in[i],1)
		    j = max (0, offsets[i,1])
		    k = min (nout, nin + offsets[i,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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

		    # Check for completely empty lines
		    lflag[i] = D_NONE
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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
		    if (wtype == S_SIGMAP) {
			dp = w[i]
			do j = 1, npts {
			    Memr[dp] = Memr[dp] / a
			    dp = dp + 1
			}
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    a = scales[i]
		    b = -zeros[i]
		    if (lflag[i] == D_ALL) {
			dp = d[i]
			do j = 1, npts {
			    Memd[dp] = Memd[dp] / a + b
			    dp = dp + 1
			}
			if (wtype == S_SIGMAP) {
			    dp = w[i]
			    do j = 1, npts {
				Memr[dp] = Memr[dp] / a
				dp = dp + 1
			    }
			}
		    } else if (lflag[i] == D_MIX) {
			nin = IM_LEN(in[i],1)
			j = max (0, offsets[i,1])
			k = min (nout, nin + offsets[i,1])
			npix = k - j
			n1 = 1 + j
			n2 = n1 + npix - 1
			dp = d[i] + n1 - 1
			mp = m[i] + n1 - 1
			do j = n1, n2 {
			    if (Memi[mp] == 0)
				Memd[dp] = Memd[dp] / a + b
			    dp = dp + 1
			    mp = mp + 1
			}
			if (wtype == S_SIGMAP) {
			    dp = w[i] + n1 - 1
			    mp = m[i] + n1 - 1
			    do j = n1, n2 {
				if (Memi[mp] == 0)
				    Memr[dp] = Memr[dp] / a
				dp = dp + 1
				mp = mp + 1
			    }
			}
		    }
		}
	    }
	}

	# Compute weights from sigmas.
	if (wtype == S_SIGMAP) {
	    if (dflag == D_ALL) {
		do i = 1, nimages {
		    dp = w[i]
		    do j = 1, npts {
			a = Memr[dp]
			if (a > 0.)
			    Memr[dp] = 1. / (a**2)
			dp = dp + 1
		    }
		}
	    } else if (dflag == D_MIX) {
		do i = 1, nimages {
		    if (lflag[i] == D_ALL) {
			dp = w[i]
			do j = 1, npts {
			    a = Memr[dp]
			    if (a > 0.)
				Memr[dp] = 1. / (a**2)
			    dp = dp + 1
			}
		    } else if (lflag[i] == D_MIX) {
			nin = IM_LEN(in[i],1)
			j = max (0, offsets[i,1])
			k = min (nout, nin + offsets[i,1])
			npix = k - j
			n1 = 1 + j
			n2 = n1 + npix - 1
			dp = w[i] + n1 - 1
			mp = m[i] + n1 - 1
			do j = n1, n2 {
			    if (Memi[mp] == 0) {
				a = Memr[dp]
				if (a > 0.)
				    Memr[dp] = 1. / (a**2)
			    }
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
		    nin = IM_LEN(in[l],1)
		    j = max (0, offsets[l,1])
		    k = min (nout, nin + offsets[l,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    ip = id[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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
		    l = lflag[i]
		    nin = IM_LEN(in[l],1)
		    j = max (0, offsets[l,1])
		    k = min (nout, nin + offsets[l,1])
		    npix = k - j
		    n1 = 1 + j
		    n2 = n1 + npix - 1
		    dp = d[i] + n1 - 1
		    mp = m[i] + n1 - 1
		    do j = n1, n2 {
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

