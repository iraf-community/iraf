# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"../icombine.h"

define	NMAX	100000	# Maximum number of pixels to sample


# IC_STAT -- Compute image statistics within specified section.
# The image section is relative to a reference image which may be
# different than the input image and may have an offset.  Only a
# subsample of pixels is used.  Masked and thresholded pixels are
# ignored.  Only the desired statistics are computed to increase
# efficiency.

procedure ic_stats (im, imref, section, offsets, image, nimages,
	domode, domedian, domean, mode, median, mean)

pointer	im			 # Data image
pointer	imref			 # Reference image for image section
char	section[ARB]		 # Image section
long	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

size_t	sz_val
long	l_val
int	i, j, ndim
size_t	n
long	nv, k
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp

long	imgnls()
real	asums()
short	ic_modes()

include	"../icombine.com"

begin
	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (v1, sz_val, TY_LONG)
	call salloc (v2, sz_val, TY_LONG)
	call salloc (dv, sz_val, TY_LONG)
	call salloc (va, sz_val, TY_LONG)
	call salloc (vb, sz_val, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	sz_val = IM_MAXDIM
	l_val = 1
	call amovkl (l_val, Meml[v1], sz_val)
	call amovkl (l_val, Meml[va], sz_val)
	call amovkl (l_val, Meml[dv], sz_val)
	sz_val = ndim
	call amovl (IM_LEN(imref,1), Meml[vb], sz_val)
	call ic_section (section, Meml[va], Meml[vb], Meml[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Meml[va+i-1] = Meml[va+i-1] - offsets[image,i]
		Meml[vb+i-1] = Meml[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Meml[v1+i] = max (1, min (Meml[va+i], Meml[vb+i]))
		Meml[v2+i] = min (IM_LEN(im,i+1), max (Meml[va+i], Meml[vb+i]))
		Meml[dv+i] = j
		nv = max (1, (Meml[v2+i] - Meml[v1+i]) / Meml[dv+i] + 1)
		Meml[v2+i] = Meml[v1+i] + (nv - 1) * Meml[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	sz_val = IM_MAXDIM
	call amovl (Meml[v1], Meml[va], sz_val)
	Meml[va] = 1
	if (project)
	   Meml[va+ndim] = image
	sz_val = IM_MAXDIM
	call amovl (Meml[va], Meml[vb], sz_val)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_SHORT)
	dp = data
	while (imgnls (im, lp, Meml[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Meml[va], mask)
	    lp = lp + Meml[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			a = Mems[lp]
			if (a >= lthresh && a <= hthresh) {
			    Mems[dp] = a
			    dp = dp + 1
			}
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			Mems[dp] = Mems[lp]
			dp = dp + 1
			lp = lp + Meml[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Meml[v1] - 1
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    a = Mems[lp]
			    if (a >= lthresh && a <= hthresh) {
				Mems[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    Mems[dp] = Mems[lp]
			    dp = dp + 1
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Meml[va+i-1] = Meml[va+i-1] + Meml[dv+i-1]
		if (Meml[va+i-1] <= Meml[v2+i-1])
		    break
		Meml[va+i-1] = Meml[v1+i-1]
	    }
	    if (i > ndim)
		break
	    sz_val = IM_MAXDIM
	    call amovl (Meml[va], Meml[vb], sz_val)
	}

	# Close mask until it is needed again.
	call ic_mclose1 (image, nimages)

	n = dp - data
	if (n < 1) {
	    call sfree (sp)
	    call error (1, "Image section contains no pixels")
	}

	# Compute only statistics needed.
	if (domode || domedian) {
	    call asrts (Mems[data], Mems[data], n)
	    mode = ic_modes (Mems[data], n)
	    median = Mems[data+n/2-1]
	}
	if (domean)
	    mean = asums (Mems[data], n) / n

	call sfree (sp)
end


define	NMIN	10	# Minimum number of pixels for mode calculation
define	ZRANGE	0.7	# Fraction of pixels about median to use
define	ZSTEP	0.01	# Step size for search for mode
define	ZBIN	0.1	# Bin size for mode.

# IC_MODE -- Compute mode of an array.  The mode is found by binning
# with a bin size based on the data range over a fraction of the
# pixels about the median and a bin step which may be smaller than the
# bin size.  If there are too few points the median is returned.
# The input array must be sorted.

short procedure ic_modes (a, n)

short	a[n]			# Data array
size_t	n			# Number of points

long	i, j, k, nmax
real	z1, z2, zstep, zbin
short	mode
bool	fp_equalr()

begin
	if (n < NMIN)
	    return (a[n/2])

	# Compute the mode.  The array must be sorted.  Consider a
	# range of values about the median point.  Use a bin size which
	# is ZBIN of the range.  Step the bin limits in ZSTEP fraction of
	# the bin size.

	i = 1 + n * (1. - ZRANGE) / 2.
	j = 1 + n * (1. + ZRANGE) / 2.
	z1 = a[i]
	z2 = a[j]
	if (fp_equalr (z1, z2)) {
	    mode = z1
	    return (mode)
	}

	zstep = ZSTEP * (z2 - z1)
	zbin = ZBIN * (z2 - z1)
	zstep = max (1., zstep)
	zbin = max (1., zbin)

	z1 = z1 - zstep
	k = i
	nmax = 0
	repeat {
	    z1 = z1 + zstep
	    z2 = z1 + zbin
	    for (; i < j && a[i] < z1; i=i+1)
		;
	    for (; k < j && a[k] < z2; k=k+1)
		;
	    if (k - i > nmax) {
	        nmax = k - i
	        mode = a[(i+k)/2]
	    }
	} until (k >= j)

	return (mode)
end

# IC_STAT -- Compute image statistics within specified section.
# The image section is relative to a reference image which may be
# different than the input image and may have an offset.  Only a
# subsample of pixels is used.  Masked and thresholded pixels are
# ignored.  Only the desired statistics are computed to increase
# efficiency.

procedure ic_stati (im, imref, section, offsets, image, nimages,
	domode, domedian, domean, mode, median, mean)

pointer	im			 # Data image
pointer	imref			 # Reference image for image section
char	section[ARB]		 # Image section
long	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

size_t	sz_val
long	l_val
int	i, j, ndim
size_t	n
long	nv, k
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp

long	imgnli()
real	asumi()
int	ic_modei()

include	"../icombine.com"

begin
	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (v1, sz_val, TY_LONG)
	call salloc (v2, sz_val, TY_LONG)
	call salloc (dv, sz_val, TY_LONG)
	call salloc (va, sz_val, TY_LONG)
	call salloc (vb, sz_val, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	sz_val = IM_MAXDIM
	l_val = 1
	call amovkl (l_val, Meml[v1], sz_val)
	call amovkl (l_val, Meml[va], sz_val)
	call amovkl (l_val, Meml[dv], sz_val)
	sz_val = ndim
	call amovl (IM_LEN(imref,1), Meml[vb], sz_val)
	call ic_section (section, Meml[va], Meml[vb], Meml[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Meml[va+i-1] = Meml[va+i-1] - offsets[image,i]
		Meml[vb+i-1] = Meml[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Meml[v1+i] = max (1, min (Meml[va+i], Meml[vb+i]))
		Meml[v2+i] = min (IM_LEN(im,i+1), max (Meml[va+i], Meml[vb+i]))
		Meml[dv+i] = j
		nv = max (1, (Meml[v2+i] - Meml[v1+i]) / Meml[dv+i] + 1)
		Meml[v2+i] = Meml[v1+i] + (nv - 1) * Meml[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	sz_val = IM_MAXDIM
	call amovl (Meml[v1], Meml[va], sz_val)
	Meml[va] = 1
	if (project)
	   Meml[va+ndim] = image
	sz_val = IM_MAXDIM
	call amovl (Meml[va], Meml[vb], sz_val)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_INT)
	dp = data
	while (imgnli (im, lp, Meml[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Meml[va], mask)
	    lp = lp + Meml[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			a = Memi[lp]
			if (a >= lthresh && a <= hthresh) {
			    Memi[dp] = a
			    dp = dp + 1
			}
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			Memi[dp] = Memi[lp]
			dp = dp + 1
			lp = lp + Meml[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Meml[v1] - 1
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    a = Memi[lp]
			    if (a >= lthresh && a <= hthresh) {
				Memi[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    Memi[dp] = Memi[lp]
			    dp = dp + 1
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Meml[va+i-1] = Meml[va+i-1] + Meml[dv+i-1]
		if (Meml[va+i-1] <= Meml[v2+i-1])
		    break
		Meml[va+i-1] = Meml[v1+i-1]
	    }
	    if (i > ndim)
		break
	    sz_val = IM_MAXDIM
	    call amovl (Meml[va], Meml[vb], sz_val)
	}

	# Close mask until it is needed again.
	call ic_mclose1 (image, nimages)

	n = dp - data
	if (n < 1) {
	    call sfree (sp)
	    call error (1, "Image section contains no pixels")
	}

	# Compute only statistics needed.
	if (domode || domedian) {
	    call asrti (Memi[data], Memi[data], n)
	    mode = ic_modei (Memi[data], n)
	    median = Memi[data+n/2-1]
	}
	if (domean)
	    mean = asumi (Memi[data], n) / n

	call sfree (sp)
end


define	NMIN	10	# Minimum number of pixels for mode calculation
define	ZRANGE	0.7	# Fraction of pixels about median to use
define	ZSTEP	0.01	# Step size for search for mode
define	ZBIN	0.1	# Bin size for mode.

# IC_MODE -- Compute mode of an array.  The mode is found by binning
# with a bin size based on the data range over a fraction of the
# pixels about the median and a bin step which may be smaller than the
# bin size.  If there are too few points the median is returned.
# The input array must be sorted.

int procedure ic_modei (a, n)

int	a[n]			# Data array
size_t	n			# Number of points

long	i, j, k, nmax
real	z1, z2, zstep, zbin
int	mode
bool	fp_equalr()

begin
	if (n < NMIN)
	    return (a[n/2])

	# Compute the mode.  The array must be sorted.  Consider a
	# range of values about the median point.  Use a bin size which
	# is ZBIN of the range.  Step the bin limits in ZSTEP fraction of
	# the bin size.

	i = 1 + n * (1. - ZRANGE) / 2.
	j = 1 + n * (1. + ZRANGE) / 2.
	z1 = a[i]
	z2 = a[j]
	if (fp_equalr (z1, z2)) {
	    mode = z1
	    return (mode)
	}

	zstep = ZSTEP * (z2 - z1)
	zbin = ZBIN * (z2 - z1)
	zstep = max (1., zstep)
	zbin = max (1., zbin)

	z1 = z1 - zstep
	k = i
	nmax = 0
	repeat {
	    z1 = z1 + zstep
	    z2 = z1 + zbin
	    for (; i < j && a[i] < z1; i=i+1)
		;
	    for (; k < j && a[k] < z2; k=k+1)
		;
	    if (k - i > nmax) {
	        nmax = k - i
	        mode = a[(i+k)/2]
	    }
	} until (k >= j)

	return (mode)
end

# IC_STAT -- Compute image statistics within specified section.
# The image section is relative to a reference image which may be
# different than the input image and may have an offset.  Only a
# subsample of pixels is used.  Masked and thresholded pixels are
# ignored.  Only the desired statistics are computed to increase
# efficiency.

procedure ic_statl (im, imref, section, offsets, image, nimages,
	domode, domedian, domean, mode, median, mean)

pointer	im			 # Data image
pointer	imref			 # Reference image for image section
char	section[ARB]		 # Image section
long	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

size_t	sz_val
long	l_val
int	i, j, ndim
size_t	n
long	nv, k
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp

long	imgnll()
double	asuml()
long	ic_model()

include	"../icombine.com"

begin
	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (v1, sz_val, TY_LONG)
	call salloc (v2, sz_val, TY_LONG)
	call salloc (dv, sz_val, TY_LONG)
	call salloc (va, sz_val, TY_LONG)
	call salloc (vb, sz_val, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	sz_val = IM_MAXDIM
	l_val = 1
	call amovkl (l_val, Meml[v1], sz_val)
	call amovkl (l_val, Meml[va], sz_val)
	call amovkl (l_val, Meml[dv], sz_val)
	sz_val = ndim
	call amovl (IM_LEN(imref,1), Meml[vb], sz_val)
	call ic_section (section, Meml[va], Meml[vb], Meml[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Meml[va+i-1] = Meml[va+i-1] - offsets[image,i]
		Meml[vb+i-1] = Meml[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Meml[v1+i] = max (1, min (Meml[va+i], Meml[vb+i]))
		Meml[v2+i] = min (IM_LEN(im,i+1), max (Meml[va+i], Meml[vb+i]))
		Meml[dv+i] = j
		nv = max (1, (Meml[v2+i] - Meml[v1+i]) / Meml[dv+i] + 1)
		Meml[v2+i] = Meml[v1+i] + (nv - 1) * Meml[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	sz_val = IM_MAXDIM
	call amovl (Meml[v1], Meml[va], sz_val)
	Meml[va] = 1
	if (project)
	   Meml[va+ndim] = image
	sz_val = IM_MAXDIM
	call amovl (Meml[va], Meml[vb], sz_val)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_LONG)
	dp = data
	while (imgnll (im, lp, Meml[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Meml[va], mask)
	    lp = lp + Meml[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			a = Meml[lp]
			if (a >= lthresh && a <= hthresh) {
			    Meml[dp] = a
			    dp = dp + 1
			}
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			Meml[dp] = Meml[lp]
			dp = dp + 1
			lp = lp + Meml[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Meml[v1] - 1
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    a = Meml[lp]
			    if (a >= lthresh && a <= hthresh) {
				Meml[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    Meml[dp] = Meml[lp]
			    dp = dp + 1
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Meml[va+i-1] = Meml[va+i-1] + Meml[dv+i-1]
		if (Meml[va+i-1] <= Meml[v2+i-1])
		    break
		Meml[va+i-1] = Meml[v1+i-1]
	    }
	    if (i > ndim)
		break
	    sz_val = IM_MAXDIM
	    call amovl (Meml[va], Meml[vb], sz_val)
	}

	# Close mask until it is needed again.
	call ic_mclose1 (image, nimages)

	n = dp - data
	if (n < 1) {
	    call sfree (sp)
	    call error (1, "Image section contains no pixels")
	}

	# Compute only statistics needed.
	if (domode || domedian) {
	    call asrtl (Meml[data], Meml[data], n)
	    mode = ic_model (Meml[data], n)
	    median = Meml[data+n/2-1]
	}
	if (domean)
	    mean = asuml (Meml[data], n) / n

	call sfree (sp)
end


define	NMIN	10	# Minimum number of pixels for mode calculation
define	ZRANGE	0.7	# Fraction of pixels about median to use
define	ZSTEP	0.01	# Step size for search for mode
define	ZBIN	0.1	# Bin size for mode.

# IC_MODE -- Compute mode of an array.  The mode is found by binning
# with a bin size based on the data range over a fraction of the
# pixels about the median and a bin step which may be smaller than the
# bin size.  If there are too few points the median is returned.
# The input array must be sorted.

long procedure ic_model (a, n)

long	a[n]			# Data array
size_t	n			# Number of points

long	i, j, k, nmax
real	z1, z2, zstep, zbin
long	mode
bool	fp_equalr()

begin
	if (n < NMIN)
	    return (a[n/2])

	# Compute the mode.  The array must be sorted.  Consider a
	# range of values about the median point.  Use a bin size which
	# is ZBIN of the range.  Step the bin limits in ZSTEP fraction of
	# the bin size.

	i = 1 + n * (1. - ZRANGE) / 2.
	j = 1 + n * (1. + ZRANGE) / 2.
	z1 = a[i]
	z2 = a[j]
	if (fp_equalr (z1, z2)) {
	    mode = z1
	    return (mode)
	}

	zstep = ZSTEP * (z2 - z1)
	zbin = ZBIN * (z2 - z1)
	zstep = max (1., zstep)
	zbin = max (1., zbin)

	z1 = z1 - zstep
	k = i
	nmax = 0
	repeat {
	    z1 = z1 + zstep
	    z2 = z1 + zbin
	    for (; i < j && a[i] < z1; i=i+1)
		;
	    for (; k < j && a[k] < z2; k=k+1)
		;
	    if (k - i > nmax) {
	        nmax = k - i
	        mode = a[(i+k)/2]
	    }
	} until (k >= j)

	return (mode)
end

# IC_STAT -- Compute image statistics within specified section.
# The image section is relative to a reference image which may be
# different than the input image and may have an offset.  Only a
# subsample of pixels is used.  Masked and thresholded pixels are
# ignored.  Only the desired statistics are computed to increase
# efficiency.

procedure ic_statr (im, imref, section, offsets, image, nimages,
	domode, domedian, domean, mode, median, mean)

pointer	im			 # Data image
pointer	imref			 # Reference image for image section
char	section[ARB]		 # Image section
long	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

size_t	sz_val
long	l_val
int	i, j, ndim
size_t	n
long	nv, k
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp

long	imgnlr()
real	asumr()
real	ic_moder()

include	"../icombine.com"

begin
	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (v1, sz_val, TY_LONG)
	call salloc (v2, sz_val, TY_LONG)
	call salloc (dv, sz_val, TY_LONG)
	call salloc (va, sz_val, TY_LONG)
	call salloc (vb, sz_val, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	sz_val = IM_MAXDIM
	l_val = 1
	call amovkl (l_val, Meml[v1], sz_val)
	call amovkl (l_val, Meml[va], sz_val)
	call amovkl (l_val, Meml[dv], sz_val)
	sz_val = ndim
	call amovl (IM_LEN(imref,1), Meml[vb], sz_val)
	call ic_section (section, Meml[va], Meml[vb], Meml[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Meml[va+i-1] = Meml[va+i-1] - offsets[image,i]
		Meml[vb+i-1] = Meml[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Meml[v1+i] = max (1, min (Meml[va+i], Meml[vb+i]))
		Meml[v2+i] = min (IM_LEN(im,i+1), max (Meml[va+i], Meml[vb+i]))
		Meml[dv+i] = j
		nv = max (1, (Meml[v2+i] - Meml[v1+i]) / Meml[dv+i] + 1)
		Meml[v2+i] = Meml[v1+i] + (nv - 1) * Meml[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	sz_val = IM_MAXDIM
	call amovl (Meml[v1], Meml[va], sz_val)
	Meml[va] = 1
	if (project)
	   Meml[va+ndim] = image
	sz_val = IM_MAXDIM
	call amovl (Meml[va], Meml[vb], sz_val)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_REAL)
	dp = data
	while (imgnlr (im, lp, Meml[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Meml[va], mask)
	    lp = lp + Meml[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			a = Memr[lp]
			if (a >= lthresh && a <= hthresh) {
			    Memr[dp] = a
			    dp = dp + 1
			}
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			Memr[dp] = Memr[lp]
			dp = dp + 1
			lp = lp + Meml[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Meml[v1] - 1
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    a = Memr[lp]
			    if (a >= lthresh && a <= hthresh) {
				Memr[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    Memr[dp] = Memr[lp]
			    dp = dp + 1
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Meml[va+i-1] = Meml[va+i-1] + Meml[dv+i-1]
		if (Meml[va+i-1] <= Meml[v2+i-1])
		    break
		Meml[va+i-1] = Meml[v1+i-1]
	    }
	    if (i > ndim)
		break
	    sz_val = IM_MAXDIM
	    call amovl (Meml[va], Meml[vb], sz_val)
	}

	# Close mask until it is needed again.
	call ic_mclose1 (image, nimages)

	n = dp - data
	if (n < 1) {
	    call sfree (sp)
	    call error (1, "Image section contains no pixels")
	}

	# Compute only statistics needed.
	if (domode || domedian) {
	    call asrtr (Memr[data], Memr[data], n)
	    mode = ic_moder (Memr[data], n)
	    median = Memr[data+n/2-1]
	}
	if (domean)
	    mean = asumr (Memr[data], n) / n

	call sfree (sp)
end


define	NMIN	10	# Minimum number of pixels for mode calculation
define	ZRANGE	0.7	# Fraction of pixels about median to use
define	ZSTEP	0.01	# Step size for search for mode
define	ZBIN	0.1	# Bin size for mode.

# IC_MODE -- Compute mode of an array.  The mode is found by binning
# with a bin size based on the data range over a fraction of the
# pixels about the median and a bin step which may be smaller than the
# bin size.  If there are too few points the median is returned.
# The input array must be sorted.

real procedure ic_moder (a, n)

real	a[n]			# Data array
size_t	n			# Number of points

long	i, j, k, nmax
real	z1, z2, zstep, zbin
real	mode
bool	fp_equalr()

begin
	if (n < NMIN)
	    return (a[n/2])

	# Compute the mode.  The array must be sorted.  Consider a
	# range of values about the median point.  Use a bin size which
	# is ZBIN of the range.  Step the bin limits in ZSTEP fraction of
	# the bin size.

	i = 1 + n * (1. - ZRANGE) / 2.
	j = 1 + n * (1. + ZRANGE) / 2.
	z1 = a[i]
	z2 = a[j]
	if (fp_equalr (z1, z2)) {
	    mode = z1
	    return (mode)
	}

	zstep = ZSTEP * (z2 - z1)
	zbin = ZBIN * (z2 - z1)

	z1 = z1 - zstep
	k = i
	nmax = 0
	repeat {
	    z1 = z1 + zstep
	    z2 = z1 + zbin
	    for (; i < j && a[i] < z1; i=i+1)
		;
	    for (; k < j && a[k] < z2; k=k+1)
		;
	    if (k - i > nmax) {
	        nmax = k - i
	        mode = a[(i+k)/2]
	    }
	} until (k >= j)

	return (mode)
end

# IC_STAT -- Compute image statistics within specified section.
# The image section is relative to a reference image which may be
# different than the input image and may have an offset.  Only a
# subsample of pixels is used.  Masked and thresholded pixels are
# ignored.  Only the desired statistics are computed to increase
# efficiency.

procedure ic_statd (im, imref, section, offsets, image, nimages,
	domode, domedian, domean, mode, median, mean)

pointer	im			 # Data image
pointer	imref			 # Reference image for image section
char	section[ARB]		 # Image section
long	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

size_t	sz_val
long	l_val
int	i, j, ndim
size_t	n
long	nv, k
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp

long	imgnld()
double	asumd()
double	ic_moded()

include	"../icombine.com"

begin
	call smark (sp)
	sz_val = IM_MAXDIM
	call salloc (v1, sz_val, TY_LONG)
	call salloc (v2, sz_val, TY_LONG)
	call salloc (dv, sz_val, TY_LONG)
	call salloc (va, sz_val, TY_LONG)
	call salloc (vb, sz_val, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	sz_val = IM_MAXDIM
	l_val = 1
	call amovkl (l_val, Meml[v1], sz_val)
	call amovkl (l_val, Meml[va], sz_val)
	call amovkl (l_val, Meml[dv], sz_val)
	sz_val = ndim
	call amovl (IM_LEN(imref,1), Meml[vb], sz_val)
	call ic_section (section, Meml[va], Meml[vb], Meml[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Meml[va+i-1] = Meml[va+i-1] - offsets[image,i]
		Meml[vb+i-1] = Meml[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Meml[v1+i] = max (1, min (Meml[va+i], Meml[vb+i]))
		Meml[v2+i] = min (IM_LEN(im,i+1), max (Meml[va+i], Meml[vb+i]))
		Meml[dv+i] = j
		nv = max (1, (Meml[v2+i] - Meml[v1+i]) / Meml[dv+i] + 1)
		Meml[v2+i] = Meml[v1+i] + (nv - 1) * Meml[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	sz_val = IM_MAXDIM
	call amovl (Meml[v1], Meml[va], sz_val)
	Meml[va] = 1
	if (project)
	   Meml[va+ndim] = image
	sz_val = IM_MAXDIM
	call amovl (Meml[va], Meml[vb], sz_val)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_DOUBLE)
	dp = data
	while (imgnld (im, lp, Meml[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Meml[va], mask)
	    lp = lp + Meml[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			a = Memd[lp]
			if (a >= lthresh && a <= hthresh) {
			    Memd[dp] = a
			    dp = dp + 1
			}
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			Memd[dp] = Memd[lp]
			dp = dp + 1
			lp = lp + Meml[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Meml[v1] - 1
		if (dothresh) {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    a = Memd[lp]
			    if (a >= lthresh && a <= hthresh) {
				Memd[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		} else {
		    do k = Meml[v1], Meml[v2], Meml[dv] {
			if (Memi[mp] == 0) {
			    Memd[dp] = Memd[lp]
			    dp = dp + 1
			}
			mp = mp + Meml[dv]
			lp = lp + Meml[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Meml[va+i-1] = Meml[va+i-1] + Meml[dv+i-1]
		if (Meml[va+i-1] <= Meml[v2+i-1])
		    break
		Meml[va+i-1] = Meml[v1+i-1]
	    }
	    if (i > ndim)
		break
	    sz_val = IM_MAXDIM
	    call amovl (Meml[va], Meml[vb], sz_val)
	}

	# Close mask until it is needed again.
	call ic_mclose1 (image, nimages)

	n = dp - data
	if (n < 1) {
	    call sfree (sp)
	    call error (1, "Image section contains no pixels")
	}

	# Compute only statistics needed.
	if (domode || domedian) {
	    call asrtd (Memd[data], Memd[data], n)
	    mode = ic_moded (Memd[data], n)
	    median = Memd[data+n/2-1]
	}
	if (domean)
	    mean = asumd (Memd[data], n) / n

	call sfree (sp)
end


define	NMIN	10	# Minimum number of pixels for mode calculation
define	ZRANGE	0.7	# Fraction of pixels about median to use
define	ZSTEP	0.01	# Step size for search for mode
define	ZBIN	0.1	# Bin size for mode.

# IC_MODE -- Compute mode of an array.  The mode is found by binning
# with a bin size based on the data range over a fraction of the
# pixels about the median and a bin step which may be smaller than the
# bin size.  If there are too few points the median is returned.
# The input array must be sorted.

double procedure ic_moded (a, n)

double	a[n]			# Data array
size_t	n			# Number of points

long	i, j, k, nmax
real	z1, z2, zstep, zbin
double	mode
bool	fp_equalr()

begin
	if (n < NMIN)
	    return (a[n/2])

	# Compute the mode.  The array must be sorted.  Consider a
	# range of values about the median point.  Use a bin size which
	# is ZBIN of the range.  Step the bin limits in ZSTEP fraction of
	# the bin size.

	i = 1 + n * (1. - ZRANGE) / 2.
	j = 1 + n * (1. + ZRANGE) / 2.
	z1 = a[i]
	z2 = a[j]
	if (fp_equalr (z1, z2)) {
	    mode = z1
	    return (mode)
	}

	zstep = ZSTEP * (z2 - z1)
	zbin = ZBIN * (z2 - z1)

	z1 = z1 - zstep
	k = i
	nmax = 0
	repeat {
	    z1 = z1 + zstep
	    z2 = z1 + zbin
	    for (; i < j && a[i] < z1; i=i+1)
		;
	    for (; k < j && a[k] < z2; k=k+1)
		;
	    if (k - i > nmax) {
	        nmax = k - i
	        mode = a[(i+k)/2]
	    }
	} until (k >= j)

	return (mode)
end

