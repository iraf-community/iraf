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
int	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

int	i, j, ndim, n, nv
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp, imgnls()

real	asums()
short	ic_modes()

include	"../icombine.com"

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (dv, IM_MAXDIM, TY_LONG)
	call salloc (va, IM_MAXDIM, TY_LONG)
	call salloc (vb, IM_MAXDIM, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	call amovki (1, Memi[v1], IM_MAXDIM)
	call amovki (1, Memi[va], IM_MAXDIM)
	call amovki (1, Memi[dv], IM_MAXDIM)
	call amovi (IM_LEN(imref,1), Memi[vb], ndim)
	call ic_section (section, Memi[va], Memi[vb], Memi[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Memi[va+i-1] = Memi[va+i-1] - offsets[image,i]
		Memi[vb+i-1] = Memi[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Memi[v1+i] = max (1, min (Memi[va+i], Memi[vb+i]))
		Memi[v2+i] = min (IM_LEN(im,i+1), max (Memi[va+i], Memi[vb+i]))
		Memi[dv+i] = j
		nv = max (1, (Memi[v2+i] - Memi[v1+i]) / Memi[dv+i] + 1)
		Memi[v2+i] = Memi[v1+i] + (nv - 1) * Memi[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	call amovl (Memi[v1], Memi[va], IM_MAXDIM)
	Memi[va] = 1
	if (project)
	   Memi[va+ndim] = image
	call amovl (Memi[va], Memi[vb], IM_MAXDIM)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_SHORT)
	dp = data
	while (imgnls (im, lp, Memi[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Memi[va], mask)
	    lp = lp + Memi[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			a = Mems[lp]
			if (a >= lthresh && a <= hthresh) {
			    Mems[dp] = a
			    dp = dp + 1
			}
			lp = lp + Memi[dv]
		    }
		} else {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			Mems[dp] = Mems[lp]
			dp = dp + 1
			lp = lp + Memi[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Memi[v1] - 1
		if (dothresh) {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			if (Memi[mp] == 0) {
			    a = Mems[lp]
			    if (a >= lthresh && a <= hthresh) {
				Mems[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Memi[dv]
			lp = lp + Memi[dv]
		    }
		} else {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			if (Memi[mp] == 0) {
			    Mems[dp] = Mems[lp]
			    dp = dp + 1
			}
			mp = mp + Memi[dv]
			lp = lp + Memi[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Memi[va+i-1] = Memi[va+i-1] + Memi[dv+i-1]
		if (Memi[va+i-1] <= Memi[v2+i-1])
		    break
		Memi[va+i-1] = Memi[v1+i-1]
	    }
	    if (i > ndim)
		break
	    call amovl (Memi[va], Memi[vb], IM_MAXDIM)
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
int	n			# Number of points

int	i, j, k, nmax
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
int	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

int	i, j, ndim, n, nv
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp, imgnli()

real	asumi()
int	ic_modei()

include	"../icombine.com"

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (dv, IM_MAXDIM, TY_LONG)
	call salloc (va, IM_MAXDIM, TY_LONG)
	call salloc (vb, IM_MAXDIM, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	call amovki (1, Memi[v1], IM_MAXDIM)
	call amovki (1, Memi[va], IM_MAXDIM)
	call amovki (1, Memi[dv], IM_MAXDIM)
	call amovi (IM_LEN(imref,1), Memi[vb], ndim)
	call ic_section (section, Memi[va], Memi[vb], Memi[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Memi[va+i-1] = Memi[va+i-1] - offsets[image,i]
		Memi[vb+i-1] = Memi[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Memi[v1+i] = max (1, min (Memi[va+i], Memi[vb+i]))
		Memi[v2+i] = min (IM_LEN(im,i+1), max (Memi[va+i], Memi[vb+i]))
		Memi[dv+i] = j
		nv = max (1, (Memi[v2+i] - Memi[v1+i]) / Memi[dv+i] + 1)
		Memi[v2+i] = Memi[v1+i] + (nv - 1) * Memi[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	call amovl (Memi[v1], Memi[va], IM_MAXDIM)
	Memi[va] = 1
	if (project)
	   Memi[va+ndim] = image
	call amovl (Memi[va], Memi[vb], IM_MAXDIM)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_INT)
	dp = data
	while (imgnli (im, lp, Memi[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Memi[va], mask)
	    lp = lp + Memi[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			a = Memi[lp]
			if (a >= lthresh && a <= hthresh) {
			    Memi[dp] = a
			    dp = dp + 1
			}
			lp = lp + Memi[dv]
		    }
		} else {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			Memi[dp] = Memi[lp]
			dp = dp + 1
			lp = lp + Memi[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Memi[v1] - 1
		if (dothresh) {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			if (Memi[mp] == 0) {
			    a = Memi[lp]
			    if (a >= lthresh && a <= hthresh) {
				Memi[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Memi[dv]
			lp = lp + Memi[dv]
		    }
		} else {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			if (Memi[mp] == 0) {
			    Memi[dp] = Memi[lp]
			    dp = dp + 1
			}
			mp = mp + Memi[dv]
			lp = lp + Memi[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Memi[va+i-1] = Memi[va+i-1] + Memi[dv+i-1]
		if (Memi[va+i-1] <= Memi[v2+i-1])
		    break
		Memi[va+i-1] = Memi[v1+i-1]
	    }
	    if (i > ndim)
		break
	    call amovl (Memi[va], Memi[vb], IM_MAXDIM)
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
int	n			# Number of points

int	i, j, k, nmax
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

procedure ic_statr (im, imref, section, offsets, image, nimages,
	domode, domedian, domean, mode, median, mean)

pointer	im			 # Data image
pointer	imref			 # Reference image for image section
char	section[ARB]		 # Image section
int	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

int	i, j, ndim, n, nv
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp, imgnlr()

real	asumr()
real	ic_moder()

include	"../icombine.com"

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (dv, IM_MAXDIM, TY_LONG)
	call salloc (va, IM_MAXDIM, TY_LONG)
	call salloc (vb, IM_MAXDIM, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	call amovki (1, Memi[v1], IM_MAXDIM)
	call amovki (1, Memi[va], IM_MAXDIM)
	call amovki (1, Memi[dv], IM_MAXDIM)
	call amovi (IM_LEN(imref,1), Memi[vb], ndim)
	call ic_section (section, Memi[va], Memi[vb], Memi[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Memi[va+i-1] = Memi[va+i-1] - offsets[image,i]
		Memi[vb+i-1] = Memi[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Memi[v1+i] = max (1, min (Memi[va+i], Memi[vb+i]))
		Memi[v2+i] = min (IM_LEN(im,i+1), max (Memi[va+i], Memi[vb+i]))
		Memi[dv+i] = j
		nv = max (1, (Memi[v2+i] - Memi[v1+i]) / Memi[dv+i] + 1)
		Memi[v2+i] = Memi[v1+i] + (nv - 1) * Memi[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	call amovl (Memi[v1], Memi[va], IM_MAXDIM)
	Memi[va] = 1
	if (project)
	   Memi[va+ndim] = image
	call amovl (Memi[va], Memi[vb], IM_MAXDIM)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_REAL)
	dp = data
	while (imgnlr (im, lp, Memi[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Memi[va], mask)
	    lp = lp + Memi[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			a = Memr[lp]
			if (a >= lthresh && a <= hthresh) {
			    Memr[dp] = a
			    dp = dp + 1
			}
			lp = lp + Memi[dv]
		    }
		} else {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			Memr[dp] = Memr[lp]
			dp = dp + 1
			lp = lp + Memi[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Memi[v1] - 1
		if (dothresh) {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			if (Memi[mp] == 0) {
			    a = Memr[lp]
			    if (a >= lthresh && a <= hthresh) {
				Memr[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Memi[dv]
			lp = lp + Memi[dv]
		    }
		} else {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			if (Memi[mp] == 0) {
			    Memr[dp] = Memr[lp]
			    dp = dp + 1
			}
			mp = mp + Memi[dv]
			lp = lp + Memi[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Memi[va+i-1] = Memi[va+i-1] + Memi[dv+i-1]
		if (Memi[va+i-1] <= Memi[v2+i-1])
		    break
		Memi[va+i-1] = Memi[v1+i-1]
	    }
	    if (i > ndim)
		break
	    call amovl (Memi[va], Memi[vb], IM_MAXDIM)
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
int	n			# Number of points

int	i, j, k, nmax
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
int	offsets[nimages,ARB]	 # Image section offset from data to reference
int	image			 # Image index (for mask I/O)
int	nimages			 # Number of images in  offsets.
bool	domode, domedian, domean # Statistics to compute
real	mode, median, mean	 # Statistics

int	i, j, ndim, n, nv
real	a
pointer	sp, v1, v2, dv, va, vb
pointer	data, mask, dp, lp, mp, imgnld()

double	asumd()
double	ic_moded()

include	"../icombine.com"

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (dv, IM_MAXDIM, TY_LONG)
	call salloc (va, IM_MAXDIM, TY_LONG)
	call salloc (vb, IM_MAXDIM, TY_LONG)

	# Determine the image section parameters.  This must be in terms of
	# the data image pixel coordinates though the section may be specified
	# in terms of the reference image coordinates.  Limit the number of
	# pixels in each dimension to a maximum.

	ndim = IM_NDIM(im)
	if (project)
	    ndim = ndim - 1
	call amovki (1, Memi[v1], IM_MAXDIM)
	call amovki (1, Memi[va], IM_MAXDIM)
	call amovki (1, Memi[dv], IM_MAXDIM)
	call amovi (IM_LEN(imref,1), Memi[vb], ndim)
	call ic_section (section, Memi[va], Memi[vb], Memi[dv], ndim)
	if (im != imref)
	    do i = 1, ndim {
		Memi[va+i-1] = Memi[va+i-1] - offsets[image,i]
		Memi[vb+i-1] = Memi[vb+i-1] - offsets[image,i]
	    }

	do j = 1, 10 {
	    n = 1
	    do i = 0, ndim-1 {
		Memi[v1+i] = max (1, min (Memi[va+i], Memi[vb+i]))
		Memi[v2+i] = min (IM_LEN(im,i+1), max (Memi[va+i], Memi[vb+i]))
		Memi[dv+i] = j
		nv = max (1, (Memi[v2+i] - Memi[v1+i]) / Memi[dv+i] + 1)
		Memi[v2+i] = Memi[v1+i] + (nv - 1) * Memi[dv+i]
		n = n * nv
	    }
	    if (n < NMAX)
		break
	}

	call amovl (Memi[v1], Memi[va], IM_MAXDIM)
	Memi[va] = 1
	if (project)
	   Memi[va+ndim] = image
	call amovl (Memi[va], Memi[vb], IM_MAXDIM)

	# Accumulate the pixel values within the section.  Masked pixels and
	# thresholded pixels are ignored.

	call salloc (data, n, TY_DOUBLE)
	dp = data
	while (imgnld (im, lp, Memi[vb]) != EOF) {
	    call ic_mget1 (im, image, nimages, offsets[image,1], Memi[va], mask)
	    lp = lp + Memi[v1] - 1
	    if (dflag == D_ALL) {
		if (dothresh) {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			a = Memd[lp]
			if (a >= lthresh && a <= hthresh) {
			    Memd[dp] = a
			    dp = dp + 1
			}
			lp = lp + Memi[dv]
		    }
		} else {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			Memd[dp] = Memd[lp]
			dp = dp + 1
			lp = lp + Memi[dv]
		    }
		}
	    } else if (dflag == D_MIX) {
		mp = mask + Memi[v1] - 1
		if (dothresh) {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			if (Memi[mp] == 0) {
			    a = Memd[lp]
			    if (a >= lthresh && a <= hthresh) {
				Memd[dp] = a
				dp = dp + 1
			    }
			}
			mp = mp + Memi[dv]
			lp = lp + Memi[dv]
		    }
		} else {
		    do i = Memi[v1], Memi[v2], Memi[dv] {
			if (Memi[mp] == 0) {
			    Memd[dp] = Memd[lp]
			    dp = dp + 1
			}
			mp = mp + Memi[dv]
			lp = lp + Memi[dv]
		    }
		}
	    }
	    for (i=2; i<=ndim; i=i+1) {
		Memi[va+i-1] = Memi[va+i-1] + Memi[dv+i-1]
		if (Memi[va+i-1] <= Memi[v2+i-1])
		    break
		Memi[va+i-1] = Memi[v1+i-1]
	    }
	    if (i > ndim)
		break
	    call amovl (Memi[va], Memi[vb], IM_MAXDIM)
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
int	n			# Number of points

int	i, j, k, nmax
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

