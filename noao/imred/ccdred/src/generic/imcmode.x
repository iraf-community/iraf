include	<imhdr.h>

define	NMIN	10	# Minimum number of pixels for mode calculation
define	NXMAX	100	# Maximum number of pixels per dimension
define	ZRANGE	0.8	# Fraction of pixels about median to use
define	ZSTEP	0.01	# Step size for search for mode
define	ZBIN	0.1	# Bin size for mode.


# IMC_MODE -- Compute the mode within a section of an image.  The image is
# specified by a pointer, the section is specified by the starting and ending
# points and the step.  The number of pixels used is limited if the image
# section is large.  The mode is found by binning the pixels with a bin
# size based on the range over a fraction of the pixels about the median
# and a bin step which may be smaller than the bins size.  If there are too
# few points then the median is returned.

real procedure imc_moder (im, x1, x2, xs)

pointer	im			# Image
int	x1[ARB]			# Starting pixel
int	x2[ARB]			# Ending pixel
int	xs[ARB]			# Step

real	mode, amedr()
real	z1, z2, zstep, zbin
int	i, n, nx, nmax
pointer	sp, v1, v2, dv, va, vb, data, ptr1, ptr2, ptr3, imgnlr()
bool	fp_equalr()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (dv, IM_MAXDIM, TY_LONG)
	call salloc (va, IM_MAXDIM, TY_LONG)
	call salloc (vb, IM_MAXDIM, TY_LONG)

	# Accumulate the pixel values within the section.
	# Limit the number of pixels in each dimension to a maximum

	n = 1
	do i = 1, IM_NDIM(im) {
	    Meml[v1+i-1] = min (x1[i], x2[i])
	    Meml[v2+i-1] = max (x1[i], x2[i])
	    Meml[dv+i-1] = abs (xs[i])
	    nx = min (NXMAX, (Meml[v2+i-1] - Meml[v1+i-1]) / Meml[dv+i-1] + 1)
	    if (nx == 1)
	        Meml[dv+i-1] = 1
	    else
	        Meml[dv+i-1] = (Meml[v2+i-1] - Meml[v1+i-1]) / (nx - 1)
	    Meml[v2+i-1] = Meml[v1+i-1] + (nx - 1) * Meml[dv+i-1]
	    n = n * nx
	}
	call amovl (Meml[v1], Meml[va], IM_MAXDIM)
	Meml[va] = 1

	call salloc (data, n, TY_REAL)
	ptr1 = data
	call amovl (Meml[va], Meml[vb], IM_MAXDIM)
	while (imgnlr (im, ptr2, Meml[vb]) != EOF) {
	    ptr2 = ptr2 + Meml[v1] - 1
	    do i = Meml[v1], Meml[v2], Meml[dv] {
		Memr[ptr1] = Memr[ptr2]
		ptr1 = ptr1 + 1
		ptr2 = ptr2 + Meml[dv]
	    }
	    for (i=2; i<=IM_NDIM(im); i=i+1) {
		Meml[va+i-1] = Meml[va+i-1] + Meml[dv+i-1]
		if (Meml[va+i-1] <= Meml[v2+i-1])
		    break
		Meml[va+i-1] = Meml[v1+i-1]
	    }
	    if (i > IM_NDIM(im))
		break
	    call amovl (Meml[va], Meml[vb], IM_MAXDIM)
	}

	# If there are less than NMIN points then return the median.
	if (n < NMIN) {
	    mode = amedr (Memr[data], n)
	    call sfree (sp)
	    return (mode)
	}

	# Compute the mode.  First sort the pixel values.  Consider a
	# range of values about the median point.  Use a
	# bin size which is ZBIN of the range.  Step the bin limits
	# in ZSTEP fraction of the bin size.

	call asrtr (Memr[data], Memr[data], n)
	ptr1 = data + n * (1. - ZRANGE) / 2.
	ptr2 = ptr1
	ptr3 = data + n * (1. + ZRANGE) / 2.
	z1 = Memr[ptr1]
	z2 = Memr[ptr3]
	if (fp_equalr (z1, z2)) {
	    mode = z1
	    call sfree (sp)
	    return (mode)
	}

	zstep = ZSTEP * (z2 - z1)
	zbin = ZBIN * (z2 - z1)

	nmax = 0
	repeat {
	    for (; ptr1 < ptr3 && Memr[ptr1] < z1; ptr1=ptr1+1)
		;
	    z2 = z1 + zbin
	    for (; ptr2 < ptr3 && Memr[ptr2] < z2; ptr2=ptr2+1)
		;
	    if (ptr2 - ptr1 > nmax) {
	        nmax = ptr2 - ptr1
	        mode = Memr[(ptr2+ptr1)/2]
	    }
	    z1 = z1 + zstep
	} until (ptr2 >= ptr3)

	call sfree (sp)
	return (mode)
end

# IMC_MODE -- Compute the mode within a section of an image.  The image is
# specified by a pointer, the section is specified by the starting and ending
# points and the step.  The number of pixels used is limited if the image
# section is large.  The mode is found by binning the pixels with a bin
# size based on the range over a fraction of the pixels about the median
# and a bin step which may be smaller than the bins size.  If there are too
# few points then the median is returned.

short procedure imc_modes (im, x1, x2, xs)

pointer	im			# Image
int	x1[ARB]			# Starting pixel
int	x2[ARB]			# Ending pixel
int	xs[ARB]			# Step

short	mode, ameds()
real	z1, z2, zstep, zbin
int	i, n, nx, nmax
pointer	sp, v1, v2, dv, va, vb, data, ptr1, ptr2, ptr3, imgnls()
bool	fp_equalr()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (dv, IM_MAXDIM, TY_LONG)
	call salloc (va, IM_MAXDIM, TY_LONG)
	call salloc (vb, IM_MAXDIM, TY_LONG)

	# Accumulate the pixel values within the section.
	# Limit the number of pixels in each dimension to a maximum

	n = 1
	do i = 1, IM_NDIM(im) {
	    Meml[v1+i-1] = min (x1[i], x2[i])
	    Meml[v2+i-1] = max (x1[i], x2[i])
	    Meml[dv+i-1] = abs (xs[i])
	    nx = min (NXMAX, (Meml[v2+i-1] - Meml[v1+i-1]) / Meml[dv+i-1] + 1)
	    if (nx == 1)
	        Meml[dv+i-1] = 1
	    else
	        Meml[dv+i-1] = (Meml[v2+i-1] - Meml[v1+i-1]) / (nx - 1)
	    Meml[v2+i-1] = Meml[v1+i-1] + (nx - 1) * Meml[dv+i-1]
	    n = n * nx
	}
	call amovl (Meml[v1], Meml[va], IM_MAXDIM)
	Meml[va] = 1

	call salloc (data, n, TY_SHORT)
	ptr1 = data
	call amovl (Meml[va], Meml[vb], IM_MAXDIM)
	while (imgnls (im, ptr2, Meml[vb]) != EOF) {
	    ptr2 = ptr2 + Meml[v1] - 1
	    do i = Meml[v1], Meml[v2], Meml[dv] {
		Mems[ptr1] = Mems[ptr2]
		ptr1 = ptr1 + 1
		ptr2 = ptr2 + Meml[dv]
	    }
	    for (i=2; i<=IM_NDIM(im); i=i+1) {
		Meml[va+i-1] = Meml[va+i-1] + Meml[dv+i-1]
		if (Meml[va+i-1] <= Meml[v2+i-1])
		    break
		Meml[va+i-1] = Meml[v1+i-1]
	    }
	    if (i > IM_NDIM(im))
		break
	    call amovl (Meml[va], Meml[vb], IM_MAXDIM)
	}

	# If there are less than NMIN points then return the median.
	if (n < NMIN) {
	    mode = ameds (Mems[data], n)
	    call sfree (sp)
	    return (mode)
	}

	# Compute the mode.  First sort the pixel values.  Consider a
	# range of values about the median point.  Use a
	# bin size which is ZBIN of the range.  Step the bin limits
	# in ZSTEP fraction of the bin size.

	call asrts (Mems[data], Mems[data], n)
	ptr1 = data + n * (1. - ZRANGE) / 2.
	ptr2 = ptr1
	ptr3 = data + n * (1. + ZRANGE) / 2.
	z1 = Mems[ptr1]
	z2 = Mems[ptr3]
	if (fp_equalr (z1, z2)) {
	    mode = z1
	    call sfree (sp)
	    return (mode)
	}

	zstep = ZSTEP * (z2 - z1)
	zbin = ZBIN * (z2 - z1)
	zstep = max (1., zstep)
	zbin = max (1., zbin)

	nmax = 0
	repeat {
	    for (; ptr1 < ptr3 && Mems[ptr1] < z1; ptr1=ptr1+1)
		;
	    z2 = z1 + zbin
	    for (; ptr2 < ptr3 && Mems[ptr2] < z2; ptr2=ptr2+1)
		;
	    if (ptr2 - ptr1 > nmax) {
	        nmax = ptr2 - ptr1
	        mode = Mems[(ptr2+ptr1)/2]
	    }
	    z1 = z1 + zstep
	} until (ptr2 >= ptr3)

	call sfree (sp)
	return (mode)
end

