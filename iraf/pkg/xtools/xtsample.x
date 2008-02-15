include	<imhdr.h>


# XT_SAMPLE -- Get sample of pixels.
#
# This routine returns a sample of unmasked pixels from an N-dim image.
# The input is the image pointer, the mask pointer (which may be NULL),
# the array to be filled, the maximum number of sample plixels, and the
# minimum number of lines to sample.  The return value is the actual number
# of pixels which will be less than or equal to the specified maximum number.
# 
# The intent of this routine is to sample fairly uniformly but efficiently.
# If nlines is zero the total number of pixels, in raster order, is divided
# into uniform steps.  But this may end up reading many lines each for a
# few pixels.  To be more efficient if nlines is greater than zero then as
# many pixels per line as possible are read to sample at least the requested
# number of lines.


int procedure xt_samples (im, bpm, sample, nsample, nlines)

pointer	im			#I Image pointer
pointer	bpm			#I Bad pixel pointer
short	sample[nsample]		#I Work array
int	nsample			#I Maximum number of sample pixels
int	nlines			#I Minimum number of lines to sample
int	nreturn			#I Number of pixels returned

long	v[IM_MAXDIM], vbuf[IM_MAXDIM]
int	i, ip, n, ndim, npix, nc
real	p, c, pstep, cstep
pointer	buf, bpmbuf

int	imgnls()

begin
	# Determine the number of pixels in the data, the number
	# to make up nsample pixels, and the pixel step.

	ndim = IM_NDIM(im)
	nc = IM_LEN(im,1)
	npix = 1
	do i = 1, ndim
	    npix = npix * IM_LEN(im,i)
	pstep = real(npix) / min (npix, nsample)

	# To insure a minimum number of lines and efficient use of
	# pixels in a line, set the column step.

	if (nlines == 0)
	    cstep = pstep
	else
	    cstep = nc / min (min(npix,nsample)/nlines, nc)

	# Step through the pixels.
	call amovkl (long(1), v, IM_MAXDIM)
	nreturn = 0
	for (p=(pstep-0.01)/2; p<npix && nreturn<nsample;) {

	    # Convert pixel number to image vector coordinates.
	    n = npix; ip = nint(p)
	    do i = ndim, 1, -1 {
	        n = n / IM_LEN(im,i)
	        v[i] = 1 + ip / n
		ip = mod (ip, n)
	    }

	    # Sample the pixels in the line.
	    if (nlines == 0)
		c = v[1]
	    else
		c = (cstep - 0.01) / 2

	    if (bpm == NULL) {
		v[1] = 1
		if (imgnls (im, buf, v) == EOF)
		    break
		for (; c<nc && nreturn<nsample; c=c+cstep) {
		    ip = nint (c)
		    nreturn = nreturn + 1
		    sample[nreturn] = Mems[buf+ip]
		    p = p + pstep
		}
	    } else {
		v[1] = 1
	        call amovl (v, vbuf, IM_MAXDIM)
		if (imgnls (bpm, bpmbuf, vbuf) == EOF)
		    break
		if (imgnls (im, buf, v) == EOF)
		    break
		for (; c<nc && nreturn<nsample; c=c+cstep) {
		    ip = nint (c)
		    if (Mems[bpmbuf+ip] == 0) {
			nreturn = nreturn + 1
			sample[nreturn] = Mems[buf+ip]
		    }
		    p = p + pstep
		}
	    }
	}

	return (nreturn)
end

int procedure xt_samplei (im, bpm, sample, nsample, nlines)

pointer	im			#I Image pointer
pointer	bpm			#I Bad pixel pointer
int	sample[nsample]		#I Work array
int	nsample			#I Maximum number of sample pixels
int	nlines			#I Minimum number of lines to sample
int	nreturn			#I Number of pixels returned

long	v[IM_MAXDIM], vbuf[IM_MAXDIM]
int	i, ip, n, ndim, npix, nc
real	p, c, pstep, cstep
pointer	buf, bpmbuf

int	imgnls()
int	imgnli()

begin
	# Determine the number of pixels in the data, the number
	# to make up nsample pixels, and the pixel step.

	ndim = IM_NDIM(im)
	nc = IM_LEN(im,1)
	npix = 1
	do i = 1, ndim
	    npix = npix * IM_LEN(im,i)
	pstep = real(npix) / min (npix, nsample)

	# To insure a minimum number of lines and efficient use of
	# pixels in a line, set the column step.

	if (nlines == 0)
	    cstep = pstep
	else
	    cstep = nc / min (min(npix,nsample)/nlines, nc)

	# Step through the pixels.
	call amovkl (long(1), v, IM_MAXDIM)
	nreturn = 0
	for (p=(pstep-0.01)/2; p<npix && nreturn<nsample;) {

	    # Convert pixel number to image vector coordinates.
	    n = npix; ip = nint(p)
	    do i = ndim, 1, -1 {
	        n = n / IM_LEN(im,i)
	        v[i] = 1 + ip / n
		ip = mod (ip, n)
	    }

	    # Sample the pixels in the line.
	    if (nlines == 0)
		c = v[1]
	    else
		c = (cstep - 0.01) / 2

	    if (bpm == NULL) {
		v[1] = 1
		if (imgnli (im, buf, v) == EOF)
		    break
		for (; c<nc && nreturn<nsample; c=c+cstep) {
		    ip = nint (c)
		    nreturn = nreturn + 1
		    sample[nreturn] = Memi[buf+ip]
		    p = p + pstep
		}
	    } else {
		v[1] = 1
	        call amovl (v, vbuf, IM_MAXDIM)
		if (imgnls (bpm, bpmbuf, vbuf) == EOF)
		    break
		if (imgnli (im, buf, v) == EOF)
		    break
		for (; c<nc && nreturn<nsample; c=c+cstep) {
		    ip = nint (c)
		    if (Mems[bpmbuf+ip] == 0) {
			nreturn = nreturn + 1
			sample[nreturn] = Memi[buf+ip]
		    }
		    p = p + pstep
		}
	    }
	}

	return (nreturn)
end

int procedure xt_sampler (im, bpm, sample, nsample, nlines)

pointer	im			#I Image pointer
pointer	bpm			#I Bad pixel pointer
real	sample[nsample]		#I Work array
int	nsample			#I Maximum number of sample pixels
int	nlines			#I Minimum number of lines to sample
int	nreturn			#I Number of pixels returned

long	v[IM_MAXDIM], vbuf[IM_MAXDIM]
int	i, ip, n, ndim, npix, nc
real	p, c, pstep, cstep
pointer	buf, bpmbuf

int	imgnls()
int	imgnlr()

begin
	# Determine the number of pixels in the data, the number
	# to make up nsample pixels, and the pixel step.

	ndim = IM_NDIM(im)
	nc = IM_LEN(im,1)
	npix = 1
	do i = 1, ndim
	    npix = npix * IM_LEN(im,i)
	pstep = real(npix) / min (npix, nsample)

	# To insure a minimum number of lines and efficient use of
	# pixels in a line, set the column step.

	if (nlines == 0)
	    cstep = pstep
	else
	    cstep = nc / min (min(npix,nsample)/nlines, nc)

	# Step through the pixels.
	call amovkl (long(1), v, IM_MAXDIM)
	nreturn = 0
	for (p=(pstep-0.01)/2; p<npix && nreturn<nsample;) {

	    # Convert pixel number to image vector coordinates.
	    n = npix; ip = nint(p)
	    do i = ndim, 1, -1 {
	        n = n / IM_LEN(im,i)
	        v[i] = 1 + ip / n
		ip = mod (ip, n)
	    }

	    # Sample the pixels in the line.
	    if (nlines == 0)
		c = v[1]
	    else
		c = (cstep - 0.01) / 2

	    if (bpm == NULL) {
		v[1] = 1
		if (imgnlr (im, buf, v) == EOF)
		    break
		for (; c<nc && nreturn<nsample; c=c+cstep) {
		    ip = nint (c)
		    nreturn = nreturn + 1
		    sample[nreturn] = Memr[buf+ip]
		    p = p + pstep
		}
	    } else {
		v[1] = 1
	        call amovl (v, vbuf, IM_MAXDIM)
		if (imgnls (bpm, bpmbuf, vbuf) == EOF)
		    break
		if (imgnlr (im, buf, v) == EOF)
		    break
		for (; c<nc && nreturn<nsample; c=c+cstep) {
		    ip = nint (c)
		    if (Mems[bpmbuf+ip] == 0) {
			nreturn = nreturn + 1
			sample[nreturn] = Memr[buf+ip]
		    }
		    p = p + pstep
		}
	    }
	}

	return (nreturn)
end

int procedure xt_sampled (im, bpm, sample, nsample, nlines)

pointer	im			#I Image pointer
pointer	bpm			#I Bad pixel pointer
double	sample[nsample]		#I Work array
int	nsample			#I Maximum number of sample pixels
int	nlines			#I Minimum number of lines to sample
int	nreturn			#I Number of pixels returned

long	v[IM_MAXDIM], vbuf[IM_MAXDIM]
int	i, ip, n, ndim, npix, nc
real	p, c, pstep, cstep
pointer	buf, bpmbuf

int	imgnls()
int	imgnld()

begin
	# Determine the number of pixels in the data, the number
	# to make up nsample pixels, and the pixel step.

	ndim = IM_NDIM(im)
	nc = IM_LEN(im,1)
	npix = 1
	do i = 1, ndim
	    npix = npix * IM_LEN(im,i)
	pstep = real(npix) / min (npix, nsample)

	# To insure a minimum number of lines and efficient use of
	# pixels in a line, set the column step.

	if (nlines == 0)
	    cstep = pstep
	else
	    cstep = nc / min (min(npix,nsample)/nlines, nc)

	# Step through the pixels.
	call amovkl (long(1), v, IM_MAXDIM)
	nreturn = 0
	for (p=(pstep-0.01)/2; p<npix && nreturn<nsample;) {

	    # Convert pixel number to image vector coordinates.
	    n = npix; ip = nint(p)
	    do i = ndim, 1, -1 {
	        n = n / IM_LEN(im,i)
	        v[i] = 1 + ip / n
		ip = mod (ip, n)
	    }

	    # Sample the pixels in the line.
	    if (nlines == 0)
		c = v[1]
	    else
		c = (cstep - 0.01) / 2

	    if (bpm == NULL) {
		v[1] = 1
		if (imgnld (im, buf, v) == EOF)
		    break
		for (; c<nc && nreturn<nsample; c=c+cstep) {
		    ip = nint (c)
		    nreturn = nreturn + 1
		    sample[nreturn] = Memd[buf+ip]
		    p = p + pstep
		}
	    } else {
		v[1] = 1
	        call amovl (v, vbuf, IM_MAXDIM)
		if (imgnls (bpm, bpmbuf, vbuf) == EOF)
		    break
		if (imgnld (im, buf, v) == EOF)
		    break
		for (; c<nc && nreturn<nsample; c=c+cstep) {
		    ip = nint (c)
		    if (Mems[bpmbuf+ip] == 0) {
			nreturn = nreturn + 1
			sample[nreturn] = Memd[buf+ip]
		    }
		    p = p + pstep
		}
	    }
	}

	return (nreturn)
end

