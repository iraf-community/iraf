include	<ctype.h>
include	<imhdr.h>


# ODCNV -- Get a line of data possibly convolved.  Also get the unconvolved
# data, the sky data, and the sky sigma data.
#
# This routine must be called sequentially starting with the first line.
# It is initialized when the first line.  Memory is freed by using a final
# call with a line of zero.

procedure convolve (in, bpm, sky, sig, exp, offset, scale, line, cnv,
	indata, bp, cnvdata, skydata, sigdata, expdata, cnvwt, logfd)

pointer	in[2]		#I Image pointers
pointer	bpm[2]		#I BPM pointer
pointer	sky[2]		#I Sky map
pointer	sig[2]		#I Sigma map
pointer	exp[2]		#I Exposure map
int	offset[2]	#I Offsets
real	scale[2]	#I Image scales
int	line		#I Line
char	cnv[ARB]	#I Convolution string
pointer	indata[2]	#O Pointers to unconvolved image data
pointer	bp		#O Bad pixel data
pointer	cnvdata		#O Pointer to convolved image data
pointer	skydata[2]	#O Pointer to sky data
pointer	sigdata[2]	#O Pointer to sigma data corrected by exposure map
pointer	expdata[2]	#O Pointer to exposure map data
real	cnvwt		#O Weight for convolved sigma
int	logfd		#I Logfile

int	i, j, k,  nx, ny, nx2, ny2, nc, nl, mode, off
real	wts, wts1
pointer	bpm2, kptr, ptr, symptr, symwptr
bool	dobpm, overlap, fp_equalr()

pointer	kernel, sym, symbuf, symwts, buf, buf2, buf3, bpbuf, bpwts, wtsl, scales
data	kernel/NULL/, sym/NULL/, symbuf/NULL/, symwts/NULL/
data	buf/NULL/, buf2/NULL/, buf3/NULL/, bpbuf/NULL/, bpwts/NULL/
data	wtsl/NULL/, scales/NULL/

errchk	cnvparse, cnvgline2

begin
	# If no convolution.
	if (cnv[1] == EOS) {
	    if (line == 0)
		return

	    call cnvgline1 (line, offset, in, bpm, indata, bp)
	    call cnvgline2 (line, offset, in, sky, sig, exp, skydata,
		sigdata, expdata)
	    cnvwt = 1
	    if (in[2] == NULL)
		cnvdata = indata[1]
	    else
		call asubr_scale (Memr[indata[1]], scale[1],
		    Memr[indata[2]], scale[2], Memr[cnvdata], IM_LEN(in[1],1))
	    return
	}

	# Free memory.
	if (line == 0) {
	    if (symbuf != NULL) {
		do i = 0, ARB {
		    ptr = Memi[symbuf+i]
		    if (ptr == -1)
			break
		    call mfree (ptr, TY_REAL)
		}
	    }
	    if (symwts != NULL) {
		do i = 0, ARB {
		    ptr = Memi[symwts+i]
		    if (ptr == -1)
			break
		    call mfree (ptr, TY_REAL)
		}
	    }
	    call mfree (scales, TY_REAL)
	    call mfree (wtsl, TY_REAL)
	    call mfree (kernel, TY_REAL)
	    call mfree (scales, TY_REAL)
	    call mfree (sym, TY_INT)
	    call mfree (symbuf, TY_POINTER)
	    call mfree (symwts, TY_POINTER)
	    call mfree (buf, TY_REAL)
	    call mfree (buf2, TY_REAL)
	    call mfree (buf3, TY_REAL)
	    call mfree (bpbuf, TY_INT)
	    call mfree (bpwts, TY_REAL)

	    return
	}

	# Initialize by getting the kernel coefficients, setting the
	# image I/O buffers using a scrolling array, and allocate memory.

	if (line == 1 || buf == NULL) {
	    if (buf != NULL) {
		if (symbuf != NULL) {
		    do i = 0, ARB {
			ptr = Memi[symbuf+i]
			if (ptr == -1)
			    break
			call mfree (ptr, TY_REAL)
		    }
		}
		if (symwts != NULL) {
		    do i = 0, ARB {
			ptr = Memi[symwts+i]
			if (ptr == -1)
			    break
			call mfree (ptr, TY_REAL)
		    }
		}
		call mfree (scales, TY_REAL)
		call mfree (wtsl, TY_REAL)
		call mfree (kernel, TY_REAL)
		call mfree (scales, TY_REAL)
		call mfree (sym, TY_INT)
		call mfree (symbuf, TY_POINTER)
		call mfree (symwts, TY_POINTER)
		call mfree (buf, TY_REAL)
		call mfree (buf2, TY_REAL)
		call mfree (buf3, TY_REAL)
		call mfree (bpbuf, TY_INT)
		call mfree (bpwts, TY_REAL)
	    }

	    nc = IM_LEN(in[1],1)
	    nl = IM_LEN(in[1],2)

	    call cnvparse (cnv, kernel, nx, ny, logfd)
	    nx2 = nx / 2
	    ny2 = ny / 2
	    call malloc (scales, ny, TY_REAL)
	    call calloc (wtsl, ny, TY_REAL)
	    call amovkr (1., Memr[scales], ny)

	    # Check for lines which are simple scalings of the first line.
	    do i = 2, ny {
		kptr = kernel + (i - 1) * nx
		wts = 0.
		do k = 0, nx-1 {
		    if (Memr[kptr+k] == 0. || Memr[kernel+k] == 0.) {
			wts = 0.
			break
		    }
		    if (wts == 0.)
			wts = Memr[kptr+k] / Memr[kernel+k]
		    else {
			wts1 = Memr[kptr+k] / Memr[kernel+k]
			if (!fp_equalr (wts, wts1))
			    break
		    }
		}
		if (wts != 0. && fp_equalr (wts, wts1)) {
		    Memr[scales+i-1] = wts
		    call amovr (Memr[kernel], Memr[kptr], nx)
		}
	    }

	    wts = 0
	    do i = 1, ny {
		kptr = kernel + (i - 1) * nx
		wts1 = 0.
		do j = 1, nx {
		    wts1 = wts1 + Memr[kptr]
		    kptr = kptr + 1
		}
		Memr[wtsl+i-1] = wts1
		wts = wts + wts1
	    }
	    if (wts != 0.) {
		call adivkr (Memr[wtsl], wts, Memr[wtsl], ny)
		call adivkr (Memr[kernel], wts, Memr[kernel], nx*ny)
	    }
	    cnvwt = sqrt (wts)

	    if (in[2] == NULL)
		bpm2 = NULL
	    else
		bpm2 = bpm[2] 
	    if (bpm[1] == NULL && bpm2 == NULL)
		dobpm = false
	    else
		dobpm = true
	    if (dobpm) {
		call malloc (bpbuf, nc*ny, TY_INT)
		call malloc (bpwts, nc, TY_REAL)
		call calloc (symwts, ny*ny+1, TY_POINTER)
		Memi[symwts+ny*ny] = -1
	    }

	    # Check for any line symmetries in the kernel.
	    call malloc (sym, ny, TY_INT)
	    call calloc (symbuf, ny*ny+1, TY_POINTER)
	    Memi[symbuf+ny*ny] = -1
	    do i = ny, 1, -1 {
		kptr = kernel + (i - 1) * nx
		do j = ny, 1, -1 {
		    ptr = kernel + (j - 1) * nx
		    do k = 0, nx-1 {
			if (Memr[kptr+k] != Memr[ptr+k])
			    break
		    }
		    if (k == nx) {
			Memi[sym+i-1] = j
			break
		    }
		}
	    }
	    do i = ny, 1, -1 {
		k = 0
		do j = ny, 1, -1
		    if (Memi[sym+j-1] == i)
			k = k + 1
		if (k == 1)
		    Memi[sym+i-1] = 0
	    }

	    call malloc (buf, nc*ny, TY_REAL)
	    if (in[2] != NULL) {
		call malloc (buf2, nc*ny, TY_REAL)
		call malloc (buf3, nc*ny, TY_REAL)
	    }

	    if (in[2] != NULL) {
		overlap = true
		if (1-offset[1] < 1 || nc-offset[1] > IM_LEN(in[2],1))
		    overlap = false
		if (1-offset[2] < 1 || nl-offset[2] > IM_LEN(in[2],2))
		    overlap = false
	    }
	    do i = 1, ny {
		call cnvgline1 (i, offset, in, bpm, indata, bp)
		off = mod (i, ny) * nc
		call amovr (Memr[indata[1]], Memr[buf+off], nc)
		if (in[2] != NULL) {
		    call amovr (Memr[indata[2]], Memr[buf2+off], nc)
		    call asubr_scale (Memr[buf+off], scale[1],
			Memr[buf2+off], scale[2], Memr[buf3+off], nc)
		}
		if (dobpm)
		    call amovi (Memi[bp], Memi[bpbuf+off], nc)
	    }
	}

	# Get new line.
	j = line +  ny2
	if (j > ny && j <= nl) {
	    call cnvgline1 (j, offset, in, bpm, indata, bp)
	    off = mod (j, ny) * nc
	    call amovr (Memr[indata[1]], Memr[buf+off], nc)
	    if (in[2] != NULL) {
		call amovr (Memr[indata[2]], Memr[buf2+off], nc)
		call asubr_scale (Memr[buf+off], scale[1],
		    Memr[buf2+off], scale[2], Memr[buf3+off], nc)
	    }
	    if (dobpm) {
		ptr = bpbuf + off
		call amovi (Memi[bp], Memi[ptr], nc)
	    }
	}

	# Compute the convolution vector with boundary reflection.
	# Save and reuse lines with the same kernel weights apart
	# from a scale factor.

	kptr = kernel
	call aclrr (Memr[cnvdata], nc)
	if (dobpm)
	    call aclrr (Memr[bpwts], nc)
	do i = 1, ny {
	    j = line + i - ny2 - 1
	    if (j < 1)
		j = 2 - j
	    else if (j > nl)
		j = 2 * nl - j
	    off = mod (j, ny) * nc
	    if (in[2] == NULL)
		ptr = buf
	    else
		ptr = buf3
	    k = Memi[sym+i-1]
	    if (k == 0) {
		mode = 1
		symptr = ptr
		symwptr = bpwts
	    } else {
		if (k == i)
		    mode = 2
		else
		    mode = 3
		symptr = Memi[symbuf+(k-1)*ny+mod(j,ny)]
		if (symptr == NULL) {
		    call malloc (symptr, nc, TY_REAL)
		    Memi[symbuf+(k-1)*ny+mod(j,ny)] = symptr
		    mode = 2
		}
		if (dobpm) {
		    symwptr = Memi[symwts+(k-1)*ny+mod(j,ny)]
		    if (symwptr == NULL) {
			call malloc (symwptr, nc, TY_REAL)
			Memi[symwts+(k-1)*ny+mod(j,ny)] = symwptr
		    }
		}
	    }
	    if (dobpm)
		call convolve2 (Memr[ptr+off], Memr[cnvdata], Memr[symptr],
		    nc, Memr[kptr], Memr[scales+i-1], nx, Memi[bpbuf+off],
		    Memr[wtsl+i-1], Memr[bpwts], Memr[symwptr], mode)
	    else
		call convolve1 (Memr[ptr+off], Memr[cnvdata], Memr[symptr],
		    nc, Memr[kptr], Memr[scales+i-1], nx, mode)
	    kptr = kptr + nx
	}
	if (dobpm) {
	    do i = 0, nc-1
		if (Memr[bpwts+i] != 0.)
		    Memr[cnvdata+i] = Memr[cnvdata+i] / Memr[bpwts+i]
	}

	# Set the output vectors.
	off = mod (line, ny) * nc
	indata[1] = buf + off 
	if (dobpm) {
	    if (bpm2 == NULL)
		bp = bpbuf + off
	    else
		call amovi (Memi[bpbuf+off], Memi[bp], nc)
	}
	if (in[2] != NULL) {
	    if (overlap)
		indata[2] = buf2 + off
	    else
		call amovr (Memr[buf2+off], Memr[indata[2]], nc)
	}
	call cnvgline2 (line, offset, in, sky, sig, exp, skydata, sigdata,
	   expdata)
end
	


# ODCNV1 --  One dimensional convolution with boundary reflection.
#
# The convolution is added to the output so that it might be used
# as part of a 2D convolution.

procedure convolve1 (in, out, save, nc, xkernel, scale, nx, mode)

real	in[nc]			#I Input data to be convolved
real	out[nc]			#O Output convolved data
real	save[nc]		#U Output saved data
int	nc			#I Number of data points
real	xkernel[nx]		#I Convolution weights
real	scale			#I Scale for saved vector
int	nx			#I Number of convolution points (must be odd)
int	mode			#I Mode (1=no save, 2=save, 3=use save)

int	i, j, k, nx2
real	val
bool	fp_equalr()

begin
	if (mode == 1) {
	    nx2 = nx / 2
	    do i = 1, nx2 {
		val = 0
		do j = 1, nx {
		    k = i + j - nx2 - 1
		    if (k < 1)
			k = 2 - k
		    val = val + in[k] * xkernel[j]
		}
		out[i] = out[i] + val
	    }
	    do i = nx2+1, nc-nx2 {
		k = i - nx2
		val = 0
		do j = 1, nx {
		    val = val + in[k] * xkernel[j]
		    k = k + 1
		}
		out[i] = out[i] + val
	    }
	    do i = nc-nx2+1, nc {
		val = 0
		do j = 1, nx {
		    k = i + j - nx2 - 1
		    if (k > nc)
			k = 2 * nc - k
		    val = val + in[k] * xkernel[j]
		}
		out[i] = out[i] + val
	    }
	} else if (mode == 2) {
	    nx2 = nx / 2
	    do i = 1, nx2 {
		val = 0
		do j = 1, nx {
		    k = i + j - nx2 - 1
		    if (k < 1)
			k = 2 - k
		    val = val + in[k] * xkernel[j]
		}
		out[i] = out[i] + val
		save[i] = val
	    }
	    do i = nx2+1, nc-nx2 {
		k = i - nx2
		val = 0
		do j = 1, nx {
		    val = val + in[k] * xkernel[j]
		    k = k + 1
		}
		out[i] = out[i] + val
		save[i] = val
	    }
	    do i = nc-nx2+1, nc {
		val = 0
		do j = 1, nx {
		    k = i + j - nx2 - 1
		    if (k > nc)
			k = 2 * nc - k
		    val = val + in[k] * xkernel[j]
		}
		out[i] = out[i] + val
		save[i] = val
	    }
	} else {
	    if (fp_equalr (1., scale)) {
		do i = 1, nc
		    out[i] = out[i] + save[i]
	    } else {
		do i = 1, nc
		    out[i] = out[i] + scale * save[i]
	    }
	}
end


# ODCNV2 --  One dimensional convolution with boundary reflection and masking.
#
# The convolution is added to the output so that it might be used
# as part of a 2D convolution.

procedure convolve2 (in, out, save, nc, xkernel, scale, nx, bp,
	wtssum, wts, wtsave, mode)

real	in[nc]			#I Input data to be convolved
real	out[nc]			#O Output convolved data
real	save[nc]		#U Output saved data
int	nc			#I Number of data points
real	xkernel[nx]		#I Convolution weights
real	scale			#I Scale for saved vector
int	nx			#I Number of convolution points (must be odd)
int	bp[nc]			#I Bad pixel data
real	wtssum			#I Sum of weights
real	wts[nc]			#I Weights
real	wtsave[nc]		#U Output saved weight data
int	mode			#I Mode (1=no save, 2=save, 3=use save)

int	i, j, k, nx2
real	val, wt
bool	fp_equalr()

begin
	if (mode == 1) {
	    nx2 = nx / 2
	    do i = 1, nx2 {
		val = 0
		wt = wtssum
		do j = 1, nx {
		    k = i + j - nx2 - 1
		    if (k < 1)
			k = 2 - k
		    if (bp[k] == 0)
			val = val + in[k] * xkernel[j]
		    else
			wt = wt - xkernel[j]
		}
		out[i] = out[i] + val
		wts[i] = wts[i] + wt
	    }
	    do i = nx2+1, nc-nx2 {
		k = i - nx2
		val = 0
		wt = wtssum
		do j = 1, nx {
		    if (bp[k] == 0)
			val = val + in[k] * xkernel[j]
		    else
			wt = wt - xkernel[j]
		    k = k + 1
		}
		out[i] = out[i] + val
		wts[i] = wts[i] + wt
	    }
	    do i = nc-nx2+1, nc {
		val = 0
		wt = wtssum
		do j = 1, nx {
		    k = i + j - nx2 - 1
		    if (k > nc)
			k = 2 * nc - k
		    if (bp[k] == 0)
			val = val + in[k] * xkernel[j]
		    else
			wt = wt - xkernel[j]
		}
		out[i] = out[i] + val
		wts[i] = wts[i] + wt
	    }
	} else if (mode == 2) {
	    nx2 = nx / 2
	    do i = 1, nx2 {
		val = 0
		wt = wtssum
		do j = 1, nx {
		    k = i + j - nx2 - 1
		    if (k < 1)
			k = 2 - k
		    if (bp[k] == 0)
			val = val + in[k] * xkernel[j]
		    else
			wt = wt - xkernel[j]
		}
		out[i] = out[i] + val
		wts[i] = wts[i] + wt
		save[i] = val
		wtsave[i] = wt
	    }
	    do i = nx2+1, nc-nx2 {
		k = i - nx2
		val = 0
		wt = wtssum
		do j = 1, nx {
		    if (bp[k] == 0)
			val = val + in[k] * xkernel[j]
		    else
			wt = wt - xkernel[j]
		    k = k + 1
		}
		out[i] = out[i] + val
		wts[i] = wts[i] + wt
		save[i] = val
		wtsave[i] = wt
	    }
	    do i = nc-nx2+1, nc {
		val = 0
		wt = wtssum
		do j = 1, nx {
		    k = i + j - nx2 - 1
		    if (k > nc)
			k = 2 * nc - k
		    if (bp[k] == 0)
			val = val + in[k] * xkernel[j]
		    else
			wt = wt - xkernel[j]
		}
		out[i] = out[i] + val
		wts[i] = wts[i] + wt
		save[i] = val
		wtsave[i] = wt
	    }
	} else {
	    if (fp_equalr (1., scale)) {
		do i = 1, nc {
		    out[i] = out[i] + save[i]
		    wts[i] = wts[i] + wtsave[i]
		}
	    } else {
		do i = 1, nc {
		    out[i] = out[i] + scale * save[i]
		    wts[i] = wts[i] + scale * wtsave[i]
		}
	    }
	}
end


# ASUBR_SCALE -- out = in1 * scale1 - in2 * scale2

procedure asubr_scale (in1, scale1, in2, scale2, out, n)

real	in1[n]			#I Input vector
real	scale1			#I Scale
real	in2[n]			#I Input vector
real	scale2			#I Scale
real	out[n]			#O Output vector
int	n			#I Number of points

int	i

begin
	if (scale1 == 1. && scale2 == 1.)
	    call asubr (in1, in2, out, n)
	else if (scale1 == 1.) {
	    do i = 1, n
		out[i] = in1[i] - in2[i] * scale2
	} else if (scale2 == 1.) {
	    do i = 1, n
		out[i] = in1[i] * scale1 - in2[i]
	} else {
	    do i = 1, n
		out[i] = in1[i] * scale1 - in2[i] * scale2
	}
end


procedure cnvgline1 (line, offset, im, bpm, imdata, bp)

int	line			#I Line to be read
int	offset[2]		#I Offsets
pointer	im[2]			#I Image pointers
pointer	bpm[2]			#I Bad pixel mask pointers
pointer	imdata[2]		#U Image data
pointer	bp			#U Bad pixel data

bool	overlap
int	nl1, nl2, loff, l2
int	nc1, nc2, nc3, off1, off2, off3, c1, c2
pointer	imgl2r(), imgl2i()


begin
	# Get data for first image.  Use IMIO buffers except the
	# bad pixel buffer is not used if there is a second image.

	imdata[1] = imgl2r (im[1], line)
	if (bpm[1] != NULL) {
	    if (im[2] == NULL)
		bp = imgl2i (bpm[1], line)
	    else
		call amovi (Memi[imgl2i(bpm[1],line)], Memi[bp],
		    IM_LEN(bpm[1],1))
	}
	if (im[2] == NULL)
	    return

	# Initialize.
	if (line == 1) {
	    nc1 = IM_LEN(im[1],1)
	    nc2 = IM_LEN(im[2],1)
	    nl1 = IM_LEN(im[1],2)
	    nl2 = IM_LEN(im[2],2)

	    overlap = true
	    if (1-offset[1] < 1 || nc1-offset[1] > nc2)
		overlap = false
	    if (1-offset[2] < 1 || nl1-offset[2] > nl2)
		overlap = false

	    off2 = -offset[1]
	    c1 = max (1, 1+off2)
	    c2 = min (nc2, nc1+off2)
	    nc2 = c2 - c1 + 1
	    off1 = c1 - off2 - 1
	    off3 = c2 - off2
	    off2 = max (0, off2)
	    nc3 = nc1 - off3
	    if (off1 > 0) {
		call aclrr (Memr[imdata[2]], off1)
		if (bpm[1] == NULL)
		    call amovki (1, Memi[bp], off1)
	    }
	    if (nc3 > 0) {
		call aclrr (Memr[imdata[2]+off3], nc3)
		if (bpm[1] == NULL)
		    call amovki (1, Memi[bp+off3], nc3)
	    }

	    loff = -offset[2]
	    if (loff < 0)
		call aclrr (Memr[imdata[2]], nc1)
	}

	l2 = line + loff
	if (l2 < 1 || l2 > nl2) {
	    call amovki (1, Memi[bp], nc1)
	    return
	}

	if (overlap) {
	    imdata[2] = imgl2r (im[2], l2) + off2
	    if (bpm[1] != NULL && bpm[2] != NULL)
		call amaxi (Memi[imgl2i(bpm[2],l2)+off2], Memi[bp], Memi[bp],
		    nc1)
	    else if (bpm[2] != NULL)
		call amovi (Memi[imgl2i(bpm[2],l2)+off2], Memi[bp], nc1)
	} else {
	    # Copy the overlapping parts of the second image to the output
	    # buffers which must be allocated externally.  Use the bad pixel
	    # mask to flag regions where there is no overlap.

	    call amovr (Memr[imgl2r(im[2],l2)+off2], Memr[imdata[2]+off1], nc2)
	    if (bpm[1] != NULL && bpm[2] != NULL) {
		call amaxi (Memi[imgl2i(bpm[2],l2)+off2], Memi[bp+off1],
		    Memi[bp+off1], nc2)
		if (off1 > 0)
		    call amovki (1, Memi[bp], off1)
		if (nc3 > 0)
		    call amovki (1, Memi[bp+off3], nc3)
	    } else if (bpm[2] != NULL)
		call amovi (Memi[imgl2i(bpm[2],l2)+off2], Memi[bp+off1], nc2)
	}
end


procedure cnvgline2 (line, offset, im, skymap, sigmap, expmap,
	skydata, sigdata, expdata)

int	line			#I Line to be read
int	offset[2]		#I Offsets
pointer	im[2]			#I Image pointers
pointer	skymap[2]		#I Sky map
pointer	sigmap[2]		#I Sky sigma map
pointer	expmap[2]		#I Exposure map
pointer	skydata[2]		#U Sky data
pointer	sigdata[2]		#U Sky sigma data
pointer	expdata[2]		#U Exposure map data

bool	overlap
int	nl1, nl2, loff, l2
int	nc1, nc2, nc3, off1, off2, off3, c1, c2
pointer	ptr

pointer	map_glr()
errchk	map_glr

begin
	# Get data for first image.

	skydata[1] = map_glr (skymap[1], line, READ_ONLY)
	if (expmap[1] == NULL)
	    sigdata[1] = map_glr (sigmap[1], line, READ_ONLY)
	else {
	    sigdata[1] = map_glr (sigmap[1], line, READ_WRITE)
	    expdata[1] = map_glr (expmap[1], line, READ_ONLY)
	    call expsigma (Memr[sigdata[1]], Memr[expdata[1]],
		IM_LEN(im[1],1), 0)
	}
	if (im[2] == NULL)
	    return

	# Initialize.
	if (line == 1) {
	    nc1 = IM_LEN(im[1],1)
	    nc2 = IM_LEN(im[2],1)
	    nl1 = IM_LEN(im[1],2)
	    nl2 = IM_LEN(im[2],2)

	    overlap = true
	    if (1-offset[1] < 1 || nc1-offset[1] > nc2)
		overlap = false
	    if (1-offset[2] < 1 || nl1-offset[2] > nl2)
		overlap = false

	    off2 = -offset[1]
	    c1 = max (1, 1+off2)
	    c2 = min (nc2, nc1+off2)
	    nc2 = c2 - c1 + 1
	    off1 = c1 - off2 - 1
	    off3 = c2 - off2
	    nc3 = nc1 - off3
	    if (off1 > 0) {
		call aclrr (Memr[skydata[2]], off1)
		call aclrr (Memr[sigdata[2]], off1)
		if (expmap[2] != NULL)
		    call aclrr (Memr[expdata[2]], off1)
	    }
	    if (nc3 > 0) {
		call aclrr (Memr[skydata[2]+off3], nc3)
		call aclrr (Memr[sigdata[2]+off3], nc3)
		if (expmap[2] != NULL)
		    call aclrr (Memr[expdata[2]+off3], nc3)
	    }

	    loff = -offset[2]
	    if (loff < 0) {
		call aclrr (Memr[skydata[2]], nc1)
		call aclrr (Memr[sigdata[2]], nc1)
		if (expmap[2] != NULL)
		    call aclrr (Memr[expdata[2]], nc1)
	    }
	}

	l2 = line + loff
	if (l2 < 1 || l2 > nl2)
	    return

	if (overlap) {
	    skydata[2] = map_glr (skymap[2], l2, READ_ONLY) + off2
	    if (expmap[2] == NULL)
		sigdata[2] = map_glr (sigmap[2], l2, READ_ONLY) + off2
	    else {
		sigdata[2] = map_glr (sigmap[2], l2, READ_WRITE) + off2
		expdata[2] = map_glr (expmap[2], l2, READ_ONLY) + off2
		call expsigma (Memr[sigdata[2]], Memr[expdata[2]], nc2, 0)
	    }
	} else {
	    # Copy the overlapping parts of the second image to the output
	    # buffers which must be allocated externally.

	    ptr = map_glr(skymap[2],l2,READ_ONLY)
	    call amovr (Memr[ptr+off2], Memr[skydata[2]+off1], nc2)
	    ptr = map_glr(sigmap[2],l2,READ_ONLY)
	    call amovr (Memr[ptr+off2], Memr[sigdata[2]+off1], nc2)
	    if (expmap[2] != NULL) {
		ptr = map_glr(expmap[2],l2,READ_ONLY)
		call amovr (Memr[ptr+off2], Memr[expdata[2]+off1], nc2)
		call expsigma (Memr[sigdata[2]], Memr[expdata[2]], nc2, 0)
	    }
	}
end


# CNVPARSE -- Parse convolution string.

procedure cnvparse (cnvstr, kernel, nx, ny, logfd)

char	cnvstr[ARB]		#I Convolution string
pointer	kernel			#O Pointer to convolution kernel elements
int	nx, ny			#O Convolution size
int	logfd			#I Log file descriptor

int	i, j, nx2, ny2
int	ip, fd, open(), fscan(), nscan(), ctor(), ctoi(), strncmp()
real	val, sx, sy
pointer	ptr
errchk	open

define	unknown_	10

begin
	kernel = NULL

	for (ip=1; IS_WHITE(cnvstr[ip]); ip=ip+1)
	    ;

	if (cnvstr[ip] == EOS) {
	    nx = 1
	    ny = 1
	    call malloc (kernel, 1, TY_REAL)
	    Memr[kernel] = 1
	} else if (cnvstr[ip] == '@') {
	    fd = open (cnvstr[ip+1], READ_ONLY, TEXT_FILE)
	    call malloc (kernel, 100, TY_REAL)
	    i = 0
	    nx = 0
	    ny = 0
	    while (fscan (fd) != EOF) {
		do j = 1, ARB {
		    call gargr (val)
		    if (nscan() < j)
			break
		    Memr[kernel+i] = val
		    i = i + 1
		    if (mod (i, 100) == 0)
			call realloc (kernel, i+100, TY_REAL)
		}
		j = j - 1
		if (nx == 0)
		    nx = j
		else if (j != nx) {
		    call close (fd)
		    call error (1,
			"Number of convolution elements inconsistent")
		}
		ny = ny + 1
	    }
	    call close (fd)
	} else if (IS_ALPHA(cnvstr[ip])) {
	    if (strncmp ("block", cnvstr[ip], 5) == 0) {
		i = 6
		if (ctoi (cnvstr[ip], i, nx) == 0 ||
		    ctoi (cnvstr[ip], i, ny) == 0)
		    goto unknown_
		call malloc (kernel, nx*ny, TY_REAL)
		call amovkr (1., Memr[kernel], nx*ny)
	    } else if (strncmp ("bilinear", cnvstr[ip], 8) == 0) {
		i = 9
		if (ctoi (cnvstr[ip], i, nx) == 0 ||
		    ctoi (cnvstr[ip], i, ny) == 0)
		    goto unknown_
		call malloc (kernel, nx*ny, TY_REAL)

		nx2 = nx / 2
		ny2 = ny / 2
		ptr = kernel
		do j = 0, ny-1 {
		    do i = 0, nx-1 {
			Memr[ptr] = (nx2-abs(nx2-i)+1) * (ny2-abs(ny2-j)+1)
			ptr = ptr + 1
		    }
		}
	    } else if (strncmp ("gauss", cnvstr[ip], 5) == 0) {
		i = 6
		if (ctoi (cnvstr[ip], i, nx) == 0 ||
		    ctoi (cnvstr[ip], i, ny) == 0)
		    goto unknown_
		if (ctor (cnvstr[ip], i, sx) == 0 ||
		    ctor (cnvstr[ip], i, sy) == 0)
		    goto unknown_
		call malloc (kernel, nx*ny, TY_REAL)

		nx2 = nx / 2
		ny2 = ny / 2
		val = 2 * sx * sy
		ptr = kernel
		do j = 0, ny-1 {
		    do i = 0, nx-1 {
			Memr[ptr] = exp (-((i-nx2)**2+(j-ny2)**2) / val)
			ptr = ptr + 1
		    }
		}
	    }
	} else {
	    call malloc (kernel, 100, TY_REAL)
	    i = 0
	    nx = 0
	    ny = 0
	    while (cnvstr[ip] != EOS) {
		do j = 1, ARB {
		    if (ctor (cnvstr, ip, val) == 0)
			break
		    Memr[kernel+i] = val
		    i = i + 1
		    if (mod (i, 100) == 0)
			call realloc (kernel, i+100, TY_REAL)
		}
		j = j - 1
		if (nx == 0)
		    nx = j
		else if (j != nx)
		    call error (1,
			"Number of convolution elements inconsistent")
		ny = ny + 1
		if (cnvstr[ip] != EOS)
		    ip = ip + 1
		for (; IS_WHITE(cnvstr[ip]); ip=ip+1)
		    ;
	    }
	}

	if (kernel == NULL)
unknown_    call error (1, "Unrecognized convolution")

	if (mod (nx, 2) != 1 || mod (ny, 2) != 1) {
	    call mfree (kernel, TY_REAL)
	    call error (1, "Convolution size must be odd")
	}

	if (logfd != NULL) {
	    ptr = kernel
	    call eprintf ("    Convolution:\n")
	    do j = 1, ny {
		call eprintf ("     ")
		do i = 1, nx {
		    call eprintf (" %7.3g")
			call pargr (Memr[ptr])
		    ptr = ptr + 1
		}
		call eprintf ("\n")
	    }
	}

end
