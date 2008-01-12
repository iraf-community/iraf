include <imhdr.h>

# PLT_GETDATA -- Decrease resolution of image by either subsampling
# or block averaging.  A pointer to the data values to be plotted is
# returned, as well as the number of columns and lines in the data matrix.

pointer procedure plt_getdata (im, sub, pre, xres, yres, nx, ny)

pointer	im			# Pointer to image header
bool	sub			# Subsample versus block average (yes/no)?
bool	pre			# Preserve aspect ratio (yes/no)?
int	xres, yres		# desired resolution
int	nx, ny			# dimensions of output array

int	nxin, nyin
pointer subras, data
pointer plt_blkaverage(), plt_subsample(), imgs2r()
errchk	plt_blkaverage, plt_subsample, calloc

begin
	# First, determine if the image resolution is to be decreased or not.
	nxin = IM_LEN(im,1)
	nyin = IM_LEN(im,2)

	if ((nxin > xres && xres != 0) || (nyin > yres && yres != 0)) {
	    # Need to decrease resolution; image can be either subsampled
	    # or block averaged.

	    if (sub) {
	        data = plt_subsample (im, xres, yres, pre, nx, ny)
		return (data)
	    } else { 
	        data = plt_blkaverage (im, xres, yres, pre, nx, ny)
		return (data)
	    }

	} else {
	    # Return entire image as data matrix.
	    nx = nxin
	    ny = nyin
	    call calloc (data, nx * ny, TY_REAL)
	    subras = imgs2r (im, 1, nxin, 1, nyin)
	    call amovr (Memr[subras], Memr[data], nx * ny)
	    return (data)
	}
end


# PLT_SUBSAMPLE -- The input data array is subsampled by the specified
# factors.  The reduced data array and its dimensions are returned.

pointer procedure plt_subsample (im, xres, yres, pre, nxout, nyout)

pointer	im			# input image
int	xres, yres		# desired output resolution
bool	pre			# preserve aspect ratio?
int	nxout, nyout		# dimensions of output array (returned)

pointer	sp, xvec, data
int	x_factor, y_factor, xrf, yrf
int	nxin, nyin, yin, ii, jj, index, nop
errchk	imgl2r, calloc
pointer	imgl2r()

begin
	call smark (sp)

	nxin = IM_LEN(im,1)
	nyin = IM_LEN(im,2)

	# User could have disabled subsampling in x or y, but not both.
	if (xres == 0)
	    xres = nxin
	if (yres == 0)
	    yres = nyin

	x_factor = (nxin + xres - 1) / xres
	y_factor = (nyin + yres - 1) / yres
	if (pre) {
	    xrf = max (x_factor, y_factor)
	    yrf = max (x_factor, y_factor)
	} else {
	    xrf = x_factor
	    yrf = y_factor
	}

	nxout = nxin / xrf
	nyout = nyin / yrf

	call eprintf ("Image will be subsampled by %d in x and %d in y\n")
	    call pargi (xrf)
	    call pargi (yrf)

	call salloc (xvec, nxin, TY_REAL)
	call calloc (data, nxout * nyout, TY_REAL)

	yin = 1
	do jj = 1, nyout {
	    call amovr (Memr[imgl2r (im, yin)], Memr[xvec],  nxin)
	    nop = 1
	    do ii = 1, nxin {
		index = data + ((jj-1) * nxout) + nop - 1
		if (mod (ii-1, xrf) == 0) {
		    Memr[index] = Memr[xvec+ii-1]
		    nop = nop + 1
		}
	    }
	    yin = yin + yrf
	}

	call sfree (sp)
	return (data)
end


# PLT_BLKAVERAGE -- Block average the input image by the specified reduction
# factors.  The reduced array and its dimensions are returned.

pointer procedure plt_blkaverage (im, xres, yres, pre, nx, ny)

pointer	im			# input image
int	xres, yres		# blocking factors
bool	pre			# preserve aspect ratio?
int	nx, ny			# dimensions of output array (returned)

real	sum
pointer	sp, xvec, data
int	nxin, nyin, nxout, nyout, nxout_full, nyout_full
int	jj, ii, index, nxcols, yin, nxrows, bfx, bfy, x_factor, y_factor
errchk 	abavr, aclrr, aaddr, salloc, calloc, imgl2r
pointer	imgl2r()

begin
	call smark (sp)

	nxin = IM_LEN(im,1)
	nyin = IM_LEN(im,2)

	# User could have disabled blockaveraging in x or y, but not both.
	if (xres == 0)
	    xres = nxin
	if (yres == 0)
	    yres = nyin

	bfx = nxin / xres
	bfy = nyin / yres
	if (pre) {
	    x_factor = max (bfx, bfy)
	    y_factor = max (bfx, bfy)
	} else{
	    x_factor = bfx
	    y_factor = bfy
	}

	call eprintf ("Image will be block averaged by %d in x and %d in y\n")
	    call pargi (x_factor)
	    call pargi (y_factor)

	nxout = (nxin + x_factor - 1) / x_factor
	nyout = (nyin + y_factor - 1) / y_factor
	nxout_full = nxin / x_factor
	nyout_full = nyin / y_factor
	nxcols = nxin - (nxout_full * x_factor)
	nxrows = nyin - (nyout_full * y_factor)

	call salloc (xvec, nxin, TY_REAL)
	call calloc (data, nxout * nyout, TY_REAL)

	yin = 1
	do jj = 1, nyout_full {
	    call aclrr (Memr[xvec], nxin)
	    do ii = 1, y_factor {
		call aaddr (Memr[imgl2r(im,yin)], Memr[xvec], Memr[xvec], nxin)
		yin = yin + 1
	    }
	    call adivkr (Memr[xvec], real(y_factor), Memr[xvec], nxin)

	    index = data + (jj-1) * nxout 
	    call abavr (Memr[xvec], Memr[index], nxout_full, x_factor) 

	    if (nxcols != 0) {
		sum = 0.0
		# deal with trailing column pixels
		do ii = 1, nxcols
		    sum = sum + Memr[xvec+(nxout_full*x_factor)+ii-1]
		Memr[index+nxout_full] = sum / real (nxcols)
	    }

	}

	if (nxrows != 0) {
	    call aclrr (Memr[xvec], nxin)
	    do ii = yin, nyin 
		call aaddr (Memr[imgl2r(im,ii)], Memr[xvec], Memr[xvec], nxin)
	    call adivkr (Memr[xvec], real (nxrows), Memr[xvec], nxin)

	    index = data + (nyout - 1) * nxout
	    call abavr (Memr[xvec], Memr[index], nxout_full, x_factor)

	    if (nxcols != 0) {
	        # Deal with trailing column pixels.
	        do ii = 1, nxcols
		    sum = sum + Memr[xvec+(nxout_full*x_factor)+ii-1]
		Memr[index+nxout_full] = sum / real (nxcols)
	    }
	}

	nx = nxout
	ny = nyout
	call sfree (sp)
	return (data)
end
