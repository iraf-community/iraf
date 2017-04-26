# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>

.help sigl2, sigl2_setup
.nf ___________________________________________________________________________
SIGL2 -- Get a line from a spatially scaled 2-dimensional image.  This procedure
works like the regular IMIO get line procedure, but rescales the input
2-dimensional image in either or both axes upon input.  If the magnification
ratio required is greater than 0 and less than 2 then linear interpolation is
used to resample the image.  If the magnification ratio is greater than or
equal to 2 then the image is block averaged by the smallest factor which
reduces the magnification to the range 0-2 and then interpolated back up to
the desired size.  In some cases this will smooth the data slightly, but the
operation is efficient and avoids aliasing effects.

	si =	sigl2_setup (im, x1,x2,nx, y1,y2,ny)
		sigl2_free (si)
	ptr =	sigl2[sr] (si, linenumber)

SIGL2_SETUP must be called to set up the transformations after mapping the
image and before performing any scaled i/o to the image.  SIGL2_FREE must be
called when finished to return buffer space.
.endhelp ______________________________________________________________________

# Scaled image descriptor for 2-dim images

define	SI_LEN		15
define	SI_MAXDIM	2		# images of 2 dimensions supported
define	SI_NBUFS	3		# nbuffers used by SIGL2

define	SI_IM		Memi[$1]	# pointer to input image header
define	SI_GRID		Memi[$1+1+$2-1]	# pointer to array of X coords
define	SI_NPIX		Memi[$1+3+$2-1]	# number of X coords
define	SI_BAVG		Memi[$1+5+$2-1]	# X block averaging factor
define	SI_INTERP	Memi[$1+7+$2-1]	# interpolate X axis
define	SI_BUF		Memi[$1+9+$2-1]	# line buffers
define	SI_TYBUF	Memi[$1+12]	# buffer type
define	SI_XOFF		Memi[$1+13]	# offset in input image to first X
define	SI_INIT		Memi[$1+14]	# YES until first i/o is done

define	OUTBUF		SI_BUF($1,3)

define	SI_TOL		(1E-5)		# close to a pixel
define	INTVAL		(abs ($1 - nint($1)) < SI_TOL)
define	SWAPI		{tempi=$2;$2=$1;$1=tempi}
define	SWAPP		{tempp=$2;$2=$1;$1=tempp}
define	NOTSET		(-9999)

# SIGL2_SETUP -- Set up the spatial transformation for SIGL2[SR].  Compute
# the block averaging factors (1 if no block averaging is required) and
# the sampling grid points, i.e., pixel coordinates of the output pixels in
# the input image.
#
# Valdes - Jan 9, 1985:
#    Nx or ny can be 1 and blocking factors can be specified.

pointer procedure sigl2_setup (im, px1, px2, nx, xblk, py1, py2, ny, yblk)

pointer	im			# the input image
real	px1, px2		# range in X to be sampled on an even grid
int	nx			# number of output pixels in X
int	xblk			# blocking factor in x
real	py1, py2		# range in Y to be sampled on an even grid
int	ny			# number of output pixels in Y
int	yblk			# blocking factor in y

int	npix, noldpix, nbavpix, i, j
int	npts[SI_MAXDIM]		# number of output points for axis
int	blksize[SI_MAXDIM]	# block averaging factor (npix per block)
real	tau[SI_MAXDIM]		# tau = p(i+1) - p(i) in fractional pixels
real	p1[SI_MAXDIM]		# starting pixel coords in each axis
real	p2[SI_MAXDIM]		# ending pixel coords in each axis
real	scalar, start
pointer	si, gp

begin
	iferr (call calloc (si, SI_LEN, TY_STRUCT))
	    call erract (EA_FATAL)

	SI_IM(si) = im
	SI_NPIX(si,1) = nx
	SI_NPIX(si,2) = ny
	SI_INIT(si) = YES

	p1[1] = px1			# X = index 1
	p2[1] = px2
	npts[1] = nx
	blksize[1] = xblk

	p1[2] = py1			# Y = index 2
	p2[2] = py2
	npts[2] = ny
	blksize[2] = yblk

	# Compute block averaging factors if not defined.
	# If there is only one pixel then the block average is the average
	# between the first and last point.

	do i = 1, SI_MAXDIM {
	    if ((blksize[i] >= 1) && !IS_INDEFI (blksize[i])) {
	        if (npts[i] == 1)
		    tau[i] = 0.
	        else
	            tau[i] = (p2[i] - p1[i]) / (npts[i] - 1)
	    } else {
		if (npts[i] == 1) {
		    tau[i] = 0.
		    blksize[i] = int (p2[i] - p1[i] + 1)
	        } else {
	            tau[i] = (p2[i] - p1[i]) / (npts[i] - 1)
	    	    if (tau[i] >= 2.0) {

			# If nx or ny is not an integral multiple of the block
			# averaging factor, noldpix is the next larger number
			# which is an integral multiple.  When the image is
			# block averaged pixels will be replicated as necessary
			# to fill the last block out to this size.  

			blksize[i] = int (tau[i])
			npix = p2[i] - p1[i] + 1
			noldpix = (npix+blksize[i]-1) / blksize[i] * blksize[i]
			nbavpix = noldpix / blksize[i]
			scalar = real (nbavpix - 1) / real (noldpix - 1)
			p1[i] = (p1[i] - 1.0) * scalar + 1.0
			p2[i] = (p2[i] - 1.0) * scalar + 1.0
			tau[i] = (p2[i] - p1[i]) / (npts[i] - 1)
		    } else
			blksize[i] = 1
		}
	    }
	}

	SI_BAVG(si,1) = blksize[1]
	SI_BAVG(si,2) = blksize[2]

	if (IS_INDEFI (xblk))
	    xblk = blksize[1]
	if (IS_INDEFI (yblk))
	    yblk = blksize[2]

	# Allocate and initialize the grid arrays, specifying the X and Y
	# coordinates of each pixel in the output image, in units of pixels
	# in the input (possibly block averaged) image.

	do i = 1, SI_MAXDIM {
	    # The X coordinate is special.  We do not want to read entire
	    # input image lines if only a range of input X values are needed.
	    # Since the X grid vector passed to ALUI (the interpolator) must
	    # contain explicit offsets into the vector being interpolated,
	    # we must generate interpolator grid points starting near 1.0.
	    # The X origin, used to read the block averaged input line, is
	    # given by XOFF.

	    if (i == 1) {
		SI_XOFF(si) = int (p1[i])
		start = p1[1] - int (p1[i]) + 1.0
	    } else
		start = p1[i]

	    # Do the axes need to be interpolated?
	    if (INTVAL(start) && INTVAL(tau[i]))
		SI_INTERP(si,i) = NO
	    else
		SI_INTERP(si,i) = YES

	    # Allocate grid buffer and set the grid points.
	    iferr (call malloc (gp, npts[i], TY_REAL))
		call erract (EA_FATAL)
	    SI_GRID(si,i) = gp
	    do j = 0, npts[i]-1
		Memr[gp+j] = start + (j * tau[i])
	}

	return (si)
end


# SIGL2_FREE -- Free storage associated with an image opened for scaled
# input.  This does not close and unmap the image.

procedure sigl2_free (si)

pointer	si
int	i

begin
	# Free SIGL2 buffers.
	do i = 1, SI_NBUFS
	    if (SI_BUF(si,i) != NULL)
		call mfree (SI_BUF(si,i), SI_TYBUF(si))

	# Free GRID buffers.
	do i = 1, SI_MAXDIM
	    if (SI_GRID(si,i) != NULL)
		call mfree (SI_GRID(si,i), TY_REAL)

	call mfree (si, TY_STRUCT)
end


# SIGL2S -- Get a line of type short from a scaled image.  Block averaging is
# done by a subprocedure; this procedure gets a line from a possibly block
# averaged image and if necessary interpolates it to the grid points of the
# output line.

pointer procedure sigl2s (si, lineno)

pointer	si		# pointer to SI descriptor
int	lineno

pointer	rawline, tempp, gp
int	i, buf_y[2], new_y[2], tempi, curbuf, altbuf
int	npix, nblks_y, ybavg, x1, x2
real	x, y, weight_1, weight_2
pointer	si_blkavgs()
errchk	si_blkavgs

begin
	npix = SI_NPIX(si,1)

	# Determine the range of X (in pixels on the block averaged input image)
	# required for the interpolator.

	gp = SI_GRID(si,1)
	x1 = SI_XOFF(si)
	x = Memr[gp+npix-1]
	x2 = x1 + int(x)
	if (INTVAL(x))
	    x2 = x2 - 1
	x2 = max (x1 + 1, x2)

	gp = SI_GRID(si,2)
	y = Memr[gp+lineno-1]

	# The following is an optimization provided for the case when it is
	# not necessary to interpolate in either X or Y.  Block averaging is
	# permitted.

	if (SI_INTERP(si,1) == NO && SI_INTERP(si,2) == NO)
	    return (si_blkavgs (SI_IM(si), x1, x2, int(y),
		SI_BAVG(si,1), SI_BAVG(si,2)))

	# If we are interpolating in Y two buffers are required, one for each
	# of the two input image lines required to interpolate in Y.  The lines
	# stored in these buffers are interpolated in X to the output grid but
	# not in Y.  Both buffers are not required if we are not interpolating
	# in Y, but we use them anyhow to simplify the code.

	if (SI_INIT(si) == YES) {
	    do i = 1, 2 {
		if (SI_BUF(si,i) != NULL)
		    call mfree (SI_BUF(si,i), SI_TYBUF(si))
		call malloc (SI_BUF(si,i), npix, TY_SHORT)
		SI_TYBUF(si) = TY_SHORT
		buf_y[i] = NOTSET
	    }
	    if (OUTBUF(si) != NULL)
		call mfree (OUTBUF(si), SI_TYBUF(si))
	    call malloc (OUTBUF(si), npix, TY_SHORT)
	    SI_INIT(si) = NO
	}

	# If the Y value of the new line is not in range of the contents of the
	# current line buffers, refill one or both buffers.  To refill we must
	# read a (possibly block averaged) input line and interpolate it onto
	# the X grid.  The X and Y values herein are in the coordinate system
	# of the (possibly block averaged) input image.

	new_y[1] = int(y)
	new_y[2] = int(y) + 1

	# Get the pair of lines whose integral Y values form an interval
	# containing the fractional Y value of the output line.  Sometimes the
	# desired line will happen to be in the other buffer already, in which
	# case we just have to swap buffers.  Often the new line will be the
	# current line, in which case nothing is done.  This latter case occurs
	# frequently when the magnification ratio is large.

	curbuf = 1
	altbuf = 2

	do i = 1, 2 {
	    if (new_y[i] == buf_y[i]) {
		;
	    } else if (new_y[i] == buf_y[altbuf]) {
		SWAPP (SI_BUF(si,1), SI_BUF(si,2))
		SWAPI (buf_y[1], buf_y[2])

	    } else {
		# Get line and interpolate onto output grid.  If interpolation
		# is not required merely copy data out.  This code is set up
		# to always use two buffers; in effect, there is one buffer of
		# look ahead, even when Y[i] is integral.  This means that we
		# will go out of bounds by one line at the top of the image.
		# This is handled by copying the last line.

		ybavg = SI_BAVG(si,2)
		nblks_y = (IM_LEN (SI_IM(si), 2) + ybavg-1) / ybavg
		if (new_y[i] <= nblks_y)
		    rawline = si_blkavgs (SI_IM(si), x1, x2, new_y[i],
			SI_BAVG(si,1), SI_BAVG(si,2))

		if (SI_INTERP(si,1) == NO)
		    call amovs (Mems[rawline], Mems[SI_BUF(si,i)], npix)
		else {
		    call aluis (Mems[rawline], Mems[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		}

		buf_y[i] = new_y[i]
	    }

	    SWAPI (altbuf, curbuf)
	}

	# We now have two line buffers straddling the output Y value,
	# interpolated to the X grid of the output line.  To complete the
	# bilinear interpolation operation we take a weighted sum of the two
	# lines.  If the range from buf_y[1] to buf_y[2] is repeatedly
	# interpolated in Y no additional i/o occurs and the linear
	# interpolation operation (ALUI) does not have to be repeated (only the
	# weighted sum is required).  If the distance of Y from one of the
	# buffers is zero then we do not even have to take a weighted sum.
	# This is not unusual because we may be called with a magnification
	# of 1.0 in Y.

	weight_1 = 1.0 - (y - buf_y[1])
	weight_2 = 1.0 - weight_1

	if (weight_2 < SI_TOL) 
	    return (SI_BUF(si,1))
	else if (weight_1 < SI_TOL)
	    return (SI_BUF(si,2))
	else {
	    call awsus (Mems[SI_BUF(si,1)], Mems[SI_BUF(si,2)],
		Mems[OUTBUF(si)], npix, weight_1, weight_2)
	    return (OUTBUF(si))
	}
end


# SI_BLKAVGS -- Get a line from a block averaged image of type short.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  If the length of an axis
# is not an integral multiple of the block size then the last pixel in the
# last block will be replicated to fill out the block; the average is still
# defined even if a block is not full.

pointer procedure si_blkavgs (im, x1, x2, y, xbavg, ybavg)

pointer	im			# input image
int	x1, x2			# range of x blocks to be read
int	y			# y block to be read
int	xbavg, ybavg		# X and Y block averaging factors

short	temp_s
int	nblks_x, nblks_y, ncols, nlines, xoff, i, j
int	first_line, nlines_in_sum, npix, nfull_blks, count
real	sum
pointer	sp, a, b
pointer	imgs2s()
errchk	imgs2s

begin
	call smark (sp)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xoff   = (x1 - 1) * xbavg + 1
	npix   = min (ncols, xoff + (x2 - x1 + 1) * xbavg - 1)

	if ((xbavg < 1) || (ybavg < 1))
	    call error (1, "si_blkavg: illegal block size")
	else if (x1 < 1 || x2 > ncols)
	    call error (2, "si_blkavg: column index out of bounds")
	else if ((xbavg == 1) && (ybavg == 1))
	    return (imgs2s (im, xoff, xoff + npix - 1, y, y))

	nblks_x = (npix   + xbavg-1) / xbavg
	nblks_y = (nlines + ybavg-1) / ybavg

	if (y < 1 || y > nblks_y)
	    call error (2, "si_blkavg: block number out of range")

	call salloc (b, nblks_x, TY_SHORT)

	if (ybavg > 1) {
	    call aclrs (Mems[b], nblks_x)
	    nlines_in_sum = 0
	}

	# Read and accumulate all input lines in the block.
	first_line = (y - 1) * ybavg + 1

	do i = first_line, min (nlines, first_line + ybavg - 1) {
	    # Get line from input image.
	    a = imgs2s (im, xoff, xoff + npix - 1, i, i)

	    # Block average line in X.
	    if (xbavg > 1) {
		# First block average only the full blocks.
		nfull_blks = npix / xbavg
		call abavs (Mems[a], Mems[a], nfull_blks, xbavg)

		# Now average the final partial block, if any.
		if (nfull_blks < nblks_x) {
		    sum = 0.0
		    count = 0
		    do j = nfull_blks * xbavg + 1, npix {
			sum = sum + Mems[a+j-1]
			count = count + 1
		    }
		    Mems[a+nblks_x-1] = sum / count
		}
	    }

	    # Add line into block sum.  Keep track of number of lines in sum
	    # so that we can compute block average later.
	    if (ybavg > 1) {
		call aadds (Mems[a], Mems[b], Mems[b], nblks_x)
		nlines_in_sum = nlines_in_sum + 1
	    }
	}

	# Compute the block average in Y from the sum of all lines block
	# averaged in X.  Overwrite buffer A, the buffer returned by IMIO.
	# This is kosher because the block averaged line is never longer
	# than an input line.

	if (ybavg > 1) {
	    temp_s = nlines_in_sum
	    call adivks (Mems[b], temp_s, Mems[a], nblks_x)
	}

	call sfree (sp)
	return (a)
end


# SIGL2R -- Get a line of type real from a scaled image.  Block averaging is
# done by a subprocedure; this procedure gets a line from a possibly block
# averaged image and if necessary interpolates it to the grid points of the
# output line.

pointer procedure sigl2r (si, lineno)

pointer	si		# pointer to SI descriptor
int	lineno

pointer	rawline, tempp, gp
int	i, buf_y[2], new_y[2], tempi, curbuf, altbuf
int	npix, nblks_y, ybavg, x1, x2
real	x, y, weight_1, weight_2
pointer	si_blkavgr()
errchk	si_blkavgr

begin
	npix = SI_NPIX(si,1)

	# Deterine the range of X (in pixels on the block averaged input image)
	# required for the interpolator.

	gp = SI_GRID(si,1)
	x1 = SI_XOFF(si)
	x = Memr[gp+npix-1]
	x2 = x1 + int(x)
	if (INTVAL(x))
	    x2 = x2 - 1
	x2 = max (x1 + 1, x2)

	gp = SI_GRID(si,2)
	y = Memr[gp+lineno-1]

	# The following is an optimization provided for the case when it is
	# not necessary to interpolate in either X or Y.  Block averaging is
	# permitted.

	if (SI_INTERP(si,1) == NO && SI_INTERP(si,2) == NO)
	    return (si_blkavgr (SI_IM(si), x1, x2, int(y),
		SI_BAVG(si,1), SI_BAVG(si,2)))

	# If we are interpolating in Y two buffers are required, one for each
	# of the two input image lines required to interpolate in Y.  The lines
	# stored in these buffers are interpolated in X to the output grid but
	# not in Y.  Both buffers are not required if we are not interpolating
	# in Y, but we use them anyhow to simplify the code.

	if (SI_INIT(si) == YES) {
	    do i = 1, 2 {
		if (SI_BUF(si,i) != NULL)
		    call mfree (SI_BUF(si,i), SI_TYBUF(si))
		call malloc (SI_BUF(si,i), npix, TY_REAL)
		SI_TYBUF(si) = TY_REAL
		buf_y[i] = NOTSET
	    }
	    if (OUTBUF(si) != NULL)
		call mfree (OUTBUF(si), SI_TYBUF(si))
	    call malloc (OUTBUF(si), npix, TY_REAL)
	    SI_INIT(si) = NO
	}

	# If the Y value of the new line is not in range of the contents of the
	# current line buffers, refill one or both buffers.  To refill we must
	# read a (possibly block averaged) input line and interpolate it onto
	# the X grid.  The X and Y values herein are in the coordinate system
	# of the (possibly block averaged) input image.

	new_y[1] = int(y)
	new_y[2] = int(y) + 1

	# Get the pair of lines whose integral Y values form an interval
	# containing the fractional Y value of the output line.  Sometimes the
	# desired line will happen to be in the other buffer already, in which
	# case we just have to swap buffers.  Often the new line will be the
	# current line, in which case nothing is done.  This latter case occurs
	# frequently when the magnification ratio is large.

	curbuf = 1
	altbuf = 2

	do i = 1, 2 {
	    if (new_y[i] == buf_y[i]) {
		;
	    } else if (new_y[i] == buf_y[altbuf]) {
		SWAPP (SI_BUF(si,1), SI_BUF(si,2))
		SWAPI (buf_y[1], buf_y[2])

	    } else {
		# Get line and interpolate onto output grid.  If interpolation
		# is not required merely copy data out.  This code is set up
		# to always use two buffers; in effect, there is one buffer of
		# look ahead, even when Y[i] is integral.  This means that we
		# will go out of bounds by one line at the top of the image.
		# This is handled by copying the last line.

		ybavg = SI_BAVG(si,2)
		nblks_y = (IM_LEN (SI_IM(si), 2) + ybavg-1) / ybavg
		if (new_y[i] <= nblks_y)
		    rawline = si_blkavgr (SI_IM(si), x1, x2, new_y[i],
			SI_BAVG(si,1), SI_BAVG(si,2))

		if (SI_INTERP(si,1) == NO)
		    call amovr (Memr[rawline], Memr[SI_BUF(si,i)], npix)
		else {
		    call aluir (Memr[rawline], Memr[SI_BUF(si,i)],
			Memr[SI_GRID(si,1)], npix)
		}

		buf_y[i] = new_y[i]
	    }

	    SWAPI (altbuf, curbuf)
	}

	# We now have two line buffers straddling the output Y value,
	# interpolated to the X grid of the output line.  To complete the
	# bilinear interpolation operation we take a weighted sum of the two
	# lines.  If the range from buf_y[1] to buf_y[2] is repeatedly
	# interpolated in Y no additional i/o occurs and the linear
	# interpolation operation (ALUI) does not have to be repeated (only the
	# weighted sum is required).  If the distance of Y from one of the
	# buffers is zero then we do not even have to take a weighted sum.
	# This is not unusual because we may be called with a magnification
	# of 1.0 in Y.

	weight_1 = 1.0 - (y - buf_y[1])
	weight_2 = 1.0 - weight_1

	if (weight_2 < SI_TOL) 
	    return (SI_BUF(si,1))
	else if (weight_1 < SI_TOL)
	    return (SI_BUF(si,2))
	else {
	    call awsur (Memr[SI_BUF(si,1)], Memr[SI_BUF(si,2)],
		Memr[OUTBUF(si)], npix, weight_1, weight_2)
	    return (OUTBUF(si))
	}
end


# SI_BLKAVGR -- Get a line from a block averaged image of type short.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  If the length of an axis
# is not an integral multiple of the block size then the last pixel in the
# last block will be replicated to fill out the block; the average is still
# defined even if a block is not full.

pointer procedure si_blkavgr (im, x1, x2, y, xbavg, ybavg)

pointer	im			# input image
int	x1, x2			# range of x blocks to be read
int	y			# y block to be read
int	xbavg, ybavg		# X and Y block averaging factors

int	nblks_x, nblks_y, ncols, nlines, xoff, i, j
int	first_line, nlines_in_sum, npix, nfull_blks, count
real	sum
pointer	sp, a, b
pointer	imgs2r()
errchk	imgs2r

begin
	call smark (sp)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xoff   = (x1 - 1) * xbavg + 1
	npix   = min (ncols, xoff + (x2 - x1 + 1) * xbavg - 1)

	if ((xbavg < 1) || (ybavg < 1))
	    call error (1, "si_blkavg: illegal block size")
	else if (x1 < 1 || x2 > ncols)
	    call error (2, "si_blkavg: column index out of bounds")
	else if ((xbavg == 1) && (ybavg == 1))
	    return (imgs2r (im, xoff, xoff + npix - 1, y, y))

	nblks_x = (npix   + xbavg-1) / xbavg
	nblks_y = (nlines + ybavg-1) / ybavg

	if (y < 1 || y > nblks_y)
	    call error (2, "si_blkavg: block number out of range")

	call salloc (b, nblks_x, TY_REAL)

	if (ybavg > 1) {
	    call aclrr (Memr[b], nblks_x)
	    nlines_in_sum = 0
	}

	# Read and accumulate all input lines in the block.
	first_line = (y - 1) * ybavg + 1

	do i = first_line, min (nlines, first_line + ybavg - 1) {
	    # Get line from input image.
	    a = imgs2r (im, xoff, xoff + npix - 1, i, i)

	    # Block average line in X.
	    if (xbavg > 1) {
		# First block average only the full blocks.
		nfull_blks = npix / xbavg
		call abavr (Memr[a], Memr[a], nfull_blks, xbavg)

		# Now average the final partial block, if any.
		if (nfull_blks < nblks_x) {
		    sum = 0.0
		    count = 0
		    do j = nfull_blks * xbavg + 1, npix {
			sum = sum + Memr[a+j-1]
			count = count + 1
		    }
		    Memr[a+nblks_x-1] = sum / count
		}
	    }

	    # Add line into block sum.  Keep track of number of lines in sum
	    # so that we can compute block average later.
	    if (ybavg > 1) {
		call aaddr (Memr[a], Memr[b], Memr[b], nblks_x)
		nlines_in_sum = nlines_in_sum + 1
	    }
	}

	# Compute the block average in Y from the sum of all lines block
	# averaged in X.  Overwrite buffer A, the buffer returned by IMIO.
	# This is kosher because the block averaged line is never longer
	# than an input line.

	if (ybavg > 1)
	    call adivkr (Memr[b], real(nlines_in_sum), Memr[a], nblks_x)

	call sfree (sp)
	return (a)
end
