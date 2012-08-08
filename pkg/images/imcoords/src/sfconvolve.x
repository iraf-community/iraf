include <imset.h>
include <math.h>
include "starfind.h"


# SF_EGPARAMS -- Calculate the parameters of the elliptical Gaussian needed
# to compute the Gaussian convolution kernel.

procedure sf_egparams (sigma, ratio, theta, nsigma, a, b, c, f, nx, ny)

real	sigma			#I sigma of Gaussian in x
real	ratio			#I ratio of half-width in y to x
real	theta			#I position angle of Gaussian
real	nsigma			#I limit of convolution
real	a, b, c, f		#O ellipse parameters
int	nx, ny			#O dimensions of the kernel

real	sx2, sy2, cost, sint, discrim
bool	fp_equalr ()

begin
	# Define some temporary variables.
	sx2 = sigma ** 2
	sy2 = (ratio * sigma) ** 2
	cost = cos (DEGTORAD (theta))
	sint = sin (DEGTORAD (theta))

	# Compute the ellipse parameters.
	if (fp_equalr (ratio, 0.0)) {
	    if (fp_equalr (theta, 0.0) || fp_equalr (theta, 180.)) {
		a = 1. / sx2
		b = 0.0
		c = 0.0
	    } else if (fp_equalr (theta, 90.0)) {
		a = 0.0
		b = 0.0
		c = 1. / sx2
	    } else
		call error (0, "SF_EGPARAMS: Cannot make 1D Gaussian.")
	    f = nsigma ** 2 / 2.
	    nx = 2 * int (max (sigma * nsigma * abs (cost), RMIN)) + 1
	    ny = 2 * int (max (sigma * nsigma * abs (sint), RMIN)) + 1
	} else {
	    a = cost ** 2 / sx2 + sint ** 2 / sy2
	    b = 2. * (1.0 / sx2 - 1.0 / sy2) * cost * sint
	    c = sint ** 2 / sx2 + cost ** 2 / sy2
	    discrim = b ** 2 - 4. * a * c
	    f = nsigma ** 2 / 2.
	    nx = 2 * int (max (sqrt (-8. * c * f / discrim), RMIN)) + 1
	    ny = 2 * int (max (sqrt (-8. * a * f / discrim), RMIN)) + 1
	}
end


# SF_EGKERNEL -- Compute the non-normalized and normalized elliptical
# Gaussian kernel and the skip array.

real procedure sf_egkernel (gkernel, ngkernel, skip, nx, ny, gsums, a, b, c, f)

real	gkernel[nx,ny]		#O output Gaussian amplitude kernel
real	ngkernel[nx,ny]		#O output normalized Gaussian amplitude kernel
int	skip[nx,ny]		#O output skip subraster
int	nx, ny			#I input dimensions of the kernel
real	gsums[ARB]		#O output array of gsums
real	a, b, c, f		#I ellipse parameters

int	i, j, x0, y0, x, y
real	rjsq, rsq, relerr, ef

begin
	# Initialize.
	x0 = nx / 2 + 1
	y0 = ny / 2 + 1
	gsums[GAUSS_PIXELS] = 0.0
	gsums[GAUSS_SUMG] = 0.0
	gsums[GAUSS_SUMGSQ] = 0.0

	# Compute the kernel and principal sums.
	do j = 1, ny {
	    y = j - y0
	    rjsq = y ** 2
	    do i = 1, nx {
		x = i - x0
		rsq = sqrt (x ** 2 + rjsq)
		ef = 0.5 * (a * x ** 2 + c * y ** 2 + b * x * y)
		gkernel[i,j] = exp (-1.0 * ef)
		if (ef <= f || rsq <= RMIN) {
		    ngkernel[i,j] = gkernel[i,j]
		    gsums[GAUSS_SUMG] = gsums[GAUSS_SUMG] + gkernel[i,j]
		    gsums[GAUSS_SUMGSQ] = gsums[GAUSS_SUMGSQ] +
		        gkernel[i,j] ** 2
		    skip[i,j] = NO
		    gsums[GAUSS_PIXELS] = gsums[GAUSS_PIXELS] + 1.0
		} else {
		    ngkernel[i,j] = 0.0
		    skip[i,j] = YES
		}
	    }
	}

	# Store the remaining sums.
	gsums[GAUSS_DENOM] = gsums[GAUSS_SUMGSQ] - gsums[GAUSS_SUMG] ** 2 /
	    gsums[GAUSS_PIXELS]
	gsums[GAUSS_SGOP] = gsums[GAUSS_SUMG] / gsums[GAUSS_PIXELS]

	# Normalize the kernel.
	do j = 1, ny {
	     do i = 1, nx {
	        if (skip[i,j] == NO)
		    ngkernel[i,j] = (gkernel[i,j] - gsums[GAUSS_SGOP]) /
			gsums[GAUSS_DENOM]
	    }
	}


	relerr = 1.0 / gsums[GAUSS_DENOM]

	return (sqrt (relerr))
end


# SF_FCONVOLVE --  Solve for the density enhancements in the case where
# datamin and datamax are not defined.

procedure sf_fconvolve (im, c1, c2, l1, l2, bwidth, imbuf, denbuf, ncols,
	nlines, kernel, skip, nxk, nyk)

pointer	im			#I pointer to the input image
int	c1, c2			#I column limits in the input image
int	l1, l2			#I line limits in the input image
int	bwidth			#I width of pixel buffer
real	imbuf[ncols,nlines]	#O the output data buffer
real	denbuf[ncols,nlines]	#O the output density enhancement buffer
int	ncols, nlines		#I dimensions of the output buffers
real	kernel[nxk,nyk]		#I the convolution kernel
int	skip[nxk,nyk]		#I the skip array
int	nxk, nyk		#I dimensions of the kernel

int	i, col1, col2, inline, index, outline
pointer	sp, lineptrs
pointer	imgs2r()
errchk	imgs2r

begin
	# Set up an array of linepointers.
	call smark (sp)
	call salloc (lineptrs, nyk, TY_POINTER)

	# Set the number of image buffers.
	call imseti (im, IM_NBUFS, nyk)

	# Set input image column limits.
	col1 = c1 - nxk / 2 - bwidth
	col2 = c2 + nxk / 2 + bwidth

	# Initialise the line buffers at the same time copying the image
	# input the data buffer. 
	inline = l1 - bwidth - nyk / 2
	do index = 1 , nyk - 1 {
	    Memi[lineptrs+index] = imgs2r (im, col1, col2, inline, inline)
	    call amovr (Memr[Memi[lineptrs+index]], imbuf[1,index], ncols)
	    inline = inline + 1
	}

	# Zero the initial density enhancement buffers.
	do i = 1, nyk / 2
	    call amovkr (0.0, denbuf[1,i], ncols)

	# Generate the output image line by line.
	do outline = 1, l2 - l1 + 2 * bwidth + 1 {

	    # Scroll the input buffers.
	    do i = 1, nyk - 1
		Memi[lineptrs+i-1] = Memi[lineptrs+i]

	    # Read in new image line and copy it into the image buffer.
	    Memi[lineptrs+nyk-1] = imgs2r (im, col1, col2, inline,
	        inline)

	    # Compute the input image line into the data buffer.
	    call amovr (Memr[Memi[lineptrs+nyk-1]], imbuf[1,index], ncols)

	    # Generate first output image line.
	    call aclrr (denbuf[1,outline+nyk/2], ncols)
	    do i = 1, nyk
		call sf_skcnvr (Memr[Memi[lineptrs+i-1]],
		    denbuf[1+nxk/2,outline+nyk/2], c2 - c1 + 2 * bwidth + 1,
		        kernel[1,i], skip[1,i], nxk)

	    inline = inline + 1
	    index = index + 1
	}

	# Zero the final density enhancement buffer lines.
	do i = nlines - nyk / 2 + 1, nlines
	    call amovkr (0.0, denbuf[1,i], ncols)

	# Free the image buffer pointers.
	call sfree (sp)
end


# SF_GCONVOLVE --  Solve for the density enhancement image in the case where
# datamin and datamax are defined.

procedure sf_gconvolve (im, c1, c2, l1, l2, bwidth, imbuf, denbuf, ncols,
	nlines, kernel, skip, nxk, nyk, gsums, datamin, datamax)

pointer	im			# pointer to the input image
int	c1, c2			#I column limits in the input image
int	l1, l2			#I line limits in the input image
int	bwidth			#I width of pixel buffer
real	imbuf[ncols,nlines]	#O the output data buffer
real	denbuf[ncols,nlines]	#O the output density enhancement buffer
int	ncols, nlines		#I dimensions of the output buffers
real	kernel[nxk,nyk]		#I the first convolution kernel
int	skip[nxk,nyk]		#I the sky array
int	nxk, nyk		#I dimensions of the kernel
real	gsums[ARB]		#U array of kernel sums
real	datamin, datamax	#I the good data minimum and maximum

int	i, nc, col1, col2, inline, index, outline
pointer	sp, lineptrs, sd, sgsq, sg, p
pointer	imgs2r()
errchk	imgs2r()

begin
	# Set up an array of linepointers.
	call smark (sp)
	call salloc (lineptrs, nyk, TY_POINTER)

	# Set the number of image buffers.
	call imseti (im, IM_NBUFS, nyk)

	# Allocate some working space.
	nc = c2 - c1 + 2 * bwidth + 1
	call salloc (sd, nc, TY_REAL)
	call salloc (sgsq, nc, TY_REAL)
	call salloc (sg, nc, TY_REAL)
	call salloc (p, nc, TY_REAL)

	# Set input image column limits.
	col1 = c1 - nxk / 2 - bwidth
	col2 = c2 + nxk / 2 + bwidth

	# Initialise the line buffers.
	inline = l1 - bwidth - nyk / 2
	do index = 1 , nyk - 1 {
	    Memi[lineptrs+index] = imgs2r (im, col1, col2, inline, inline)
	    call amovr (Memr[Memi[lineptrs+index]], imbuf[1,index], ncols)
	    inline = inline + 1
	}

	# Zero the initial density enhancement buffers.
	do i = 1, nyk / 2
	    call amovkr (0.0, denbuf[1,i], ncols)

	# Generate the output image line by line.
	do outline = 1, l2 - l1 + 2 * bwidth + 1 {

	    # Scroll the input buffers.
	    do i = 1, nyk - 1
		Memi[lineptrs+i-1] = Memi[lineptrs+i]

	    # Read in new image line.
	    Memi[lineptrs+nyk-1] = imgs2r (im, col1, col2, inline,
	        inline)

	    # Compute the input image line into the data buffer.
	    call amovr (Memr[Memi[lineptrs+nyk-1]], imbuf[1,index], ncols)

	    # Generate first output image line.
	    call aclrr (denbuf[1,outline+nyk/2], ncols)
	    call aclrr (Memr[sd], nc)
	    call amovkr (gsums[GAUSS_SUMG], Memr[sg], nc)
	    call amovkr (gsums[GAUSS_SUMGSQ], Memr[sgsq], nc)
	    call amovkr (gsums[GAUSS_PIXELS], Memr[p], nc)

	    do i = 1, nyk
		call sf_gdsum (Memr[Memi[lineptrs+i-1]],
		    denbuf[1+nxk/2,outline+nyk/2], Memr[sd],
		    Memr[sg], Memr[sgsq], Memr[p], nc, kernel[1,i],
		    skip[1,i], nxk, datamin, datamax)
	    call sf_gdavg (denbuf[1+nxk/2,outline+nyk/2], Memr[sd], Memr[sg],
	        Memr[sgsq], Memr[p], nc, gsums[GAUSS_PIXELS],
		gsums[GAUSS_DENOM], gsums[GAUSS_SGOP])

	    inline = inline + 1
	    index = index + 1
	}

	# Zero the final density enhancement buffer lines.
	do i = nlines - nyk / 2 + 1, nlines
	    call amovkr (0.0, denbuf[1,i], ncols)

	# Free the image buffer pointers.
	call sfree (sp)
end


# SF_SKCNVR -- Compute the convolution kernel using a skip array.

procedure sf_skcnvr (in, out, npix, kernel, skip, nk)

real	in[npix+nk-1]		#I the input vector
real	out[npix]		#O the output vector
int	npix			#I the size of the vector
real	kernel[ARB]		#I the convolution kernel
int	skip[ARB]		#I the skip array
int	nk			#I the size of the convolution kernel

int	i, j
real	sum

begin
	do i = 1, npix {
	    sum = out[i]
	    do j = 1, nk {
		if (skip[j] == YES)
		    next
		sum = sum + in[i+j-1] * kernel[j]
	    }
	    out[i] = sum
	}
end


# SF_GDSUM -- Compute the vector sums required to do the convolution.

procedure  sf_gdsum (in, sgd, sd, sg, sgsq, p, npix, kernel, skip, nk,
	datamin, datamax)

real	in[npix+nk-1]		#I the input vector
real	sgd[ARB]		#U the computed input/output convolution vector
real	sd[ARB]			#U the computed input/output sum vector
real	sg[ARB]			#U the input/ouput first normalization factor
real	sgsq[ARB]		#U the input/ouput second normalization factor
real	p[ARB]			#U the number of points vector
int	npix			#I the size of the vector
real	kernel[ARB]		#I the convolution kernel
int	skip[ARB]		#I the skip array
int	nk			#I the size of the convolution kernel
real	datamin, datamax	#I the good data limits.

int	i, j
real	data

begin
	do i = 1, npix {
	    do j = 1, nk {
		if (skip[j] == YES)
		    next
		data = in[i+j-1]
		if (data < datamin || data > datamax) {
		    sgsq[i] = sgsq[i] - kernel[j] ** 2
		    sg[i] = sg[i] - kernel[j]
		    p[i] = p[i] - 1.0
		} else {
		    sgd[i] = sgd[i] + kernel[j] * data 
		    sd[i] = sd[i] + data
		}
	    }
	}
end


# SF_GDAVG -- Compute the vector averages required to do the convolution.

procedure  sf_gdavg (sgd, sd, sg, sgsq, p, npix, pixels, denom, sgop)

real	sgd[ARB]		#U the computed input/output convolution vector
real	sd[ARB]			#I the computed input/output sum vector
real	sg[ARB]			#I the input/ouput first normalization factor
real	sgsq[ARB]		#U the input/ouput second normalization factor
real	p[ARB]			#I the number of points vector
int	npix			#I the size of the vector
real	pixels			#I number of pixels
real	denom			#I kernel normalization factor
real	sgop			#I kernel normalization factor

int	i

begin
	do i = 1, npix {
	    if (p[i] > 1.5) {
		if (p[i] < pixels) {
		    sgsq[i] = sgsq[i] - (sg[i] ** 2) / p[i]
		    if (sgsq[i] != 0.0)
			sgd[i] = (sgd[i] - sg[i] * sd[i] / p[i]) / sgsq[i]
		    else
			sgd[i] = 0.0
		} else
		    sgd[i] = (sgd[i] - sgop * sd[i]) / denom
	    } else
		sgd[i] = 0.0
	}
end

