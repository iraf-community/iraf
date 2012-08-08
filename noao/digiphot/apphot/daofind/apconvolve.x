include <imhdr.h>
include <imset.h>
include "../lib/find.h"

# AP_FCONVOLVE --  Solve for the density enhancement image and optionally
# the sky enhancement image in the case where datamin and datamax are not
# defined.

procedure ap_fconvolve (im, den, sky, kernel1, kernel2, skip, nxk, nyk, const2)

pointer	im			# pointer to the input image
pointer	den			# pointer to the output density image
pointer	sky			# pointer to the output sky image
real	kernel1[nxk,nyk]	# the first convolution kernel
real	kernel2[nxk,nyk]	# the second convolution kernel
int	skip[nxk,nyk]		# the skip array
int	nxk, nyk		# dimensions of the kernel
real	const2			# subtraction constant for the skyimage

int	i, ncols, nlines, col1, col2, inline, outline
pointer	sp, lineptrs, outbuf1, outbuf2
pointer	imgs2r(), impl2r()
errchk	imgs2r, impl2r, imflush

begin
	# Set up an array of linepointers.
	call smark (sp)
	call salloc (lineptrs, nyk, TY_POINTER)

	# Set the number of image buffers.
	call imseti (im, IM_NBUFS, nyk)

	ncols = IM_LEN(den,1)
	nlines = IM_LEN(den,2)

	# Set input image column limits.
	col1 = 1 - nxk / 2
	col2 = IM_LEN(im,1) + nxk / 2

	# Initialise the line buffers.
	inline = 1 - nyk / 2
	do i = 1 , nyk - 1 {
	    Memi[lineptrs+i] = imgs2r (im, col1, col2, inline, inline)
	    inline = inline + 1
	}

	# Generate the output image line by line.
	do outline = 1, nlines {

	    # Scroll the input buffers.
	    do i = 1, nyk - 1
		Memi[lineptrs+i-1] = Memi[lineptrs+i]

	    # Read in new image line.
	    Memi[lineptrs+nyk-1] = imgs2r (im, col1, col2, inline,
	        inline)

	    # Get first output image line.
	    outbuf1 = impl2r (den, outline)
	    if (outbuf1 == EOF)
		call error (0, "Error writing first output image.")

	    # Generate first output image line.
	    call aclrr (Memr[outbuf1], ncols)
	    do i = 1, nyk
		call ap_skcnvr (Memr[Memi[lineptrs+i-1]], Memr[outbuf1],
		    ncols, kernel1[1,i], skip[1,i], nxk)

	    if (sky != NULL) {

	        # Get second output image line.
	        outbuf2 = impl2r (sky, outline)
	        if (outbuf2 == EOF)
		    call error (0, "Error writing second output image.")

	        # Generate second output image line.
	        call aclrr (Memr[outbuf2], ncols)
	        do i = 1, nyk
		    call ap_skcnvr (Memr[Memi[lineptrs+i-1]], Memr[outbuf2],
		        ncols, kernel2[1,i], skip[1,i], nxk)
		call ap_w1sur (Memr[outbuf2], Memr[outbuf1], Memr[outbuf2],
		    ncols, -const2)
	    }

	    inline = inline + 1
	}

	# Flush the output image(s).
	call imflush (den)
	if (sky != NULL)
	    call imflush (sky)

	# Free the image buffer pointers.
	call sfree (sp)
end


# AP_GCONVOLVE --  Solve for the density enhancement image and optionally
# the sky enhancement image in the case where datamin and datamax are defined.

procedure ap_gconvolve (im, den, sky, kernel1, skip, nxk, nyk, gsums,
	datamin, datamax)

pointer	im			# pointer to the input image
pointer	den			# pointer to the output density image
pointer	sky			# pointer to the output sky image
real	kernel1[nxk,nyk]	# the first convolution kernel
int	skip[nxk,nyk]		# the sky array
int	nxk, nyk		# dimensions of the kernel
real	gsums[ARB]		# array of kernel sums
real	datamin, datamax	# the good data minimum and maximum

int	i, ncols, nlines, col1, col2, inline, outline
pointer	sp, lineptrs, sd, sgd, sg, sgsq, p, outbuf2
pointer	imgs2r(), impl2r()
errchk	imgs2r, impl2r, imflush

begin
	# Set up an array of linepointers.
	call smark (sp)
	call salloc (lineptrs, nyk, TY_POINTER)

	# Set the number of image buffers.
	call imseti (im, IM_NBUFS, nyk)

	ncols = IM_LEN(den,1)
	nlines = IM_LEN(den,2)

	# Allocate some working space.
	call salloc (sd, ncols, TY_REAL)
	call salloc (sgsq, ncols, TY_REAL)
	call salloc (sg, ncols, TY_REAL)
	call salloc (p, ncols, TY_REAL)

	# Set input image column limits.
	col1 = 1 - nxk / 2
	col2 = IM_LEN(im,1) + nxk / 2

	# Initialise the line buffers.
	inline = 1 - nyk / 2
	do i = 1 , nyk - 1 {
	    Memi[lineptrs+i] = imgs2r (im, col1, col2, inline, inline)
	    inline = inline + 1
	}

	# Generate the output image line by line.
	do outline = 1, nlines {

	    # Scroll the input buffers.
	    do i = 1, nyk - 1
		Memi[lineptrs+i-1] = Memi[lineptrs+i]

	    # Read in new image line.
	    Memi[lineptrs+nyk-1] = imgs2r (im, col1, col2, inline,
	        inline)

	    # Get first output image line.
	    sgd = impl2r (den, outline)
	    if (sgd == EOF)
		call error (0, "Error writing first output image.")

	    # Generate first output image line.
	    call aclrr (Memr[sgd], ncols)
	    call aclrr (Memr[sd], ncols)
	    call amovkr (gsums[GAUSS_SUMG], Memr[sg], ncols)
	    call amovkr (gsums[GAUSS_SUMGSQ], Memr[sgsq], ncols)
	    call amovkr (gsums[GAUSS_PIXELS], Memr[p], ncols)
	    do i = 1, nyk
		call ap_gdsum (Memr[Memi[lineptrs+i-1]], Memr[sgd], Memr[sd],
		    Memr[sg], Memr[sgsq], Memr[p], ncols, kernel1[1,i],
		    skip[1,i], nxk, datamin, datamax)
	    call ap_gdavg (Memr[sgd], Memr[sd], Memr[sg], Memr[sgsq],
	        Memr[p], ncols, gsums[GAUSS_PIXELS], gsums[GAUSS_DENOM],
		gsums[GAUSS_SGOP])

	    if (sky != NULL) {

	        # Get second output image line.
	        outbuf2 = impl2r (sky, outline)
	        if (outbuf2 == EOF)
		    call error (0, "Error writing second output image.")

	        # Generate second output image line.
		call ap_davg (Memr[sd], Memr[sgd], Memr[sg], Memr[p],
		    Memr[outbuf2], ncols)
	    }

	    inline = inline + 1
	}

	# Flush the output image(s).
	call imflush (den)
	if (sky != NULL)
	    call imflush (sky)

	# Free the image buffer pointers.
	call sfree (sp)
end


# AP_SKCNVR -- Compute the convolution kernel using a skip array.

procedure ap_skcnvr (in, out, npix, kernel, skip, nk)

real	in[npix+nk-1]		# the input vector
real	out[npix]		# the output vector
int	npix			# the size of the vector
real	kernel[ARB]		# the convolution kernel
int	skip[ARB]		# the skip array
int	nk			# the size of the convolution kernel

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


# AP_GDSUM -- Compute the vector sums required to do the convolution.

procedure  ap_gdsum (in, sgd, sd, sg, sgsq, p, npix, kernel, skip, nk,
	datamin, datamax)

real	in[npix+nk-1]		# the input vector
real	sgd[ARB]		# the computed input/output convolution vector
real	sd[ARB]			# the computed input/output sum vector
real	sg[ARB]			# the input/ouput first normalization factor
real	sgsq[ARB]		# the input/ouput second normalization factor
real	p[ARB]			# the number of points vector
int	npix			# the size of the vector
real	kernel[ARB]		# the convolution kernel
int	skip[ARB]		# the skip array
int	nk			# the size of the convolution kernel
real	datamin, datamax	# the good data limits.

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


# AP_GDAVG -- Compute the vector averages required to do the convolution.

procedure  ap_gdavg (sgd, sd, sg, sgsq, p, npix, pixels, denom, sgop)

real	sgd[ARB]		# the computed input/output convolution vector
real	sd[ARB]			# the computed input/output sum vector
real	sg[ARB]			# the input/ouput first normalization factor
real	sgsq[ARB]		# the input/ouput second normalization factor
real	p[ARB]			# the number of points vector
int	npix			# the size of the vector
real	pixels			# number of pixels
real	denom			# kernel normalization factor
real	sgop			# kernel normalization factor

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


# AP_DAVG -- Generate the results the optional sky output image.

procedure ap_davg (sd, sgd, sg, p, out, npix)

real	sd[ARB]			# the computed input/output sum vector
real	sgd[ARB]		# the computed input/output convolution vector
real	sg[ARB]			# the input/ouput first normalization factor
real	p[ARB]			# the number of points vector
real	out[ARB]		# the output array
int	npix			# the size of the vector

int	i

begin
	do i = 1, npix {
	    if (p[i] > 0.0)
	        out[i] = (sd[i] - sgd[i] * sg[i]) / p[i]
	    else
		out[i] = 0.0
	}
end
