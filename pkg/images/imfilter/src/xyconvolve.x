# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <imset.h>

# CNV_XYCONVOLVE -- Convolve an image with a kernel that is separable in x
# and y. The kernel dimensions may be any size.

procedure cnv_xyconvolve (im1, im2, xkernel, nxk, ykernel, nyk, boundary,
	constant, radsym) 

pointer	im1		# pointer to the input image
pointer	im2		# pointer to the output image
real	xkernel[ARB]	# the convolution kernel in x
int	nxk		# dimensions of the kernel in x
real	ykernel[ARB]	# the convolution kernel in x
int	nyk		# dimensions of the kernel in y
int	boundary	# type of boundary extension
real	constant	# constant for constant boundary extension
int	radsym		# does the kernel have radial symmetry

int	i, ncols, nlines, col1, col2, nincols, inline, outline, tempi
pointer	sp, lineptrs, imbuf, inbuf, linebuf, outbuf, bufptr, bufptr1, bufptr2
pointer	imgs2r(), impl2r()
errchk	imgs2r, impl2r

begin
	# Allocate working space.
	call smark (sp)
	call salloc (lineptrs, nyk, TY_INT)

	# Set the input image boundary conditions.
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (nxk / 2 + 1, nyk / 2 + 1))
	if (boundary == BT_CONSTANT)
	    call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Compute the number of columns and lines in the output image
	# and allocate some buffer space.
	ncols = IM_LEN(im2,1)
	if (IM_NDIM(im2) == 1)
	    nlines = 1
	else
	    nlines = IM_LEN(im2,2)
	call calloc (inbuf, ncols * nyk , TY_REAL)
	call salloc (linebuf, ncols, TY_REAL)

	# Set the input image column limits and allocate the data buffer.
	col1 = 1 - nxk / 2
	col2 = IM_LEN(im1,1) + nxk / 2
	nincols = col2 - col1 + 1

	# Initialise the line buffers.
	inline = 1 - nyk / 2
	do i = 1 , nyk {
	    Memi[lineptrs+i-1] = i
	    imbuf = imgs2r (im1, col1, col2, inline, inline)
	    if (radsym == YES)
	        call cnv_radcnvr (Memr[imbuf], Memr[inbuf+(i-1)*ncols], ncols,
	            xkernel, nxk)
	    else
	        call acnvr (Memr[imbuf], Memr[inbuf+(i-1)*ncols], ncols,
	            xkernel, nxk)
	    inline = inline + 1
	}

	# Generate the output image line by line.
	do outline = 1, nlines {

	    # Get the output image line.
	    outbuf = impl2r (im2, outline)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Generate the output image line.
	    call aclrr (Memr[outbuf], ncols)
	    if (radsym == YES) {
		do i = 1, nyk /2 {
	            bufptr1 = inbuf + (Memi[lineptrs+i-1] - 1) * ncols
	            bufptr2 = inbuf + (Memi[lineptrs+nyk-i] - 1) * ncols
		    call aaddr (Memr[bufptr1], Memr[bufptr2], Memr[linebuf],
		       ncols )
		    call cnv_awsum1 (Memr[outbuf], Memr[linebuf], Memr[outbuf],
		        ncols, ykernel[i])
		}
	        bufptr = inbuf + (Memi[lineptrs+nyk/2] - 1) * ncols
		if (mod (nyk, 2) == 1)
		    call cnv_awsum1 (Memr[outbuf], Memr[bufptr], Memr[outbuf],
		        ncols, ykernel[nyk/2+1])
	    } else {
	        do i = 1, nyk {
	            bufptr = inbuf + (Memi[lineptrs+i-1] - 1) * ncols
		    call cnv_awsum1 (Memr[outbuf], Memr[bufptr], Memr[outbuf],
		        ncols, ykernel[i])
	        }
	    }

	    # Scroll the input buffer indices.
	    tempi = Memi[lineptrs]
	    do i = 1, nyk - 1
		Memi[lineptrs+i-1] = Memi[lineptrs+i]
	    Memi[lineptrs+nyk-1] = tempi

	    # Read in the new input image line.
	    imbuf = imgs2r (im1, col1, col2, inline, inline)

	    # Do the 1D convolution and add the vector into the input buffer.
	    bufptr = inbuf + (Memi[lineptrs+nyk-1] - 1) * ncols
	    call aclrr (Memr[bufptr], ncols)
	    if (radsym == YES)
	        call cnv_radcnvr (Memr[imbuf], Memr[bufptr], ncols, xkernel,
		    nxk)
	    else
	        call acnvr (Memr[imbuf], Memr[bufptr], ncols, xkernel, nxk)

	    # Increment the input image line pointer.
	    inline = inline + 1
	}

	# Free the buffer pointers.
	call mfree (inbuf, TY_REAL)
	call sfree (sp)
end
