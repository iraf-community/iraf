# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <imset.h>

# CNV_CONVOLVE -- Convolve an image with an nxk by nyk kernel. The kernel
# dimensions are assumed to be odd.

procedure cnv_convolve (im1, im2, kernel, nxk, nyk, boundary, constant, radsym) 

pointer	im1		# pointer to the input image
pointer	im2		# pointer to the output image
real	kernel[nxk,nyk]	# the convolution kernel
int	nxk, nyk	# dimensions of the kernel
int	boundary	# type of boundary extension
real	constant	# constant for constant boundary extension
int	radsym		# does the kernel have radial symmetry ?

int	i, ncols, nlines, col1, col2, nincols, inline, outline
pointer	sp, lineptrs, linebuf, outbuf
pointer	imgs2r(), impl2r()
errchk	imgs2r, impl2r

begin
	# Set up an array of line pointers.
	call smark (sp)
	call salloc (lineptrs, nyk, TY_POINTER)

	# Set the number of image buffers.
	call imseti (im1, IM_NBUFS, nyk)

	# Set the input image boundary conditions.
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (nxk / 2 + 1, nyk / 2 + 1))
	if (boundary == BT_CONSTANT)
	    call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Define the number of output image lines and columns.
	ncols = IM_LEN(im2,1)
	if (IM_NDIM(im2) == 1)
	    nlines = 1
	else
	    nlines = IM_LEN(im2,2)

	# Set the input image column limits.
	col1 = 1 - nxk / 2
	col2 = IM_LEN(im1,1) + nxk / 2
	nincols = col2 - col1 + 1

	# Initialise the line buffers.
	inline = 1 - nyk / 2
	do i = 1 , nyk - 1 {
	    Memi[lineptrs+i] = imgs2r (im1, col1, col2, inline, inline)
	    inline = inline + 1
	}

	# Generate the output image line by line
	call salloc (linebuf, nincols, TY_REAL)
	do outline = 1, nlines {

	    # Scroll the input buffers
	    do i = 1, nyk - 1
		Memi[lineptrs+i-1] = Memi[lineptrs+i]

	    # Read in new image line
	    Memi[lineptrs+nyk-1] = imgs2r (im1, col1, col2, inline,
	        inline)

	    # Get output image line
	    outbuf = impl2r (im2, outline)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Generate output image line
	    call aclrr (Memr[outbuf], ncols)
	    if (radsym == YES) {
		do i = 1, nyk / 2 {
		    call aaddr (Memr[Memi[lineptrs+i-1]],
		        Memr[Memi[lineptrs+nyk-i]], Memr[linebuf], nincols)
	            call cnv_radcnvr (Memr[linebuf], Memr[outbuf], ncols,
		        kernel[1,i], nxk)
		}
		if (mod (nyk, 2) == 1)
	            call cnv_radcnvr (Memr[Memi[lineptrs+nyk/2]], Memr[outbuf],
		        ncols, kernel[1,nyk/2+1], nxk)
	    } else {
	        do i = 1, nyk
	            call acnvr (Memr[Memi[lineptrs+i-1]], Memr[outbuf], ncols,
		        kernel[1,i], nxk)
	    }

	    inline = inline + 1
	}

	# Free the image buffer pointers
	call sfree (sp)
end
