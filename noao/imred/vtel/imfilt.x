include <mach.h>
include <imhdr.h>
include <imset.h>
include "vt.h"

# IMFILT -- Apply a spatial averageing filter to an image by convolving the
# image with a filter kernel.  Return the resulting image in a separate
# image file.

procedure imfilt (inim, outim, kernel, kxdim, kydim, el)

pointer	inim, outim		        # input and output images
int	kxdim, kydim		        # dimensions of convolution kernel
real	kernel[kxdim, kydim]	        # convolution kernel
real	el[LEN_ELSTRUCT]		# limb ellipse structure

int	nlines, linelength, startline
int	linebuf, outline, i
int	k, offset, x2semi
int	extension, startpix, lastline
real	p, n, lpix1, lpix2
pointer	lines, tmpptr, outptr, inptr, sp

pointer	impl2r(), imgl2r(), imfglexr()
errchk	impl2r, imfglexr, imgl2r

begin
	# Set up the pointer array on the stack.
	call smark (sp)
	call salloc (lines, kydim, TY_POINTER)

	# Calculate the extension.
	extension = kxdim / 2
	offset = E_XCENTER[el] - E_XSEMIDIAMETER[el]
	x2semi = 2 * E_XSEMIDIAMETER[el]

	# Startpix is the x-coordinate of the beginning of the 1-D array
	# we pass to the convolution vector routine.  If wrong, return.

	startpix = offset - extension
	if (startpix <= 0) {
	    call printf ("convolution kernel too wide for this image\n")
	    return
	}

	# Get the dimensions of the image.
	linelength = IM_LEN(inim, 1)
	nlines = IM_LEN(inim, 2)

	# Pointers to the input and the output images are passed to this
	# subroutine by the user.
	
	# Use imseti to set up the appropriate number of input buffers.
	call imseti (inim, IM_NBUFS, kydim+1)

	# Read in the necessary number of input image lines to initially
	# fill all but one of the input line buffers.
	# First, skip over all lines that are off the limb.
	# The size of the output image is defined prior to the call
	# to this subroutine, the output image is the same size as the
	# input image.

	startline = 0
	Memi[lines] = NULL

	# Skip over empty lines.
	while (Memi[lines] == NULL) {
	    startline = startline + 1
	    Memi[lines] = imfglexr (inim, startline, el, extension)
	}

	# Fill (almost) the line buffer.
	do linebuf = 1, kydim-2
	    Memi[lines+linebuf] = imfglexr (inim, linebuf+startline,
		el, extension)

	# Copy the first startline lines from the input image into the
	# output image.
	do outline = 1, startline + (kydim/2) {

	    # Put next line to output image, get the corresponding line from
	    # the input image.
	    inptr = imgl2r (inim, outline)
	    outptr = impl2r (outim, outline)

	    # Copy the input line into the ouput line. Strip sqib.
	    do i = 1, DIM_VTFD {
	        Memr[outptr+i-1] = Memr[inptr+i-1]/16.
	    }
	}

	# Do the convolution, output line by output line.
	do outline = (kydim/2) + startline, nlines {

	    # Use ellipse parameters to determine where the limb
	    # intersections are.
	    p = (real(outline) - E_YCENTER[el])**2/E_YSEMIDIAMETER[el]**2
	    n = (1.0 - p) * E_XSEMIDIAMETER[el]**2

	    # The two limb points are:
	    lpix1 = int(-sqrt(abs(n)) + .5) + E_XCENTER[el]
	    lpix2 = int(sqrt(abs(n)) + .5) + E_XCENTER[el]

	    # Keep a copy of this input line around for filling outside
	    # the limb.
	    inptr = imgl2r (inim, outline)

	    # Scroll the buffer pointer array.
	    if (outline > ((kydim/2) + startline))
	        do i = 0, kydim - 2
		    Memi[lines+i] = Memi[lines+i+1]

	    # Get next line from input image, if it is off the limb then we
	    # are done.

	    tmpptr = imfglexr (inim, outline+((kydim/2)+1), el, extension)
	    if (tmpptr == NULL) {
		lastline = outline
		break
	    }
	    Memi[lines+kydim-1] = tmpptr

	    # Put next line to output image.
	    outptr = impl2r (outim, outline)

	    # Zero the output line.
	    call aclrr (Memr[outptr], DIM_VTFD)

	    # Here is the actual convolution, this is a do loop over the lines
	    # of the kernel, each call to acnvrs adds the convolution of a
	    # kernel line with an input line to the output line.

	    do k = 1, kydim
	        call acnvr (Memr[Memi[lines+k-1]+startpix], Memr[outptr+offset],
		    x2semi, kernel[1,k], kxdim)

	    # Fill outside the limb with orig data.
	    do i = 1, lpix1 {
	        Memr[outptr+i-1] = Memr[inptr+i-1]/16.
	    }
	    do i = lpix2, DIM_VTFD {
	        Memr[outptr+i-1] = Memr[inptr+i-1]/16.
	    }

	    # Roundoff adjustment.
	    do i = startpix, startpix+x2semi {
		if (Memr[outptr+i-1] < 0.0)
		    Memr[outptr+i-1] = Memr[outptr+i-1] - .5
		else
		    Memr[outptr+i-1] = Memr[outptr+i-1] + .5
	    }
	            
	}    # End of do loop on outline.

	# Clear the rest of the image.
	do outline = lastline, DIM_VTFD {

	    # Put next line to output image, get the corresponding line from
	    # the input image.
	    inptr = imgl2r (inim, outline)
	    outptr = impl2r (outim, outline)

	    # Copy the input line into the ouput line. Strip sqib.
	    do i = 1, DIM_VTFD {
	        Memr[outptr+i-1] = Memr[inptr+i-1]/16.
	    }
	}

	call sfree (sp)
end
