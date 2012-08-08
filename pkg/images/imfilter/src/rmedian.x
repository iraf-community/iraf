# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imset.h>
include "rmedian.h"

# MED_MEDRING -- Median ring filter an image.

procedure med_medring (med, im1, im2, boundary, constant, kernel, nxk, nyk)

pointer	med		#I pointer to the rmedian structure
pointer	im1		#I pointer to the input image
pointer	im2		#I pointer to the output image
int	boundary	#I boundary extension type
real	constant	#I constant for constant boundary extension
short	kernel[nxk,ARB]	#I the ring filter kernel 
int	nxk, nyk	#I dimensions of the kernel


int	col1, col2, ncols, line, line1, line2, nlines
pointer	inbuf, outbuf, filter
pointer	impl2r()
errchk	impl2r, med_buf, med_remedfilter

begin
	# Set the image boundary extension parameters.
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (nxk / 2, nyk / 2))
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Allocate space for the points to be medianed.
	call malloc (filter, RMED_NRING(med), TY_REAL)

	# Check for 1D images.
	if (IM_NDIM(im1) == 1)
	    nyk = 1

	# Initialize input image buffer.
	inbuf = NULL
	col1 = 1 - nxk / 2
	col2 = IM_LEN(im1, 1) + nxk / 2 
	ncols = col2 - col1 + 1

	# Generate the output image line by line.
	do line = 1, IM_LEN(im2, 2) {

	    # Define the range of lines to read.
	    line1 = line - nyk / 2
	    line2 = line + nyk / 2
	    nlines = line2 - line1 + 1

	    # Read in the appropriate range of image lines.
	    call med_buf (im1, col1, col2, line1, line2, inbuf)

	    # Get output image line.
	    outbuf = impl2r (im2, line)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Median filter the image line.
	    call med_remedfilter (med, Memr[inbuf], ncols, nlines, Memr[outbuf],
		int (IM_LEN(im2, 1)), Memr[filter], kernel, nxk, nyk)
	}

	# Free space.
	call mfree (filter, TY_REAL)
	call mfree (inbuf, TY_REAL)
end


# MED_REMEDFILTER -- Run the median window forward.

procedure med_remedfilter (med, data, nx, ny, medline, ncols, filter,
	kernel, xbox, ybox)

pointer	med			#I pointer to the rmedian structure
real	data[nx,ny]		#I buffer of image data
int	nx, ny			#I dimensions of image buffer
real	medline[ncols]		#O the output array of medians
int	ncols			#I length of output image line
real	filter[ARB]		#U the medianing filter
short	kernel[xbox,ARB]	#U the ring filter kernel
int	xbox, ybox		#U the dimensions of the kernel

int	i, j, k, nring, npts, nlo, nhi
real	zlo, zhi
real	asokr()

begin
	nring = RMED_NRING(med)
	zlo = RMED_ZLOW(med)
	zhi = RMED_ZHIGH(med)

	# Loop over the data columns.
	do i = 1, ncols {

	    # Load the filter.
	    nlo = 0
	    nhi = 0
	    npts = 0
	    do j = 1, ybox {
		do k = i, i + xbox - 1 {
		    if (kernel[k-i+1,j] == 0)
		        next
		    if (data[k,j] < zlo) {
			nlo = nlo + 1
		        next
		    }
		    if (data[k,j] > zhi) {
			nhi = nhi + 1
		        next
		    }
		    npts = npts + 1
		    filter[npts] = data[k,j]
		}
	    }

	    # Compute the median.
	    if (npts > 0)
	        medline[i] = asokr (filter, npts, (npts+1)/2)
	    else if (nlo < nhi)
		medline[i] = zhi
	    else
		medline[i] = zlo
	}
end
