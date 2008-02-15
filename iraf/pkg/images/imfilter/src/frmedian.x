# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imset.h>
include "frmedian.h"

# FMD_MEDRING -- Median ring filter an image.

procedure fmd_medring (fmd, im1, im2, boundary, constant, kernel, nxk, nyk)

pointer	fmd		#I pointer to the frmedian structure
pointer	im1		#I pointer to the input image
pointer	im2		#I pointer to the output image
int	boundary	#I boundary extension type
real	constant	#I constant for constant boundary extension
short	kernel[nxk,ARB]	#I the ring filter kernel 
int	nxk, nyk	#I dimensions of the kernel


int	col1, col2, ncols, line, line1, line2, nlines
pointer	inbuf, outbuf, hst
real	rval
bool	fp_equalr()
pointer	impl2r()
errchk	impl2r, fmd_buf, fmd_remedfilter

begin
	# Set the image boundary extension parameters.
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (nxk / 2, nyk / 2))
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Allocate space for the histogram and zero.
	call calloc (hst, FRMED_HMAX(fmd) - FRMED_HMIN(fmd) + 1, TY_INT)

	# Check for 1D images.
	if (IM_NDIM(im1) == 1)
	    nyk = 1

	# Set quantization parameters.
	if (!IS_INDEFR(FRMED_Z1(fmd)))
	    FRMED_ZMIN(fmd) = FRMED_Z1(fmd)
	if (!IS_INDEFR(FRMED_Z2(fmd)))
	    FRMED_ZMAX(fmd) = FRMED_Z2(fmd)
	if (fp_equalr (real (FRMED_HMIN(fmd)), FRMED_ZMIN(fmd)) &&
	    fp_equalr (real (FRMED_HMAX(fmd)), FRMED_ZMAX(fmd)))
	    FRMED_MAP(fmd) = NO
	else
	    FRMED_MAP(fmd) = YES
	if (IS_INDEFR(FRMED_ZLOW(fmd))) {
	    FRMED_HLOW(fmd) = FRMED_HMIN(fmd)
	} else {
	    call amapr (FRMED_ZLOW(fmd), rval, 1, FRMED_ZMIN(fmd),
	        FRMED_ZMAX(fmd), real(FRMED_HMIN(fmd)), real(FRMED_HMAX(fmd)))
	    FRMED_HLOW(fmd) = rval
	}
	if (IS_INDEFR(FRMED_ZHIGH(fmd))) {
	    FRMED_HHIGH(fmd) = FRMED_HMAX(fmd)
	} else {
	    call amapr (FRMED_ZHIGH(fmd), rval, 1, FRMED_ZMIN(fmd),
	        FRMED_ZMAX(fmd), real(FRMED_HMIN(fmd)), real(FRMED_HMAX(fmd)))
	    FRMED_HHIGH(fmd) = rval
	}

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
	    call fmd_buf (im1, col1, col2, line1, line2, inbuf, FRMED_MAP(fmd),
	        FRMED_ZMIN(fmd), FRMED_ZMAX(fmd), real (FRMED_HMIN(fmd)),
		real (FRMED_HMAX(fmd)))

	    # Get output image line.
	    outbuf = impl2r (im2, line)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Median filter the image line.
	    call fmd_remedfilter (fmd, Memi[inbuf], ncols, nlines, Memr[outbuf],
		int (IM_LEN(im2, 1)), Memi[hst], FRMED_HMAX(fmd) -
		FRMED_HMIN(fmd) + 1, kernel, nxk, nyk)

	    # Recover original data range.
	    if (FRMED_UNMAP(fmd) == YES && FRMED_MAP(fmd) == YES)
		call amapr (Memr[outbuf], Memr[outbuf], int (IM_LEN(im2,1)),
		    real (FRMED_HMIN(fmd)), real (FRMED_HMAX(fmd)),
		    FRMED_ZMIN(fmd), FRMED_ZMAX(fmd))
	}

	# Free space.
	call mfree (hst, TY_INT)
	call mfree (inbuf, TY_INT)
end


# FMD_REMEDFILTER -- Run the median window forward.

procedure fmd_remedfilter (fmd, data, nx, ny, medline, ncols, hist, nbins,
	kernel, xbox, ybox)

pointer	fmd			#I pointer to the frmedian structure
int	data[nx,ny]		#I buffer of image data
int	nx, ny			#I dimensions of image buffer
real	medline[ncols]		#O medians
int	ncols			#I length of output image line
int	hist[nbins]		#U histogram
int	nbins			#I size of histogram
short	kernel[xbox,ARB]	#I the ring filter kernel
int	xbox, ybox		#I the dimensions of the kernel

int	i, j, k, hmin, hmax, hindex, hlo, hhi, nhlo, nhhi
int	ohmin, ohmax, nring, nmedian, nzero, hsum

begin
	nring = FRMED_NRING(fmd)
	hmin = FRMED_HMIN(fmd)
	hmax = FRMED_HMAX(fmd)
	hlo = FRMED_HLOW(fmd)
	hhi = FRMED_HHIGH(fmd)

	# Calculate the medians for a line.
	do i = 1, ncols {

	    # Add points.
	    nhlo = 0
	    nhhi = 0
	    ohmin = hhi
	    ohmax = hlo
	    do j = 1, ybox {
		do k = 1, xbox {
		    if (kernel[k,j] == 0)
		        next
		    if (data[i-1+k,j] < hlo) {
		        nhlo = nhlo + 1
		        next
		    }
		    if (data[i-1+k,j] > hhi) {
		        nhhi = nhhi + 1
		        next
		    }
		    if (data[i-1+k,j] < ohmin)
			ohmin = data[i-1+k,j]
		    if (data[i-1+k,j] > ohmax)
			ohmax = data[i-1+k,j]
		    hindex = data[i-1+k,j] - hmin + 1
		    hist[hindex] = hist[hindex] + 1
		}
	    }

	    # Compute the new median and clear the histogram.
	    nzero = nring - nhlo - nhhi
	    if (nzero > 0) {

		nmedian = (nzero - 1) / 2
		hsum = 0
		do j = ohmin - hmin + 1, ohmax - hmin + 1 {
		    if ((hsum + hist[j]) > nmedian)
			break
		    hsum = hsum + hist[j]
		}
		medline[i] = j + hmin - 1
	        call aclri (hist[ohmin-hmin+1], ohmax - ohmin + 1)

	    } else if (nhlo < nhhi)
	        medline[i] = hhi
	    else
	        medline[i] = hlo
	}
end
