# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imhdr.h>
include <imset.h>

# FMD_BOX -- Median filter an image.

procedure fmd_box (im1, im2, boundary, constant)

pointer	im1		# pointer to the input image
pointer	im2		# pointer to the output image
int	boundary	# boundary extension type
real	constant	# constant for constant boundary extension

int	col1, col2, ncols, line, line1, line2, nlines
pointer	inbuf, outbuf, hst
bool	fp_equalr()
pointer	impl2r()
errchk	impl2r, fmd_buf, fmd_boxset, fmd_boxfilter

include "fmedian.com"

begin
	# Set the image boundary extension parameters.
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (xbox / 2, ybox / 2))
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Allocate space for the histogram and zero.
	call calloc (hst, hmax - hmin + 1, TY_SHORT)

	# Check for 1D images.
	if (IM_NDIM(im1) == 1)
	    ybox = 1

	# Set quantization parameters.
	if (!IS_INDEF(z1))
	    zmin = z1
	if (!IS_INDEF(z2))
	    zmax = z2
	if (fp_equalr (real (hmin), zmin) && fp_equalr (real (hmax), zmax))
	    map = NO
	else
	    map = YES

	# Initialize input image buffer.
	inbuf = NULL
	col1 = 1 - xbox / 2
	col2 = IM_LEN(im1, 1) + xbox / 2 
	ncols = col2 - col1 + 1

	# Generate the output image line by line.
	do line = 1, IM_LEN(im2, 2) {

	    # Define the range of lines to read.
	    line1 = line - ybox / 2
	    line2 = line + ybox / 2
	    nlines = line2 - line1 + 1

	    # Read in the appropriate range of image lines.
	    call fmd_buf (im1, col1, col2, line1, line2, inbuf, map,
	        zmin, zmax, real (hmin), real (hmax))

	    # Set up median filter array for each line scanned.
	    call fmd_boxset (Memi[inbuf], ncols, nlines, Mems[hst],
		hmax - hmin + 1, line)

	    # Get output image line.
	    outbuf = impl2r (im2, line)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Median filter the image line.
	    call fmd_boxfilter (Memi[inbuf], ncols, nlines, Memr[outbuf],
		int (IM_LEN(im2, 1)), Mems[hst], hmax - hmin + 1, line)

	    # Recover original data range.
	    if (unmap == YES && map == YES)
		call amapr (Memr[outbuf], Memr[outbuf], int (IM_LEN(im2,1)),
		    real (hmin), real (hmax), zmin, zmax)
	}

	# Free space.
	call mfree (hst, TY_SHORT)
	call mfree (inbuf, TY_INT)
end


# FMD_BOXSET -- Set up median array for the beginning of each image line.

procedure fmd_boxset (data, nx, ny, hist, nbins, line)

int	data[nx, ny]			# image data buffer
int	nx				# number of columns in image buffer
int	ny				# number of lines in the image buffer
short	hist[nbins]			# histogram
int	nbins				# number of histogram bins
int	line				# line number

int	i, j
int	index
pointer	sp, filter

int	amedi()

include "fmedian.com"

begin
	# Initialize.
	if (line == 1)  {

	    call smark (sp)
	    call salloc (filter, xbox * ybox, TY_INT)

	    # Load filter.
	    index = 0
	    do j = 1, ybox {
		do i = 1, xbox {
		    Memi[filter+index] = data[i,j]
		    index = index + 1
		}
	    }

	    # Load histogram.
	    call fmd_ashgmi (Memi[filter], xbox * ybox, hist, nbins, hmin, hmax)

	    # Calculate the current median.
	    median = amedi (Memi[filter], xbox * ybox)

	    # Calculate the number less than the current median.
	    nltmedian = 0
	    do i = 1, xbox * ybox {
		if (Memi[filter+i-1] < median)
		    nltmedian = nltmedian + 1
	    }

	    nmedian = (xbox * ybox - 1) / 2

	    call sfree (sp)

	} else {

	    # Add new points.
	    if (mod (line, 2) == 0) {
	        do i = nx - xbox + 1, nx {
		    index = data[i, ny] - hmin + 1
		    hist[index] = hist[index] + 1
		    if (data[i,ny] < median)
		        nltmedian = nltmedian + 1
	        }
	    } else {
	        do i = 1, xbox {
		    index = data[i, ny] - hmin + 1
		    hist[index] = hist[index] + 1
		    if (data[i,ny] < median)
		        nltmedian = nltmedian + 1
	        }
	    }

	    # Calculate the new current median.
	    if (nltmedian > nmedian) {
		do i = 1, nbins {
		    median = median - 1
		    nltmedian = nltmedian - hist[median-hmin+1]
		    if (nltmedian <= nmedian)
			break
		}
	    } else {
		do i = 1 , nbins {
		    if (nltmedian + hist[median-hmin+1] > nmedian)
			break
		    nltmedian = nltmedian + hist[median-hmin+1]
		    median = median + 1
		}
	    }

	}
end


# FMD_BOXFILTER -- Median filter a single image line.

procedure fmd_boxfilter (data, nx, ny, medline, ncols, hist, nbins, line)

int	data[nx, ny]	# Image data
int	nx, ny		# Dimensions of data
real	medline[ncols]	# Median array
int	ncols		# Number of output image columns
short	hist[nbins]	# histogram
int	nbins		# length of histogram
int	line		# current line number

include	"fmedian.com"

begin
	if (mod (line, 2) != 0)
	    call fmd_forward_filter (data, nx, ny, medline, ncols, hist, nbins)
	else
	    call fmd_rev_filter (data, nx, ny, medline, ncols, hist, nbins)
end


# FMD_FORWARD_FILTER -- Run the median window forward.

procedure fmd_forward_filter (data, nx, ny, medline, ncols, hist, nbins)

int	data[nx,ny]		# buffer of image data
int	nx, ny			# dimensions of image buffer
real	medline[ncols]		# medians
int	ncols			# length of output image line
short	hist[nbins]		# histogram
int	nbins			# size of histogram

int	i, j, dindex, hindex

include	"fmedian.com"

begin
	# Calculate the medians for a line.
	dindex = 1
	do i = 1, ncols - 1 {

	    # Set median.
	    medline[i] = median

	    # Delete points.
	    do j = 1, ybox {
		hindex = data[dindex,j] - hmin + 1
		hist[hindex] = hist[hindex] - 1
		if (data[dindex,j] < median)
		    nltmedian = nltmedian - 1
	    }

	    # Add points.
	    do j = 1, ybox {
		hindex = data[dindex+xbox,j] - hmin + 1
		hist[hindex] = hist[hindex] + 1
		if (data[dindex+xbox,j] < median)
		    nltmedian = nltmedian + 1
	    }

	    # Calculate the new current median.
	    if (nltmedian > nmedian) {
		do j = 1, nbins {
		    median = median - 1
		    nltmedian = nltmedian - hist[median-hmin+1]
		    if (nltmedian  <= nmedian)
			break
		}
	    } else {
		do j = 1, nbins {
		    if (nltmedian + hist[median-hmin+1] > nmedian)
			break
		    nltmedian = nltmedian + hist[median-hmin+1]
		    median = median + 1
		}
	    }

	    dindex = dindex + 1

	}

	# Set the last median.
	medline[ncols] = median

	# Delete the points from the last row.
	do i = nx - xbox + 1, nx {
	    hindex = data[i,1] - hmin + 1
	    hist[hindex] = hist[hindex] - 1
	    if (data[i,1] < median)
		nltmedian = nltmedian - 1
	}
end


# FMD_REV_FILTER -- Median filter the line in the reverse direction.

procedure fmd_rev_filter (data, nx, ny, medline, ncols, hist, nbins)

int	data[nx,ny]		# buffer of image data
int	nx, ny			# dimensions of image buffer
real	medline[ncols]		# medians
int	ncols			# length of output image line
short	hist[nbins]		# histogram
int	nbins			# size of histogram

int	i, j, dindex, hindex

include "fmedian.com"

begin
	# Calculate the medians for a line.
	dindex = nx
	do i = ncols, 2, -1 {

	    # Set median.
	    medline[i] = median

	    # Delete points.
	    do j = 1, ybox {
		hindex = data[dindex,j] - hmin + 1
		hist[hindex] = hist[hindex] - 1
		if (data[dindex,j] < median)
		    nltmedian = nltmedian - 1
	    }

	    # Add points.
	    do j = 1, ybox {
		hindex = data[dindex-xbox,j] - hmin + 1
		hist[hindex] = hist[hindex] + 1
		if (data[dindex-xbox,j] < median)
		    nltmedian = nltmedian + 1
	    }

	    # Calculate the new current median.
	    if (nltmedian > nmedian) {
		do j = 1, nbins {
		    median = median - 1
		    nltmedian = nltmedian - hist[median-hmin+1]
		    if (nltmedian  <= nmedian)
			break
		}
	    } else {
		do j = 1, nbins {
		    if (nltmedian + hist[median-hmin+1] > nmedian)
			break
		    nltmedian = nltmedian + hist[median-hmin+1]
		    median = median + 1
		}
	    }

	    dindex = dindex - 1

	}

	# Set the last median.
	medline[1] = median

	# Delete the points from the last row.
	do i = 1, xbox {
	    hindex = data[i,1] - hmin + 1
	    hist[hindex] = hist[hindex] - 1
	    if (data[i,1] < median)
		nltmedian = nltmedian - 1
	}
end
