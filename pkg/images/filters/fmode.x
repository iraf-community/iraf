# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <imhdr.h>
include <imset.h>

# FMO_BOX -- Modal filter an image

procedure fmo_box (im1, im2, boundary, constant)

pointer	im1		# pointer to the input image
pointer	im2		# pointer to the output image
int	boundary	# boundary extension type
real	constant	# constant for constant boundary extension

int	col1, col2, ncols, line, line1, line2, nlines
pointer	inbuf, outbuf, hst

bool	fp_equalr()
pointer	impl2r()

errchk	impl2r, fmd_buf, fmo_boxset, fmo_boxfilter

include "fmode.com"

begin
	# set image boundary extension parameters
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (xbox / 2, ybox / 2))
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# allocate space for the histogram and zero
	call calloc (hst, hmax - hmin + 1, TY_SHORT)

	# check for 1D images
	if (IM_NDIM(im1) == 1)
	    ybox = 1

	# set quantization parameters
	if (!IS_INDEF(z1))
	    zmin = z1
	if (!IS_INDEF(z2))
	    zmax = z2
	if (fp_equalr (real (hmin), zmin) && fp_equalr (real (hmax), zmax))
	    map = NO
	else
	    map = YES

	# initialize input image buffer
	inbuf = NULL
	col1 = 1 - xbox / 2
	col2 = IM_LEN(im1, 1) + xbox / 2 
	ncols = col2 - col1 + 1

	# generate the output image line by line
	do line = 1, IM_LEN(im2, 2) {

	    # define the range of lines to read
	    line1 = line - ybox / 2
	    line2 = line + ybox / 2
	    nlines = line2 - line1 + 1

	    # read in the appropriate range of image lines
	    call fmd_buf (im1, col1, col2, line1, line2, inbuf, map,
	        zmin, zmax, real (hmin), real (hmax))

	    # set up median filter array for each line scanned
	    call fmo_boxset (Memi[inbuf], ncols, nlines, Mems[hst],
		hmax - hmin + 1, line)

	    # get output image line
	    outbuf = impl2r (im2, line)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # modal filter the image line
	    call fmo_boxfilter (Memi[inbuf], ncols, nlines, Memr[outbuf],
		int (IM_LEN(im2, 1)), Mems[hst], hmax - hmin + 1, line)

	    # recover original data range
	    if (unmap == YES && map == YES) {
		if (fp_equalr (real (hmin), real (hmax)))
		    call amovkr (zmin, Memr[outbuf], int (IM_LEN(im2,1)))
		else
		    call amapr (Memr[outbuf], Memr[outbuf], int (IM_LEN(im2,1)),
		        real (hmin), real (hmax), zmin, zmax)
	    }
	}

	# free space
	call mfree (hst, TY_SHORT)
	call mfree (inbuf, TY_INT)
end

# FMO_BOXSET -- Set up modal array for the beginning of each image line

procedure fmo_boxset (data, nx, ny, hist, nbins, line)

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

include "fmode.com"

begin
	# Initialize
	if (line == 1)  {

	    call smark (sp)
	    call salloc (filter, xbox * ybox, TY_INT)

	    # Load filter
	    index = 0
	    sum = 0.
	    do j = 1, ybox {
		do i = 1, xbox {
		    Memi[filter+index] = data[i,j]
		    sum = sum + data[i,j]
		    index = index + 1
		}
	    }

	    # Load histogram
	    call fmd_ashgmi (Memi[filter], xbox * ybox, hist, nbins, hmin, hmax)

	    # Calculate the sum and the current median
	    median = amedi (Memi[filter], xbox * ybox)

	    # Calculate the number less than the current median
	    nltmedian = 0
	    do i = 1, xbox * ybox {
		if (Memi[filter+i-1] < median)
		    nltmedian = nltmedian + 1
	    }

	    nmedian = (xbox * ybox - 1) / 2

	    call sfree (sp)

	} else {

	    # Add new points
	    if (mod (line, 2) == 0) {
	        do i = nx - xbox + 1, nx {
		    sum = sum + data[i,ny]
		    index = data[i, ny] - hmin + 1
		    hist[index] = hist[index] + 1
		    if (data[i,ny] < median)
		        nltmedian = nltmedian + 1
	        }
	    } else {
	        do i = 1, xbox {
		    sum = sum + data[i,ny]
		    index = data[i, ny] - hmin + 1
		    hist[index] = hist[index] + 1
		    if (data[i,ny] < median)
		        nltmedian = nltmedian + 1
	        }
	    }

	    # Calculate the new current median
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

# FMO_BOXFILTER -- Median filter a single image line

procedure fmo_boxfilter (data, nx, ny, modline, ncols, hist, nbins, line)

int	data[nx, ny]	# Image data
int	nx, ny		# Dimensions of data
real	modline[ncols]	# Median array
int	ncols		# Number of output image columns
short	hist[nbins]	# histogram
int	nbins		# length of histogram
int	line		# current line number

include	"fmode.com"

begin
	if (mod (line, 2) != 0)
	    call fmo_forward_filter (data, nx, ny, modline, ncols, hist, nbins)
	else
	    call fmo_rev_filter (data, nx, ny, modline, ncols, hist, nbins)
end

# FMO_FORWARD_FILTER -- Run the median window forward

procedure fmo_forward_filter (data, nx, ny, modline, ncols, hist, nbins)

int	data[nx,ny]		# buffer of image data
int	nx, ny			# dimensions of image buffer
real	modline[ncols]		# medians
int	ncols			# length of output image line
short	hist[nbins]		# histogram
int	nbins			# size of histogram

int	i, j, nbox, dindex, hindex

include	"fmode.com"

begin
	nbox = xbox * ybox

	# Calculate the medians for a line
	dindex = 1
	do i = 1, ncols - 1 {

	    # Set median
	    modline[i] = 3. * median - 2. * sum / nbox

	    # Delete points
	    do j = 1, ybox {
		sum = sum - data[dindex,j]
		hindex = data[dindex,j] - hmin + 1
		hist[hindex] = hist[hindex] - 1
		if (data[dindex,j] < median)
		    nltmedian = nltmedian - 1
	    }

	    # Add points
	    do j = 1, ybox {
		sum = sum + data[dindex+xbox,j]
		hindex = data[dindex+xbox,j] - hmin + 1
		hist[hindex] = hist[hindex] + 1
		if (data[dindex+xbox,j] < median)
		    nltmedian = nltmedian + 1
	    }

	    # Calculate the new current median
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

	# Set the last median
	modline[ncols] = 3. * median - 2. * sum / nbox

	# Delete the points from the last row
	do i = nx - xbox + 1, nx {
	    sum = sum - data[i,1]
	    hindex = data[i,1] - hmin + 1
	    hist[hindex] = hist[hindex] - 1
	    if (data[i,1] < median)
		nltmedian = nltmedian - 1
	}
end

# FMO_REV_FILTER -- Modalfilter the line in the reverse direction

procedure fmo_rev_filter (data, nx, ny, modline, ncols, hist, nbins)

int	data[nx,ny]		# buffer of image data
int	nx, ny			# dimensions of image buffer
real	modline[ncols]		# medians
int	ncols			# length of output image line
short	hist[nbins]		# histogram
int	nbins			# size of histogram

int	i, j, nbox, dindex, hindex

include "fmode.com"

begin
	nbox = xbox * ybox

	# Calculate the medians for a line
	dindex = nx
	do i = ncols, 2, -1 {

	    # Set median
	    modline[i] = 3. * median - 2. * sum / nbox

	    # Delete points
	    do j = 1, ybox {
		sum = sum - data[dindex,j]
		hindex = data[dindex,j] - hmin + 1
		hist[hindex] = hist[hindex] - 1
		if (data[dindex,j] < median)
		    nltmedian = nltmedian - 1
	    }

	    # Add points
	    do j = 1, ybox {
		sum = sum + data[dindex-xbox,j]
		hindex = data[dindex-xbox,j] - hmin + 1
		hist[hindex] = hist[hindex] + 1
		if (data[dindex-xbox,j] < median)
		    nltmedian = nltmedian + 1
	    }

	    # Calculate the new current median
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

	# Set the last median
	modline[1] = 3. * median - 2. * sum / nbox

	# Delete the points from the last row
	do i = 1, xbox {
	    sum = sum - data[i,1]
	    hindex = data[i,1] - hmin + 1
	    hist[hindex] = hist[hindex] - 1
	    if (data[i,1] < median)
		nltmedian = nltmedian - 1
	}
end
