# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imset.h>
include "fmode.h"

# FMD_MODBOX -- Modal filter an image.

procedure fmd_modbox (fmd, im1, im2, boundary, constant)

pointer	fmd		#I pointer to the fmedian structure
pointer	im1		#I pointer to the input image
pointer	im2		#I pointer to the output image
int	boundary	#I boundary extension type
real	constant	#I constant for constant boundary extension

size_t	sz_val, sz_val1
long	l_val
long	col1, col2, line, line1, line2
size_t	ncols, nlines
pointer	inbuf, outbuf, hst
real	rval
bool	fp_equalr()
pointer	impl2r()
errchk	impl2r, fmd_buf, fmd_modboxset, fmd_modboxfilter

begin
	# Set the image boundary extension parameters.
	call imseti (im1, IM_TYBNDRY, boundary)
	l_val = max (FMOD_XBOX(fmd) / 2, FMOD_YBOX(fmd)/ 2)
	call imsetl (im1, IM_NBNDRYPIX, l_val)
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Allocate space for the histogram and zero.
	sz_val = FMOD_HMAX(fmd) - FMOD_HMIN(fmd) + 1
	call calloc (hst, sz_val, TY_LONG)

	# Check for 1D images.
	if (IM_NDIM(im1) == 1)
	    FMOD_YBOX(fmd) = 1

	# Set quantization parameters.
	if (!IS_INDEFR(FMOD_Z1(fmd)))
	    FMOD_ZMIN(fmd) = FMOD_Z1(fmd)
	if (!IS_INDEFR(FMOD_Z2(fmd)))
	    FMOD_ZMAX(fmd) = FMOD_Z2(fmd)
	if (fp_equalr (real (FMOD_HMIN(fmd)), FMOD_ZMIN(fmd)) &&
	    fp_equalr (real (FMOD_HMAX(fmd)), FMOD_ZMAX(fmd)))
	    FMOD_MAP(fmd) = NO
	else
	    FMOD_MAP(fmd) = YES
	if (IS_INDEFR(FMOD_ZLOW(fmd))) {
	    FMOD_HLOW(fmd) = FMOD_HMIN(fmd)
	} else {
	    sz_val = 1
	    call amapr (FMOD_ZLOW(fmd), rval, sz_val, FMOD_ZMIN(fmd),
	        FMOD_ZMAX(fmd), real(FMOD_HMIN(fmd)), real(FMOD_HMAX(fmd)))
	    FMOD_HLOW(fmd) = rval
	}
	if (IS_INDEFR(FMOD_ZHIGH(fmd))) {
	    FMOD_HHIGH(fmd) = FMOD_HMAX(fmd)
	} else {
	    sz_val = 1
	    call amapr (FMOD_ZHIGH(fmd), rval, sz_val, FMOD_ZMIN(fmd),
	        FMOD_ZMAX(fmd), real(FMOD_HMIN(fmd)), real(FMOD_HMAX(fmd)))
	    FMOD_HHIGH(fmd) = rval
	}

	# Initialize input image buffer.
	inbuf = NULL
	col1 = 1 - FMOD_XBOX(fmd) / 2
	col2 = IM_LEN(im1, 1) + FMOD_XBOX(fmd) / 2 
	ncols = col2 - col1 + 1

	# Generate the output image line by line.
	do line = 1, IM_LEN(im2, 2) {

	    # Define the range of lines to read.
	    line1 = line - FMOD_YBOX(fmd) / 2
	    line2 = line + FMOD_YBOX(fmd) / 2
	    nlines = line2 - line1 + 1

	    # Read in the appropriate range of image lines.
	    call fmd_buf (im1, col1, col2, line1, line2, inbuf, FMOD_MAP(fmd),
	        FMOD_ZMIN(fmd), FMOD_ZMAX(fmd), real (FMOD_HMIN(fmd)),
		real (FMOD_HMAX(fmd)))

	    # Set up modal filter array for each line scanned.
	    sz_val = FMOD_HMAX(fmd) - FMOD_HMIN(fmd) + 1
	    call fmd_modboxset (fmd, Memi[inbuf], ncols, nlines, Meml[hst],
				sz_val, line)

	    # Get output image line.
	    outbuf = impl2r (im2, line)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Modal filter the image line.
	    sz_val = IM_LEN(im2, 1)
	    sz_val1 = FMOD_HMAX(fmd) - FMOD_HMIN(fmd) + 1
	    call fmd_modboxfilter (fmd, Memi[inbuf], ncols, nlines,
				Memr[outbuf], sz_val, Meml[hst], sz_val1, line)

	    # Recover original data range.
	    if (FMOD_UNMAP(fmd) == YES && FMOD_MAP(fmd) == YES) {
		sz_val = IM_LEN(im2,1)
		call amapr (Memr[outbuf], Memr[outbuf], sz_val,
		    real (FMOD_HMIN(fmd)), real (FMOD_HMAX(fmd)),
		    FMOD_ZMIN(fmd), FMOD_ZMAX(fmd))
	    }
	}

	# Free space.
	call mfree (hst, TY_LONG)
	call mfree (inbuf, TY_INT)
end


# FMD_MODBOXSET -- Set up median array for the beginning of each image line.

procedure fmd_modboxset (fmd, data, nx, ny, hist, nbins, line)

pointer fmd			#I pointer to the fmode structure
int	data[nx, ny]		#I image data buffer
size_t	nx			#I number of columns in image buffer
size_t	ny			#I number of lines in the image buffer
long	hist[nbins]		#U histogram
size_t	nbins			#I number of histogram bins
long	line			#I line number

size_t	sz_val
long	l_val
size_t	xbox, ybox, index
long	nmedian, nltmedian, nzero, i, j
int	hmin, hmax, hlo, hhi, nhlo, nhhi, median
pointer	sp, filter
real	sum
int	amedi()
long	lmod()

begin
	xbox = FMOD_XBOX(fmd)
	ybox = FMOD_YBOX(fmd)
	hmin = FMOD_HMIN(fmd)
	hmax = FMOD_HMAX(fmd)
	hlo = FMOD_HLOW(fmd)
	hhi = FMOD_HHIGH(fmd)

	# Initialize.
	if (line == 1)  {

	    call smark (sp)
	    sz_val = xbox * ybox
	    call salloc (filter, sz_val, TY_INT)

	    # Load filter.
	    index = 0
	    nhlo = 0
	    nhhi = 0
	    sum = 0.0
	    do j = 1, ybox {
		do i = 1, xbox {
		    if (data[i,j] < hlo) {
			nhlo = nhlo + 1
			next
		    }
		    if (data[i,j] > hhi) {
			nhhi = nhhi + 1
			next
		    }
		    Memi[filter+index] = data[i,j]
		    sum = sum + data[i,j]
		    index = index + 1
		}
	    }

	    # Load histogram.
	    if (index > 0)
	        call fmd_ashgmi (Memi[filter], index, hist, nbins, hmin, hmax)

	    # Calculate the current median.
	    if (index > 0)
	        median = amedi (Memi[filter], index)
	    else if (nhlo < nhhi)
		median = hhi
	    else
		median = hlo

	    # Calculate the number less than the current median.
	    nltmedian = 0
	    if (index > 0) {
	        nltmedian = 0
	        do i = 1, index {
		    if (Memi[filter+i-1] < median)
		        nltmedian = nltmedian + 1
	        }
	        nmedian = (index - 1) / 2
	    } else
		nmedian = 0

	    call sfree (sp)

	} else {

	    median = FMOD_MEDIAN(fmd)
	    nltmedian = FMOD_NLTMEDIAN(fmd)
	    nmedian = FMOD_NMEDIAN(fmd)
	    sum = FMOD_SUM(fmd)
	    nhlo = FMOD_NHLOW(fmd)
	    nhhi = FMOD_NHHIGH(fmd)

	    # Add new points.
	    l_val = 2
	    if (lmod (line, l_val) == 0) {
	        do i = nx - xbox + 1, nx {
		    if (data[i,ny] < hlo) {
			nhlo = nhlo + 1
			next
		    }
		    if (data[i,ny] > hhi) {
			nhhi = nhhi + 1
			next
		    }
		    sum = sum + data[i,ny]
		    index = data[i,ny] - hmin + 1
		    hist[index] = hist[index] + 1
		    if (data[i,ny] < median)
		        nltmedian = nltmedian + 1
	        }
	    } else {
	        do i = 1, xbox {
		    if (data[i,ny] < hlo) {
			nhlo = nhlo + 1
			next
		    }
		    if (data[i,ny] > hhi) {
			nhhi = nhhi + 1
			next
		    }
		    sum = sum + data[i,ny]
		    index = data[i,ny] - hmin + 1
		    hist[index] = hist[index] + 1
		    if (data[i,ny] < median)
		        nltmedian = nltmedian + 1
	        }
	    }
	    nzero = xbox * ybox - nhlo - nhhi
	    if (nzero > 0)
		nmedian = (nzero - 1) / 2
	    else
		nmedian = 0

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

	# Store the results.
	FMOD_MEDIAN(fmd) = median
	FMOD_NMEDIAN(fmd) = nmedian
	FMOD_NLTMEDIAN(fmd) = nltmedian
	FMOD_NHLOW(fmd) = nhlo
	FMOD_NHHIGH(fmd) = nhhi
	FMOD_SUM(fmd) = sum
end


# FMD_MODBOXFILTER -- Median filter a single image line.

procedure fmd_modboxfilter (fmd, data, nx, ny, medline, ncols, hist,
	nbins, line)

pointer	fmd		#I pointer to the fmode structure
int	data[nx, ny]	#I image data
size_t	nx, ny		#I dimensions of data
real	medline[ncols]	#O median array
size_t	ncols		#I number of output image columns
long	hist[nbins]	#U histogram
size_t	nbins		#I length of histogram
long	line		#I current line number

long	l_val
long	lmod()

begin
	l_val = 2
	if (lmod (line, l_val) != 0)
	    call fmd_oforward_filter (fmd, data, nx, ny, medline, ncols,
	        hist, nbins)
	else
	    call fmd_orev_filter (fmd, data, nx, ny, medline, ncols,
	        hist, nbins)
end


# FMD_OFORWARD_FILTER -- Run the median window forward.

procedure fmd_oforward_filter (fmd, data, nx, ny, medline, ncols, hist, nbins)

pointer	fmd			#I pointer to the fmode structure
int	data[nx,ny]		#I buffer of image data
size_t	nx, ny			#I dimensions of image buffer
real	medline[ncols]		#O medians
size_t	ncols			#I length of output image line
long	hist[nbins]		#U histogram
size_t	nbins			#I size of histogram

size_t	xbox, ybox
long	nmedian, nltmedian, nzero, dindex, i, j
int	median, hmin, hmax, hindex, hlo, hhi, nhlo, nhhi
real	sum

begin
	xbox = FMOD_XBOX(fmd)
	ybox = FMOD_YBOX(fmd)
	hmin = FMOD_HMIN(fmd)
	hmax = FMOD_HMAX(fmd)
	hlo = FMOD_HLOW(fmd)
	hhi = FMOD_HHIGH(fmd)

	median = FMOD_MEDIAN(fmd)
	nmedian = FMOD_NMEDIAN(fmd)
	nltmedian = FMOD_NLTMEDIAN(fmd)
	sum = FMOD_SUM(fmd)
	nhlo = FMOD_NHLOW(fmd)
	nhhi = FMOD_NHHIGH(fmd)

	# Calculate the medians for a line.
	dindex = 1
	do i = 1, ncols - 1 {

	    # Set median.
	    nzero = xbox * ybox - nhlo - nhhi
	    if (nzero > 0)
	        medline[i] = 3.0 * median - 2.0 * sum / nzero
	    else if (nhlo < nhhi)
	        medline[i] = hhi
	    else
	        medline[i] = hlo

	    # Delete points.
	    do j = 1, ybox {
		if (data[dindex,j] < hlo) {
		    nhlo = nhlo - 1
		    next
		}
		if (data[dindex,j] > hhi) {
		    nhhi = nhhi - 1
		    next
		}
		sum = sum - data[dindex,j]
		hindex = data[dindex,j] - hmin + 1
		hist[hindex] = hist[hindex] - 1
		if (data[dindex,j] < median)
		    nltmedian = nltmedian - 1
	    }

	    # Add points.
	    do j = 1, ybox {
		if (data[dindex+xbox,j] < hlo) {
		    nhlo = nhlo + 1
		    next
		}
		if (data[dindex+xbox,j] > hhi) {
		    nhhi = nhhi + 1
		    next
		}
		sum = sum + data[dindex+xbox,j]
		hindex = data[dindex+xbox,j] - hmin + 1
		hist[hindex] = hist[hindex] + 1
		if (data[dindex+xbox,j] < median)
		    nltmedian = nltmedian + 1
	    }

	    nzero = xbox * ybox - nhlo - nhhi
	    if (nzero > 0)
		nmedian = (nzero - 1) / 2
	    else
		nmedian = 0

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
	nzero = xbox * ybox - nhlo - nhhi
	if (nzero > 0)
	    medline[ncols] = 3.0 * median - 2.0 * sum / nzero
	else if (nhlo < nhhi)
	    medline[ncols] = hhi
	else
	    medline[ncols] = hlo

	# Delete the points from the last row.
	do i = nx - xbox + 1, nx {
	    if (data[i,1] < hlo) {
		nhlo = nhlo - 1
		next
	    }
	    if (data[i,1] > hhi) {
		nhhi = nhhi - 1
		next
	    }
	    sum = sum - data[i,1]
	    hindex = data[i,1] - hmin + 1
	    hist[hindex] = hist[hindex] - 1
	    if (data[i,1] < median)
		nltmedian = nltmedian - 1
	}

	nzero = xbox * ybox - nhlo - nhhi
	if (nzero > 0)
	    nmedian = (nzero - 1) / 2
	else
	    nmedian = 0

	FMOD_SUM(fmd) = sum
	FMOD_MEDIAN(fmd) = median
	FMOD_NMEDIAN(fmd) = nmedian
	FMOD_NLTMEDIAN(fmd) = nltmedian
	FMOD_NHLOW(fmd) = nhlo
	FMOD_NHHIGH(fmd) = nhhi
end


# FMD_OREV_FILTER -- Median filter the line in the reverse direction.

procedure fmd_orev_filter (fmd, data, nx, ny, medline, ncols, hist, nbins)

pointer	fmd			#I pointer to the fmode structure
int	data[nx,ny]		#I buffer of image data
size_t	nx, ny			#I dimensions of image buffer
real	medline[ncols]		#O medians
size_t	ncols			#I length of output image line
long	hist[nbins]		#U histogram
size_t	nbins			#I size of histogram

size_t	xbox, ybox
long	dindex, nmedian, nltmedian, nzero, i, j
int	hmin, hmax, hindex, hlo, hhi, nhlo, nhhi, median
real	sum

begin
	xbox = FMOD_XBOX(fmd)
	ybox = FMOD_YBOX(fmd)
	hmin = FMOD_HMIN(fmd)
	hmax = FMOD_HMAX(fmd)
	hlo = FMOD_HLOW(fmd)
	hhi = FMOD_HHIGH(fmd)

	sum = FMOD_SUM(fmd)
	median = FMOD_MEDIAN(fmd)
	nmedian = FMOD_NMEDIAN(fmd)
	nltmedian = FMOD_NLTMEDIAN(fmd)
	nhlo = FMOD_NHLOW(fmd)
	nhhi = FMOD_NHHIGH(fmd)

	# Calculate the medians for a line.
	dindex = nx
	do i = ncols, 2, -1 {

	    # Set median.
	    nzero = xbox * ybox - nhlo - nhhi
	    if (nzero > 0)
	        medline[i] = 3.0 * median - 2.0 * sum / nzero
	    else if (nhlo < nhhi)
	        medline[i] = hhi
	    else
	        medline[i] = hlo

	    # Delete points.
	    do j = 1, ybox {
		if (data[dindex,j] < hlo) {
		    nhlo = nhlo - 1
		    next
		}
		if (data[dindex,j] > hhi) {
		    nhhi = nhhi - 1
		    next
		}
		sum = sum - data[dindex,j]
		hindex = data[dindex,j] - hmin + 1
		hist[hindex] = hist[hindex] - 1
		if (data[dindex,j] < median)
		    nltmedian = nltmedian - 1
	    }

	    # Add points.
	    do j = 1, ybox {
		if (data[dindex-xbox,j] < hlo) {
		    nhlo = nhlo + 1
		    next
		}
		if (data[dindex-xbox,j] > hhi) {
		    nhhi = nhhi + 1
		    next
		}
		sum = sum + data[dindex-xbox,j]
		hindex = data[dindex-xbox,j] - hmin + 1
		hist[hindex] = hist[hindex] + 1
		if (data[dindex-xbox,j] < median)
		    nltmedian = nltmedian + 1
	    }

	    nzero = xbox * ybox - nhlo - nhhi
	    if (nzero > 0)
		nmedian = (nzero - 1) / 2
	    else
		nmedian = 0

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
	nzero = xbox * ybox - nhlo - nhhi
	if (nzero > 0)
	    medline[1] = 3.0 * median - 2.0 * sum / nzero
	else if (nhlo < nhhi)
	    medline[1] = hhi
	else
	    medline[1] = hlo

	# Delete the points from the last row.
	do i = 1, xbox {
	    if (data[i,1] < hlo) {
		nhlo = nhlo - 1
		next
	    }
	    if (data[i,1] > hhi) {
		nhhi = nhhi - 1
		next
	    }
	    sum = sum - data[i,1]
	    hindex = data[i,1] - hmin + 1
	    hist[hindex] = hist[hindex] - 1
	    if (data[i,1] < median)
		nltmedian = nltmedian - 1
	}

	nzero = xbox * ybox - nhlo - nhhi
	if (nzero > 0)
	    nmedian = (nzero - 1) / 2
	else
	    nmedian = 0

	FMOD_SUM(fmd) = sum
	FMOD_MEDIAN(fmd) = median
	FMOD_NMEDIAN(fmd) = nmedian
	FMOD_NLTMEDIAN(fmd) = nltmedian
	FMOD_NHLOW(fmd) = nhlo
	FMOD_NHHIGH(fmd) = nhhi
end
