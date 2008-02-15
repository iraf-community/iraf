# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imhdr.h>
include <imset.h>
include "fmedian.h"

# FMD_MEDBOX -- Median filter an image.

procedure fmd_medbox (fmd, im1, im2, boundary, constant)

pointer	fmd		#I pointer to the fmedian structure
pointer	im1		#I pointer to the input image
pointer	im2		#I pointer to the output image
int	boundary	#I boundary extension type
real	constant	#I constant for constant boundary extension

int	col1, col2, ncols, line, line1, line2, nlines
pointer	inbuf, outbuf, hst
real	rval
bool	fp_equalr()
pointer	impl2r()
errchk	impl2r, fmd_buf, fmd_medboxset, fmd_medboxfilter

begin
	# Set the image boundary extension parameters.
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (FMED_XBOX(fmd) / 2,
	    FMED_YBOX(fmd)/ 2))
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Allocate space for the histogram and zero.
	call calloc (hst, FMED_HMAX(fmd) - FMED_HMIN(fmd) + 1, TY_INT)

	# Check for 1D images.
	if (IM_NDIM(im1) == 1)
	    FMED_YBOX(fmd) = 1

	# Set quantization parameters.
	if (!IS_INDEFR(FMED_Z1(fmd)))
	    FMED_ZMIN(fmd) = FMED_Z1(fmd)
	if (!IS_INDEFR(FMED_Z2(fmd)))
	    FMED_ZMAX(fmd) = FMED_Z2(fmd)
	if (fp_equalr (real (FMED_HMIN(fmd)), FMED_ZMIN(fmd)) &&
	    fp_equalr (real (FMED_HMAX(fmd)), FMED_ZMAX(fmd)))
	    FMED_MAP(fmd) = NO
	else
	    FMED_MAP(fmd) = YES
	if (IS_INDEFR(FMED_ZLOW(fmd))) {
	    FMED_HLOW(fmd) = FMED_HMIN(fmd)
	} else {
	    call amapr (FMED_ZLOW(fmd), rval, 1, FMED_ZMIN(fmd),
	        FMED_ZMAX(fmd), real(FMED_HMIN(fmd)), real(FMED_HMAX(fmd)))
	    FMED_HLOW(fmd) = rval
	}
	if (IS_INDEFR(FMED_ZHIGH(fmd))) {
	    FMED_HHIGH(fmd) = FMED_HMAX(fmd)
	} else {
	    call amapr (FMED_ZHIGH(fmd), rval, 1, FMED_ZMIN(fmd),
	        FMED_ZMAX(fmd), real(FMED_HMIN(fmd)), real(FMED_HMAX(fmd)))
	    FMED_HHIGH(fmd) = rval
	}

	# Initialize input image buffer.
	inbuf = NULL
	col1 = 1 - FMED_XBOX(fmd) / 2
	col2 = IM_LEN(im1, 1) + FMED_XBOX(fmd) / 2 
	ncols = col2 - col1 + 1

	# Generate the output image line by line.
	do line = 1, IM_LEN(im2, 2) {

	    # Define the range of lines to read.
	    line1 = line - FMED_YBOX(fmd) / 2
	    line2 = line + FMED_YBOX(fmd) / 2
	    nlines = line2 - line1 + 1

	    # Read in the appropriate range of image lines.
	    call fmd_buf (im1, col1, col2, line1, line2, inbuf, FMED_MAP(fmd),
	        FMED_ZMIN(fmd), FMED_ZMAX(fmd), real (FMED_HMIN(fmd)),
		real (FMED_HMAX(fmd)))

	    # Set up median filter array for each line scanned.
	    call fmd_medboxset (fmd, Memi[inbuf], ncols, nlines, Memi[hst],
		FMED_HMAX(fmd) - FMED_HMIN(fmd) + 1, line)

	    # Get output image line.
	    outbuf = impl2r (im2, line)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Median filter the image line.
	    call fmd_medboxfilter (fmd, Memi[inbuf], ncols, nlines,
	        Memr[outbuf], int (IM_LEN(im2, 1)), Memi[hst],
		FMED_HMAX(fmd) - FMED_HMIN(fmd) + 1, line)

	    # Recover original data range.
	    if (FMED_UNMAP(fmd) == YES && FMED_MAP(fmd) == YES)
		call amapr (Memr[outbuf], Memr[outbuf], int (IM_LEN(im2,1)),
		    real (FMED_HMIN(fmd)), real (FMED_HMAX(fmd)),
		    FMED_ZMIN(fmd), FMED_ZMAX(fmd))
	}

	# Free space.
	call mfree (hst, TY_INT)
	call mfree (inbuf, TY_INT)
end


# FMD_MEDBOXSET -- Set up median array for the beginning of each image line.

procedure fmd_medboxset (fmd, data, nx, ny, hist, nbins, line)

pointer fmd				#I pointer to the fmedian structure
int	data[nx, ny]			#I image data buffer
int	nx				#I number of columns in image buffer
int	ny				#I number of lines in the image buffer
int	hist[nbins]			#U histogram
int	nbins				#I number of histogram bins
int	line				#I line number

int	i, j, xbox, ybox, hmin, hmax, hlo, hhi, nhlo, nhhi, index
int	median, nmedian, nltmedian, nzero
pointer	sp, filter
int	amedi()

begin
	xbox = FMED_XBOX(fmd)
	ybox = FMED_YBOX(fmd)
	hmin = FMED_HMIN(fmd)
	hmax = FMED_HMAX(fmd)
	hlo = FMED_HLOW(fmd)
	hhi = FMED_HHIGH(fmd)

	# Initialize.
	if (line == 1)  {

	    call smark (sp)
	    call salloc (filter, xbox * ybox, TY_INT)

	    # Load filter.
	    index = 0
	    nhlo = 0
	    nhhi = 0
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

	    median = FMED_MEDIAN(fmd)
	    nltmedian = FMED_NLTMEDIAN(fmd)
	    nmedian = FMED_NMEDIAN(fmd)
	    nhlo = FMED_NHLOW(fmd)
	    nhhi = FMED_NHHIGH(fmd)

	    # Add new points.
	    if (mod (line, 2) == 0) {
	        do i = nx - xbox + 1, nx {
		    if (data[i,ny] < hlo) {
			nhlo = nhlo + 1
			next
		    }
		    if (data[i,ny] > hhi) {
			nhhi = nhhi + 1
			next
		    }
		    index = data[i, ny] - hmin + 1
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
		    index = data[i, ny] - hmin + 1
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
	FMED_MEDIAN(fmd) = median
	FMED_NMEDIAN(fmd) = nmedian
	FMED_NLTMEDIAN(fmd) = nltmedian
	FMED_NHLOW(fmd) = nhlo
	FMED_NHHIGH(fmd) = nhhi
end


# FMD_MEDBOXFILTER -- Median filter a single image line.

procedure fmd_medboxfilter (fmd, data, nx, ny, medline, ncols, hist,
	nbins, line)

pointer	fmd		#I pointer to the fmedian structure
int	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	medline[ncols]	#O median array
int	ncols		#I number of output image columns
int	hist[nbins]	#U histogram
int	nbins		#I length of histogram
int	line		#I current line number

begin
	if (mod (line, 2) != 0)
	    call fmd_eforward_filter (fmd, data, nx, ny, medline, ncols,
	        hist, nbins)
	else
	    call fmd_erev_filter (fmd, data, nx, ny, medline, ncols,
	        hist, nbins)
end


# FMD_EFORWARD_FILTER -- Run the median window forward.

procedure fmd_eforward_filter (fmd, data, nx, ny, medline, ncols, hist, nbins)

pointer	fmd			#I pointer to the fmedian structure
int	data[nx,ny]		#I buffer of image data
int	nx, ny			#I dimensions of image buffer
real	medline[ncols]		#O medians
int	ncols			#I length of output image line
int	hist[nbins]		#U histogram
int	nbins			#I size of histogram

int	i, j, xbox, ybox, dindex, hmin, hmax, hindex, hlo, hhi, nhlo, nhhi
int	median, nmedian, nltmedian, nzero

begin
	xbox = FMED_XBOX(fmd)
	ybox = FMED_YBOX(fmd)
	hmin = FMED_HMIN(fmd)
	hmax = FMED_HMAX(fmd)
	hlo = FMED_HLOW(fmd)
	hhi = FMED_HHIGH(fmd)

	median = FMED_MEDIAN(fmd)
	nmedian = FMED_NMEDIAN(fmd)
	nltmedian = FMED_NLTMEDIAN(fmd)
	nhlo = FMED_NHLOW(fmd)
	nhhi = FMED_NHHIGH(fmd)

	# Calculate the medians for a line.
	dindex = 1
	do i = 1, ncols - 1 {

	    # Set median.
	    if ((xbox * ybox - nhlo - nhhi) > 0)
	        medline[i] = median
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
	if ((xbox * ybox - nhlo - nhhi) > 0)
	    medline[ncols] = median
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

	FMED_MEDIAN(fmd) = median
	FMED_NMEDIAN(fmd) = nmedian
	FMED_NLTMEDIAN(fmd) = nltmedian
	FMED_NHLOW(fmd) = nhlo
	FMED_NHHIGH(fmd) = nhhi
end


# FMD_EREV_FILTER -- Median filter the line in the reverse direction.

procedure fmd_erev_filter (fmd, data, nx, ny, medline, ncols, hist, nbins)

pointer	fmd			#I pointer to the fmedian structure
int	data[nx,ny]		#I buffer of image data
int	nx, ny			#I dimensions of image buffer
real	medline[ncols]		#O medians
int	ncols			#I length of output image line
int	hist[nbins]		#U histogram
int	nbins			#I size of histogram

int	i, j, xbox, ybox, dindex, hmin, hmax, hindex, hlo, hhi, nhlo, nhhi
int	median, nmedian, nltmedian, nzero

begin
	xbox = FMED_XBOX(fmd)
	ybox = FMED_YBOX(fmd)
	hmin = FMED_HMIN(fmd)
	hmax = FMED_HMAX(fmd)
	hlo = FMED_HLOW(fmd)
	hhi = FMED_HHIGH(fmd)

	median = FMED_MEDIAN(fmd)
	nmedian = FMED_NMEDIAN(fmd)
	nltmedian = FMED_NLTMEDIAN(fmd)
	nhlo = FMED_NHLOW(fmd)
	nhhi = FMED_NHHIGH(fmd)

	# Calculate the medians for a line.
	dindex = nx
	do i = ncols, 2, -1 {

	    # Set median.
	    if ((xbox * ybox - nhlo - nhhi) > 0)
	        medline[i] = median
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
	if ((xbox * ybox - nhlo - nhhi) > 0)
	    medline[1] = median
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

	FMED_MEDIAN(fmd) = median
	FMED_NMEDIAN(fmd) = nmedian
	FMED_NLTMEDIAN(fmd) = nltmedian
	FMED_NHLOW(fmd) = nhlo
	FMED_NHHIGH(fmd) = nhhi
end
