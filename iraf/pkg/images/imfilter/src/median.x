# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imset.h>
include <imhdr.h>
include "median.h"

# MDE_MEDBOX -- Median filter an image using a rectangular window.

procedure mde_medbox (mde, im1, im2, boundary, constant)

pointer	mde		#I pointer to the median structure
pointer	im1		#I pointer to the input image
pointer	im2		#I pointer to the output image
int	boundary	#I boundary extension type
real	constant	#I constant for constant boundary extension

int	col1, col2, ncols, line, line1, line2
pointer	filter, left, right, inbuf, outbuf
pointer	impl2r()
errchk	impl2r, med_buf, mde_medboxset, mde_xefilter, mde_yefilter
errchk	med_boxfilter

begin
	# Check for 1D images.
	if (IM_NDIM(im1) == 1)
	    MED_YBOX(mde) = 1

	# Set the median filtering buffers.
	call calloc (filter, MED_XBOX(mde) * MED_YBOX(mde) + 1, TY_REAL)
	call calloc (left, MED_XBOX(mde) * MED_YBOX(mde), TY_INT)
	call calloc (right, MED_XBOX(mde) * MED_YBOX(mde), TY_INT)

	# Set the input image boundary extension parameters.
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (MED_XBOX(mde) / 2,
	    MED_YBOX(mde) / 2))
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Set the line buffer parameters.
	inbuf = NULL
	col1 = 1 - MED_XBOX(mde) / 2
	col2 = IM_LEN(im1, 1) + MED_XBOX(mde) / 2 
	ncols = col2 - col1 + 1

	# Generate the output image line by line.
	do line = 1, IM_LEN(im2, 2) {

	    # Get ybox image lines
	    line1 = line - MED_YBOX(mde) / 2
	    line2 = line + MED_YBOX(mde) / 2

	    # Read in the appropriate range of image lines.
	    call med_buf (im1, col1, col2, line1, line2, inbuf)

	    # Set up median filter array for each image line.
	    call mde_medboxset (mde, Memr[inbuf], ncols, MED_YBOX(mde),
	        Memr[filter], Memi[left], Memi[right], line)

	    # Get the output image line.
	    outbuf = impl2r (im2, line)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Median filter the image line.
	    if (MED_XBOX(mde) == 1)
		call mde_yefilter (mde, Memr[inbuf], ncols, MED_YBOX(mde),
		    Memr[filter], Memr[outbuf], int (IM_LEN(im2,1)))
	    else if (MED_YBOX(mde) == 1)
		call mde_xefilter (mde, Memr[inbuf], ncols, MED_YBOX(mde),
		    Memr[outbuf], int (IM_LEN(im2,1)), Memr[filter],
		    Memi[left], Memi[right])
	    else
	        call mde_medboxfilter (mde, Memr[inbuf], ncols, MED_YBOX(mde),
		    Memr[outbuf], int (IM_LEN(im2, 1)), Memr[filter],
		    Memi[left], Memi[right], line)
	}

	# Free the image and filter buffers.
	call mfree (inbuf, TY_REAL)
	call mfree (filter, TY_REAL)
	call mfree (left, TY_INT)
	call mfree (right, TY_INT)
end


# MDE_MEDBOXSET -- Set up median array for the beginning of each image line
# The image is raster scanned so that the direction of scanning changes
# for each line. Odd numbered lines are scanned forwards and even numbered
# lines are scanned backward. If the median filters is one dimensional
# the lines are scanned forward.

procedure mde_medboxset (mde, data, nx, ny, filter, left, right, line)

pointer	mde				#I pointer to the median structure
real	data[nx, ny]			#I image data buffer
int	nx				#I number of columns in image buffer
int	ny				#I number of lines in the image buffer
real	filter[ARB]			#U array of elements to be sorted
int	left[ARB]			#U array of back pointers
int	right[ARB]			#U array of forward pointers
int	line				#I line number

int	i, j, k, l, xbox, ybox, nlo, nhi, npts, nptsp1, start, finish, mp
pointer	sp, insert, index
real	zlo, zhi

begin
	# Get algorithm parameters.
	xbox = MED_XBOX(mde)
	ybox = MED_YBOX(mde)
	zlo = MED_ZLOW(mde)
	zhi = MED_ZHIGH(mde)

	call smark (sp)

	# Initialize.
	if (xbox == 1) {

	    npts = 0
	    nlo = 0
	    nhi = 0

	    do i = 1, ybox {
		if (data[1,i] < zlo) {
		    nlo = nlo + 1
		    next
		}
		if (data[1,i] > zhi) {
		    nhi = nhi + 1
		    next
		}
		npts = npts + 1
		filter[npts] = data[1,i]
	    }
	    if (npts > 0)
	        call med_ashsrt (filter, npts)

	    nptsp1 = npts + 1
	    mp = 1

	} else if (line == 1 || ybox == 1) {
	    
	    npts = xbox * ybox
	    nptsp1 = npts + 1
	    mp = 1

	    call salloc (index, npts, TY_INT)

	    # Load the filter kernel.
	    nlo = 0
	    nhi = 0
	    k = 1
	    do i = 1, xbox {
		do j = 1, ybox {
		    if (data[i,j] < zlo)
			nlo = nlo + 1
		    if (data[i,j] > zhi)
			nhi = nhi + 1
		    filter[k] = data[i,j]
		    Memi[index+k-1] = k
		    k = k + 1
		}
	    }

	    # Sort the initial filter kernel index array.
	    call med_gshsrt (filter, Memi[index], npts)

	    # Set up the sorted linked list parameters.
	    start = Memi[index]
	    finish = Memi[index+npts-1]
	    left[start] = 0
	    do i = 2, npts
		left[Memi[index+i-1]] = Memi[index+i-2]
	    do i = 1, npts - 1
		right[Memi[index+i-1]] = Memi[index+i]
	    right[finish] = npts + 1

	} else if (mod (line, 2) == 1) {

	    npts = MED_NPTS(mde)
	    nptsp1 = MED_NPTSP1(mde)
	    mp = MED_MP(mde)
	    start = MED_START(mde)
	    finish = MED_FINISH(mde)
	    nlo = MED_NLOW(mde)
	    nhi = MED_NHIGH(mde)

	    call salloc (index, xbox, TY_INT)
	    call salloc (insert, xbox, TY_REAL)

	    # Xbox elements are deleted when lines are changed.
	    # These elements are always located in the first
	    # column of the filter kernel.
	    do i = 1, npts, ybox {
	       if (filter[i] < zlo)
		    nlo = nlo - 1
	        if (filter[i] > zhi)
	            nhi = nhi - 1
		if (i == start) {
		    start = right[i]
		    left[right[i]] = 0
		} else if (i == finish) {
		    finish = left[i]
		    right[left[i]] = nptsp1
		} else {
		    left[right[i]] = left[i]
		    right[left[i]] = right[i]
		}
	    }

	    # Read in the new points.
	    do i = 1, xbox {
	        if (data[i,ny] < zlo)
		    nlo = nlo + 1
		if (data[i,ny] > zhi)
		    nhi = nhi + 1
		Memr[insert+i-1] = data[i,ny]
		Memi[index+i-1] = i
	    }

	    # Sort the new points.
	    call med_gshsrt (Memr[insert], Memi[index], xbox)

	    # Adjust the median pointer.
	    mp = mp + ybox
	    if (mp > npts)
		mp = 1

	    j = start
	    do i = 1, xbox {

		# Insert the new point into the filter kernel.
		l = Memi[index+i-1]
		k = mod (mp + (l - 1) * ybox, npts)
		filter[k] = Memr[insert+l-1]

		# Find the element to the right of the inserted point.
		while (j != right[finish] && Memr[insert+l-1] > filter[j])
		    j = right[j]

		# Make insertions by adjusting the forward and backward links.
		if (j == start) {
		    left[start] = k
		    left[k] = 0
		    right[k] = start
		    start = k
		} else if (j == right[finish]) {
		    right[finish] = k
		    left[k] = finish
		    right[k] = npts + 1
		    finish = k
		} else {
		    left[k] = left[j]
		    right[k] = right[left[j]]
		    right[left[j]] = k
		    left[j] = k
		}
	    }

	} else {
	    
	    npts = MED_NPTS(mde)
	    nptsp1 = MED_NPTSP1(mde)
	    mp = MED_MP(mde)
	    start = MED_START(mde)
	    finish = MED_FINISH(mde)
	    nlo = MED_NLOW(mde)
	    nhi = MED_NHIGH(mde)

	    call salloc (index, xbox, TY_INT)
	    call salloc (insert, xbox, TY_REAL)

	    # Xbox elements are deleted when lines are changed.
	    # These elements are always located in the first
	    # column of the filter kernel.
	    do i = 1, npts, ybox {
	        if (filter[i] < zlo)
	            nlo = nlo - 1
		if (filter[i] > zhi)
		    nhi = nhi - 1
		if (i == start) {
		    start = right[i]
		    left[right[i]] = 0
		} else if (i == finish) {
		    finish = left[i]
		    right[left[i]] = nptsp1
		} else {
		    left[right[i]] = left[i]
		    right[left[i]] = right[i]
		}
	    }

	    # Find points to be inserted.
	    j = nx - xbox + 1 
	    do i = 1, xbox {
	        if (data[j,ny] < zlo)
		    nlo = nlo + 1
		if (data[j,ny] > zhi)
		    nhi = nhi + 1
		Memr[insert+i-1] = data[j,ny]
		Memi[index+i-1] = i
		j = j + 1
	    }

	    # Sort the new points.
	    call med_gshsrt (Memr[insert], Memi[index], xbox)

	    # Do a merge sort of the old and new points.
	    j = start
	    do i = 1, xbox {

		# Insert the new point into the filter kernel
		l = Memi[index+i-1]
		k = mod (mp + (l - 1) * ybox, npts)
		filter[k] = Memr[insert+l-1]

		# Find the element to the right of the inserted point
		while (j != right[finish] && Memr[insert+l-1] > filter[j])
		    j = right[j]

		# Make insertions by adjusting the forward and backward links
		if (j == start) {
		    left[start] = k
		    left[k] = 0
		    right[k] = start
		    start = k
		} else if (j == right[finish]) {
		    right[finish] = k
		    left[k] = finish
		    right[k] = npts + 1
		    finish = k
		} else {
		    left[k] = left[j]
		    right[k] = right[left[j]]
		    right[left[j]] = k
		    left[j] = k
		}
	    }

	    # Adjust the filter kernel pointer for backscanned lines
	    mp = mp - ybox
	    if (mp < 1)
		mp = npts + mp
	}

	MED_NPTS(mde) = npts
	MED_NPTSP1(mde) = nptsp1
	MED_MP(mde) = mp
	MED_START(mde) = start
	MED_FINISH(mde) = finish
	MED_NLOW(mde) = nlo
	MED_NHIGH(mde) = nhi

	call sfree (sp)
end


# MDE_MEDBOXFILTER -- Median filter a single image line.

procedure mde_medboxfilter (mde, data, nx, ny, median, ncols, filter, left,
	right, line)

pointer	mde		#I pointer to the median structure
real	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	median[ncols]	#O median array
int	ncols		#I number of output image columns
real	filter[ARB]	#U the median array of points to be filtered
int	left[ARB]	#U the array of back pointers
int	right[ARB]	#U the array of forward pointers
int	line		#I current line number

begin
	if (mod (line, 2) == 0)
	    call mde_ereverse_boxfilter (mde, data, nx, ny, median, ncols,
		filter, left, right)
	else
	    call mde_eforward_boxfilter (mde, data, nx, ny, median, ncols,
	        filter, left, right)
end


# MDE_EFORWARD_BOXFILTER -- Median filter a single image line

procedure mde_eforward_boxfilter (mde, data, nx, ny, median, ncols,
	filter, left, right)

pointer	mde		#I pointer to the median filtering structure
real	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	median[ncols]	#O median array
int	ncols		#I number of output image columns
real	filter[ARB]	#U the array of points to be filtered
int	left[ARB]	#U the array of back pointers
int	right[ARB]	#U the array of forward pointers

int	i, j, k, l, col, nzero, nhalf, xbox, ybox, npts, nptsp1
int	nlo, nhi, start, finish, mp
real	zlo, zhi
pointer	sp, index


begin
	xbox = MED_XBOX(mde)
	ybox = MED_YBOX(mde)
	zlo = MED_ZLOW(mde)
	zhi = MED_ZHIGH(mde)
	npts = MED_NPTS(mde)
	nptsp1 = MED_NPTSP1(mde)

	start = MED_START(mde)
	finish = MED_FINISH(mde)
	mp = MED_MP(mde)
	nlo = MED_NLOW(mde)
	nhi = MED_NHIGH(mde)

	call smark (sp)
	call salloc (index, ybox, TY_INT)

	col = 1 + xbox
	do i = 1, ncols - 1 {

	    # Calculate the median
	    k = start
	    nzero = npts - nlo - nhi
	    nhalf = (nzero - 1) / 2
	    do j = 1, nlo + nhalf
	        k = right[k]
	    if (nzero > 0)
	        median[i] = filter[k]
	    else if (nlo < nhi)
		median[i] = zhi
	    else
		median[i] = zlo

	    # Delete points.
	    do j = mp, mp + ybox - 1 {

		if (filter[j] < zlo)
		    nlo = nlo - 1
		if (filter[j] > zhi)
		    nhi = nhi - 1

		if (j == start) {
		    start = right[j]
		    left[right[j]] = 0
		} else if (j == finish) {
		    finish = left[j]
		    right[left[j]] = nptsp1
		} else {
		    right[left[j]] = right[j]
		    left[right[j]] = left[j]
		}

	    }

	    # Update the median kernel.
	    do j = 1, ybox {
		if (data[col,j] < zlo)
		    nlo = nlo + 1
		if (data[col,j] > zhi)
		    nhi = nhi + 1
		filter[mp+j-1] = data[col,j]
		Memi[index+j-1] = j
	    }

	    # Sort array to be inserted.
	    call med_gshsrt (filter[mp], Memi[index], ybox)

	    # Merge the sorted lists.
	    k = start
	    do j = 1, ybox {

		# Position in filter kernel of new point
		l = Memi[index+j-1] + mp - 1

		# Find the element to the right of the point to be inserted
		while (filter[l] > filter[k] && k != right[finish])
		    k = right[k]

		# Update the linked list
		if (k == start) {
		    left[start] = l
		    left[l] = 0
		    right[l] = start
		    start = l
		} else if (k == right[finish]) {
		    right[finish] = l
		    left[l] = finish
		    right[l] = nptsp1
		    finish = l
		} else {
		    left[l] = left[k]
		    right[l] = right[left[k]]
		    right[left[k]] = l
		    left[k] = l
		}

	    }

	    # Increment the median pointer.
	    mp = mp + ybox
	    if (mp > npts)
		mp = 1

	    col = col + 1
	}

	# Calculate the last median.
	k = start
	nzero = npts - nlo - nhi
	nhalf = (nzero - 1) / 2
	do j = 1, nlo + nhalf
	    k = right[k]
	if (nzero > 0)
	    median[ncols] = filter[k]
	else if (nlo < nhi)
	    median[ncols] = zhi
	else
	    median[ncols] = zlo

	MED_START(mde) = start
	MED_FINISH(mde) = finish
	MED_MP(mde) = mp
	MED_NLOW(mde) = nlo
	MED_NHIGH(mde) = nhi

	call sfree (sp)
end


# MDE_EREV_BOXFILTER -- Median filter a single image line in reverse

procedure mde_ereverse_boxfilter (mde, data, nx, ny, median, ncols,
	filter, left, right)

pointer	mde		#I pointer to the median fitting structure
real	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	median[ncols]	#O median array
int	ncols		#I number of output image columns
real	filter[ARB]	#U the array of data to be filtered
int	left[ARB]	#U the array of back pointers
int	right[ARB]	#U the array of forward pointers

int	i, j, k, l, col, nhalf, xbox, ybox, npts, start, finish, nlo, nhi, mp
int	nptsp1, nzero
pointer	sp, index
real	zlo, zhi

begin
	xbox = MED_XBOX(mde)
	ybox = MED_YBOX(mde)
	npts = MED_NPTS(mde)
	nptsp1 = MED_NPTSP1(mde)
	zlo = MED_ZLOW(mde)
	zhi = MED_ZHIGH(mde)

	start = MED_START(mde)
	finish = MED_FINISH(mde)
	mp = MED_MP(mde)
	nlo = MED_NLOW(mde)
	nhi = MED_NHIGH(mde)

	call smark (sp)
	call salloc (index, ybox, TY_INT)

	col = nx - xbox
	do i = ncols, 2, - 1 {

	    # Calculate the median.
	    k = start
	    nzero = npts - nlo - nhi
	    nhalf = (nzero - 1) / 2
	    do j = 1, nlo + nhalf
	        k = right[k]
	    if (nzero > 0)
	        median[i] = filter[k]
	    else if (nlo < nhi)
		median[i] = zhi
	    else
		median[i] = zlo

	    # Delete points.
	    do j = mp, mp + ybox - 1 {

		if (filter[j] < zlo)
		    nlo = nlo - 1
		if (filter[j] > zhi)
		    nhi = nhi - 1
		if (j == start) {
		    start = right[j]
		    left[right[j]] = 0
		} else if (j == finish) {
		    finish = left[j]
		    right[left[j]] = nptsp1
		} else {
		    right[left[j]] = right[j]
		    left[right[j]] = left[j]
		}

	    }

	    # Update the median kernel.
	    do j = 1, ybox {
		if (data[col,j] < zlo)
		    nlo = nlo + 1
		if (data[col,j] > zhi)
		    nhi = nhi + 1
		filter[mp+j-1] = data[col,j]
		Memi[index+j-1] = j
	    }

	    # Sort array to be inserted.
	    call med_gshsrt (filter[mp], Memi[index], ybox)

	    # Merge the sorted lists.
	    k = start
	    do j = 1, ybox {

		# Find position in filter kernel of new point.
		l = Memi[index+j-1] + mp - 1

		# Find the element to the right of the point to be inserted.
		while (filter[l] > filter[k] && k != right[finish])
		    k = right[k]

		# Update the linked list.
		if (k == start) {
		    left[start] = l
		    left[l] = 0
		    right[l] = start
		    start = l
		} else if (k == right[finish]) {
		    right[finish] = l
		    left[l] = finish
		    right[l] = nptsp1
		    finish = l
		} else {
		    left[l] = left[k]
		    right[l] = right[left[k]]
		    right[left[k]] = l
		    left[k] = l
		}

	    }

	    # Increment the median pointer.
	    mp = mp - ybox
	    if (mp < 1)
		mp = mp + npts

	    col = col - 1
	}

	# Calculate the last median.
	k = start
	nzero = npts - nlo - nhi
	nhalf = (nzero - 1) / 2
	do j = 1, nlo + nhalf
	    k = right[k]
	if (nzero > 0)
	    median[1] = filter[k]
	else if (nlo < nhi)
	    median[1] = zhi
	else
	    median[1] = zlo

	MED_START(mde) = start
	MED_FINISH(mde) = finish
	MED_MP(mde) = mp
	MED_NLOW(mde) = nlo
	MED_NHIGH(mde) = nhi

	call sfree (sp)
end


# MDE_XEFILTER -- Median filter a single image line in the x direction.
# The filter always moves from left to right.

procedure mde_xefilter (mde, data, nx, ny, median, ncols, filter, left, right)

pointer	mde		#I pointer to the median structure
real	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	median[ncols]	#O median array
int	ncols		#I number of output image columns
real	filter[ARB]	#U the array of points to be medianed
int	left[ARB]	#U the array of back pointers
int	right[ARB]	#U the array of forward pointers

int	i, j, k, start, finish, mp, xbox, npts, nptsp1, nhalf, nlo, nhi, nzero
real	zlo, zhi

begin
	xbox = MED_XBOX(mde)
	npts = MED_NPTS(mde)
	nptsp1 = MED_NPTSP1(mde)
	zlo = MED_ZLOW(mde)
	zhi = MED_ZHIGH(mde)

	start = MED_START(mde)
	finish = MED_FINISH(mde)
	mp = MED_MP(mde)
	nlo = MED_NLOW(mde)
	nhi = MED_NHIGH(mde)

	# Median filter an image line.
	do i = 1, ncols - 1 {

	    # Calculate the median.
	    k = start
	    nzero = npts - nhi - nlo
	    nhalf = (nzero - 1) / 2
	    do j = 1, nlo + nhalf
		k = right[k]
	    if (nzero > 0)
	        median[i] = filter[k]
	    else if (nlo < nhi)
	        median[i] = zhi
	    else
	        median[i] = zlo

	    # Delete points from the filter kernel.
	    if (filter[mp] < zlo)
		nlo = nlo - 1
	    if (filter[mp] > zhi)
		nhi = nhi - 1

	    if (mp == start) {
		start = right[mp]
		left[right[mp]] = 0
	    } else
		right[left[mp]] = right[mp]

	    if (mp == finish) {
		finish = left[mp]
		right[left[mp]] = nptsp1
	    } else
		left[right[mp]] = left[mp]

	    # Update the median kernel.
	    if (data[i+xbox,1] < zlo)
		nlo = nlo + 1
	    if (data[i+xbox,1] > zhi)
		nhi = nhi + 1
	    filter[mp] = data[i+xbox,1]

	    # Find the point to the right of the point to be inserted.
	    k = start
    	    while (k != right[finish] && filter[mp] > filter[k])
		k = right[k]

	    # Insert points into the filter kernel.
	    if (k == start) {
		left[start] = mp
		left[mp] = 0
		right[mp] = start
		start = mp
	    } else if (k == right[finish]) {
		right[finish] = mp
		left[mp] = finish
		right[mp] = nptsp1
		finish = mp
	    } else {
		left[mp] = left[k]
		right[mp] = right[left[k]]
		right[left[k]] = mp
		left[k] = mp
	    }

	    # Increment median counter
	    mp = mp + 1
	    if (mp > npts)
		mp = 1
	}

	# Calculate the last median
	nzero = npts - nhi - nlo
	nhalf = (nzero - 1) / 2
	k = start
	do j = 1, nlo + nhalf
	    k = right[k]
	if (nzero > 0)
	    median[ncols] = filter[k]
	else if (nlo < nhi)
	    median[ncols] = zhi
	else
	    median[ncols] = zlo

	MED_START(mde) = start
	MED_FINISH(mde) = finish
	MED_MP(mde) = mp
	MED_NLOW(mde) = nlo
	MED_NHIGH(mde) = nhi
end


# MDE_YEFILTER -- Median filter a single image line in the y direction

procedure mde_yefilter (mde, data, nx, ny, filter, median, ncols)

pointer	mde		#I pointer to the median structure
real	data[nx,ny]	#I image data
int	nx, ny		#I dimensions of data
real	filter[ARB]	#U array containing the points to be medianed
real	median[ncols]	#O median array
int	ncols		#I number of output image columns

int	i, j, npts, nlo, nhi
real	zlo, zhi

begin
	zlo = MED_ZLOW(mde)
	zhi = MED_ZHIGH(mde)

	npts = MED_NPTS(mde)
	nlo = MED_NLOW(mde)
	nhi = MED_NHIGH(mde)

	do i = 1, ncols - 1 {

	    # Calculate the new median.
	    if (npts > 0)
	        median[i] = filter[(npts+1)/2]
	    else if (nlo < nhi)
		median[i] = zhi
	    else
		median[i] = zlo

	    # Update the median kernel.
	    npts = 0
	    nlo = 0
	    nhi = 0
	    do j = 1, ny {
		if (data[i+1,j] < zlo) {
		    nlo = nlo + 1
		    next
		}
		if (data[i+1,j] > zhi) {
		    nhi = nhi + 1
		    next
		}
		npts = npts + 1
	        filter[npts] = data[i+1,j]
	    }

	    if (npts > 0)
	        call med_ashsrt (filter, npts)

	}

	# Calculate the last median
	if (npts > 0)
	    median[ncols] = filter[(npts+1)/2]
	else if (nlo < nhi)
	    median[ncols] = zhi
	else
	    median[ncols] = zlo

	# Store the results.
	MED_NPTS(mde) = npts
	MED_NLOW(mde) = nlo
	MED_NHIGH(mde) = nhi
end
