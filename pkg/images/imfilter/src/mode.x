# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <imset.h>
include <imhdr.h>
include "mode.h"

# MDE_MODBOX -- Modal filter an image using a rectangular window.

procedure mde_modbox (mde, im1, im2, boundary, constant)

pointer	mde		#I pointer to the mode structure
pointer	im1		#I pointer to the input image
pointer	im2		#I pointer to the output image
int	boundary	#I boundary extension type
real	constant	#I constant for constant boundary extension

int	col1, col2, ncols, line, line1, line2
pointer	filter, left, right, inbuf, outbuf
pointer	impl2r()
errchk	impl2r, med_buf, mde_medboxset, med_xefilter, mde_yefilter
errchk	med_boxfilter

begin
	# Check for 1D images.
	if (IM_NDIM(im1) == 1)
	    MOD_YBOX(mde) = 1

	# Set the mode filtering buffers.
	call calloc (filter, MOD_XBOX(mde) * MOD_YBOX(mde) + 1, TY_REAL)
	call calloc (left, MOD_XBOX(mde) * MOD_YBOX(mde), TY_INT)
	call calloc (right, MOD_XBOX(mde) * MOD_YBOX(mde), TY_INT)

	# Set the input image boundary extension parameters.
	call imseti (im1, IM_TYBNDRY, boundary)
	call imseti (im1, IM_NBNDRYPIX, max (MOD_XBOX(mde) / 2,
	    MOD_YBOX(mde) / 2))
	call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Set the line buffer parameters.
	inbuf = NULL
	col1 = 1 - MOD_XBOX(mde) / 2
	col2 = IM_LEN(im1, 1) + MOD_XBOX(mde) / 2 
	ncols = col2 - col1 + 1

	# Generate the output image line by line.
	do line = 1, IM_LEN(im2, 2) {

	    # Get ybox image lines
	    line1 = line - MOD_YBOX(mde) / 2
	    line2 = line + MOD_YBOX(mde) / 2

	    # Read in the appropriate range of image lines.
	    call med_buf (im1, col1, col2, line1, line2, inbuf)

	    # Set up modal filter array for each image line.
	    call mde_modboxset (mde, Memr[inbuf], ncols, MOD_YBOX(mde),
	        Memr[filter], Memi[left], Memi[right], line)

	    # Get the output image line.
	    outbuf = impl2r (im2, line)
	    if (outbuf == EOF)
		call error (0, "Error writing output image.")

	    # Modal filter the image line.
	    if (MOD_XBOX(mde) == 1)
		call mde_yofilter (mde, Memr[inbuf], ncols, MOD_YBOX(mde),
		    Memr[filter], Memr[outbuf], int (IM_LEN(im2,1)))
	    else if (MOD_YBOX(mde) == 1)
		call mde_xofilter (mde, Memr[inbuf], ncols, MOD_YBOX(mde),
		    Memr[outbuf], int (IM_LEN(im2,1)), Memr[filter],
		    Memi[left], Memi[right])
	    else
	        call mde_modboxfilter (mde, Memr[inbuf], ncols, MOD_YBOX(mde),
		    Memr[outbuf], int (IM_LEN(im2, 1)), Memr[filter],
		    Memi[left], Memi[right], line)
	}

	# Free the image and filter buffers.
	call mfree (inbuf, TY_REAL)
	call mfree (filter, TY_REAL)
	call mfree (left, TY_INT)
	call mfree (right, TY_INT)
end


# MDE_MODBOXSET -- Set up mode array for the beginning of each image line
# The image is raster scanned so that the direction of scanning changes
# for each line. Odd numbered lines are scanned forwards and even numbered
# lines are scanned backward. If the mode filters is one dimensional
# the lines are scanned forward.

procedure mde_modboxset (mde, data, nx, ny, filter, left, right, line)

pointer	mde			#I pointer to the mode structure
real	data[nx, ny]		#I image data buffer
int	nx			#I number of columns in image buffer
int	ny			#I number of lines in the image buffer
real	filter[ARB]		#U array of elements to be sorted
int	left[ARB]		#U array of back pointers
int	right[ARB]		#U array of forward pointers
int	line			#I line number

int	i, j, k, l, xbox, ybox, nlo, nhi, npts, nptsp1, start, finish, mp
pointer	sp, insert, index
real	sum, zlo, zhi

begin
	# Get algorithm parameters.
	xbox = MOD_XBOX(mde)
	ybox = MOD_YBOX(mde)
	zlo = MOD_ZLOW(mde)
	zhi = MOD_ZHIGH(mde)

	call smark (sp)

	# Initialize.
	if (xbox == 1) {

	    npts = 0
	    nlo = 0
	    nhi = 0
	    sum = 0.0

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
		sum = sum + data[1,i]
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
	    sum = 0.0
	    k = 1
	    do i = 1, xbox {
		do j = 1, ybox {
		    if (data[i,j] < zlo)
			nlo = nlo + 1
		    else if (data[i,j] > zhi)
			nhi = nhi + 1
		    else
			sum = sum + data[i,j]
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

	    npts = MOD_NPTS(mde)
	    nptsp1 = MOD_NPTSP1(mde)
	    mp = MOD_MP(mde)
	    start = MOD_START(mde)
	    finish = MOD_FINISH(mde)
	    nlo = MOD_NLOW(mde)
	    nhi = MOD_NHIGH(mde)
	    sum = MOD_SUM(mde)

	    call salloc (index, xbox, TY_INT)
	    call salloc (insert, xbox, TY_REAL)

	    # Xbox elements are deleted when lines are changed.
	    # These elements are always located in the first
	    # column of the filter kernel.
	    do i = 1, npts, ybox {
	        if (filter[i] < zlo)
		    nlo = nlo - 1
	        else if (filter[i] > zhi)
	            nhi = nhi - 1
		else
		    sum = sum - filter[i]
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
		else if (data[i,ny] > zhi)
		    nhi = nhi + 1
		else
		    sum = sum + data[i,ny]
		Memr[insert+i-1] = data[i,ny]
		Memi[index+i-1] = i
	    }

	    # Sort the new points.
	    call med_gshsrt (Memr[insert], Memi[index], xbox)

	    # Adjust the mode pointer.
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
	    
	    npts = MOD_NPTS(mde)
	    nptsp1 = MOD_NPTSP1(mde)
	    mp = MOD_MP(mde)
	    start = MOD_START(mde)
	    finish = MOD_FINISH(mde)
	    nlo = MOD_NLOW(mde)
	    nhi = MOD_NHIGH(mde)
	    sum = MOD_SUM(mde)

	    call salloc (index, xbox, TY_INT)
	    call salloc (insert, xbox, TY_REAL)

	    # Xbox elements are deleted when lines are changed.
	    # These elements are always located in the first
	    # column of the filter kernel.
	    do i = 1, npts, ybox {
	        if (filter[i] < zlo)
	            nlo = nlo - 1
		else if (filter[i] > zhi)
		    nhi = nhi - 1
		else
		    sum = sum - filter[i]
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
		else if (data[j,ny] > zhi)
		    nhi = nhi + 1
		else
		    sum = sum + data[j,ny]
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

	MOD_NPTS(mde) = npts
	MOD_NPTSP1(mde) = nptsp1
	MOD_MP(mde) = mp
	MOD_START(mde) = start
	MOD_FINISH(mde) = finish
	MOD_NLOW(mde) = nlo
	MOD_NHIGH(mde) = nhi
	MOD_SUM(mde) = sum

	call sfree (sp)
end


# MDE_MODBOXFILTER -- Modal filter a single image line.

procedure mde_modboxfilter (mde, data, nx, ny, mode, ncols, filter, left,
	right, line)

pointer	mde		#I pointer to the mode structure
real	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	mode[ncols]	#O mode array
int	ncols		#I number of output image columns
real	filter[ARB]	#U the mode array of points to be filtered
int	left[ARB]	#U the array of back pointers
int	right[ARB]	#U the array of forward pointers
int	line		#I current line number

begin
	if (mod (line, 2) == 0)
	    call mde_oreverse_boxfilter (mde, data, nx, ny, mode, ncols,
		filter, left, right)
	else
	    call mde_oforward_boxfilter (mde, data, nx, ny, mode, ncols,
	        filter, left, right)
end


# MDE_OFORWARD_BOXFILTER -- Median filter a single image line

procedure mde_oforward_boxfilter (mde, data, nx, ny, mode, ncols,
	filter, left, right)

pointer	mde		#I pointer to the mode filtering structure
real	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	mode[ncols]	#O mode array
int	ncols		#I number of output image columns
real	filter[ARB]	#U the array of points to be filtered
int	left[ARB]	#U the array of back pointers
int	right[ARB]	#U the array of forward pointers

int	i, j, k, l, col, nzero, nhalf, xbox, ybox, npts, nptsp1
int	nlo, nhi, start, finish, mp
real	sum, zlo, zhi
pointer	sp, index

begin
	xbox = MOD_XBOX(mde)
	ybox = MOD_YBOX(mde)
	zlo = MOD_ZLOW(mde)
	zhi = MOD_ZHIGH(mde)
	npts = MOD_NPTS(mde)
	nptsp1 = MOD_NPTSP1(mde)

	start = MOD_START(mde)
	finish = MOD_FINISH(mde)
	mp = MOD_MP(mde)
	nlo = MOD_NLOW(mde)
	nhi = MOD_NHIGH(mde)
	sum = MOD_SUM(mde)

	call smark (sp)
	call salloc (index, ybox, TY_INT)

	col = 1 + xbox
	do i = 1, ncols - 1 {

	    # Calculate the mode.
	    k = start
	    nzero = npts - nlo - nhi
	    nhalf = (nzero - 1) / 2
	    do j = 1, nlo + nhalf
	        k = right[k]
	    if (nzero > 0)
	        mode[i] = 3.0 * filter[k] - 2.0 * sum / nzero
	    else if (nlo < nhi)
		mode[i] = zhi
	    else
		mode[i] = zlo

	    # Delete points.
	    do j = mp, mp + ybox - 1 {

		if (filter[j] < zlo)
		    nlo = nlo - 1
		else if (filter[j] > zhi)
		    nhi = nhi - 1
		else
		    sum = sum - filter[j]

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

	    # Update the mode kernel.
	    do j = 1, ybox {
		if (data[col,j] < zlo)
		    nlo = nlo + 1
		else if (data[col,j] > zhi)
		    nhi = nhi + 1
		else
		    sum = sum + data[col,j]
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

	    # Increment the mode pointer.
	    mp = mp + ybox
	    if (mp > npts)
		mp = 1

	    col = col + 1
	}

	# Calculate the last mode.
	k = start
	nzero = npts - nlo - nhi
	nhalf = (nzero - 1) / 2
	do j = 1, nlo + nhalf
	    k = right[k]
	if (nzero > 0)
	    mode[ncols] = 3.0 * filter[k] - 2.0 * sum / nzero
	else if (nlo < nhi)
	    mode[ncols] = zhi
	else
	    mode[ncols] = zlo

	MOD_START(mde) = start
	MOD_FINISH(mde) = finish
	MOD_MP(mde) = mp
	MOD_NLOW(mde) = nlo
	MOD_NHIGH(mde) = nhi
	MOD_SUM(mde) = sum

	call sfree (sp)
end


# MDE_OREV_BOXFILTER -- Median filter a single image line in reverse

procedure mde_oreverse_boxfilter (mde, data, nx, ny, mode, ncols,
	filter, left, right)

pointer	mde		#I pointer to the mode fitting structure
real	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	mode[ncols]	#O mode array
int	ncols		#I number of output image columns
real	filter[ARB]	#U the array of data to be filtered
int	left[ARB]	#U the array of back pointers
int	right[ARB]	#U the array of forward pointers

int	i, j, k, l, col, nhalf, xbox, ybox, npts, start, finish, nlo, nhi, mp
int	nptsp1, nzero
pointer	sp, index
real	sum, zlo, zhi

begin
	xbox = MOD_XBOX(mde)
	ybox = MOD_YBOX(mde)
	npts = MOD_NPTS(mde)
	nptsp1 = MOD_NPTSP1(mde)
	zlo = MOD_ZLOW(mde)
	zhi = MOD_ZHIGH(mde)

	start = MOD_START(mde)
	finish = MOD_FINISH(mde)
	mp = MOD_MP(mde)
	nlo = MOD_NLOW(mde)
	nhi = MOD_NHIGH(mde)
	sum = MOD_SUM(mde)

	call smark (sp)
	call salloc (index, ybox, TY_INT)

	col = nx - xbox
	do i = ncols, 2, - 1 {

	    # Calculate the mode.
	    k = start
	    nzero = npts - nlo - nhi
	    nhalf = (nzero - 1) / 2
	    do j = 1, nlo + nhalf
	        k = right[k]
	    if (nzero > 0)
	        mode[i] = 3.0 * filter[k] - 2.0 * sum / nzero
	    else if (nlo < nhi)
		mode[i] = zhi
	    else
		mode[i] = zlo

	    # Delete points.
	    do j = mp, mp + ybox - 1 {

		if (filter[j] < zlo)
		    nlo = nlo - 1
		else if (filter[j] > zhi)
		    nhi = nhi - 1
		else
		    sum = sum - filter[j]
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

	    # Update the mode kernel.
	    do j = 1, ybox {
		if (data[col,j] < zlo)
		    nlo = nlo + 1
		else if (data[col,j] > zhi)
		    nhi = nhi + 1
		else
		    sum = sum + data[col,j]
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

	    # Increment the mode pointer.
	    mp = mp - ybox
	    if (mp < 1)
		mp = mp + npts

	    col = col - 1
	}

	# Calculate the last mode.
	k = start
	nzero = npts - nlo - nhi
	nhalf = (nzero - 1) / 2
	do j = 1, nlo + nhalf
	    k = right[k]
	if (nzero > 0)
	    mode[1] = 3.0 * filter[k] - 2.0 * sum / nzero
	else if (nlo < nhi)
	    mode[1] = zhi
	else
	    mode[1] = zlo

	MOD_START(mde) = start
	MOD_FINISH(mde) = finish
	MOD_MP(mde) = mp
	MOD_NLOW(mde) = nlo
	MOD_NHIGH(mde) = nhi
	MOD_SUM(mde) = sum

	call sfree (sp)
end


# MDE_XOFILTER -- Modal filter a single image line in the x direction.
# The filter always moves from left to right.

procedure mde_xofilter (mde, data, nx, ny, mode, ncols, filter, left, right)

pointer	mde		#I pointer to the mode structure
real	data[nx, ny]	#I image data
int	nx, ny		#I dimensions of data
real	mode[ncols]	#O mode array
int	ncols		#I number of output image columns
real	filter[ARB]	#U the array of points to be modal filtered
int	left[ARB]	#U the array of back pointers
int	right[ARB]	#U the array of forward pointers

int	i, j, k, start, finish, mp, xbox, npts, nptsp1, nhalf, nlo, nhi, nzero
real	sum, zlo, zhi

begin
	xbox = MOD_XBOX(mde)
	npts = MOD_NPTS(mde)
	nptsp1 = MOD_NPTSP1(mde)
	zlo = MOD_ZLOW(mde)
	zhi = MOD_ZHIGH(mde)

	start = MOD_START(mde)
	finish = MOD_FINISH(mde)
	mp = MOD_MP(mde)
	nlo = MOD_NLOW(mde)
	nhi = MOD_NHIGH(mde)
	sum = MOD_SUM(mde)

	# Modal filter an image line.
	do i = 1, ncols - 1 {

	    # Calculate the mode.
	    k = start
	    nzero = npts - nhi - nlo
	    nhalf = (nzero - 1) / 2
	    do j = 1, nlo + nhalf
		k = right[k]
	    if (nzero > 0)
	        mode[i] = 3.0 * filter[k] - 2.0 * sum / nzero
	    else if (nlo < nhi)
	        mode[i] = zhi
	    else
	        mode[i] = zlo

	    # Delete points from the filter kernel.
	    if (filter[mp] < zlo)
		nlo = nlo - 1
	    else if (filter[mp] > zhi)
		nhi = nhi - 1
	    else
		sum = sum - filter[mp]

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

	    # Update the mode kernel.
	    if (data[i+xbox,1] < zlo)
		nlo = nlo + 1
	    else if (data[i+xbox,1] > zhi)
		nhi = nhi + 1
	    else
	        sum = sum + data[i+xbox,1]
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

	    # Increment mode counter
	    mp = mp + 1
	    if (mp > npts)
		mp = 1
	}

	# Calculate the last mode.
	nzero = npts - nhi - nlo
	nhalf = (nzero - 1) / 2
	k = start
	do j = 1, nlo + nhalf
	    k = right[k]
	if (nzero > 0)
	    mode[ncols] = 3.0 * filter[k] - 2.0 * sum / nzero
	else if (nlo < nhi)
	    mode[ncols] = zhi
	else
	    mode[ncols] = zlo

	MOD_START(mde) = start
	MOD_FINISH(mde) = finish
	MOD_MP(mde) = mp
	MOD_NLOW(mde) = nlo
	MOD_NHIGH(mde) = nhi
	MOD_SUM(mde) = sum
end


# MDE_YOFILTER -- Modal filter a single image line in the y direction.

procedure mde_yofilter (mde, data, nx, ny, filter, mode, ncols)

pointer	mde		#I pointer to the mode structure
real	data[nx,ny]	#I image data
int	nx, ny		#I dimensions of data
real	filter[ARB]	#U array containing the points to be modal filtered
real	mode[ncols]	#O the mode array
int	ncols		#I number of output image columns

int	i, j, npts, nlo, nhi
real	sum, zlo, zhi

begin
	zlo = MOD_ZLOW(mde)
	zhi = MOD_ZHIGH(mde)

	npts = MOD_NPTS(mde)
	nlo = MOD_NLOW(mde)
	nhi = MOD_NHIGH(mde)
	sum = MOD_SUM(mde)

	do i = 1, ncols - 1 {

	    # Calculate the new mode.
	    if (npts > 0)
	        mode[i] = 3.0 * filter[(npts+1)/2] - 2.0 * sum / npts
	    else if (nlo < nhi)
		mode[i] = zhi
	    else
		mode[i] = zlo

	    # Update the mode kernel.
	    npts = 0
	    nlo = 0
	    nhi = 0
	    sum = 0.0
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
		sum = sum + data[i+1,j] 
	        filter[npts] = data[i+1,j]
	    }

	    if (npts > 0)
	        call med_ashsrt (filter, npts)

	}

	# Calculate the last mode.
	if (npts > 0)
	    mode[ncols] = 3.0 * filter[(npts+1)/2] - 2.0 * sum / npts
	else if (nlo < nhi)
	    mode[ncols] = zhi
	else
	    mode[ncols] = zlo

	# Store the results.
	MOD_NPTS(mde) = npts
	MOD_NLOW(mde) = nlo
	MOD_NHIGH(mde) = nhi
	MOD_SUM(Mde) = sum
end
