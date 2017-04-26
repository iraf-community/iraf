# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"../imsum.h"

define	TMINSW		1.00	# Relative timings for nvecs = 5
define	TMXMNSW		1.46
define	TMED3		0.18
define	TMED5		0.55

# IMSUM -- Sum or average images with optional high and low pixel rejection.
# 
# This procedure has to be clever in not exceeding the maximum number of images
# which can be mapped at one time.  If no pixels are being rejected then the
# images can be summed (or averaged) in blocks using the output image to hold
# intermediate results.  If pixels are being rejected then lines from all
# images must be obtained.  If the number of images exceeds the maximum
# then only a subset of the images are kept mapped and the remainder are
# mapped and unmapped for each line.  This, of course, is inefficient but
# there is no other way.


procedure imsums (list, output, im_out, nlow, nhigh, option)

int	list				# List of input images
char	output[ARB]			# Output image
pointer	im_out				# Output image pointer
int	nlow				# Number of low pixels to reject
int	nhigh				# Number of high pixels to reject
char	option[ARB]			# Output option

int	i, n, nimages, naccept, npix, ndone, pass
short	const
pointer	sp, input, v1, v2, im, buf, buf1, buf_in, buf_out

bool	streq()
int	imtlen(), imtgetim(), imtrgetim()
pointer	immap(), imgnls(), impnls()
errchk	immap, imunmap, imgnls, impnls

begin
	# Initialize.
	nimages = imtlen (list)
	naccept = nimages - nlow - nhigh
	const = naccept
	npix = IM_LEN(im_out, 1)
	if (naccept < 1)
	    call error (0, "Number of rejected pixels is too large")

	# Allocate memory.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (im, nimages, TY_INT)

	# If there are no pixels to be rejected avoid calls to reject pixels
	# and do the operation in blocks so that the number of images mapped
	# does not exceed the maximum.  The output image is used to
	# store intermediate results.

	if ((nlow == 0) && (nhigh == 0)) {
	    pass = 0
	    ndone = 0
	    repeat {
		n = 0
		while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	            Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		    n = n + 1
		    if (n == IMS_MAX)
			break
	        }
		ndone = ndone + n

	        pass = pass + 1
		if (pass > 1) {
		    call imunmap (im_out)
		    im_out = immap (output, READ_WRITE, 0)
		}

	        call amovkl (long(1), Meml[v1], IM_MAXDIM)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

		# For each input line compute an output line.
		while (impnls (im_out, buf_out, Meml[v2]) != EOF) {

		    # Clear the output buffer during the first pass and
		    # read in the partial sum from the output image during
		    # subsequent passes.

		    if (pass == 1)
			call aclrs (Mems[buf_out], npix)
		    else {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnls (im_out, buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call amovs (Mems[buf_in], Mems[buf_out], npix)
		    }

		    # Accumulate lines from each input image.
	    	    do i = 1, n {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnls (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call aadds (Mems[buf_in], Mems[buf_out],
			    Mems[buf_out], npix)
		    }

		    # If all images have been accumulated and averaging then
		    # divide by the number of images.
		    if ((ndone == nimages) && streq (option, "average"))
			call adivks (Mems[buf_out], const, Mems[buf_out],
			    npix)

		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		}

		do i = 1, n
		    call imunmap (Memi[im+i-1])
	    } until (ndone == nimages)

	    # Finish up.
	    call sfree (sp)
	    return
	}
	       

	# Map the input images up to the maximum allowed.  The remainder
	# will be mapped during each line.
	n = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
	    n = n + 1
	    if (n == IMS_MAX - 1)
		break
	}

	# Allocate additional buffer space.
	call salloc (buf, nimages, TY_INT)
	if (nimages - n > 0)
	    call salloc (buf1, (nimages-n)*npix, TY_SHORT)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	# Compute output lines for each input line.
	while (impnls (im_out, buf_out, Meml[v2]) != EOF) {

	    # Read lines from the images which remain open.
	    for (i = 1; i <= n; i = i + 1) {
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnls (Memi[im+i-1], Memi[buf+i-1], Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
	    }

	    # For all additional images map the image, read a line, copy the
	    # data to a buffer since the image buffer is reused, and unmap
	    # the image.  
	    for (; i <= nimages; i = i + 1) {
		if (imtrgetim (list, i, Memc[input], SZ_FNAME) == EOF)
		    break
		Memi[im+i-1] = immap (Memc[input], READ_ONLY, 0)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnls (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
		Memi[buf+i-1] = buf1 + (i - n - 1) * npix
		call amovs (Mems[buf_in], Mems[Memi[buf+i-1]], npix)
		call imunmap (Memi[im+i-1])
	    }
		
	    # Reject pixels.
	    call imrejs (Memi[buf], nimages, Mems[buf_out], npix, nlow, nhigh)

	    # If averaging divide the sum by the number of images averaged.
	    if ((naccept > 1) && streq (option, "average")) {
		const = naccept
		call adivks (Mems[buf_out], const, Mems[buf_out], npix)
	    }

	    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	}

	# Finish up.
	do i = 1, n
	    call imunmap (Memi[im+i-1])
	call sfree (sp)
end


# IMREJ --  Reject the number of high and low points and sum the rest.

procedure imrejs (a, nvecs, b, npts, nlow, nhigh)

pointer	a[nvecs]		# Pointers to set of vectors
int	nvecs			# Number of vectors
short	b[npts]			# Output vector
int	npts			# Number of points in the vectors
int	nlow			# Number of low points to be rejected
int	nhigh			# Number of high points to be rejected

int	i, j
int	naccept, minrej, npairs, nlow1, nhigh1
real	tmedian, time1, time2

begin
	naccept = nvecs - nlow - nhigh

	# If no points are rejected return the sum.

	if (naccept == nvecs) {
	    call amovs (Mems[a[1]], b, npts)
	    for (j = 2; j <= naccept; j = j + 1)
		call aadds (Mems[a[j]], b, b, npts)
	    return
	}

	minrej = min (nlow, nhigh)
	npairs = minrej
	nlow1 = nlow - npairs
	nhigh1 = nhigh - npairs

	if ((naccept == 1) && (npairs > 0)) {
	    if (npairs == 1) {
	        tmedian = TMED3
		npairs = npairs - 1
	    } else {
		tmedian = TMED5
	        npairs = npairs - 2
	    }
	} else
	    tmedian = 0

	# Compare the time required to reject the minimum number
	# of low or high points and extract the number of points to accept
	# with the time to reject pairs and the excess number of low or
	# high points to either reach a median of 3 or 5 points or isolate
	# the acceptable points.

	time1 = TMINSW * (minrej + naccept)
	time2 = tmedian + TMXMNSW * npairs + TMINSW * (nlow1 + nhigh1)

	i = nvecs
	if (time1 < time2) {

 	    # Sort the nlow and naccept points
	    if (nlow < nhigh) {
	        for (j = 1; j <= nlow + naccept; j = j + 1) {
	            call minsws (a, i, npts)
		    i = i - 1
	        }
	    	call amovs (Mems[a[nhigh+1]], b, npts)
		for (j = nhigh+2; j <= nhigh+naccept; j = j + 1)
		    call aadds (Mems[a[j]], b, b, npts)

	    # Sort the nhigh and naccept points
	    } else {
	        for (j = 1; j <= nhigh + naccept; j = j + 1) {
	            call maxsws (a, i, npts)
		    i = i - 1
	        }
	    	call amovs (Mems[a[nlow+1]], b, npts)
		for (j = nlow+2; j <= nlow+naccept; j = j + 1)
		    call aadds (Mems[a[j]], b, b, npts)
	    }

	} else {
	    # Reject the npairs low and high points.
	    for (j = 1; j <= npairs; j = j + 1) {
	        call mxmnsws (a, i, npts)
		i = i - 2
	    }
	    # Reject the excess low points.
	    for (j = 1; j <= nlow1; j = j + 1) {
	        call minsws (a, i, npts)
		i = i - 1
	    }
	    # Reject the excess high points.
	    for (j = 1; j <= nhigh1; j = j + 1) {
	        call maxsws (a, i, npts)
		i = i - 1
	    }

	    # Check if the remaining points constitute a 3 or 5 point median
	    # or the set of desired points.
	    if (tmedian == 0.) {
	        call amovs (Mems[a[1]], b, npts)
	        for (j = 2; j <= naccept; j = j + 1)
		    call aadds (Mems[a[j]], b, b, npts)
	    } else if (tmedian == TMED3) {
		call amed3s (Mems[a[1]], Mems[a[2]], Mems[a[3]], b, npts)
	    } else {
		call amed5s (Mems[a[1]], Mems[a[2]], Mems[a[3]],
		        Mems[a[4]], Mems[a[5]], b, npts)
	    }
	}
end


# MINSW -- Given an array of vector pointers for each element in the vectors
# swap the minimum element with that of the last vector.

procedure minsws (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmin
short	temp

begin
	do i = 0, npts - 1 {
	    kmin = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Mems[k] < Mems[kmin])
		    kmin = k
	    }
	    if (k != kmin) {
	        temp = Mems[k]
	        Mems[k] = Mems[kmin]
	        Mems[kmin] = temp
	    }
	}
end


# MAXSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector.

procedure maxsws (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax
short	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Mems[k] > Mems[kmax])
		    kmax = k
	    }
	    if (k != kmax) {
	        temp = Mems[k]
	        Mems[k] = Mems[kmax]
	        Mems[kmax] = temp
	    }
	}
end


# MXMNSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector and the minimum element
# with that of the next to last vector.  The number of vectors must be greater
# than 1.

procedure mxmnsws (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax, kmin
short	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    kmin = kmax
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Mems[k] > Mems[kmax])
		    kmax = k
		else if (Mems[k] < Mems[kmin])
		    kmin = k
	    }
	    temp = Mems[k]
	    Mems[k] = Mems[kmax]
	    Mems[kmax] = temp
	    if (kmin == k) {
	        j = a[nvecs - 1] + i
	        temp = Mems[j]
		Mems[j] = Mems[kmax]
		Mems[kmax] = temp
	    } else {
	        j = a[nvecs - 1] + i
	        temp = Mems[j]
	        Mems[j] = Mems[kmin]
	        Mems[kmin] = temp
	    }
	}
end

procedure imsumi (list, output, im_out, nlow, nhigh, option)

int	list				# List of input images
char	output[ARB]			# Output image
pointer	im_out				# Output image pointer
int	nlow				# Number of low pixels to reject
int	nhigh				# Number of high pixels to reject
char	option[ARB]			# Output option

int	i, n, nimages, naccept, npix, ndone, pass
int	const
pointer	sp, input, v1, v2, im, buf, buf1, buf_in, buf_out

bool	streq()
int	imtlen(), imtgetim(), imtrgetim()
pointer	immap(), imgnli(), impnli()
errchk	immap, imunmap, imgnli, impnli

begin
	# Initialize.
	nimages = imtlen (list)
	naccept = nimages - nlow - nhigh
	const = naccept
	npix = IM_LEN(im_out, 1)
	if (naccept < 1)
	    call error (0, "Number of rejected pixels is too large")

	# Allocate memory.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (im, nimages, TY_INT)

	# If there are no pixels to be rejected avoid calls to reject pixels
	# and do the operation in blocks so that the number of images mapped
	# does not exceed the maximum.  The output image is used to
	# store intermediate results.

	if ((nlow == 0) && (nhigh == 0)) {
	    pass = 0
	    ndone = 0
	    repeat {
		n = 0
		while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	            Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		    n = n + 1
		    if (n == IMS_MAX)
			break
	        }
		ndone = ndone + n

	        pass = pass + 1
		if (pass > 1) {
		    call imunmap (im_out)
		    im_out = immap (output, READ_WRITE, 0)
		}

	        call amovkl (long(1), Meml[v1], IM_MAXDIM)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

		# For each input line compute an output line.
		while (impnli (im_out, buf_out, Meml[v2]) != EOF) {

		    # Clear the output buffer during the first pass and
		    # read in the partial sum from the output image during
		    # subsequent passes.

		    if (pass == 1)
			call aclri (Memi[buf_out], npix)
		    else {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnli (im_out, buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call amovi (Memi[buf_in], Memi[buf_out], npix)
		    }

		    # Accumulate lines from each input image.
	    	    do i = 1, n {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnli (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call aaddi (Memi[buf_in], Memi[buf_out],
			    Memi[buf_out], npix)
		    }

		    # If all images have been accumulated and averaging then
		    # divide by the number of images.
		    if ((ndone == nimages) && streq (option, "average"))
			call adivki (Memi[buf_out], const, Memi[buf_out],
			    npix)

		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		}

		do i = 1, n
		    call imunmap (Memi[im+i-1])
	    } until (ndone == nimages)

	    # Finish up.
	    call sfree (sp)
	    return
	}
	       

	# Map the input images up to the maximum allowed.  The remainder
	# will be mapped during each line.
	n = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
	    n = n + 1
	    if (n == IMS_MAX - 1)
		break
	}

	# Allocate additional buffer space.
	call salloc (buf, nimages, TY_INT)
	if (nimages - n > 0)
	    call salloc (buf1, (nimages-n)*npix, TY_INT)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	# Compute output lines for each input line.
	while (impnli (im_out, buf_out, Meml[v2]) != EOF) {

	    # Read lines from the images which remain open.
	    for (i = 1; i <= n; i = i + 1) {
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnli (Memi[im+i-1], Memi[buf+i-1], Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
	    }

	    # For all additional images map the image, read a line, copy the
	    # data to a buffer since the image buffer is reused, and unmap
	    # the image.  
	    for (; i <= nimages; i = i + 1) {
		if (imtrgetim (list, i, Memc[input], SZ_FNAME) == EOF)
		    break
		Memi[im+i-1] = immap (Memc[input], READ_ONLY, 0)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnli (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
		Memi[buf+i-1] = buf1 + (i - n - 1) * npix
		call amovi (Memi[buf_in], Memi[Memi[buf+i-1]], npix)
		call imunmap (Memi[im+i-1])
	    }
		
	    # Reject pixels.
	    call imreji (Memi[buf], nimages, Memi[buf_out], npix, nlow, nhigh)

	    # If averaging divide the sum by the number of images averaged.
	    if ((naccept > 1) && streq (option, "average")) {
		const = naccept
		call adivki (Memi[buf_out], const, Memi[buf_out], npix)
	    }

	    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	}

	# Finish up.
	do i = 1, n
	    call imunmap (Memi[im+i-1])
	call sfree (sp)
end


# IMREJ --  Reject the number of high and low points and sum the rest.

procedure imreji (a, nvecs, b, npts, nlow, nhigh)

pointer	a[nvecs]		# Pointers to set of vectors
int	nvecs			# Number of vectors
int	b[npts]			# Output vector
int	npts			# Number of points in the vectors
int	nlow			# Number of low points to be rejected
int	nhigh			# Number of high points to be rejected

int	i, j
int	naccept, minrej, npairs, nlow1, nhigh1
real	tmedian, time1, time2

begin
	naccept = nvecs - nlow - nhigh

	# If no points are rejected return the sum.

	if (naccept == nvecs) {
	    call amovi (Memi[a[1]], b, npts)
	    for (j = 2; j <= naccept; j = j + 1)
		call aaddi (Memi[a[j]], b, b, npts)
	    return
	}

	minrej = min (nlow, nhigh)
	npairs = minrej
	nlow1 = nlow - npairs
	nhigh1 = nhigh - npairs

	if ((naccept == 1) && (npairs > 0)) {
	    if (npairs == 1) {
	        tmedian = TMED3
		npairs = npairs - 1
	    } else {
		tmedian = TMED5
	        npairs = npairs - 2
	    }
	} else
	    tmedian = 0

	# Compare the time required to reject the minimum number
	# of low or high points and extract the number of points to accept
	# with the time to reject pairs and the excess number of low or
	# high points to either reach a median of 3 or 5 points or isolate
	# the acceptable points.

	time1 = TMINSW * (minrej + naccept)
	time2 = tmedian + TMXMNSW * npairs + TMINSW * (nlow1 + nhigh1)

	i = nvecs
	if (time1 < time2) {

 	    # Sort the nlow and naccept points
	    if (nlow < nhigh) {
	        for (j = 1; j <= nlow + naccept; j = j + 1) {
	            call minswi (a, i, npts)
		    i = i - 1
	        }
	    	call amovi (Memi[a[nhigh+1]], b, npts)
		for (j = nhigh+2; j <= nhigh+naccept; j = j + 1)
		    call aaddi (Memi[a[j]], b, b, npts)

	    # Sort the nhigh and naccept points
	    } else {
	        for (j = 1; j <= nhigh + naccept; j = j + 1) {
	            call maxswi (a, i, npts)
		    i = i - 1
	        }
	    	call amovi (Memi[a[nlow+1]], b, npts)
		for (j = nlow+2; j <= nlow+naccept; j = j + 1)
		    call aaddi (Memi[a[j]], b, b, npts)
	    }

	} else {
	    # Reject the npairs low and high points.
	    for (j = 1; j <= npairs; j = j + 1) {
	        call mxmnswi (a, i, npts)
		i = i - 2
	    }
	    # Reject the excess low points.
	    for (j = 1; j <= nlow1; j = j + 1) {
	        call minswi (a, i, npts)
		i = i - 1
	    }
	    # Reject the excess high points.
	    for (j = 1; j <= nhigh1; j = j + 1) {
	        call maxswi (a, i, npts)
		i = i - 1
	    }

	    # Check if the remaining points constitute a 3 or 5 point median
	    # or the set of desired points.
	    if (tmedian == 0.) {
	        call amovi (Memi[a[1]], b, npts)
	        for (j = 2; j <= naccept; j = j + 1)
		    call aaddi (Memi[a[j]], b, b, npts)
	    } else if (tmedian == TMED3) {
		call amed3i (Memi[a[1]], Memi[a[2]], Memi[a[3]], b, npts)
	    } else {
		call amed5i (Memi[a[1]], Memi[a[2]], Memi[a[3]],
		        Memi[a[4]], Memi[a[5]], b, npts)
	    }
	}
end


# MINSW -- Given an array of vector pointers for each element in the vectors
# swap the minimum element with that of the last vector.

procedure minswi (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmin
int	temp

begin
	do i = 0, npts - 1 {
	    kmin = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memi[k] < Memi[kmin])
		    kmin = k
	    }
	    if (k != kmin) {
	        temp = Memi[k]
	        Memi[k] = Memi[kmin]
	        Memi[kmin] = temp
	    }
	}
end


# MAXSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector.

procedure maxswi (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax
int	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memi[k] > Memi[kmax])
		    kmax = k
	    }
	    if (k != kmax) {
	        temp = Memi[k]
	        Memi[k] = Memi[kmax]
	        Memi[kmax] = temp
	    }
	}
end


# MXMNSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector and the minimum element
# with that of the next to last vector.  The number of vectors must be greater
# than 1.

procedure mxmnswi (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax, kmin
int	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    kmin = kmax
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memi[k] > Memi[kmax])
		    kmax = k
		else if (Memi[k] < Memi[kmin])
		    kmin = k
	    }
	    temp = Memi[k]
	    Memi[k] = Memi[kmax]
	    Memi[kmax] = temp
	    if (kmin == k) {
	        j = a[nvecs - 1] + i
	        temp = Memi[j]
		Memi[j] = Memi[kmax]
		Memi[kmax] = temp
	    } else {
	        j = a[nvecs - 1] + i
	        temp = Memi[j]
	        Memi[j] = Memi[kmin]
	        Memi[kmin] = temp
	    }
	}
end

procedure imsuml (list, output, im_out, nlow, nhigh, option)

int	list				# List of input images
char	output[ARB]			# Output image
pointer	im_out				# Output image pointer
int	nlow				# Number of low pixels to reject
int	nhigh				# Number of high pixels to reject
char	option[ARB]			# Output option

int	i, n, nimages, naccept, npix, ndone, pass
long	const
pointer	sp, input, v1, v2, im, buf, buf1, buf_in, buf_out

bool	streq()
int	imtlen(), imtgetim(), imtrgetim()
pointer	immap(), imgnll(), impnll()
errchk	immap, imunmap, imgnll, impnll

begin
	# Initialize.
	nimages = imtlen (list)
	naccept = nimages - nlow - nhigh
	const = naccept
	npix = IM_LEN(im_out, 1)
	if (naccept < 1)
	    call error (0, "Number of rejected pixels is too large")

	# Allocate memory.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (im, nimages, TY_INT)

	# If there are no pixels to be rejected avoid calls to reject pixels
	# and do the operation in blocks so that the number of images mapped
	# does not exceed the maximum.  The output image is used to
	# store intermediate results.

	if ((nlow == 0) && (nhigh == 0)) {
	    pass = 0
	    ndone = 0
	    repeat {
		n = 0
		while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	            Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		    n = n + 1
		    if (n == IMS_MAX)
			break
	        }
		ndone = ndone + n

	        pass = pass + 1
		if (pass > 1) {
		    call imunmap (im_out)
		    im_out = immap (output, READ_WRITE, 0)
		}

	        call amovkl (long(1), Meml[v1], IM_MAXDIM)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

		# For each input line compute an output line.
		while (impnll (im_out, buf_out, Meml[v2]) != EOF) {

		    # Clear the output buffer during the first pass and
		    # read in the partial sum from the output image during
		    # subsequent passes.

		    if (pass == 1)
			call aclrl (Meml[buf_out], npix)
		    else {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnll (im_out, buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call amovl (Meml[buf_in], Meml[buf_out], npix)
		    }

		    # Accumulate lines from each input image.
	    	    do i = 1, n {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnll (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call aaddl (Meml[buf_in], Meml[buf_out],
			    Meml[buf_out], npix)
		    }

		    # If all images have been accumulated and averaging then
		    # divide by the number of images.
		    if ((ndone == nimages) && streq (option, "average"))
			call adivkl (Meml[buf_out], const, Meml[buf_out],
			    npix)

		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		}

		do i = 1, n
		    call imunmap (Memi[im+i-1])
	    } until (ndone == nimages)

	    # Finish up.
	    call sfree (sp)
	    return
	}
	       

	# Map the input images up to the maximum allowed.  The remainder
	# will be mapped during each line.
	n = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
	    n = n + 1
	    if (n == IMS_MAX - 1)
		break
	}

	# Allocate additional buffer space.
	call salloc (buf, nimages, TY_INT)
	if (nimages - n > 0)
	    call salloc (buf1, (nimages-n)*npix, TY_LONG)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	# Compute output lines for each input line.
	while (impnll (im_out, buf_out, Meml[v2]) != EOF) {

	    # Read lines from the images which remain open.
	    for (i = 1; i <= n; i = i + 1) {
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnll (Memi[im+i-1], Memi[buf+i-1], Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
	    }

	    # For all additional images map the image, read a line, copy the
	    # data to a buffer since the image buffer is reused, and unmap
	    # the image.  
	    for (; i <= nimages; i = i + 1) {
		if (imtrgetim (list, i, Memc[input], SZ_FNAME) == EOF)
		    break
		Memi[im+i-1] = immap (Memc[input], READ_ONLY, 0)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnll (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
		Memi[buf+i-1] = buf1 + (i - n - 1) * npix
		call amovl (Meml[buf_in], Meml[Memi[buf+i-1]], npix)
		call imunmap (Memi[im+i-1])
	    }
		
	    # Reject pixels.
	    call imrejl (Memi[buf], nimages, Meml[buf_out], npix, nlow, nhigh)

	    # If averaging divide the sum by the number of images averaged.
	    if ((naccept > 1) && streq (option, "average")) {
		const = naccept
		call adivkl (Meml[buf_out], const, Meml[buf_out], npix)
	    }

	    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	}

	# Finish up.
	do i = 1, n
	    call imunmap (Memi[im+i-1])
	call sfree (sp)
end


# IMREJ --  Reject the number of high and low points and sum the rest.

procedure imrejl (a, nvecs, b, npts, nlow, nhigh)

pointer	a[nvecs]		# Pointers to set of vectors
int	nvecs			# Number of vectors
long	b[npts]			# Output vector
int	npts			# Number of points in the vectors
int	nlow			# Number of low points to be rejected
int	nhigh			# Number of high points to be rejected

int	i, j
int	naccept, minrej, npairs, nlow1, nhigh1
real	tmedian, time1, time2

begin
	naccept = nvecs - nlow - nhigh

	# If no points are rejected return the sum.

	if (naccept == nvecs) {
	    call amovl (Meml[a[1]], b, npts)
	    for (j = 2; j <= naccept; j = j + 1)
		call aaddl (Meml[a[j]], b, b, npts)
	    return
	}

	minrej = min (nlow, nhigh)
	npairs = minrej
	nlow1 = nlow - npairs
	nhigh1 = nhigh - npairs

	if ((naccept == 1) && (npairs > 0)) {
	    if (npairs == 1) {
	        tmedian = TMED3
		npairs = npairs - 1
	    } else {
		tmedian = TMED5
	        npairs = npairs - 2
	    }
	} else
	    tmedian = 0

	# Compare the time required to reject the minimum number
	# of low or high points and extract the number of points to accept
	# with the time to reject pairs and the excess number of low or
	# high points to either reach a median of 3 or 5 points or isolate
	# the acceptable points.

	time1 = TMINSW * (minrej + naccept)
	time2 = tmedian + TMXMNSW * npairs + TMINSW * (nlow1 + nhigh1)

	i = nvecs
	if (time1 < time2) {

 	    # Sort the nlow and naccept points
	    if (nlow < nhigh) {
	        for (j = 1; j <= nlow + naccept; j = j + 1) {
	            call minswl (a, i, npts)
		    i = i - 1
	        }
	    	call amovl (Meml[a[nhigh+1]], b, npts)
		for (j = nhigh+2; j <= nhigh+naccept; j = j + 1)
		    call aaddl (Meml[a[j]], b, b, npts)

	    # Sort the nhigh and naccept points
	    } else {
	        for (j = 1; j <= nhigh + naccept; j = j + 1) {
	            call maxswl (a, i, npts)
		    i = i - 1
	        }
	    	call amovl (Meml[a[nlow+1]], b, npts)
		for (j = nlow+2; j <= nlow+naccept; j = j + 1)
		    call aaddl (Meml[a[j]], b, b, npts)
	    }

	} else {
	    # Reject the npairs low and high points.
	    for (j = 1; j <= npairs; j = j + 1) {
	        call mxmnswl (a, i, npts)
		i = i - 2
	    }
	    # Reject the excess low points.
	    for (j = 1; j <= nlow1; j = j + 1) {
	        call minswl (a, i, npts)
		i = i - 1
	    }
	    # Reject the excess high points.
	    for (j = 1; j <= nhigh1; j = j + 1) {
	        call maxswl (a, i, npts)
		i = i - 1
	    }

	    # Check if the remaining points constitute a 3 or 5 point median
	    # or the set of desired points.
	    if (tmedian == 0.) {
	        call amovl (Meml[a[1]], b, npts)
	        for (j = 2; j <= naccept; j = j + 1)
		    call aaddl (Meml[a[j]], b, b, npts)
	    } else if (tmedian == TMED3) {
		call amed3l (Meml[a[1]], Meml[a[2]], Meml[a[3]], b, npts)
	    } else {
		call amed5l (Meml[a[1]], Meml[a[2]], Meml[a[3]],
		        Meml[a[4]], Meml[a[5]], b, npts)
	    }
	}
end


# MINSW -- Given an array of vector pointers for each element in the vectors
# swap the minimum element with that of the last vector.

procedure minswl (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmin
long	temp

begin
	do i = 0, npts - 1 {
	    kmin = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Meml[k] < Meml[kmin])
		    kmin = k
	    }
	    if (k != kmin) {
	        temp = Meml[k]
	        Meml[k] = Meml[kmin]
	        Meml[kmin] = temp
	    }
	}
end


# MAXSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector.

procedure maxswl (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax
long	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Meml[k] > Meml[kmax])
		    kmax = k
	    }
	    if (k != kmax) {
	        temp = Meml[k]
	        Meml[k] = Meml[kmax]
	        Meml[kmax] = temp
	    }
	}
end


# MXMNSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector and the minimum element
# with that of the next to last vector.  The number of vectors must be greater
# than 1.

procedure mxmnswl (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax, kmin
long	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    kmin = kmax
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Meml[k] > Meml[kmax])
		    kmax = k
		else if (Meml[k] < Meml[kmin])
		    kmin = k
	    }
	    temp = Meml[k]
	    Meml[k] = Meml[kmax]
	    Meml[kmax] = temp
	    if (kmin == k) {
	        j = a[nvecs - 1] + i
	        temp = Meml[j]
		Meml[j] = Meml[kmax]
		Meml[kmax] = temp
	    } else {
	        j = a[nvecs - 1] + i
	        temp = Meml[j]
	        Meml[j] = Meml[kmin]
	        Meml[kmin] = temp
	    }
	}
end

procedure imsumr (list, output, im_out, nlow, nhigh, option)

int	list				# List of input images
char	output[ARB]			# Output image
pointer	im_out				# Output image pointer
int	nlow				# Number of low pixels to reject
int	nhigh				# Number of high pixels to reject
char	option[ARB]			# Output option

int	i, n, nimages, naccept, npix, ndone, pass
real	const
pointer	sp, input, v1, v2, im, buf, buf1, buf_in, buf_out

bool	streq()
int	imtlen(), imtgetim(), imtrgetim()
pointer	immap(), imgnlr(), impnlr()
errchk	immap, imunmap, imgnlr, impnlr

begin
	# Initialize.
	nimages = imtlen (list)
	naccept = nimages - nlow - nhigh
	const = naccept
	npix = IM_LEN(im_out, 1)
	if (naccept < 1)
	    call error (0, "Number of rejected pixels is too large")

	# Allocate memory.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (im, nimages, TY_INT)

	# If there are no pixels to be rejected avoid calls to reject pixels
	# and do the operation in blocks so that the number of images mapped
	# does not exceed the maximum.  The output image is used to
	# store intermediate results.

	if ((nlow == 0) && (nhigh == 0)) {
	    pass = 0
	    ndone = 0
	    repeat {
		n = 0
		while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	            Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		    n = n + 1
		    if (n == IMS_MAX)
			break
	        }
		ndone = ndone + n

	        pass = pass + 1
		if (pass > 1) {
		    call imunmap (im_out)
		    im_out = immap (output, READ_WRITE, 0)
		}

	        call amovkl (long(1), Meml[v1], IM_MAXDIM)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

		# For each input line compute an output line.
		while (impnlr (im_out, buf_out, Meml[v2]) != EOF) {

		    # Clear the output buffer during the first pass and
		    # read in the partial sum from the output image during
		    # subsequent passes.

		    if (pass == 1)
			call aclrr (Memr[buf_out], npix)
		    else {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnlr (im_out, buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call amovr (Memr[buf_in], Memr[buf_out], npix)
		    }

		    # Accumulate lines from each input image.
	    	    do i = 1, n {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnlr (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call aaddr (Memr[buf_in], Memr[buf_out],
			    Memr[buf_out], npix)
		    }

		    # If all images have been accumulated and averaging then
		    # divide by the number of images.
		    if ((ndone == nimages) && streq (option, "average"))
			call adivkr (Memr[buf_out], const, Memr[buf_out],
			    npix)

		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		}

		do i = 1, n
		    call imunmap (Memi[im+i-1])
	    } until (ndone == nimages)

	    # Finish up.
	    call sfree (sp)
	    return
	}
	       

	# Map the input images up to the maximum allowed.  The remainder
	# will be mapped during each line.
	n = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
	    n = n + 1
	    if (n == IMS_MAX - 1)
		break
	}

	# Allocate additional buffer space.
	call salloc (buf, nimages, TY_INT)
	if (nimages - n > 0)
	    call salloc (buf1, (nimages-n)*npix, TY_REAL)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	# Compute output lines for each input line.
	while (impnlr (im_out, buf_out, Meml[v2]) != EOF) {

	    # Read lines from the images which remain open.
	    for (i = 1; i <= n; i = i + 1) {
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnlr (Memi[im+i-1], Memi[buf+i-1], Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
	    }

	    # For all additional images map the image, read a line, copy the
	    # data to a buffer since the image buffer is reused, and unmap
	    # the image.  
	    for (; i <= nimages; i = i + 1) {
		if (imtrgetim (list, i, Memc[input], SZ_FNAME) == EOF)
		    break
		Memi[im+i-1] = immap (Memc[input], READ_ONLY, 0)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnlr (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
		Memi[buf+i-1] = buf1 + (i - n - 1) * npix
		call amovr (Memr[buf_in], Memr[Memi[buf+i-1]], npix)
		call imunmap (Memi[im+i-1])
	    }
		
	    # Reject pixels.
	    call imrejr (Memi[buf], nimages, Memr[buf_out], npix, nlow, nhigh)

	    # If averaging divide the sum by the number of images averaged.
	    if ((naccept > 1) && streq (option, "average")) {
		const = naccept
		call adivkr (Memr[buf_out], const, Memr[buf_out], npix)
	    }

	    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	}

	# Finish up.
	do i = 1, n
	    call imunmap (Memi[im+i-1])
	call sfree (sp)
end


# IMREJ --  Reject the number of high and low points and sum the rest.

procedure imrejr (a, nvecs, b, npts, nlow, nhigh)

pointer	a[nvecs]		# Pointers to set of vectors
int	nvecs			# Number of vectors
real	b[npts]			# Output vector
int	npts			# Number of points in the vectors
int	nlow			# Number of low points to be rejected
int	nhigh			# Number of high points to be rejected

int	i, j
int	naccept, minrej, npairs, nlow1, nhigh1
real	tmedian, time1, time2

begin
	naccept = nvecs - nlow - nhigh

	# If no points are rejected return the sum.

	if (naccept == nvecs) {
	    call amovr (Memr[a[1]], b, npts)
	    for (j = 2; j <= naccept; j = j + 1)
		call aaddr (Memr[a[j]], b, b, npts)
	    return
	}

	minrej = min (nlow, nhigh)
	npairs = minrej
	nlow1 = nlow - npairs
	nhigh1 = nhigh - npairs

	if ((naccept == 1) && (npairs > 0)) {
	    if (npairs == 1) {
	        tmedian = TMED3
		npairs = npairs - 1
	    } else {
		tmedian = TMED5
	        npairs = npairs - 2
	    }
	} else
	    tmedian = 0

	# Compare the time required to reject the minimum number
	# of low or high points and extract the number of points to accept
	# with the time to reject pairs and the excess number of low or
	# high points to either reach a median of 3 or 5 points or isolate
	# the acceptable points.

	time1 = TMINSW * (minrej + naccept)
	time2 = tmedian + TMXMNSW * npairs + TMINSW * (nlow1 + nhigh1)

	i = nvecs
	if (time1 < time2) {

 	    # Sort the nlow and naccept points
	    if (nlow < nhigh) {
	        for (j = 1; j <= nlow + naccept; j = j + 1) {
	            call minswr (a, i, npts)
		    i = i - 1
	        }
	    	call amovr (Memr[a[nhigh+1]], b, npts)
		for (j = nhigh+2; j <= nhigh+naccept; j = j + 1)
		    call aaddr (Memr[a[j]], b, b, npts)

	    # Sort the nhigh and naccept points
	    } else {
	        for (j = 1; j <= nhigh + naccept; j = j + 1) {
	            call maxswr (a, i, npts)
		    i = i - 1
	        }
	    	call amovr (Memr[a[nlow+1]], b, npts)
		for (j = nlow+2; j <= nlow+naccept; j = j + 1)
		    call aaddr (Memr[a[j]], b, b, npts)
	    }

	} else {
	    # Reject the npairs low and high points.
	    for (j = 1; j <= npairs; j = j + 1) {
	        call mxmnswr (a, i, npts)
		i = i - 2
	    }
	    # Reject the excess low points.
	    for (j = 1; j <= nlow1; j = j + 1) {
	        call minswr (a, i, npts)
		i = i - 1
	    }
	    # Reject the excess high points.
	    for (j = 1; j <= nhigh1; j = j + 1) {
	        call maxswr (a, i, npts)
		i = i - 1
	    }

	    # Check if the remaining points constitute a 3 or 5 point median
	    # or the set of desired points.
	    if (tmedian == 0.) {
	        call amovr (Memr[a[1]], b, npts)
	        for (j = 2; j <= naccept; j = j + 1)
		    call aaddr (Memr[a[j]], b, b, npts)
	    } else if (tmedian == TMED3) {
		call amed3r (Memr[a[1]], Memr[a[2]], Memr[a[3]], b, npts)
	    } else {
		call amed5r (Memr[a[1]], Memr[a[2]], Memr[a[3]],
		        Memr[a[4]], Memr[a[5]], b, npts)
	    }
	}
end


# MINSW -- Given an array of vector pointers for each element in the vectors
# swap the minimum element with that of the last vector.

procedure minswr (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmin
real	temp

begin
	do i = 0, npts - 1 {
	    kmin = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memr[k] < Memr[kmin])
		    kmin = k
	    }
	    if (k != kmin) {
	        temp = Memr[k]
	        Memr[k] = Memr[kmin]
	        Memr[kmin] = temp
	    }
	}
end


# MAXSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector.

procedure maxswr (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax
real	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memr[k] > Memr[kmax])
		    kmax = k
	    }
	    if (k != kmax) {
	        temp = Memr[k]
	        Memr[k] = Memr[kmax]
	        Memr[kmax] = temp
	    }
	}
end


# MXMNSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector and the minimum element
# with that of the next to last vector.  The number of vectors must be greater
# than 1.

procedure mxmnswr (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax, kmin
real	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    kmin = kmax
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memr[k] > Memr[kmax])
		    kmax = k
		else if (Memr[k] < Memr[kmin])
		    kmin = k
	    }
	    temp = Memr[k]
	    Memr[k] = Memr[kmax]
	    Memr[kmax] = temp
	    if (kmin == k) {
	        j = a[nvecs - 1] + i
	        temp = Memr[j]
		Memr[j] = Memr[kmax]
		Memr[kmax] = temp
	    } else {
	        j = a[nvecs - 1] + i
	        temp = Memr[j]
	        Memr[j] = Memr[kmin]
	        Memr[kmin] = temp
	    }
	}
end

procedure imsumd (list, output, im_out, nlow, nhigh, option)

int	list				# List of input images
char	output[ARB]			# Output image
pointer	im_out				# Output image pointer
int	nlow				# Number of low pixels to reject
int	nhigh				# Number of high pixels to reject
char	option[ARB]			# Output option

int	i, n, nimages, naccept, npix, ndone, pass
double	const
pointer	sp, input, v1, v2, im, buf, buf1, buf_in, buf_out

bool	streq()
int	imtlen(), imtgetim(), imtrgetim()
pointer	immap(), imgnld(), impnld()
errchk	immap, imunmap, imgnld, impnld

begin
	# Initialize.
	nimages = imtlen (list)
	naccept = nimages - nlow - nhigh
	const = naccept
	npix = IM_LEN(im_out, 1)
	if (naccept < 1)
	    call error (0, "Number of rejected pixels is too large")

	# Allocate memory.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (im, nimages, TY_INT)

	# If there are no pixels to be rejected avoid calls to reject pixels
	# and do the operation in blocks so that the number of images mapped
	# does not exceed the maximum.  The output image is used to
	# store intermediate results.

	if ((nlow == 0) && (nhigh == 0)) {
	    pass = 0
	    ndone = 0
	    repeat {
		n = 0
		while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	            Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
		    n = n + 1
		    if (n == IMS_MAX)
			break
	        }
		ndone = ndone + n

	        pass = pass + 1
		if (pass > 1) {
		    call imunmap (im_out)
		    im_out = immap (output, READ_WRITE, 0)
		}

	        call amovkl (long(1), Meml[v1], IM_MAXDIM)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

		# For each input line compute an output line.
		while (impnld (im_out, buf_out, Meml[v2]) != EOF) {

		    # Clear the output buffer during the first pass and
		    # read in the partial sum from the output image during
		    # subsequent passes.

		    if (pass == 1)
			call aclrd (Memd[buf_out], npix)
		    else {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnld (im_out, buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call amovd (Memd[buf_in], Memd[buf_out], npix)
		    }

		    # Accumulate lines from each input image.
	    	    do i = 1, n {
			call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
			if (imgnld (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    	    call error (0, "Error reading input image")
			call aaddd (Memd[buf_in], Memd[buf_out],
			    Memd[buf_out], npix)
		    }

		    # If all images have been accumulated and averaging then
		    # divide by the number of images.
		    if ((ndone == nimages) && streq (option, "average"))
			call adivkd (Memd[buf_out], const, Memd[buf_out],
			    npix)

		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		}

		do i = 1, n
		    call imunmap (Memi[im+i-1])
	    } until (ndone == nimages)

	    # Finish up.
	    call sfree (sp)
	    return
	}
	       

	# Map the input images up to the maximum allowed.  The remainder
	# will be mapped during each line.
	n = 0
	while (imtgetim (list, Memc[input], SZ_FNAME) != EOF) {
	    Memi[im+n] = immap (Memc[input], READ_ONLY, 0)
	    n = n + 1
	    if (n == IMS_MAX - 1)
		break
	}

	# Allocate additional buffer space.
	call salloc (buf, nimages, TY_INT)
	if (nimages - n > 0)
	    call salloc (buf1, (nimages-n)*npix, TY_DOUBLE)

	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovl (Meml[v1], Meml[v2], IM_MAXDIM)

	# Compute output lines for each input line.
	while (impnld (im_out, buf_out, Meml[v2]) != EOF) {

	    # Read lines from the images which remain open.
	    for (i = 1; i <= n; i = i + 1) {
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnld (Memi[im+i-1], Memi[buf+i-1], Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
	    }

	    # For all additional images map the image, read a line, copy the
	    # data to a buffer since the image buffer is reused, and unmap
	    # the image.  
	    for (; i <= nimages; i = i + 1) {
		if (imtrgetim (list, i, Memc[input], SZ_FNAME) == EOF)
		    break
		Memi[im+i-1] = immap (Memc[input], READ_ONLY, 0)
		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
		if (imgnld (Memi[im+i-1], buf_in, Meml[v2]) == EOF)
		    call error (0, "Error reading input image")
		Memi[buf+i-1] = buf1 + (i - n - 1) * npix
		call amovd (Memd[buf_in], Memd[Memi[buf+i-1]], npix)
		call imunmap (Memi[im+i-1])
	    }
		
	    # Reject pixels.
	    call imrejd (Memi[buf], nimages, Memd[buf_out], npix, nlow, nhigh)

	    # If averaging divide the sum by the number of images averaged.
	    if ((naccept > 1) && streq (option, "average")) {
		const = naccept
		call adivkd (Memd[buf_out], const, Memd[buf_out], npix)
	    }

	    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
	}

	# Finish up.
	do i = 1, n
	    call imunmap (Memi[im+i-1])
	call sfree (sp)
end


# IMREJ --  Reject the number of high and low points and sum the rest.

procedure imrejd (a, nvecs, b, npts, nlow, nhigh)

pointer	a[nvecs]		# Pointers to set of vectors
int	nvecs			# Number of vectors
double	b[npts]			# Output vector
int	npts			# Number of points in the vectors
int	nlow			# Number of low points to be rejected
int	nhigh			# Number of high points to be rejected

int	i, j
int	naccept, minrej, npairs, nlow1, nhigh1
real	tmedian, time1, time2

begin
	naccept = nvecs - nlow - nhigh

	# If no points are rejected return the sum.

	if (naccept == nvecs) {
	    call amovd (Memd[a[1]], b, npts)
	    for (j = 2; j <= naccept; j = j + 1)
		call aaddd (Memd[a[j]], b, b, npts)
	    return
	}

	minrej = min (nlow, nhigh)
	npairs = minrej
	nlow1 = nlow - npairs
	nhigh1 = nhigh - npairs

	if ((naccept == 1) && (npairs > 0)) {
	    if (npairs == 1) {
	        tmedian = TMED3
		npairs = npairs - 1
	    } else {
		tmedian = TMED5
	        npairs = npairs - 2
	    }
	} else
	    tmedian = 0

	# Compare the time required to reject the minimum number
	# of low or high points and extract the number of points to accept
	# with the time to reject pairs and the excess number of low or
	# high points to either reach a median of 3 or 5 points or isolate
	# the acceptable points.

	time1 = TMINSW * (minrej + naccept)
	time2 = tmedian + TMXMNSW * npairs + TMINSW * (nlow1 + nhigh1)

	i = nvecs
	if (time1 < time2) {

 	    # Sort the nlow and naccept points
	    if (nlow < nhigh) {
	        for (j = 1; j <= nlow + naccept; j = j + 1) {
	            call minswd (a, i, npts)
		    i = i - 1
	        }
	    	call amovd (Memd[a[nhigh+1]], b, npts)
		for (j = nhigh+2; j <= nhigh+naccept; j = j + 1)
		    call aaddd (Memd[a[j]], b, b, npts)

	    # Sort the nhigh and naccept points
	    } else {
	        for (j = 1; j <= nhigh + naccept; j = j + 1) {
	            call maxswd (a, i, npts)
		    i = i - 1
	        }
	    	call amovd (Memd[a[nlow+1]], b, npts)
		for (j = nlow+2; j <= nlow+naccept; j = j + 1)
		    call aaddd (Memd[a[j]], b, b, npts)
	    }

	} else {
	    # Reject the npairs low and high points.
	    for (j = 1; j <= npairs; j = j + 1) {
	        call mxmnswd (a, i, npts)
		i = i - 2
	    }
	    # Reject the excess low points.
	    for (j = 1; j <= nlow1; j = j + 1) {
	        call minswd (a, i, npts)
		i = i - 1
	    }
	    # Reject the excess high points.
	    for (j = 1; j <= nhigh1; j = j + 1) {
	        call maxswd (a, i, npts)
		i = i - 1
	    }

	    # Check if the remaining points constitute a 3 or 5 point median
	    # or the set of desired points.
	    if (tmedian == 0.) {
	        call amovd (Memd[a[1]], b, npts)
	        for (j = 2; j <= naccept; j = j + 1)
		    call aaddd (Memd[a[j]], b, b, npts)
	    } else if (tmedian == TMED3) {
		call amed3d (Memd[a[1]], Memd[a[2]], Memd[a[3]], b, npts)
	    } else {
		call amed5d (Memd[a[1]], Memd[a[2]], Memd[a[3]],
		        Memd[a[4]], Memd[a[5]], b, npts)
	    }
	}
end


# MINSW -- Given an array of vector pointers for each element in the vectors
# swap the minimum element with that of the last vector.

procedure minswd (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmin
double	temp

begin
	do i = 0, npts - 1 {
	    kmin = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memd[k] < Memd[kmin])
		    kmin = k
	    }
	    if (k != kmin) {
	        temp = Memd[k]
	        Memd[k] = Memd[kmin]
	        Memd[kmin] = temp
	    }
	}
end


# MAXSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector.

procedure maxswd (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax
double	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memd[k] > Memd[kmax])
		    kmax = k
	    }
	    if (k != kmax) {
	        temp = Memd[k]
	        Memd[k] = Memd[kmax]
	        Memd[kmax] = temp
	    }
	}
end


# MXMNSW -- Given an array of vector pointers for each element in the vectors
# swap the maximum element with that of the last vector and the minimum element
# with that of the next to last vector.  The number of vectors must be greater
# than 1.

procedure mxmnswd (a, nvecs, npts)

pointer	a[nvecs]			# Array of vector pointers
int	nvecs				# Number of vectors
int	npts				# Number of points in the vectors

int	i, j, k, kmax, kmin
double	temp

begin
	do i = 0, npts - 1 {
	    kmax = a[1] + i
	    kmin = kmax
	    do j = 2, nvecs {
		k = a[j] + i
	        if (Memd[k] > Memd[kmax])
		    kmax = k
		else if (Memd[k] < Memd[kmin])
		    kmin = k
	    }
	    temp = Memd[k]
	    Memd[k] = Memd[kmax]
	    Memd[kmax] = temp
	    if (kmin == k) {
	        j = a[nvecs - 1] + i
	        temp = Memd[j]
		Memd[j] = Memd[kmax]
		Memd[kmax] = temp
	    } else {
	        j = a[nvecs - 1] + i
	        temp = Memd[j]
	        Memd[j] = Memd[kmin]
	        Memd[kmin] = temp
	    }
	}
end

