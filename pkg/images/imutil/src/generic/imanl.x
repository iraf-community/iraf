# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMA_NL -- For each line in the output image lines from the input images
# are returned.  The input images are repeated as necessary.  EOF is returned
# when the last line of the output image has been reached.  One dimensional
# images are read only once and the data pointers are assumed to be unchanged
# from previous calls.  The image line vectors must be initialized externally
# and then left untouched.
# 
# This procedure is typically used when operations upon lines or pixels
# make sense in mixed dimensioned images.  For example to add a one dimensional
# image to all lines of a higher dimensional image or to subtract a
# two dimensional image from all bands of three dimensional image.
# The lengths of the common dimensions should generally be checked
# for equality with xt_imleneq.


int procedure ima_nls (im, data, v, nimages)

pointer	im[nimages]		# IMIO pointers; the first one is the output
pointer	data[nimages]		# Returned data pointers
long	v[IM_MAXDIM, nimages]	# Line vectors
int	nimages			# Number of images

int	i

int	impnls(), imgnls()

begin
	if (impnls (im[1], data[1], v[1,1]) == EOF)
	    return (EOF)

	for (i=2; i <= nimages; i=i+1) {
	    if (imgnls (im[i], data[i], v[1,i]) == EOF) {
	        if (IM_NDIM(im[i]) > 1) {
	            call amovkl (long(1), v[1,i], IM_MAXDIM)
                    if (imgnls (im[i], data[i], v[1,i]) == EOF)
			call error (0, "Error reading image line")
	        }
	    }
	}

	return (OK)
end

int procedure ima_nli (im, data, v, nimages)

pointer	im[nimages]		# IMIO pointers; the first one is the output
pointer	data[nimages]		# Returned data pointers
long	v[IM_MAXDIM, nimages]	# Line vectors
int	nimages			# Number of images

int	i

int	impnli(), imgnli()

begin
	if (impnli (im[1], data[1], v[1,1]) == EOF)
	    return (EOF)

	for (i=2; i <= nimages; i=i+1) {
	    if (imgnli (im[i], data[i], v[1,i]) == EOF) {
	        if (IM_NDIM(im[i]) > 1) {
	            call amovkl (long(1), v[1,i], IM_MAXDIM)
                    if (imgnli (im[i], data[i], v[1,i]) == EOF)
			call error (0, "Error reading image line")
	        }
	    }
	}

	return (OK)
end

int procedure ima_nll (im, data, v, nimages)

pointer	im[nimages]		# IMIO pointers; the first one is the output
pointer	data[nimages]		# Returned data pointers
long	v[IM_MAXDIM, nimages]	# Line vectors
int	nimages			# Number of images

int	i

int	impnll(), imgnll()

begin
	if (impnll (im[1], data[1], v[1,1]) == EOF)
	    return (EOF)

	for (i=2; i <= nimages; i=i+1) {
	    if (imgnll (im[i], data[i], v[1,i]) == EOF) {
	        if (IM_NDIM(im[i]) > 1) {
	            call amovkl (long(1), v[1,i], IM_MAXDIM)
                    if (imgnll (im[i], data[i], v[1,i]) == EOF)
			call error (0, "Error reading image line")
	        }
	    }
	}

	return (OK)
end

int procedure ima_nlr (im, data, v, nimages)

pointer	im[nimages]		# IMIO pointers; the first one is the output
pointer	data[nimages]		# Returned data pointers
long	v[IM_MAXDIM, nimages]	# Line vectors
int	nimages			# Number of images

int	i

int	impnlr(), imgnlr()

begin
	if (impnlr (im[1], data[1], v[1,1]) == EOF)
	    return (EOF)

	for (i=2; i <= nimages; i=i+1) {
	    if (imgnlr (im[i], data[i], v[1,i]) == EOF) {
	        if (IM_NDIM(im[i]) > 1) {
	            call amovkl (long(1), v[1,i], IM_MAXDIM)
                    if (imgnlr (im[i], data[i], v[1,i]) == EOF)
			call error (0, "Error reading image line")
	        }
	    }
	}

	return (OK)
end

int procedure ima_nld (im, data, v, nimages)

pointer	im[nimages]		# IMIO pointers; the first one is the output
pointer	data[nimages]		# Returned data pointers
long	v[IM_MAXDIM, nimages]	# Line vectors
int	nimages			# Number of images

int	i

int	impnld(), imgnld()

begin
	if (impnld (im[1], data[1], v[1,1]) == EOF)
	    return (EOF)

	for (i=2; i <= nimages; i=i+1) {
	    if (imgnld (im[i], data[i], v[1,i]) == EOF) {
	        if (IM_NDIM(im[i]) > 1) {
	            call amovkl (long(1), v[1,i], IM_MAXDIM)
                    if (imgnld (im[i], data[i], v[1,i]) == EOF)
			call error (0, "Error reading image line")
	        }
	    }
	}

	return (OK)
end

