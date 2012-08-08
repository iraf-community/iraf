# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>

# MAXMIN -- Get the minimum and maximum pixel values of an image.  If valid
# header values are available they are used, otherwise the image is sampled
# on an even grid and the min and max values of this sample are returned.

procedure maxmin (im, zmin, zmax, nsample_lines)

pointer	im
real	zmin, zmax		# min and max intensity values
int	nsample_lines		# amount of image to sample

int	step, ncols, nlines, sample_size, imlines, i
real	minval, maxval
pointer	imgl2r()

begin
	# Only calculate minimum, maximum pixel values if the current
	# values are unknown, or if the image was modified since the
	# old values were computed.

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	if (IM_LIMTIME(im) >= IM_MTIME(im)) {
	    # Use min and max values in image header if they are up to date.
	    zmin = IM_MIN(im)
	    zmax = IM_MAX(im)

	} else {
	    zmin = MAX_REAL
	    zmax = -MAX_REAL

	    # Try to include a constant number of pixels in the sample
	    # regardless of the image size.  The entire image is used if we
	    # have a small image, and at least sample_lines lines are read
	    # if we have a large image.

	    sample_size = 512 * nsample_lines
	    imlines = min(nlines, max(nsample_lines, sample_size / ncols))
	    step = nlines / (imlines + 1)

	    do i = 1 + step, nlines, max (1, step) {
		call alimr (Memr[imgl2r(im,i)], ncols, minval, maxval)
		zmin = min (zmin, minval)
		zmax = max (zmax, maxval)
	    }
	}
end
