# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IM_MINMAX -- Compute the minimum and maximum pixel values of an image.
# Works for images of any dimensionality, size, or datatype, although
# the min and max values can currently only be stored in the image header
# as real values.

procedure im_minmax (im, min_value, max_value)

pointer	im				# image descriptor
real	min_value			# minimum pixel value in image (out)
real	max_value			# maximum pixel value in image (out)

pointer	buf
bool	first_line
long	v[IM_MAXDIM]
short	minval_s, maxval_s
long	minval_l, maxval_l
real	minval_r, maxval_r
int	imgnls(), imgnll(), imgnlr()
errchk	amovkl, imgnls, imgnll, imgnlr, alims, aliml, alimr

begin
	call amovkl (long(1), v, IM_MAXDIM)		# start vector
	first_line = true
	min_value = INDEF
	max_value = INDEF

	switch (IM_PIXTYPE(im)) {
	case TY_SHORT:
	    while (imgnls (im, buf, v) != EOF) {
		call alims (Mems[buf], IM_LEN(im,1), minval_s, maxval_s)
		if (first_line) {
		    min_value = minval_s
		    max_value = maxval_s
		    first_line = false
		} else {
		    if (minval_s < min_value)
			min_value = minval_s
		    if (maxval_s > max_value)
			max_value = maxval_s
		}
	    }
	case TY_USHORT, TY_INT, TY_LONG:
	    while (imgnll (im, buf, v) != EOF) {
		call aliml (Meml[buf], IM_LEN(im,1), minval_l, maxval_l)
		if (first_line) {
		    min_value = minval_l
		    max_value = maxval_l
		    first_line = false
		} else {
		    if (minval_l < min_value)
			min_value = minval_l
		    if (maxval_l > max_value)
			max_value = maxval_l
		}
	    }
	default:
	    while (imgnlr (im, buf, v) != EOF) {
		call alimr (Memr[buf], IM_LEN(im,1), minval_r, maxval_r)
		if (first_line) {
		    min_value = minval_r
		    max_value = maxval_r
		    first_line = false
		} else {
		    if (minval_r < min_value)
			min_value = minval_r
		    if (maxval_r > max_value)
			max_value = maxval_r
		}
	    }
	}
end
