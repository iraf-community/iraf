# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IM_VMINMAX -- Compute the minimum and maximum pixel values of an image.
# Works for images of any dimensionality, size, or datatype, although
# the min and max values can currently only be stored in the image header
# as real values.

procedure im_vminmax (im, min_value, max_value, vmin, vmax)

pointer	im				# image descriptor
real	min_value			# minimum pixel value in image (out)
real	max_value			# maximum pixel value in image (out)
long	vmin[ARB], vmax[ARB]		# v vectors

pointer	buf
bool	first_line
int	colmin, colmax
long	v[IM_MAXDIM], ovmin[IM_MAXDIM], ovmax[IM_MAXDIM]
short	minval_s, maxval_s
long	minval_l, maxval_l
real	minval_r, maxval_r
int	imgnls(), imgnll(), imgnlr()

begin
	call amovkl (long(1), v, IM_MAXDIM)		# start vector
	call amovkl (long(1), ovmin, IM_MAXDIM)
	call amovkl (long(1), ovmax, IM_MAXDIM)
	call amovkl (long(1), vmin, IM_MAXDIM)
	call amovkl (long(1), vmax, IM_MAXDIM)
	first_line = true
	min_value = INDEF
	max_value = INDEF

	switch (IM_PIXTYPE(im)) {
	case TY_SHORT:
	    while (imgnls (im, buf, v) != EOF) {
		call valims (Mems[buf], IM_LEN(im,1), minval_s, maxval_s,
		    colmin, colmax)
		if (first_line) {
		    min_value = minval_s
		    max_value = maxval_s
		    vmin[1] = colmin
		    vmax[1] = colmax
		    first_line = false
		} else {
		    if (minval_s < min_value) {
			min_value = minval_s
			vmin[1] = colmin
			call amovl (ovmin[2], vmin[2], IM_NDIM(im) - 1)
		    }
		    if (maxval_s > max_value) {
			max_value = maxval_s
			vmax[1] = colmax
			call amovl (ovmax[2], vmax[2], IM_NDIM(im) - 1)
		    }
		}
		call amovl (v[2], ovmin[2], IM_NDIM(im) - 1)
		call amovl (v[2], ovmax[2], IM_NDIM(im) - 1)
	    }
	case TY_USHORT, TY_INT, TY_LONG:
	    while (imgnll (im, buf, v) != EOF) {
		call valiml (Meml[buf], IM_LEN(im,1), minval_l, maxval_l,
		    colmin, colmax)
		if (first_line) {
		    min_value = minval_l
		    max_value = maxval_l
		    vmin[1] = colmin
		    vmax[1] = colmax
		    first_line = false
		} else {
		    if (minval_l < min_value) {
			min_value = minval_l
			vmin[1] = colmin
			call amovl (ovmin[2], vmin[2], IM_NDIM(im) - 1)
		    }
		    if (maxval_l > max_value) {
			max_value = maxval_l
			vmax[1] = colmax
			call amovl (ovmax[2], vmax[2], IM_NDIM(im) - 1)
		    }
		}
		call amovl (v[2], ovmin[2], IM_NDIM(im) - 1)
		call amovl (v[2], ovmax[2], IM_NDIM(im) - 1)
	    }
	default:
	    while (imgnlr (im, buf, v) != EOF) {
		call valimr (Memr[buf], IM_LEN(im,1), minval_r, maxval_r,
		    colmin, colmax)
		if (first_line) {
		    min_value = minval_r
		    max_value = maxval_r
		    vmin[1] = colmin
		    vmax[1] = colmax
		    first_line = false
		} else {
		    if (minval_r < min_value) {
			min_value = minval_r
			vmin[1] = colmin
			call amovl (ovmin[2], vmin[2], IM_NDIM(im) - 1)
		    }
		    if (maxval_r > max_value) {
			max_value = maxval_r
			vmax[1] = colmax
			call amovl (ovmax[2], vmax[2], IM_NDIM(im) - 1)
		    }
		}
		call amovl (v[2], ovmin[2], IM_NDIM(im) - 1)
		call amovl (v[2], ovmax[2], IM_NDIM(im) - 1)
	    }
	}
end

# ALIM -- Compute the limits (minimum and maximum values) of a vector.

procedure valims (a, npix, minval_s, maxval_s, colmin, colmax)

short	a[ARB], minval_s, maxval_s, value
int	colmin, colmax, npix, i

begin
	minval_s = a[1]
	maxval_s = a[1]
	colmin = 1
	colmax = 1

	do i = 1, npix {
	    value = a[i]
		 if (value < minval_s) {
		    minval_s = value
		    colmin = i
		} else if (value > maxval_s) {
		    maxval_s = value
		    colmax = i
		}
	}
end

# ALIM -- Compute the limits (minimum and maximum values) of a vector.

procedure valiml (a, npix, minval_l, maxval_l, colmin, colmax)

long	a[ARB], minval_l, maxval_l, value
int	colmin, colmax, npix, i

begin
	minval_l = a[1]
	maxval_l = a[1]
	colmin = 1
	colmax = 1

	do i = 1, npix {
	    value = a[i]
		if (value < minval_l) {
		    minval_l = value
		    colmin = i
		} else if (value > maxval_l) {
		    maxval_l = value
		    colmax = i
		}
	}
end

# ALIM -- Compute the limits (minimum and maximum values) of a vector.

procedure valimr (a, npix, minval_r, maxval_r, colmin, colmax)

real	a[ARB], minval_r, maxval_r, value
int	colmin, colmax, npix, i

begin
	minval_r = a[1]
	maxval_r = a[1]
	colmin = 1
	colmax = 1

	do i = 1, npix {
	    value = a[i]
		if (value < minval_r) {
		    minval_r = value
		    colmin = i
		} else if (value > maxval_r) {
		    maxval_r = value
		    colmax = i
		}
	}
end
