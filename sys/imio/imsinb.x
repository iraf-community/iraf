# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMSINB -- Determine whether or not a section is in bounds.  Out of bounds
# references are permissible if boundary extension is enabled.  The actual
# dimensionality of the image need not agree with that of the section.

int procedure imsinb (im, vs, ve, ndim)

pointer	im			# image descriptor
long	vs[ARB], ve[ARB]	# logical section
int	ndim			# dimensionality of section

int	i
int	lo, hi, bwidth
define	oob_ 91

begin
	# First check if the section is entirely within bounds.  If this is the
	# case no boundary extension will be required, making optimization
	# possible.

	do i = 1, ndim {
	    hi = IM_LEN(im,i)
	    if (vs[i] < 1 || vs[i] > hi)
		goto oob_
	    if (ve[i] < 1 || ve[i] > hi)
		goto oob_
	}

	return (YES)				# section is within bounds

	# There is at least one out of bounds reference.  Check that all such
	# references are within NBNDRYPIX of the nearest boundary.  NDIM may
	# be greater than IM_NDIM, since IMIO sets the lengths of the excess
	# dimensions to 1.  In effect every image has up to MAXDIM dimensions.
 oob_
	bwidth = IM_VNBNDRYPIX(im)
	lo = 1 - bwidth
	hi = 1 + bwidth

	do i = 1, ndim {
	    hi = IM_LEN(im,i) + bwidth
	    if (vs[i] < lo || vs[i] > hi)
		return (ERR)			# section is illegal
	    if (ve[i] < lo || ve[i] > hi)
		return (ERR)
	}

	return (NO)				# section is oob but legal
end
