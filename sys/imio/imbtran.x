# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<imio.h>

# IMBTRAN -- Transform a point (x,y), possibly lying outside the boundary of
# the N-dimensional image, back into the image using the current boundary
# extension technique.

procedure imbtran (im, v1, v2, ndim)

pointer	im			# image descriptor
long	v1[IM_MAXDIM]		# input, out of bounds point
long	v2[IM_MAXDIM]		# transformed point (output)
int	ndim			# number of dimensions to transform

int	i
long	vin, vmax

begin
	switch (IM_VTYBNDRY(im)) {
	case BT_NEAREST:
	    do i = 1, ndim {
		vmax = IM_SVLEN(im,i)
		vin  = v1[i]

		if (vin < 1)
		    v2[i] = 1
		else if (vin > vmax)
		    v2[i] = vmax
		else
		    v2[i] = vin
	    }

	case BT_REFLECT:
	    do i = 1, ndim {
		vmax = IM_SVLEN(im,i)
		vin  = v1[i]

		if (vin < 1)
		    v2[i] = 1 + (1 - vin)
		else if (vin > vmax)
		    v2[i] = vmax - (vin - vmax)
		else
		    v2[i] = vin
	    }

	case BT_WRAP:
	    do i = 1, ndim {
		vmax = IM_SVLEN(im,i)
		vin  = v1[i]

		while (vin < 1)
		    vin = vin + vmax
		while (vin > vmax)
		    vin = vin - vmax
		v2[i] = vin
	    }

	default:
	    do i = 1, ndim
		v2[i] = v1[i]
	}
end
