# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# XT_IMLENEQ -- Determine if the lengths of the common image dimensions
# are equal.

bool procedure xt_imleneq (im1, im2)

pointer	im1			# First IMIO pointer
pointer	im2			# Second IMIO pointer

int	i, ndim

begin
	ndim = min (IM_NDIM (im1), IM_NDIM (im2))
	do i = 1, ndim {
	    if (IM_LEN (im1, i) != IM_LEN (im2, i))
		return (FALSE)
	}
	return (TRUE)
end
