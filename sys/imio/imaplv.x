# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMAPLV -- Transform the logical vector LV (which references an image section)
# into a physical vector (which references the physical image).

procedure imaplv (im, lv, pv, ndim)

pointer	im
long	lv[ndim], pv[IM_MAXDIM]
int	ndim
int	loff		# logical offset (subscript)
int	nldims		# number of logical dimensions
int	i, j		# i = logical dim index, j = physical dim index

begin
	i = 1
	nldims = min (IM_NDIM(im), ndim)

	do j = 1, IM_NPHYSDIM(im) {
	    if (i <= nldims && IM_VMAP(im,i) == j) {
		loff = lv[i]
		i = i + 1
	    } else
		loff = 1
	    pv[j] = IM_VOFF(im,j) + IM_VSTEP(im,j) * loff
	}
end
