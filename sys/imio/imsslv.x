# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>

# IMSSLV -- Given two vectors (VS, VE) defining the starting and ending
# physical coordinates of the two pixels defining an image section,
# initialize the "loop index" vector V, and the "loop increment" vector,
# VINC.  Compute NPIX, the number of pixels in a line segment.

procedure imsslv (im, vs, ve, v, vinc, npix)

pointer	im
long	vs[IM_MAXDIM], ve[IM_MAXDIM]
long	v[IM_MAXDIM], vinc[IM_MAXDIM], npix, step
int	i

begin
	# Determine the direction in which each dimension is to be
	# traversed.

	do i = 1, IM_NPHYSDIM(im) {
	    step = abs (IM_VSTEP(im,i))
	    if (vs[i] <= ve[i])
		vinc[i] = step
	    else
		vinc[i] = -step
	}

	# Initialize the extraction vector (passed to IMRDS? to read a
	# contiguous array of pixels).  Compute the length of a line,
	# allowing for decimation by the step size.

	do i = 1, IM_NPHYSDIM(im)
	    v[i] = vs[i]

	if (vs[1] > ve[1])
	    v[1] = ve[1]

	npix = (ve[1] - vs[1]) / vinc[1] + 1
end
