# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IM_PROJECTION -- Given an image section of arbitrary dimension, compute
# the projection along a single axis by taking the average over the other
# axes.  We do not know about bad pixels.

procedure im_projection (im, pv, npix, axis)

pointer	im
real	pv[npix]		# receives the projection vector
int	npix			# length of projection vector
int	axis			# the axis to be projected to (x=1)

int	i, lastv
long	v[IM_MAXDIM], nsum, totpix
pointer	pix
real	asumr()
pointer	imgnlr()
errchk	imgnlr

begin
	if (im == NULL)
	    call error (1, "Image projection operator called with null im")
	if (axis < 1 || axis > IM_NDIM(im))
	    call error (2, "Attempt to take projection over nonexistent axis")

	call aclrr (pv, npix)
	call amovkl (long(1), v, IM_MAXDIM)

	switch (axis) {
	case 1:
	    # Since the image is read line by line, it is easy to compute the
	    # projection along the x-axis (axis 1).  We merely sum all of the
	    # image lines.

	    while (imgnlr (im, pix, v) != EOF)
		call aaddr (Memr[pix], pv, pv, npix)

	default:
	    # Projecting along any other axis when reading the image line
	    # by line is a bit difficult to understand.  Basically, the
	    # element 'axis' of the V vector (position of the line in the
	    # image) gives us the index into the appropriate element of
	    # pv.  When computing the projection over multiple dimensions,
	    # the same output element will be referenced repeatedly.  All
	    # of the elmenents of the input line are summed and added into
	    # this output element.

	    for (lastv=v[axis];  imgnlr (im, pix, v) != EOF;  lastv=v[axis]) {
		i = lastv
		if (i <= npix)
		    pv[i] = pv[i] + asumr (Memr[pix], IM_LEN(im,1))
	    }
	}

	# Now compute the number of pixels contributing to each element
	# of the output vector.  This is the number of pixels in the image
	# divided by the length of the projection.

	totpix = 1
	do i = 1, IM_NDIM(im)
	    if (i == axis)
		totpix = totpix * min (npix, IM_LEN(im,i))
	    else
		totpix = totpix * IM_LEN(im,i)
	nsum = totpix / min (npix, IM_LEN(im,axis))

	# Compute the average by dividing by the number if pixels summed at
	# each point.
	call adivkr (pv, real(nsum), pv, npix)
end
