# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<imio.h>

# IMWBPX -- Write a line segment from an image with boundary extension.  The
# line segment is broken up into three parts, i.e., left, center, and right.
# The left and right (out of bounds) regions are discarded, and the center
# region, if any, is written to the image.  Inbounds data is conserved if a
# subraster which extends out of bounds is read and then rewritten, i.e.,
# a read followed immediately by a rewrite of the same data does not modify 
# the image.

procedure imwbpx (im, ibuf, totpix, v, vinc)

pointer	im			# image descriptor
char	ibuf[ARB]		# typeless buffer containing the data
int	totpix			# total number of pixels to write
long	v[ARB]			# vector pointer to start of line segment
long	vinc[ARB]		# step on each axis

bool	oob
int	npix, ndim, sz_pixel, btype, ip, xstep, step, i
long	xs[3], xe[3], x1, x2, p, v1[IM_MAXDIM], v2[IM_MAXDIM], linelen
errchk	imwrpx
include	<szpixtype.inc>

begin
	sz_pixel = pix_size[IM_PIXTYPE(im)]
	ndim = IM_NPHYSDIM(im)

	# Flip the input array if the step size in X is negative.
	if (vinc[1] < 0)
	    call imaflp (ibuf, totpix, sz_pixel)

	# Cache the left and right endpoints of the line segment and the
	# image line length.

	xstep = abs (IM_VSTEP(im,1))
	linelen = IM_SVLEN(im,1)
	x1 = v[1]
	x2 = x1 + (totpix * xstep) - 1

	# Compute the endpoints of the line segment in the three x-regions of 
	# the image.

	xs[1] = x1				# left oob region
	xe[1] = min (0, x2)
	xs[2] = max (x1, 1)			# central inbounds region
	xe[2] = min (x2, linelen)
	xs[3] = max (x1, linelen + 1)		# right oob region
	xe[3] = x2

	# Perform bounds mapping on the entire vector.  The mapping for all
	# dimensions higher than the first is invariant in what follows.

	call imbtran (im, v, v1, ndim)

	# Copy V1 to V2 and determine if the whole thing is out of bounds.
	oob = false
	do i = 2, ndim {
	    p = v1[i]
	    v2[i] = p
	    if (p < 1 || p > IM_SVLEN(im,i))
		oob = true
	}
	
	btype = IM_VTYBNDRY(im)
	ip = 1

	do i = 1, 3 {
	    # Skip to next region if there are no pixels in this region.
	    npix = (xe[i] - xs[i]) / xstep + 1
	    if (npix <= 0)
		next

	    # Map the endpoints of the segment.
	    call imbtran (im, xs[i], v1[1], 1)
	    call imbtran (im, xe[i], v2[1], 1)

	    # Compute the starting vector V1, step in X, and the number of
	    # pixels in the region allowing for subsampling.

	    if (v1[1] > v2[1]) {
		step = -xstep
		v1[1] = v2[1]
	    } else
		step = xstep

	    # Write the pixels if inbounds.
	    if (i == 2 && !oob)
		call imwrpx (im, ibuf[ip], npix, v1, step)

	    ip = ip + (npix * sz_pixel)
	}
end
