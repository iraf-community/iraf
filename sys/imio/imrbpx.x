# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<imio.h>

# IMRBPX -- Read a line segment from an image with boundary extension.  The
# line segment is broken up into three parts, i.e., left, center, and right.
# The endpoints of each segment, if out of bounds, are mapped back into the
# image using the current boundary extension technique.  The mapped line
# segment in physical coordinates is then extracted; if an image section is
# defined the section transformation has already been performed before we are
# called.  After all three segments have been extracted the entire line
# segment is flipped if the flip flag is set.

procedure imrbpx (im, obuf, totpix, v, vinc)

pointer	im			# image descriptor
char	obuf[ARB]		# typeless output buffer
int	totpix			# total number of pixels to extract
long	v[ARB]			# vector pointer to start of line segment
long	vinc[ARB]		# step on each axis

bool	oob
char	pixval[8]
int	npix, ndim, sz_pixel, btype, op, off, step, xstep, imtyp, i, j, k, ncp
long	xs[3], xe[3], x1, x2, p, v1[IM_MAXDIM], v2[IM_MAXDIM], linelen
errchk	imrdpx
include	<szpixtype.inc>

begin
	sz_pixel = pix_size[IM_PIXTYPE(im)]
	ndim = IM_NPHYSDIM(im)

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
	
	# Extract that portion of the line segment falling in each region
	# into the output buffer.  There are two classes of boundary extension
	# techniques, those that fill the out of bounds area with a constant,
	# and those that map the oob area into a vector lying within the bounds
	# of the image.

	btype = IM_VTYBNDRY(im)
	imtyp = IM_PIXTYPE(im)
	op = 1

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

	    # Perform the boundary extension.
            if ((imtyp == TY_INT || imtyp == TY_LONG) && SZ_INT != SZ_INT32)
                ncp = sz_pixel * 2
	    else
	        ncp = sz_pixel

	    call aclrc (pixval, 8)
	    if ((i == 2 && !oob) || btype == BT_REFLECT || btype == BT_WRAP)
		call imrdpx (im, obuf[op], npix, v1, step)
	    else {
		# Use constant or value of nearest boundary pixel.
		if (btype == BT_CONSTANT)
		    call impakr (IM_OOBPIX(im), pixval, 1, IM_PIXTYPE(im))
		else
		    call imrdpx (im, pixval, 1, v1, step)

	    	if ((imtyp == TY_INT || imtyp == TY_LONG) && SZ_INT != SZ_INT32)
	            call iupk32 (pixval, pixval, 2) 
		
		# Fill the output array.
		off = op - 1
		do j = 1, npix {
		    do k = 1, ncp
			obuf[off+k] = pixval[k]
		    off = off + ncp
		}
	    }

	    op = op + (npix * ncp)
	}

	# Flip the output array if the step size in X is negative.
	if (vinc[1] < 0)
	    call imaflp (obuf, totpix, sz_pixel)
end
