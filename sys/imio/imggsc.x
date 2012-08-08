# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plset.h>
include	<imhdr.h>
include	<imio.h>

# IMGGSC -- Get a general section, any datatype (called by one of the typed
# procedures, which subsequently convert the datatype of the pixels returned
# by this routine).  The mapping of the subraster in the input buffer to the
# imagefile is described by the section descriptor vectors VS and VE.  Images
# of up to IM_MAXDIM dimensions are permitted, and each dimension may be
# accessed in either the forward or reverse direction.

pointer procedure imggsc (im, vs, ve, ndim, dtype, totpix)

pointer	im			# image descriptor
long	vs[ARB], ve[ARB]	# logical coords of corners of section
int	ndim			# dimensionality of section
int	dtype			# datatype of pixels desired
long	totpix			# total pixels in section (output)

bool	rlio
pointer	sp, px, bp, line, rl_high
long	v[IM_MAXDIM], vinc[IM_MAXDIM]
long	pvs[IM_MAXDIM], pve[IM_MAXDIM]
int	sz_pixel, inbounds, npix, xstep, n

pointer	imgibf()
int	imsinb(), imloop(), pl_p2ri(), sizeof()
errchk	imgibf, imrdpx, imrbpx
include <szpixtype.inc>

begin
	#sz_pixel = sizeof(IM_PIXTYPE(im))
	#sz_pixel = max ( sizeof(dtype), sizeof(IM_PIXTYPE(im)) )
	#sz_pixel = pix_size[IM_PIXTYPE(im)]
	sz_pixel = sizeof(IM_PIXTYPE(im))
	rlio = (and (IM_PLFLAGS(im), PL_RLIO+PL_FAST) == PL_RLIO)

	# Check that the section does not extend out of bounds.
	inbounds = imsinb (im, vs, ve, ndim)
	if (inbounds == ERR)
	    call imerr (IM_NAME(im), SYS_IMREFOOB)

	# Get an (input) buffer to put the pixels into.  Map the logical
	# section into a physical section.  Prepare the section descriptor
	# do-loop index and increment vectors V, VINC.

	bp = imgibf (im, vs, ve, ndim, dtype)
	call imaplv (im, vs, pvs, ndim)
	call imaplv (im, ve, pve, ndim)
	call imsslv (im, pvs, pve, v, vinc, npix)

	# A temporary pixel buffer is required for RLIO conversions.
	if (rlio) {
	    call smark (sp)
	    call salloc (px, npix, TY_INT)
	}

	line = bp
	totpix = 0
	rl_high = bp - 1

	# Read the section into the input buffer, line segment by line segment,
	# advancing through the dimensions in storage order (leftmost subscript
	# varies fastest).

	repeat {
	    xstep = vinc[1]

	    # Convert the pixel array to a range list? (image masks).  This is
	    # done more efficiently at a lower level if no complex geometric
	    # transformations are required (due to sections or OOB references).

	    if (rlio) {
		if (inbounds == YES)
		    call imrdpx (im, Memi[px], npix, v, xstep) 
		else
		    call imrbpx (im, Memi[px], npix, v, xstep) 

		if (rl_high >= line)
		    call imerr (IM_NAME(im), SYS_IMRLOVFL)
		else {
		    n = pl_p2ri (Memi[px], 1, Memc[line], npix)
		    rl_high = line + (n * RL_LENELEM * sz_pixel) - 1
		}

	    } else {
		if (inbounds == YES)
		    call imrdpx (im, Memc[line], npix, v, xstep) 
		else
		    call imrbpx (im, Memc[line], npix, v, xstep) 
	    }

	    line = line + (npix * sz_pixel)
	    totpix = totpix + npix

	} until (imloop (v, pvs, pve, vinc, IM_NPHYSDIM(im)) == LOOP_DONE)

	if (rlio)
	    call sfree (sp)

	return ((bp - 1) / sizeof(dtype) + 1)
end
