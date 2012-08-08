# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>

# IMFLSH -- Flush the output buffer to the pixel storage file (not
# dependent on the datatype of the pixels).  The mapping of the subraster
# in the output buffer to the imagefile is described by the section
# descriptor vectors in the buffer descriptor.  Images up to IM_MAXDIM
# dimensions are permitted, and each dimension may be accessed in either
# the forward or reverse direction.

procedure imflsh (im, bp, vs, ve, ndim)

pointer	im			# image descriptor
pointer	bp			# pointer to buffer containing the data
long	vs[ARB], ve[ARB]	# logical coordinates of section to be written
int	ndim			# dimensionality of the section

pointer	line
long	pvs[IM_MAXDIM], pve[IM_MAXDIM]
long	v[IM_MAXDIM], vinc[IM_MAXDIM]
int	sz_pixel, sz_dtype, inbounds, npix, xstep
int	imsinb(), imloop(), sizeof()
errchk	imwrpx, imwbpx
include <szpixtype.inc>

begin
        sz_dtype = sizeof (IM_PIXTYPE(im))
        sz_pixel = pix_size[IM_PIXTYPE(im)]

	# Check if the section extends out of bounds.
	inbounds = imsinb (im, vs, ve, ndim)
	if (inbounds == ERR)
	    call imerr (IM_NAME(im), SYS_IMREFOOB)

	# Map the logical section into a physical section.  Prepare the
	# section descriptor do-loop index and increment vectors V, VINC.

	call imaplv (im, vs, pvs, ndim)
	call imaplv (im, ve, pve, ndim)
	call imsslv (im, pvs, pve, v, vinc, npix)

	line = bp

	# Write the section to the output image, line segment by line segment,
	# advancing through the dimensions in storage order (leftmost subscript
	# varies fastest).

	repeat {
	    # Call IMWRPX directly if section is inbounds.
	    xstep = vinc[1]
	    if (inbounds == YES)
		call imwrpx (im, Memc[line], npix, v, xstep) 
	    else
		call imwbpx (im, Memc[line], npix, v, xstep) 
	    line = line + npix * sz_pixel
	} until (imloop (v, pvs, pve, vinc, IM_NPHYSDIM(im)) == LOOP_DONE)
end
