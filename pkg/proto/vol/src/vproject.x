include <math.h>
include <imhdr.h>
include "pvol.h"

define	incr_	91


# VPROJECT -- Volume rotation, incremental projection algorithm.
# Routine attempts to hold as much of the input image in memory as possible.
# Constructs output image one complete line at a time, determining the
# contributing voxels for each ray by an incremental rasterizer-like algorithm.

procedure vproject (im1, im2, vp, use_both)
pointer im1		# Input volume image
pointer	im2		# Output projection image
pointer	vp		# Volume projection descriptor
bool	use_both	# Use both opacity and intensity from 4D image

int	plines, pcols, oline, oband, rot, nvox, oldsize
int	pnx,pny, len_x, px1,px2, ix,iy,iz,ih, ndims
real	phalf
double	rx1,ry1,rx2,ry2, orx1,ory1,orx2,ory2, xc,yc, pdx,pdy, pstep_dx,pstep_dy
double	astep, theta, theta0, uc_theta
pointer	sp, vox_x,vox_y, optr, ioptr, buf_in
bool	first_pass
long	vs[3], ve[3], ivs[4], ive[4]

pointer	imggss(), imggsi(), imggsl(), imggsr(), imggsd(), imggsx()
pointer	impgsr()

begin
	ix = IM_LEN(im1,1)
	iy = IM_LEN(im1,2)
	iz = IM_LEN(im1,3)
	if (use_both) {
	    ih = 2
	    ndims = 4
	} else {
	    ih = 1
	    ndims = 3
	}

	# Set up coordinates for rotation by aligning the center of the working
	# projection plane ("p-plane") with the center of the volume image.

	pnx = iz			# volume image bands become p-plane X
	pny = iy			# volume image lines become p-plane Y
	plines = int (DIS(double(pnx),double(pny)))
	if (mod (plines, 2) == 0)
	    plines = plines + 1
	pcols = IM_LEN(im2,1)
	phalf = (plines - 1) / 2	# distance from center to bottom pline
	IM_LEN(im2,2) = plines
	IM_LEN(im2,3) = NFRAMES(vp)
	xc = 0.5 * (pnx + 1)
	yc = 0.5 * (pny + 1)

	# Allocate index arrays for contributing voxels.
	call smark (sp)
	call salloc (vox_x, plines, TY_INT)
	call salloc (vox_y, plines, TY_INT)

	astep = DDEGTORAD (DEGREES(vp))	# angular increment in radians
	
	# Determine how much memory we can use, and adjust working set.
	call pv_gmem (im1, im2, use_both, VERBOSE(vp), MAX_WS(vp), len_x,
	    oldsize)

	# Read as much of the input image as possible into memory, in column
	# blocks so we can project through all lines and bands in memory; we
	# only want to read each voxel of the input image once.

	ivs[2] = 1
	ive[2] = iy
	ivs[3] = 1
	ive[3] = iz
	ivs[4] = 1
	ive[4] = 2
	first_pass = true
	do px1 = 1, ix, len_x {
	    px2 = px1 + len_x - 1
	    if (px2 > ix)
		px2 = ix
	    if (VERBOSE(vp) == YES) {
		call eprintf ("px1=%d, px2=%d, len_x=%d\n")
		call pargi (px1); call pargi (px2); call pargi (px2-px1+1)
	    }
	    ivs[1] = px1
	    ive[1] = px2
	    switch (IM_PIXTYPE (im1)) {
	    case TY_SHORT:
		buf_in = imggss (im1, ivs, ive, ndims)
	    case TY_INT:
		buf_in = imggsi (im1, ivs, ive, ndims)
	    case TY_LONG:
		buf_in = imggsl (im1, ivs, ive, ndims)
	    case TY_REAL:
		buf_in = imggsr (im1, ivs, ive, ndims)
	    case TY_DOUBLE:
		buf_in = imggsd (im1, ivs, ive, ndims)
	    case TY_COMPLEX:
		buf_in = imggsx (im1, ivs, ive, ndims)
	    default:
		call error (3, "unknown pixel type")
	    }

	    # Invariant part of output image section:
	    vs[1] = 1
	    ve[1] = pcols

	    # Produce one output image band per rotation step around vol image.
	    theta0 = DDEGTORAD (INIT_THETA(vp))
	    oband = 1
	    do rot = 1, NFRAMES(vp) {
		theta = theta0 + (rot - 1) * astep
		uc_theta = theta	 # unit-circle for quadrant comparisons.
		while (uc_theta >= DTWOPI)
		    uc_theta = uc_theta - DTWOPI

		# Determine line endpoints intersecting the image boundary for
		# central projection line.

		orx1 = xc - phalf * cos (uc_theta)
		orx2 = xc + phalf * cos (uc_theta)
		ory1 = yc - phalf * sin (uc_theta)
		ory2 = yc + phalf * sin (uc_theta)

		# Offset central projection line to hit the bottom image line of
		# the projection plane (won't necessarily pass through image).

		pdx = phalf * sin (uc_theta)
		pdy = phalf * cos (uc_theta)
		pstep_dx = sin (uc_theta)
		pstep_dy = cos (uc_theta)
		orx1 = orx1 + pdx
		ory1 = ory1 - pdy
		orx2 = orx2 + pdx
		ory2 = ory2 - pdy
		rx1 = orx1
		ry1 = ory1
		rx2 = orx2
		ry2 = ory2

		do oline = 1, plines {

		    # Get voxel indices in p-plane contributing to central ray.
		    call vgetincrem (rx1,ry1, rx2,ry2, pnx,pny,plines, nvox,
			Memi[vox_x], Memi[vox_y])

		    # Initialize output line.
		    vs[2] = oline
		    ve[2] = oline
		    vs[3] = oband
		    ve[3] = oband

		    # If first pass through output image, initialize output
		    # pixels.  Otherwise, we must read existing part of output
		    # image into output buffer.

		    if (first_pass) {
			optr = impgsr (im2, vs, ve, 3)

			# If opacity model, initialize output to incident int.
			if (PTYPE(vp) == P_ATTENUATE)
			    call amovkr (IZERO(vp), Memr[optr], pcols)
			else
			    call aclrr (Memr[optr], pcols)
		    } else {
			ioptr = imggsr (im2, vs, ve, 3)
			optr = impgsr (im2, vs, ve, 3)
			call amovr (Memr[ioptr], Memr[optr], pcols)
		    }

		    # Project each contributing voxel into output image line.
		    if (nvox > 0)
			switch (IM_PIXTYPE (im1)) {
			case TY_SHORT:
			    call vtransmits (Mems[buf_in], (px2-px1+1),iy,iz,ih,
				px1,px2, Memi[vox_y], Memi[vox_x], nvox,
				Memr[optr], vp)
			case TY_INT:
			    call vtransmiti (Memi[buf_in], (px2-px1+1),iy,iz,ih,
				px1,px2, Memi[vox_y], Memi[vox_x], nvox,
				Memr[optr], vp)
			case TY_LONG:
			    call vtransmitl (Meml[buf_in], (px2-px1+1),iy,iz,ih,
				px1,px2, Memi[vox_y], Memi[vox_x], nvox,
				Memr[optr], vp)
			case TY_REAL:
			    call vtransmitr (Memr[buf_in], (px2-px1+1),iy,iz,ih,
				px1,px2, Memi[vox_y], Memi[vox_x], nvox,
				Memr[optr], vp)
			case TY_DOUBLE:
			    call vtransmitd (Memd[buf_in], (px2-px1+1),iy,iz,ih,
				px1,px2, Memi[vox_y], Memi[vox_x], nvox,
				Memr[optr], vp)
			case TY_COMPLEX:
			    call vtransmitx (Memx[buf_in], (px2-px1+1),iy,iz,ih,
				px1,px2, Memi[vox_y], Memi[vox_x], nvox,
				Memr[optr], vp)
			}

		    # Offset endpoints for next projection line.
		    rx1 = orx1 - oline * pstep_dx
		    ry1 = ory1 + oline * pstep_dy
		    rx2 = orx2 - oline * pstep_dx
		    ry2 = ory2 + oline * pstep_dy
		}

		# Set up for next rotation.
		oband = oband + 1
		if (VERBOSE(vp) == YES) {
		    call eprintf ("...end of rotation %d, theta %7.2f\n")
		    call pargi (rot); call pargd (DRADTODEG(theta))
		}
	    }

	    first_pass = false
	    call imflush (im2)
	}

	call sfree (sp)
	call fixmem (oldsize)
end
