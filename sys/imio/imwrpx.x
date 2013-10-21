# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<syserr.h>
include	<plset.h>
include	<imhdr.h>
include	<imio.h>

# IMWRPX -- Write NPIX pixels, starting with the pixel at the coordinates
# specified by the vector V, from the buffer BUF to the pixel storage file.

procedure imwrpx (im, buf, npix, v, xstep)

pointer	im			# image descriptor
char	buf[ARB]		# generic buffer containing data to be written
int	npix			# number of pixels to be written
long	v[ARB]			# physical coords of first pixel to be written
int	xstep			# step size between output pixels

bool	rlio
long	offset
pointer	pl, sp, ibuf
long	o_v[IM_MAXDIM]
int	sz_pixel, sz_dtype, nbytes, nchars, ip, step

int	sizeof()
long	imnote()
errchk	imerr, imwrite
include	<szpixtype.inc>

begin
	pl = IM_PL(im)
	sz_dtype = sizeof (IM_PIXTYPE(im))
	sz_pixel = pix_size[IM_PIXTYPE(im)]
	step = abs (xstep)
	if (v[1] < 1 || ((npix-1) * step) + v[1] > IM_SVLEN(im,1))
	    call imerr (IM_NAME(im), SYS_IMREFOOB)

	# Flip the pixel array end for end.
	if (xstep < 0)
	    #call imaflp (buf, npix, sz_dtype)
	    call imaflp (buf, npix, sz_pixel)

	# Byte swap if necessary.
	if (IM_SWAP(im) == YES) {
	    nbytes = npix * sz_dtype * SZB_CHAR
	    switch (sz_dtype * SZB_CHAR) {
	    case 2:
		call bswap2 (buf, 1, buf, 1, nbytes)
	    case 4:
		call bswap4 (buf, 1, buf, 1, nbytes)
	    case 8:
		call bswap8 (buf, 1, buf, 1, nbytes)
	    }
	}


	if (pl != NULL) {

	    # Need to unpack again on 64-bit systems.
	    if ((IM_PIXTYPE(im) == TY_INT || IM_PIXTYPE(im) == TY_LONG) &&
		SZ_INT != SZ_INT32) {
		    call iupk32 (buf, buf, npix)
	    }

	    # Write to a pixel list.
	    rlio = (and (IM_PLFLAGS(im), PL_FAST+PL_RLIO) == PL_FAST+PL_RLIO)
	    call amovl (v, o_v, IM_MAXDIM)
	    nchars = npix * sz_pixel

	    switch (IM_PIXTYPE(im)) {
	    case TY_SHORT:
		if (rlio)
		    call pl_plrs (pl, v, buf, 0, npix, PIX_SRC)
		else if (step == 1)
		    call pl_plps (pl, v, buf, 0, npix, PIX_SRC)
		else {
		    do ip = 1, nchars, sz_pixel {
			call pl_plpi (pl, o_v, buf[ip], 0, 1, PIX_SRC)
			o_v[1] = o_v[1] + step
		    }
		}
	    case TY_INT, TY_LONG:
		if (rlio)
		    call pl_plri (pl, v, buf, 0, npix, PIX_SRC)
		else if (step == 1)
		    call pl_plpi (pl, v, buf, 0, npix, PIX_SRC)
		else {
		    do ip = 1, nchars, sz_pixel {
			call pl_plpi (pl, o_v, buf[ip], 0, 1, PIX_SRC)
			o_v[1] = o_v[1] + step
		    }
		}
	    default:
		call smark (sp)
		call salloc (ibuf, npix, TY_INT)

		call acht (buf, Memi[ibuf], npix, IM_PIXTYPE(im), TY_INT)
		if (rlio)
		    call pl_plri (pl, v, Memi[ibuf], 0, npix, PIX_SRC)
		else if (step == 1)
		    call pl_plpi (pl, v, Memi[ibuf], 0, npix, PIX_SRC)
		else {
		    do ip = 1, npix {
			call pl_plpi (pl, o_v, Memi[ibuf+ip-1], 0, 1, PIX_SRC)
			o_v[1] = o_v[1] + step
		    }
		}
		call sfree (sp)
	    }

	} else {
	    # Write to a file.  Compute size of transfer.  If transferring
	    # an entire line, increase size of transfer to the physical line
	    # length, to avoid having to enblock the data.  NOTE: buffer must
	    # be large enough to guarantee no memory violation.

	    offset = imnote (im, v)

	    # If not subsampling (stepsize 1), write buffer to file in a
	    # single transfer.  Otherwise, the pixels are not contiguous,
	    # and must be written individually.

	    if (step == 1) {
		if (v[1] == 1 && npix == IM_SVLEN(im,1))
		    nchars = IM_PHYSLEN(im,1) * sz_pixel
		else
		    nchars = npix * sz_pixel
		call imwrite (im, buf, nchars, offset)

	    } else {
		nchars = npix * sz_pixel
		for (ip=1;  ip <= nchars;  ip=ip+sz_pixel) {
		    call imwrite (im, buf[ip], sz_pixel, offset)
		    offset = offset + (sz_pixel * step)
		}
	    }
	}
end
