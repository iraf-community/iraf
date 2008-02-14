# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include	<plio.h>

# PL_PIXROP -- Rasterop between source and destination pixel arrays.

procedure pl_pixropl (px_src,xs,src_maxval, px_dst,ds,dst_maxval, npix, rop)

long	px_src[ARB]		#I source pixel array
int	xs			#I starting pixel index in src
int	src_maxval		#I max pixel value in src mask
long	px_dst[ARB]		#O destination pixel array
int	ds			#I starting pixel index in dst
int	dst_maxval		#I max pixel value in dst mask
int	npix			#I number of pixels to convert
int	rop			#I rasterop

pointer	sp, src
int	opcode, i
long	data, ceil, src_value
int	and(), or(), xor(), not()
define	out_ 91

begin
	opcode = R_OPCODE(rop)
	data   = R_DATA(rop)
	ceil   = 0

	# Pixel value to be used if input mask is boolean.
	if (src_maxval == 1) {
	    src_value = data
	    if (src_value <= 0)
		src_value = dst_maxval
	}

	# Handle the easy cases first.
	switch (opcode) {
	case PIX_CLR:
	    call aclrl (px_dst[ds], npix)
	    return
	case PIX_SET:
	    call amovkl (data, px_dst[ds], npix)
	    goto out_
	case PIX_SRC:
	    if (src_maxval != 1)
		call amovl (px_src[xs], px_dst[ds], npix)
	    else {
		do i = 1, npix
		    if (px_src[xs+i-1] > 0)
			px_dst[ds+i-1] = src_value
		    else
			px_dst[ds+i-1] = 0
	    }

	    goto out_
	case PIX_DST:
	    return	# no-op
	}

	# Integer or boolean source mask?
	if (src_maxval != 1) {
	    # Integer source mask; operate directly on source mask.

	    switch (opcode) {
	    case PIX_NOTSRC:
		do i = 1, npix
		    px_dst[ds+i-1] = not (px_src[xs+i-1])
	    case PIX_NOTDST:
		do i = 1, npix
		    px_dst[ds+i-1] = not (px_dst[xs+i-1])

	    case PIX_SRC_AND_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = and (px_src[xs+i-1], px_dst[ds+i-1])
	    case PIX_SRC_OR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] =  or (px_src[xs+i-1], px_dst[ds+i-1])
	    case PIX_SRC_XOR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = xor (px_src[xs+i-1], px_dst[ds+i-1])

	    case PIX_SRC_AND_NOTDST:
		do i = 1, npix
		    px_dst[ds+i-1] = and (px_src[xs+i-1], not(px_dst[ds+i-1]))
	    case PIX_SRC_OR_NOTDST:
		do i = 1, npix
		    px_dst[ds+i-1] =  or (px_src[xs+i-1], not(px_dst[ds+i-1]))
	    case PIX_NOTSRC_AND_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = and (not(px_src[xs+i-1]), px_dst[ds+i-1])
	    case PIX_NOTSRC_OR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] =  or (not(px_src[xs+i-1]), px_dst[ds+i-1])

	    case PIX_NOT_SRC_AND_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = not (and (px_src[xs+i-1], px_dst[ds+i-1]))
	    case PIX_NOT_SRC_OR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = not ( or (px_src[xs+i-1], px_dst[ds+i-1]))
	    case PIX_NOT_SRC_XOR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = not (xor (px_src[xs+i-1], px_dst[ds+i-1]))
	    }

	} else {
	    # Boolean source mask; use integer DATA value from ROP if source
	    # mask pixel is set.

	    call smark (sp)
	    call salloc (src, npix, TY_LONG)

	    do i = 1, npix
		if (px_src[xs+i-1] > 0)
		    Meml[src+i-1] = src_value
		else
		    Meml[src+i-1] = 0

	    switch (opcode) {
	    case PIX_NOTSRC:
		do i = 1, npix
		    px_dst[ds+i-1] = not (Meml[src+i-1])
	    case PIX_NOTDST:
		do i = 1, npix
		    px_dst[ds+i-1] = not (px_dst[xs+i-1])

	    case PIX_SRC_AND_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = and (Meml[src+i-1], px_dst[ds+i-1])
	    case PIX_SRC_OR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] =  or (Meml[src+i-1], px_dst[ds+i-1])
	    case PIX_SRC_XOR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = xor (Meml[src+i-1], px_dst[ds+i-1])

	    case PIX_SRC_AND_NOTDST:
		do i = 1, npix
		    px_dst[ds+i-1] = and (Meml[src+i-1], not(px_dst[ds+i-1]))
	    case PIX_SRC_OR_NOTDST:
		do i = 1, npix
		    px_dst[ds+i-1] =  or (Meml[src+i-1], not(px_dst[ds+i-1]))
	    case PIX_NOTSRC_AND_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = and (not(Meml[src+i-1]), px_dst[ds+i-1])
	    case PIX_NOTSRC_OR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] =  or (not(Meml[src+i-1]), px_dst[ds+i-1])

	    case PIX_NOT_SRC_AND_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = not (and (Meml[src+i-1], px_dst[ds+i-1]))
	    case PIX_NOT_SRC_OR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = not ( or (Meml[src+i-1], px_dst[ds+i-1]))
	    case PIX_NOT_SRC_XOR_DST:
		do i = 1, npix
		    px_dst[ds+i-1] = not (xor (Meml[src+i-1], px_dst[ds+i-1]))
	    }

	    call sfree (sp)
	}
out_
	# If writing to an integer mask, mask the data to the indicated max
	# value (necessary to avoid very large values if any NOT operations
	# occurred).  If writing to a boolean mask, map positive integer mask
	# values to 1.

	if (dst_maxval == 1) {
	    data = 1
	    call argtl (px_dst[ds], npix, ceil, data)
	} else if (dst_maxval > 1) {
	    data = dst_maxval
	    call aandkl (px_dst[ds], data, px_dst[ds], npix)
	}
end
