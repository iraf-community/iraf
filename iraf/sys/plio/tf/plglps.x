# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_GLP -- Get a line segment as a pixel array, applying the given ROP to
# combine the pixels with those of the output array.

procedure pl_glps (pl, v, px_dst, px_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
short	px_dst[ARB]		#O output pixel array
int	px_depth		#I pixel depth, bits
int	npix			#I number of pixels desired
int	rop			#I rasterop

int	np
pointer	sp, px_out, ll_src
pointer	pl_access()
int	pl_l2ps()
errchk	pl_access

begin
	ll_src = pl_access (pl,v)
	if (!R_NEED_DST(rop)) {
	    np = pl_l2ps (Mems[ll_src], v[1], px_dst, npix)
	    return
	}

	call smark (sp)
	call salloc (px_out, npix, TY_SHORT)

	np = pl_l2ps (Mems[ll_src], v[1], Mems[px_out], npix)
	call pl_pixrops (Mems[px_out], 1, PL_MAXVAL(pl),
	    px_dst, 1, MV(px_depth), npix, rop)

	call sfree (sp)
end
