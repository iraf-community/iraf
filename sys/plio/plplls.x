# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_PLLS -- Put a line segment input as a line list to a mask, applying the
# given ROP to combine the line segment with the existing line of the mask.

procedure pl_plls (pl, v, ll_src, ll_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
short	ll_src[ARB]		#I input line list
int	ll_depth		#I line list depth, bits
int	npix			#I number of pixels to be set
int	rop			#I rasterop

pointer	sp, ll_out, ll_dst
pointer	pl_access()
errchk	pl_access

begin
	if (!R_NEED_DST(rop) && v[1] == 1 && npix == PL_AXLEN(pl,1))
	    call pl_update (pl, v, ll_src)
	else {
	    call smark (sp)
	    call salloc (ll_out, LL_MAXLEN(pl), TY_SHORT)

	    ll_dst = pl_access (pl,v)
	    call pl_linerop (ll_src, 1, PL_MAXVAL(pl), Mems[ll_dst], v[1],
		MV(ll_depth), Mems[ll_out], npix, rop)
	    call pl_update (pl, v, Mems[ll_out])

	    call sfree (sp)
	}
end
