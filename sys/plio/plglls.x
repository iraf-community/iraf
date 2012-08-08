# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_GLLS -- Get a line segment as a list list, applying the given ROP to
# combine the pixels with those of the output line list.

procedure pl_glls (pl, v, ll_dst, ll_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
short	ll_dst[ARB]		#O output line list
int	ll_depth		#I line list depth, bits
int	npix			#I number of pixels desired
int	rop			#I rasterop

int	ll_len
pointer	sp, ll_out, ll_src
pointer	pl_access()
errchk	pl_access

begin
	ll_src = pl_access (pl,v)
	if (!R_NEED_DST(rop) && v[1] == 1 && npix == PL_AXLEN(pl,1)) {
	    ll_len = LP_LEN(ll_src)
	    call amovs (Mems[ll_src], ll_dst, ll_len)

	} else {
	    call smark (sp)
	    call salloc (ll_out, LL_MAXLEN(pl), TY_SHORT)

	    call pl_linerop (Mems[ll_src], v[1], PL_MAXVAL(pl), ll_dst, 1,
		MV(ll_depth), Mems[ll_out], npix, rop)
	    ll_len = LP_LEN(ll_out)
	    call amovs (Mems[ll_out], ll_dst, ll_len)

	    call sfree (sp)
	}
end
