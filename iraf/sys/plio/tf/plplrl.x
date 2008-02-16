# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_PLR -- Put a line segment input as a range list to a mask, applying the
# given ROP to combine the line segment with the existing line of the mask.

procedure pl_plrl (pl, v, rl_src, rl_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
long	rl_src[ARB]		#I input range list
int	rl_depth		#I range list depth, bits
int	npix			#I number of pixels to be set
int	rop			#I rasterop

int	ll_len
pointer	sp, ll_src, ll_out, ll_dst
pointer	pl_access()
int	pl_r2ll()
errchk	pl_access

begin
	call smark (sp)
	call salloc (ll_src, LL_MAXLEN(pl), TY_SHORT)

	# Convert the range list to a line list.
	ll_len = pl_r2ll (rl_src, 1, Mems[ll_src], npix)

	if (!R_NEED_DST(rop) && v[1] == 1 && npix == PL_AXLEN(pl,1))
	    call pl_update (pl, v, Mems[ll_src])
	else {
	    call salloc (ll_out, LL_MAXLEN(pl), TY_SHORT)
	    ll_dst = pl_access (pl,v)
	    call pl_linerop (Mems[ll_src], 1, PL_MAXVAL(pl), Mems[ll_dst], v[1],
		MV(rl_depth), Mems[ll_out], npix, rop)
	    call pl_update (pl, v, Mems[ll_out])
	}

	call sfree (sp)
end
