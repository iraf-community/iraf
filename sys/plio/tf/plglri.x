# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_GLR -- Get a line segment as a range list, applying the given ROP to
# combine the pixels with those of the output list.

procedure pl_glri (pl, v, rl_dst, rl_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
int	rl_dst[ARB]		#O output range list
int	rl_depth		#I range list depth, bits
int	npix			#I number of pixels desired
int	rop			#I rasterop

int	nr
pointer	sp, rl_out, ll_src
int	pl_access(), pl_l2ri()
errchk	pl_access

begin
	ll_src = Ref (pl, pl_access(pl,v))
	if (!R_NEED_DST(rop))
	    nr = pl_l2ri (Mems[ll_src], v[1], rl_dst, npix)
	else {
	    call smark (sp)
	    call salloc (rl_out, min(RL_MAXLEN(pl),npix*3), TY_INT)

	    nr = pl_l2ri (Mems[ll_src], v[1], Memi[rl_out], npix)
	    call pl_rangeropi (Memi[rl_out], 1, PL_MAXVAL(pl),
		rl_dst, 1, MV(rl_depth), npix, rop)

	    call sfree (sp)
	}
end
