# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>
include	<plset.h>

# PL_GLR -- Get a line segment as a range list, applying the given ROP to
# combine the pixels with those of the output list.

procedure pl_glri (pl, v, rl_dst, rl_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
int	rl_dst[ARB]		#O output range list
int	rl_depth		#I range list depth, bits
size_t	npix			#I number of pixels desired
int	rop			#I rasterop

size_t	sz_val
size_t	mr
long	lval
int	nr
pointer	sp, rl_out, rl_src, ll_src
pointer	pl_access()
int	pl_l2ri()
errchk	pl_access

begin
	ll_src = pl_access (pl,v)
	if (!R_NEED_DST(rop))
	    nr = pl_l2ri (Mems[ll_src], v[1], rl_dst, npix)
	else {
	    call smark (sp)
	    mr = min (RL_MAXLEN(pl), npix * 3)
	    call salloc (rl_src, mr, TY_INT)
	    call salloc (rl_out, mr, TY_INT)

	    nr = pl_l2ri (Mems[ll_src], v[1], Memi[rl_src], npix)
	    lval = 1
	    call pl_rangeropi (Memi[rl_src], lval, PL_MAXVAL(pl),
			              rl_dst,  lval, MV(rl_depth),
				      Memi[rl_out], npix, rop)

	     # Copy out the edited range list.
	     sz_val = RLI_LEN(rl_out)
	     call amovi (Memi[rl_out], rl_dst, sz_val)

	    call sfree (sp)
	}
end
