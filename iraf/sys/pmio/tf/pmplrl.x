# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	<plio.h>

# PM_PLR -- Put a line segment input as a range list to a mask, applying the
# given ROP to combine the pixels with those of the output mask.

procedure pm_plrl (pl, v, rl_src, rl_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
long	rl_src[3,ARB]		#I input range list
int	rl_depth		#I range list pixel depth, bits
int	npix			#I number of pixels affected
int	rop			#I rasterop

pointer	sp, ll_src
int	ll_len, pl_r2ll()
include	"../pmio.com"

begin
	if (PM_MAPXY(pl) == NO)
	    call pl_plrl (pl, v, rl_src, rl_depth, npix, rop)
	else {
	    call smark (sp)
	    call salloc (ll_src, LL_MAXLEN(pl), TY_SHORT)

	    ll_len = pl_r2ll (rl_src, 1, Mems[ll_src], npix)
	    call pm_plls (pl, v, Mems[ll_src], rl_depth, npix, rop)

	    call sfree (sp)
	}
end
