# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	<plio.h>

# PM_PLP -- Put a line segment input as a pixel array a mask, applying the
# given ROP to combine the pixels with those of the mask.

procedure pm_plpl (pl, v, px_src, px_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
long	px_src[ARB]		#I input pixel array
int	px_depth		#I pixel depth, bits
int	npix			#I number of pixels affected
int	rop			#I rasterop

pointer	sp, ll_src
int	ll_len, pl_p2ll()
include	"../pmio.com"

begin
	if (PM_MAPXY(pl) == NO)
	    call pl_plpl (pl, v, px_src, px_depth, npix, rop)
	else {
	    call smark (sp)
	    call salloc (ll_src, LL_MAXLEN(pl), TY_SHORT)

	    ll_len = pl_p2ll (px_src, 1, Mems[ll_src], npix)
	    call pm_plls (pl, v, Mems[ll_src], px_depth, npix, rop)

	    call sfree (sp)
	}
end
