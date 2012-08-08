# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pmset.h>
include	<plio.h>

# PM_GLLS -- Get a line segment as a list list, applying the given ROP to
# combine the pixels with those of the output line list.

procedure pm_glls (pl, v, ll_dst, ll_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
short	ll_dst[ARB]		#O output line list
int	ll_depth		#I line list depth, bits
int	npix			#I number of pixels desired
int	rop			#I rasterop

int	ll_len, temp, np, step, xstep
pointer	sp, px_src, ll_src, ll_out, im
int	pl_p2li()
include	"pmio.com"

begin
	im = PM_REFIM(pl)
	if (PM_MAPXY(pl) == NO) {
	    call pl_glls (pl, v, ll_dst, ll_depth, npix, rop)
	    return
	}

	call smark (sp)
	call salloc (ll_src, LL_MAXLEN(pl), TY_SHORT)

	# Determine physical coords of line segment.
	call amovl (v, v3, PM_MAXDIM)
	call imaplv (im, v3, v1, PM_MAXDIM)
	v3[1] = v3[1] + npix - 1
	call imaplv (im, v3, v2, PM_MAXDIM)

	# Get line scaling parameters.
	if (npix <= 1)
	    xstep = 1
	else
	    xstep = (v2[1] - v1[1]) / (npix - 1)
	step = xstep
	if (xstep < 0) {
	    temp = v1[1];  v1[1] = v2[1];  v2[1] = temp
	    step = -step
	}

	# Extract the pixels.
	np = (npix - 1) * step + 1
	call salloc (px_src, np, TY_INT)
	call pl_glpi (pl, v1, Memi[px_src], 0, np, PIX_SRC)

	# Subsample and flip if necessary.
	if (step > 1)
	    call imsamp (Memi[px_src], Memi[px_src], npix, SZ_INT, step)
	if (xstep < 0)
	    call imaflp (Memi[px_src], npix, SZ_INT)

	# Convert to a line list.
	ll_len = pl_p2li (Memi[px_src], 1, Mems[ll_src], npix)

	# Copy to or combine with destination.
	if (!R_NEED_DST(rop)) {
	    ll_len = LP_LEN(ll_src)
	    call amovs (Mems[ll_src], ll_dst, ll_len)
	} else {
	    call salloc (ll_out, LL_MAXLEN(pl), TY_SHORT)
	    call pl_linerop (Mems[ll_src], 1, PL_MAXVAL(pl), ll_dst, 1,
		MV(ll_depth), Mems[ll_out], npix, rop)
	    ll_len = LP_LEN(ll_out)
	    call amovs (Mems[ll_out], ll_dst, ll_len)
	}

	call sfree (sp)
end
