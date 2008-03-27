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
size_t	npix			#I number of pixels desired
int	rop			#I rasterop

size_t	sz_val
int	ll_len
long	temp, step, xstep, lval
size_t	np
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
	sz_val = PM_MAXDIM
	call amovl (v, v3, sz_val)
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
	if (step > 1) {
	    # arg1,2: incompatible pointer
	    call imsamp (Memi[px_src], Memi[px_src], npix, SZ_INT, step)
	}
	if (xstep < 0) {
	    # arg1: incompatible pointer
	    call imaflp (Memi[px_src], npix, SZ_INT)
	}

	# Convert to a line list.
	lval = 1
	ll_len = pl_p2li (Memi[px_src], lval, Mems[ll_src], npix)

	# Copy to or combine with destination.
	if (!R_NEED_DST(rop)) {
	    ll_len = LP_LEN(ll_src)
	    sz_val = ll_len
	    call amovs (Mems[ll_src], ll_dst, sz_val)
	} else {
	    call salloc (ll_out, LL_MAXLEN(pl), TY_SHORT)
	    lval = 1
	    call pl_linerop (Mems[ll_src], lval, PL_MAXVAL(pl), ll_dst, lval,
			     MV(ll_depth), Mems[ll_out], npix, rop)
	    ll_len = LP_LEN(ll_out)
	    sz_val = ll_len
	    call amovs (Mems[ll_out], ll_dst, sz_val)
	}

	call sfree (sp)
end
