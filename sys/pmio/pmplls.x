# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	<plio.h>

# PM_PLLS -- Put a line segment input as a list list to a mask, applying the
# given ROP to combine the pixels with those of the output mask.

procedure pm_plls (pl, v, ll_raw, ll_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
short	ll_raw[ARB]		#I input line list
int	ll_depth		#I line list depth, bits
int	npix			#I number of pixels affected
int	rop			#I rasterop

pointer	sp, ll_src, ll_dst, ll_stn, ll_out, px_src, im
int	ll_len, step, xstep, temp, np, ip, i
int	pl_l2pi(), pl_p2li()
pointer	pl_access()
include	"pmio.com"

begin
	im = PM_REFIM(pl)
	if (PM_MAPXY(pl) == NO) {
	    call pl_plls (pl, v, ll_raw, ll_depth, npix, rop)
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

	np = (npix - 1) * step + 1
	ll_stn = NULL

	# Resample and flip the line list if necessary.  Construct a stencil
	# list if the step size is greater than 1.

	if (xstep < 0 || step > 1) {
	    call salloc (px_src, np, TY_INT)
	    i = pl_l2pi (ll_raw, 1, Memi[px_src], npix)
	    call aclri (Memi[px_src+i], np - i)

	    # Flip data array.
	    if (xstep < 0)
		call imaflp (Memi[px_src], npix, SZ_INT)

	    if (step > 1) {
		# Resample data array.
		ip = px_src + npix - 1
		do i = np, 1, -step {
		    Memi[px_src+i-1] = Memi[ip]
		    ip = ip - 1
		}

		# Construct stencil.
		call salloc (ll_stn, LL_MAXLEN(pl), TY_SHORT)
		call aclri (Memi[px_src], np)
		do i = 1, np, step
		    Memi[px_src+i-1] = 1
		ll_len = pl_p2li (Memi[px_src], 1, Mems[ll_stn], np)
	    }

	    # Convert flipped and resampled data back to line list.
	    ll_len = pl_p2li (Memi[px_src], 1, Mems[ll_src], np)

	} else {
	    ll_len = LL_LEN(ll_raw)
	    call amovs (ll_raw, Mems[ll_src], ll_len)
	}

	# Copy to or combine with destination.
	if (ll_stn == NULL)
	    call pl_plls (pl, v1, Mems[ll_src], ll_depth, np, rop)
	else {
	    call salloc (ll_out, LL_MAXLEN(pl), TY_SHORT)
	    ll_dst = pl_access (pl, v1)
	    call pl_linestencil (Mems[ll_src],  1, MV(ll_depth),
		Mems[ll_dst], v1, PL_MAXVAL(pl), Mems[ll_stn], 1,
		Mems[ll_out], np, rop)
	    call pl_update (pl, v1, Mems[ll_out])
	}

	call sfree (sp)
end
