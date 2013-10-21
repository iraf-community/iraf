# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<pmset.h>
include	<plio.h>

# PM_GLR -- Get a line segment as a range list, applying the given ROP to
# combine the pixels with those of the output line list.  Note that this
# operator uses IMIO if a section transformation is needed, hence if the 
# application also uses IMIO to directly access the mask image, care must
# be taken to avoid confusion over the use of IMIO allocated pixel buffers.

procedure pm_glrl (pl, v, rl_dst, rl_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
long	rl_dst[3,ARB]		#O output line list
int	rl_depth		#I line list depth, bits
int	npix			#I number of pixels desired
int	rop			#I rasterop

int	rl_len, temp, step, xstep, np
pointer	sp, px_src, rl_src, rl_out, im
include	"../pmio.com"
int	pl_p2rl()

begin
	im = PM_REFIM(pl)
	if (PM_MAPXY(pl) == NO) {
	    call pl_glrl (pl, v, rl_dst, rl_depth, npix, rop)
	    return
	}

	call smark (sp)
	call salloc (rl_src, RL_MAXLEN(pl), TY_LONG)

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
	call salloc (px_src, np, TY_LONG)
	call pl_glpl (pl, v1, Meml[px_src], 0, np, PIX_SRC)

	# Subsample and flip if necessary.
	if (step > 1)
	    call imsamp (Meml[px_src], Meml[px_src], npix, SZ_LONG, step)
	if (xstep < 0)
	    call imaflp (Meml[px_src], npix, SZ_LONG)

	# Convert to a range list.
	rl_len = pl_p2rl (Meml[px_src], 1, Meml[rl_src], npix)

	# Copy to or combine with destination.
	if (!R_NEED_DST(rop)) {
	    rl_len = RLI_LEN(rl_src) * RL_LENELEM
	    call amovl (Meml[rl_src], rl_dst, rl_len)
	} else {
	    call salloc (rl_out, RL_MAXLEN(pl), TY_LONG)
	    call pl_rangeropl (Meml[rl_src], 1, PL_MAXVAL(pl), rl_dst, 1,
		MV(rl_depth), Meml[rl_out], npix, rop)
		rl_len = RLI_LEN(rl_out) * RL_LENELEM
	    call amovl (Meml[rl_out], rl_dst, rl_len)
	}

	call sfree (sp)
end
