# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>
include	<plio.h>

# PM_GLP -- Get a line segment as a pixel array, applying the given ROP to
# combine the pixels with those of the output array.

procedure pm_glpi (pl, v, px_dst, px_depth, npix, rop)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I vector coords of line segment
int	px_dst[ARB]		#O output pixel array
int	px_depth		#I pixel depth, bits
int	npix			#I number of pixels desired
int	rop			#I rasterop

int	temp, np, step, xstep
pointer	sp, px_src, px_out, im
include	"../pmio.com"

begin
	im = PM_REFIM(pl)
	if (PM_MAPXY(pl) == NO) {
	    call pl_glpi (pl, v, px_dst, px_depth, npix, rop)
	    return
	}

	call smark (sp)

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

	if (!R_NEED_DST(rop))
	    call amovi (Memi[px_src], px_dst, npix)
	else {
	    call salloc (px_out, npix, TY_INT)
	    call pl_pixropi (Memi[px_src], 1, PL_MAXVAL(pl), px_dst, 1,
		MV(px_depth), npix, rop)
	    call amovi (Memi[px_out], px_dst, npix)
	}

	call sfree (sp)
end
