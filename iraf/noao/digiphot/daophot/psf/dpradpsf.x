include <gset.h>
include "../lib/daophotdef.h"
include "../lib/psfdef.h"

define	FRACTION	0.10

# DP_RADPSF -- Draw a radial profile plot of a data subraster containing a
# candidate psf star.

procedure dp_radpsf (dao, subras, ncols, nlines, x1, y1, title, gp)

pointer	dao				# pointer to DAOPHOT structure
real	subras[ncols,nlines]		# data subraster
int	ncols, nlines			# dimesnions of subraster
int	x1, y1				# coordinates of left hand corner
char	title[ARB]			# title string
pointer	gp				# pointer to graphics descriptor

int	npts
pointer	psf, sp, radius, intensity, str
real	ymin, ymax, r1, r2, i1, i2
int	dp_rivectors()

begin
	# Get the pointer to the DAOPHOT PSF fitting substructure.
	psf = DP_PSF (dao)

	# Allocate temporary space.
	call smark (sp)
	call salloc (radius, ncols * nlines, TY_REAL)
	call salloc (intensity, ncols * nlines, TY_REAL)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Compute the radial profile.
	npts = dp_rivectors (subras, ncols, nlines, x1, y1, DP_CUR_PSFX(psf),
	    DP_CUR_PSFY(psf), DP_PSFRAD(dao), Memr[radius], Memr[intensity])
	call alimr (Memr[intensity], npts, ymin, ymax)

	# Make the plot.
	call gclear (gp)
	r1 = -FRACTION * DP_PSFRAD(dao)
	r2 = DP_PSFRAD(dao) + FRACTION * DP_PSFRAD(dao)
	i1 = ymin - FRACTION * (ymax - ymin)
	i2 = ymax + FRACTION * (ymax - ymin)
	call gswind (gp, r1, r2, i1, i2)
	call glabax (gp, title, "Radius (pixels)", "Intensity (counts)")
	call gpmark (gp, Memr[radius], Memr[intensity], npts, GM_PLUS, 1.0,
	    1.0)

	# Mark the zero radius line.
	call gamove (gp, 0.0, i1)
	call gadraw (gp, 0.0, i2)

	# Mark the sky level.
	call gamove (gp, r1, DP_CUR_PSFSKY(psf))
	call gadraw (gp, r2, DP_CUR_PSFSKY(psf))

	# Mark the half-width at half-maximum.
	call gamove (gp, DP_FWHMPSF(dao) / 2.0, i1)
	call gadraw (gp, DP_FWHMPSF(dao) / 2.0, i2)
	call sprintf (Memc[str], SZ_LINE, "Half-width half-maximum = %0.2f")
	    call pargr (DP_FWHMPSF(dao) / 2.0)
	call gtext (gp, DP_FWHMPSF(dao) / 2.0, i2, Memc[str],
	    "q=h;u=180;v=t;p=r")

	# Mark the fitting radius.
	call gamove (gp, DP_FITRAD(dao), i1)
	call gadraw (gp, DP_FITRAD(dao), i2)
	call sprintf (Memc[str], SZ_LINE, "Fitting radius = %0.2f")
	    call pargr (DP_FITRAD(dao))
	call gtext (gp, DP_FITRAD(dao), i2, Memc[str], "q=h;u=180;v=t;p=r")

	call gflush (gp)
	call sfree (sp)
end
