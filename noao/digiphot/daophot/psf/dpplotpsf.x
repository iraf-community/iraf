include "../lib/daophotdef.h"
include "../lib/psfdef.h"
include "../lib/psf.h"

# DP_PLOTPSF -- Plot the psf using the default plot type. 

procedure dp_plotpsf (dao, subrast, ncols, nlines, gd)

pointer	dao			# pointer to DAOPHOT structure
real	subrast[ncols,nlines]	# image subraster
int	ncols, nlines		# dimensions of the subraster
pointer	gd			# pointer to the graphics stream

pointer	sp, title, psf, psfpl

begin
	# Return if the graphics stream is undefined.
	if (gd == NULL)
	    return

	# Initialize various daophot pointers.
	psf   = DP_PSF (dao)
	psfpl = DP_PSFPLOT (psf)

	# Construct the title.
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	call sprintf (Memc[title], SZ_LINE, "Star: %d  X: %g  Y: %g  Mag: %g")
	    call pargi (DP_CUR_PSFID(psf))
	    call pargr (DP_CUR_PSFX(psf))
	    call pargr (DP_CUR_PSFY(psf))
	    call pargr (DP_CUR_PSFMAG(psf))

	# Initialize the contour plot parameters.
	DP_CCEIL (psfpl) = 0.0
	DP_CFLOOR (psfpl) = 0.0
	DP_CZERO (psfpl) = DP_PSFMIN (psf)

	# Initialize plot.
	if (DP_PLOT_TYPE(psfpl) == DP_MESHPLOT)
	    call dp_surfpsf (dao, subrast, ncols, nlines, Memc[title], gd)
	else if (DP_PLOT_TYPE(psfpl) == DP_CONTOURPLOT)
	    call dp_contpsf (dao, subrast, ncols, nlines, Memc[title], gd)

	call gdeactivate (gd, 0)
	call sfree (sp)
end
