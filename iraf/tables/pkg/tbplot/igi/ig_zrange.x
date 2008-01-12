include "igi.h"

#  IG_ZRANGE -- Set the range of image pixel values to map to the display range.

## 25 June 1992.  ZGL

procedure ig_zrange (igs)

pointer	igs			# igi parameters structure

pointer	igps			# Plot parameters structure
real	zmin, zmax		# Range

real	get_real()

errchk	get_real

begin
	call lcmdcat (igs, YES)

	iferr (zmin = get_real (igs))
	    # Invalid parameter
	    return

	if (IS_INDEF(zmin)) {
	    # No arguments;  autoscale on the data
	    zmin = INDEF
	    zmax = INDEF

	} else {
	    # Valid minimum;  there should be one more argument

	    # Maximum
	    iferr (zmax = get_real (igs))
		# Invalid parameter
		return
	}

	call cmdcat (igs, NO)

	call ii_zrange (igs, zmin, zmax)

	if (DEBUG_OUTPUT(igs) == YES) {
	    igps = PLOT_PARMS(igs)
	    call eprintf ("Z range:  %f %f ")
		call pargr (MG_ZMIN(igps))
		call pargr (MG_ZMAX(igps))
	}
end


procedure ii_zrange (igs, zmin, zmax)

pointer	igs			# igi parameters structure
real	zmin, zmax		# Range

pointer	igps			# Plot parameters structure
real	dmin, dmax		# Range
int	nx, ny
int	npts

begin
	igps = PLOT_PARMS(igs)

	if (IS_INDEF(zmin) || IS_INDEF(zmax)) {
	    # Find range of input data

	    if (MG_ZDATAP(igps) == NULL) {
		call eprintf (" No Z (pixmap) data ")
		return
	    }

	    # Size of pixmap raster
	    nx = MG_ZNPTSX(igps) 
	    ny = MG_ZNPTSY(igps)

	    npts = nx * ny
	    call alimr (Memr[MG_ZDATAP(igps)], npts, dmin, dmax)

	    if (IS_INDEF(zmin))
		zmin = dmin

	    if (IS_INDEF(zmax))
		zmax = dmax
	}

	MG_ZMIN(igps) = zmin
	MG_ZMAX(igps) = zmax
end
