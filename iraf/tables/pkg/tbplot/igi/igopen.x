include <tbset.h>
include "igi.h"

pointer procedure igopen (gp, mode)

#  IGOPEN -- Initialize igi.  Allocate the structures, initialize 
#  parameters and open graphics.

## 12 June 1992  Add test for redirected STDOUT.  ZGL
## 8 July 1993  Break out command-related stuff to igcopen().

pointer	gp		# I:  Graphics descriptor
int	mode		# I:  Mode to open the graphics device.

pointer	igs		# igi structure
pointer	igps		# parameters structure
real	vl, vr, vb, vt	# Original viewport.
real	wl, wr, wb, wt	# Original window.

begin
	# Allocate the igi parameters structure
	call malloc (igs, LEN_IGS, TY_STRUCT)

	# Allocate the plot parameters structure
	call malloc (igps, LEN_MGSTR, TY_STRUCT)
	PLOT_PARMS(igs) = igps

	GIO_GP(igs) = gp

	# Allocate the string parameter buffers
	call malloc (MG_DATAFN_P(igps),  SZ_FNAME,   TY_CHAR)
	call malloc (MG_COLNAME_P(igps), int(SZ_COLNAME), TY_CHAR)
	call malloc (MG_TITLE_P(igps),   SZ_LINE,    TY_CHAR)
	call malloc (MG_XLABEL_P(igps),  SZ_LINE,    TY_CHAR)
	call malloc (MG_YLABEL_P(igps),  SZ_LINE,    TY_CHAR)
	call malloc (MG_LTYPE_P(igps),   SZ_LINE,    TY_CHAR)
	call malloc (MG_PTYPE_P(igps),   SZ_LINE,    TY_CHAR)
	call malloc (MG_TICKFMT_P(igps), SZ_LINE,    TY_CHAR)

	# If mode is APPEND, remember the viewport and scaling used
	# in the current plot.  Set these after the ig_reset call.
	if (mode == APPEND) {
	    call ggview (gp, vl, vr, vb, vt)
	    call ggwind (gp, wl, wr, wb, wt)
	}
	
	# Set parameters to default values
	call ig_reset (igs)

	# If mode is APPEND, set the location and limits to what was
	# originally specified.  Location is equivalent to GIO viewport.
	# Limits is equivalent to the scaling.
	if (mode == APPEND) {
	    call ii_location (igs, vl, vr, vb, vt)
	    call ii_limits (igs, wl, wr, wb, wt)
	}

	return (igs)
end
