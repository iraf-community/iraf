include <gset.h>
include "igi.h"

#  IG_LWEIGHT -- Set the device-dependent line width 

procedure ig_lweight (igs)

pointer	igs		# Parameters structure

pointer	tokvals		# Token value structure
pointer	igps		# Plot parameters structure
int	token
real	lw

int	gettok()

begin
	call lcmdcat (igs, YES)

	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	token = gettok (igs)

	if (token == CONSTANT) {
	    if (LOP_TYPE(tokvals) == TY_INT)
		lw = real (LOP_VALI(tokvals))
	    else
		lw = LOP_VALR(tokvals)
	    call lcmdcat (igs, YES)
	} else if (IS_NEWCOMMAND(token)) {
	    # No argument;  echo the current value
	    call printf ("Relative line weight (width):  %.1f ")
		call pargr (MG_LWEIGHT(igps))
	    call cmdcat (igs, NO)
	    return
	} else {
	    call eprintf ("Line weight (width) must be a constant ")
	    return
	}

	call ii_lweight (igs, lw)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Relative line weight (width):  %.1f ")
		call pargr (lw)
	}

	call cmdcat (igs, NO)
end


procedure ii_lweight (igs, lw)

pointer	igs		# Parameters structure
real	lw		# Line width

pointer	igps		# Plot parameters structure
real	fill

begin
	igps = PLOT_PARMS(igs)
	fill = MG_PNTFILL(igps) * MG_LWEIGHT(igps)

	# Adjust the filled point factor to save rasters
	MG_PNTFILL(igps) = fill / lw
	MG_LWEIGHT(igps) = max (1.0, lw)

	call gsetr (GIO_GP(igs), G_PLWIDTH, lw)
end
