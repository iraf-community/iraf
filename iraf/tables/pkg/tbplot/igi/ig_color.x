include <gset.h>
include "igi.h"

#  IG_COLOR -- Set the device-dependent color 

procedure ig_color (igs)

pointer	igs		# Parameters structure

pointer	tokvals		# Token value structure
pointer	igps		# Plot parameters structure
int	token
int	ci

int	gettok()

begin
	call lcmdcat (igs, YES)

	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	token = gettok (igs)

	if (token == CONSTANT) {
	    if (LOP_TYPE(tokvals) == TY_INT)
		ci = LOP_VALI(tokvals)
	    else
		ci = int (LOP_VALR(tokvals))
	    call lcmdcat (igs, YES)

	} else if (IS_NEWCOMMAND(token)) {
	    # No argument;  echo the current value
	    call printf ("Color index:  %d ")
		call pargi (MG_COLOR(igps))
	    call cmdcat (igs, NO)
	    return

	} else {
	    call eprintf ("Color index must be a constant ")
	    return
	}

	call ii_color (igs, ci)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Color index:  %d ")
		call pargi (ci)
	}

	call cmdcat (igs, NO)
end


procedure ii_color (igs, ci)

pointer	igs		# Parameters structure
int	ci		# Color index

pointer	igps		# Plot parameters structure
int	ici

begin
	igps = PLOT_PARMS(igs)

	ici = min (ci, MAX_CI)
	ici = max (ci, 0)

	MG_COLOR(igps) = ici

	call gseti (GIO_GP(igs), G_PLCOLOR, ici)
	call gseti (GIO_GP(igs), G_PMCOLOR, ici)
	call gseti (GIO_GP(igs), G_FACOLOR, ici)
	call gseti (GIO_GP(igs), G_TXCOLOR, ici)
end
