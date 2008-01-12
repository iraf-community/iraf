include "igi.h"

#  IG_TICKSIZE -- Set the BOX tick spacing

#  8/20/91 Removed ^Ls. ZGL
#  1/27/93 Fix INDEF tests.

procedure ig_ticksize (igs)

pointer	igs		# igi structure

real	minorx, majorx, minory, majory
pointer	igps		# Plot parameters structure

real	get_real()

errchk	get_real

begin
	call lcmdcat (igs, YES)

	iferr (minorx = get_real(igs))
	    return

	igps = PLOT_PARMS(igs)

	if (IS_INDEFR (minorx)) {
	    # No arguments;  list the current values
	    call eprintf ("Tick spacing:  %f %f %f %f ")
		call pargr (MG_MINORX(igps))
		call pargr (MG_MAJORX(igps))
		call pargr (MG_MINORY(igps))
		call pargr (MG_MAJORY(igps))
	    return
	} else {
	    # First argument present;  get another
	    iferr (majorx = get_real(igs))
		return
	    if (IS_INDEFR (majorx)) {
		minory = INDEFR
		majory = INDEFR
	    } else {
		# Second argument present;  get another
		iferr (minory = get_real(igs))
		    return
		if (IS_INDEFR (minory))
		    majory = INDEFR
		else
		    # Third argument present;  get another
		    iferr (majory = get_real(igs))
			return
	    }
	}

	call ii_ticksize (igs, minorx, majorx, minory, majory)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Tick intervals:\t%.1e\t%.1e\t%.1e\t%.1e ")
		call pargr (minorx)
		call pargr (majorx)
		call pargr (minory)
		call pargr (majory)
	}

	call cmdcat  (igs, NO)
end


procedure ii_ticksize (igs, minorx, majorx, minory, majory)

pointer	igs		# igi structure
real	minorx, majorx, minory, majory

pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	if (IS_INDEFR (minorx))
	    MG_MINORX(igps) = 0.0
	else
	    MG_MINORX(igps) = minorx

	if (IS_INDEFR (majorx))
	    MG_MAJORX(igps) = 0.0
	else
	    MG_MAJORX(igps) = majorx

	if (IS_INDEFR (minory))
	    MG_MINORY(igps) = 0.0
	else
	    MG_MINORY(igps) = minory

	if (IS_INDEFR (majory))
	    MG_MAJORY(igps) = 0.0
	else
	    MG_MAJORY(igps) = majory
end
