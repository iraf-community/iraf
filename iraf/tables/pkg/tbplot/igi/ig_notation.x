include "igi.h"

#  8/20/91 Removed ^Ls. ZGL
#  1/27/93 Fix INDEF tests.

procedure ig_notation (igs)

pointer	igs		# igi structure

real	xlexp, xhexp, ylexp, yhexp

real	get_real()

errchk	get_real

begin
	iferr (xlexp = get_real(igs))
	    return
	if (IS_INDEFR (xlexp)) {
	    xhexp = INDEFR
	    ylexp = INDEFR
	    yhexp = INDEFR
	} else {
	    iferr (xhexp = get_real(igs))
		return
	    if (IS_INDEFR (xhexp)) {
		ylexp = INDEFR
		yhexp = INDEFR
	    } else {
		iferr (ylexp = get_real(igs))
		    return
		if (IS_INDEFR (ylexp))
		    yhexp = INDEFR
		else
		    iferr (yhexp = get_real(igs))
			return
	    }
	}

	call ii_notation (igs, xlexp, xhexp, ylexp, yhexp)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Exponential notation limits:\t%.1e\t%.1e\t%.1e\t%.1e ")
		call pargr (xlexp)
		call pargr (xhexp)
		call pargr (ylexp)
		call pargr (yhexp)
	}
end


procedure ii_notation (igs, xlexp, xhexp, ylexp, yhexp)

pointer	igs		# igi structure
real	xlexp, xhexp, ylexp, yhexp

pointer	igps		# Plot parameters structure

begin
	igps = PLOT_PARMS(igs)

	if (IS_INDEFR (xlexp) || IS_INDEFR (xhexp)) {
	    MG_XLEXP(igps) = 1.0e-4
	    MG_XHEXP(igps) = 1.0e5
	} else {
	    MG_XLEXP(igps) = xlexp
	    MG_XHEXP(igps) = xhexp
	}

	if (IS_INDEFR (ylexp) || IS_INDEFR (yhexp)) {
	    MG_YLEXP(igps) = 1.0e-4
	    MG_YHEXP(igps) = 1.0e5
	} else {
	    MG_YLEXP(igps) = ylexp
	    MG_YHEXP(igps) = yhexp
	}
end
