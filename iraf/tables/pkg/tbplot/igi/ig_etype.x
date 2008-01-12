include "igi.h"

#  IG_ETYPE -- Set the error bar style

#  8/20/91 Removed ^Ls. ZGL
#  1/27/93 Fix INDEF tests.

procedure ig_etype (igs)

pointer	igs			# igi structure

int	etype			# Error bar style

int	get_int()

errchk	get_int

begin
	call lcmdcat (igs, YES)

	iferr (etype = get_int (igs))
	    return

	if (IS_INDEFR (etype)) {
	    # No argument;  list the current value
	    if (DEBUG_OUTPUT(igs) == YES) {
		call eprintf ("Error bar style:  %d ")
		    call pargi (MG_EBTYPE(PLOT_PARMS(igs)))
	    }
	    return
	}

	call cmdcat (igs, NO)

	call ii_etype (igs, etype)
end


procedure ii_etype (igs, etype)

pointer	igs
int	etype

begin
	if (IS_INDEFI (etype))
	    MG_EBTYPE(PLOT_PARMS(igs)) = BAR_TICK
	else
	    MG_EBTYPE(PLOT_PARMS(igs)) = etype
end
