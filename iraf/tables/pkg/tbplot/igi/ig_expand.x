include "igi.h"

#  8/20/91 Removed ^Ls. ZGL

procedure ig_expand (igs)

pointer	igs		# Parameters structure

pointer	tokvals		# Token value structure
int	token
real	size
pointer	igps		# Plot parameters structure

int	gettok()

begin
	call lcmdcat (igs, YES)
	tokvals = TOKEN_VALUE(igs)

	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    igps = PLOT_PARMS(igs)
	    call eprintf ("Size:  %.1f;  Text:  %.3f;  Point:  %.3f (NDC units) ")
		call pargr (MG_EXPAND(igps))
		call pargr (MG_EXPAND(igps) * MG_CHARSIZE(igps))
		call pargr (MG_EXPAND(igps) * MG_PNTSIZE(igps))
	    return
	} if (token != CONSTANT) {
	    call eprintf ("Text and Marker size must be a constant ")
	    return
	}

	call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)

	if (LOP_TYPE(tokvals) == TY_INT)
	    size = real (LOP_VALI(tokvals))
	else
	    size = LOP_VALR(tokvals)

	call ii_expand (igs, size)

	if (DEBUG_OUTPUT(igs) == YES) {
	    igps = PLOT_PARMS(igs)
	    call eprintf ("Size:  %.3f;  Text:  %.3f;  Point:  %.3f (NDC units) ")
		call pargr (MG_EXPAND(igps))
		call pargr (MG_EXPAND(igps) * MG_CHARSIZE(igps))
		call pargr (MG_EXPAND(igps) * MG_PNTSIZE(igps))
	}
end


procedure ii_expand (igs, size)

pointer	igs		# Parameters structure
real	size

begin
	MG_EXPAND(PLOT_PARMS(igs)) = size
end
