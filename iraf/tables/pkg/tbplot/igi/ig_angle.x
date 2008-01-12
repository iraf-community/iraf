include "igi.h"

#  8/20/91 Removed ^Ls. ZGL

procedure ig_angle (igs)

pointer	igs		# Parameters structure

int	in		# Input stream descriptor
pointer	tokvals		# Token value structure
int	token
real	angle

int	gettok()

begin
	call lcmdcat (igs, YES)
	in = INPUT_SOURCE(igs)
	tokvals = TOKEN_VALUE(igs)

	token = gettok (igs)

	if (IS_NEWCOMMAND(token)) {
	    call eprintf ("Text and point marker rotation angle:  %.1f ")
		call pargr (MG_ANGLE(PLOT_PARMS(igs)))
	    return
	} else if (token != CONSTANT) {
	    call eprintf ("Angle must be a constant ")
	    return
	}

	call lcmdcat (igs, NO)
	call cmdcat  (igs, NO)

	if (LOP_TYPE(tokvals) == TY_INT)
	    angle = real (LOP_VALI(tokvals))
	else
	    angle = LOP_VALR(tokvals)

	call ii_angle (igs, angle)

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Angle:  %.1f ")
		call pargr (MG_ANGLE(PLOT_PARMS(igs)))
	}
end


procedure ii_angle (igs, angle)

pointer	igs		# Parameters structure
real	angle

begin
	MG_ANGLE(PLOT_PARMS(igs)) = angle
end
