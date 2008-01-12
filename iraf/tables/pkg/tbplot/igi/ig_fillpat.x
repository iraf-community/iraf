include "igi.h"

## 7/17/92  ZGL
## 10/7/92  Print fill pattern type for command with no args.  ZGL

procedure ig_fillpat (igs)

pointer	igs		# Parameters structure

pointer	tokvals		# Token value structure
pointer	igps		# Plot parameters structure

int	token

int	gettok()

begin
	call lcmdcat (igs, YES)

	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	token = gettok (igs)

	if (token == CONSTANT) {
	    if (LOP_TYPE(tokvals) == TY_INT)
		MG_FILLPAT(igps) = LOP_VALI(tokvals)

	    else
		MG_FILLPAT(igps) = int (LOP_VALR(tokvals))

	    call lcmdcat (igs, NO)
	    call cmdcat  (igs, NO)

	} else if (IS_NEWCOMMAND(token)) {
	    #  No argument;  list current value
	    call printf ("Fill pattern:  %d ")
		call pargi (MG_FILLPAT(igps))

	    switch (MG_FILLPAT(igps)) {
	    case CLEAR_FILL:
		call printf ("(clear) ")

	    case HOLLOW_FILL:
		call printf ("(hollow) ")

	    case SOLID_FILL:
		call printf ("(solid) ")
	    }

	    if (MG_FILLPAT(igps) >= HATCH_FILL)
		call printf ("(hatch) ")

	    return

	} else {
	    call eprintf ("Invalid FILLPAT argument:  %s ")
		call pargstr (LOP_VALC(tokvals))
	    return
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Fill pattern: %d ")
		call pargi (MG_FILLPAT(igps))
	}
end
