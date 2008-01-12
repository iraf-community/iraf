include "igi.h"

#  8/20/91 Removed ^Ls. ZGL

procedure ig_ltype (igs)

pointer	igs		# Parameters structure

pointer	tokvals		# Token value structure
pointer	igps		# Plot parameters structure

int	token

int	gettok(), ltypci()

begin
	call lcmdcat (igs, YES)

	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	token = gettok (igs)

	if (token == CONSTANT) {
	    if (LOP_TYPE(tokvals) == TY_INT)
		MG_LTYPEN(igps) = LOP_VALI(tokvals)
	    else
		MG_LTYPEN(igps) = int (LOP_VALR(tokvals))
	    call ltypic (MG_LTYPEN(igps), MG_LTYPE(igps), SZ_LINE)
	    call lcmdcat (igs, NO)
	    call cmdcat  (igs, NO)
	} else if (IS_NEWCOMMAND(token)) {
	    call printf ("Line style:  %s (%d) ")
		call pargstr (MG_LTYPE(igps))
		call pargi (MG_LTYPEN(igps))
	    return
	} else if (token == IDENTIFIER || token == STRING) {
	    call strcpy (LOP_VALC(tokvals), MG_LTYPE(igps), SZ_LINE)
	    MG_LTYPEN(igps) = ltypci (MG_LTYPE(igps))
	    call lcmdcat (igs, NO)
	    call cmdcat  (igs, NO)
	} else {
	    call eprintf ("Invalid LTYPE argument:  %s ")
		call pargstr (LOP_VALC(tokvals))
	    return
	}

	if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Line style:  %s (%d) ")
		call pargstr (MG_LTYPE(igps))
		call pargi (MG_LTYPEN(igps))
	}
end


int procedure ltypci (ctype)

char	ctype[ARB]

int	lt
string	typdic	"|clear|solid|dotted|dotdash|dashed|"

int	strdic()

begin
	# Convert string line type to integer code
	call strlwr (ctype)
	lt = strdic (ctype, ctype, SZ_LINE, typdic)

	switch (lt) {
	case 0:
	    call eprintf ("Unrecognized or ambiguous line type:  %s ")
		call pargstr (ctype)
	case 1:
	    lt = CLEAR_LINE
	case 2:
	    lt = SOLID_LINE
	case 3:
	    lt = DOTTED_LINE
	case 4:
	    lt = DOT_SHORT_DASH
	case 5:
	    lt = SHORT_DASH
	}

	return (lt)
end


procedure ltypic (ltype, ctype, maxch)

int	ltype
char	ctype[ARB]
int	maxch

begin
	# Convert integer line type code to character string
	switch (ltype) {
	case CLEAR_LINE:
	    call strcpy ("clear", ctype, maxch)
	case SOLID_LINE:
	    call strcpy ("solid", ctype, maxch)
	case DOTTED_LINE:
	    call strcpy ("dotted", ctype, maxch)
	case SHORT_DASH:
	    call strcpy ("dashed", ctype, maxch)
	case LONG_DASH:
	    call strcpy ("dashed", ctype, maxch)
	case DOT_SHORT_DASH:
	    call strcpy ("dotdash", ctype, maxch)
	case DOT_LONG_DASH:
	    call strcpy ("dotdash", ctype, maxch)
	case SHORT_LONG_DASH:
	    call strcpy ("dashed", ctype, maxch)
	default:
	    call strcpy ("solid", ctype, maxch)
	    ltype = 0
	}
end
