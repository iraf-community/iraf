include "igi.h"

## 3/10/93  Try to fix problems with returning to default (all) input
##          rows.  There seems to be a problem using INDEF, so we'll
##          use zero.  Curiously, "indef" works but "INDEF" gives a
##          floating point error -- there's something fishy in the
##          parser.  ZGL

procedure ig_lines (igs)

pointer	igs		# igi parameters structure

pointer	tokvals		# Token value structure
pointer	igps		# parameters structure
int	token
int	frow, lrow

int	gettok()

begin
	call lcmdcat (igs, YES)
	tokvals = TOKEN_VALUE(igs)
	igps = PLOT_PARMS(igs)

	token = gettok (igs)

	if (token == CONSTANT) {
	    if (LOP_TYPE(tokvals) == TY_INT)
		frow = LOP_VALI(tokvals)
	    else if (IS_INDEFR(LOP_VALR(tokvals)))
		frow = INDEFI
	    else
		frow = int (LOP_VALR(tokvals))

	    if (frow < 1)
	        frow = INDEFI

	    call lcmdcat (igs, NO)

	} else if (IS_NEWCOMMAND(token)) {
	    # No arguments;  list the range of lines
	    call printf ("Range of input rows:  %d to %d ")
		call pargi (MG_FROW(igps))
		call pargi (MG_LROW(igps))

	    call cmdcat (igs, NO)
	    return

	} else {
#	    call eprintf ("Starting data row must be a constant ")
	    frow = INDEFI
#	    return
	}

	MG_FROW(igps) = frow

	token = gettok (igs)

	if (token == CONSTANT) {
	    if (LOP_TYPE(tokvals) == TY_INT)
		lrow = LOP_VALI(tokvals)

	    else if (IS_INDEFR(LOP_VALR(tokvals)))
		lrow = INDEFI
	    else
		lrow = int (LOP_VALR(tokvals))

	    if (lrow <= 0)
	        lrow = INDEFI

	    call lcmdcat (igs, NO)

	} else if (IS_NEWCOMMAND(token)) {
	    # No argument;  one row only or reset to INDEF
	    lrow = frow

	} else {
#	    call eprintf ("Ending data row must be a constant ")
	    lrow = INDEFI
	}

	MG_LROW(igps) = lrow
	call cmdcat (igs, NO)
end
