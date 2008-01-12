include "igi.h"

#  GET_REAL -- Parse a token and return a real valued argument
#  If the token is not numeric, issue an error.  If the token is numeric 
#  but not real, do the type conversion.

real procedure get_real (igs)

pointer	igs			# igi parameters structure

int	token
pointer	tokvals			# Token value structure
real	value

int	gettok()

begin
	tokvals = TOKEN_VALUE(igs)
	token   = gettok (igs)

	if (token == CONSTANT) {
	    if (LOP_TYPE(tokvals) == TY_REAL)
		value = LOP_VALR(tokvals)
	    else
		value = real (LOP_VALI(tokvals))
	    call lcmdcat (igs, NO)
	} else if (IS_NEWCOMMAND(token))
	    value = INDEF
	else {
	    value = INDEF
	    call error (0, "Numeric value required ")
	}

	return (value)
end
