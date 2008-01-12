include "igi.h"

#  GET_INT -- Return an integer argument.  If the argument is not 
#  numeric, issue an error.  If the argument is numeric but not integer, 
#  do the type conversion, i.e., truncate.

int procedure get_int (igs)

pointer	igs		# igi parameters structure

int	token
pointer	tokvals
int	value

int	gettok()

begin
	tokvals = TOKEN_VALUE(igs)

	token = gettok (igs)

	if (token == CONSTANT) {
	    if (LOP_TYPE(tokvals) == TY_INT)
		value = LOP_VALI(tokvals)
	    else
		value = int (LOP_VALR(tokvals))
	    call lcmdcat (igs, NO)
	} else if (IS_NEWCOMMAND(token))
	    value = INDEFI
	else {
	    value = INDEFI
	    call error (0, "Argument is not numeric ")
	}

	return (value)
end
