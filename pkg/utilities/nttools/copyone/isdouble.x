include	<ctype.h>
include	<mach.h>

# IS_DOUBLE -- Check to see if a real number is actually double precision
#
# B.Simon	15-Dec-94	First Code

int procedure is_double (token)

char	token[ARB]	# i: token containing number
#--
int	ic, ndigit

begin
	# Count number of digits in mantissa and look for D exponent

	ndigit = 0
	for (ic = 1; token[ic] != EOS; ic = ic + 1) {
	    if (token[ic] == 'D' || token[ic] == 'd') {
		return (TY_DOUBLE)

	    } else if (token[ic] == 'E' || token[ic] == 'e') { 
		break
	    }

	    if (IS_DIGIT(token[ic]))
		ndigit = ndigit + 1
	}

	# If no D exponent, set the type according to the number of digits

	if (ndigit > NDIGITS_RP) {
	    return (TY_DOUBLE)
	} else {
	    return (TY_REAL)
	}
end
