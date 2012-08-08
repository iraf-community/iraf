include <ctype.h>
include	<lexnum.h>

# B.Simon	16-Apr-99	first code

# IS_NUMBER -- Test string to see if it represents a number

bool procedure is_number (str)

char	str[ARB]	# i: String to be tested
#--
int	ic, nc, type
int	lexnum()

begin
	# Use lexnum to determine string type

	ic = 1
	type = lexnum (str, ic, nc)

	# Any non-white characters after the number 
	# indicate this is not a number

	ic = ic + nc
	while (str[ic] != EOS) {
	    if (! IS_WHITE(str[ic]))
		return (false)

	    ic = ic + 1
	}

	# Test for numeric types and return result of test

	return (type == LEX_OCTAL || type == LEX_DECIMAL || type == LEX_REAL)
end
