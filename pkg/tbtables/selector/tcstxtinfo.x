include "tcs.h"

# TCS_TXTINFO -- Get text information about a column

procedure tcs_txtinfo (descrip, what, str, maxch)

pointer	descrip		# i: column selector
int	what		# i: parameter
char	str[ARB]	# o: text information
int	maxch		# i: length of string
#--

begin
	call tbcigt (TCS_COLUMN(descrip), what, str, maxch)
end
