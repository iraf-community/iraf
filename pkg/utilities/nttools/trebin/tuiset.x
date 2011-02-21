include "trebin.h"

# tuiset -- set interpolation type
# 
#
# P.E. Hodge, 18-Apr-88  Subroutine created

procedure tuiset (func, i_func)

char	func[ARB]		# i: interpolation function
int	i_func			# o: interpolation function code
#--
int	strncmp()

begin
	if (func[1] == 'n')
	    i_func = I_NEAREST
	else if (func[1] == 'l')
	    i_func = I_LINEAR
	else if (strncmp (func, "poly3", 5) == 0)
	    i_func = I_POLY3
	else if (func[1] == 's')
	    i_func = I_SPLINE
	else
	    call error (1, "unknown interpolation function")
end
