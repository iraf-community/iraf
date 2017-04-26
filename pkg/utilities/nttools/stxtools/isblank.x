include	<ctype.h>

# ISBLANK -- Return true if the string is entirely white space
#
# B.Simon	11-Nov-87	First Code

bool procedure isblank (str)

char	str[ARB]	# i: string to be tested
int	ip

begin
	do ip = 1, ARB
	    if (str[ip] == EOS)
		return (true)
	    else if (! IS_WHITE(str[ip]) )
		return (false)
end
