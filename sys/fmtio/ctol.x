# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# CTOL -- Simple character to long integer (decimal radix).

int procedure ctol (str, ip, lval)

char	str[ARB]	# decimal encoded numeric string
int	ip		# starting index in string (input/output)
long	lval		# decoded integer value (output)

bool	neg
long	sum
int	ip_start
int	strncmp()

begin
	while (IS_WHITE (str[ip]))
	    ip = ip + 1
	ip_start = ip

	# Check for "INDEF".
	if (str[ip] == 'I')
	    if (strncmp (str[ip], "INDEF", 5) == 0)
		if (!IS_ALNUM (str[ip+5])) {
		    lval = INDEFL
		    ip = ip + 5
		    return (5)
		}

	neg = false
	if (IS_DIGIT (str[ip+1]))
	    if (str[ip] == '-') {
		neg = true
		ip = ip + 1
	    } else if (str[ip] == '+')
		ip = ip + 1

	sum = 0
	while (IS_DIGIT (str[ip])) {
	    sum = sum * 10 + TO_INTEG (str[ip])
	    ip = ip + 1
	}

	if (neg)
	    lval = -sum
	else
	    lval = sum

	return (ip - ip_start)
end
