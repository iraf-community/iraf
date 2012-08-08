# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# CTOI -- Simple character to integer (decimal radix).

int procedure ctoi (str, ip, ival)

char	str[ARB]	# decimal encoded numeric string
int	ip		# starting index in string (input/output)
int	ival		# decoded integer value (output)

bool	neg
int	sum
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
		    ival = INDEFI
		    ip = ip + 5
		    return (5)
		}

	neg = (str[ip] == '-')
	if (neg)
	    ip = ip + 1

	sum = 0
	while (IS_DIGIT (str[ip])) {
	    sum = sum * 10 + TO_INTEG (str[ip])
	    ip = ip + 1
	}

	if (neg)
	    ival = -sum
	else
	    ival = sum

	return (ip - ip_start)
end
