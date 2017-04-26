# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>

# ITOC -- Integer to character string.  We do not resolve this into a call
# to GLTOC for reasons of efficiency.

int procedure itoc (ival, str, maxch)

int	ival, maxch
char	str[ARB]

char	buf[MAX_DIGITS]
int	b_op, s_op, num, temp
int	gstrcpy()

begin	
	s_op = 1

	if (IS_INDEFI (ival)) {
	    return (gstrcpy ("INDEF", str, maxch))
	} else if (ival < 0) {
	    str[1] = '-'
	    s_op = 2
	    num = -ival
	} else
	    num = ival

	# Encode nonnegative number in BUF, least significant digits first.

	b_op = 0
	repeat {
	    temp = num / 10
	    b_op = b_op + 1
	    buf[b_op] = TO_DIGIT (num - temp * 10)
	    num = temp
	} until (num == 0)

	# Copy encoded number to output string, reversing the order of the
	# digits so that the most significant digits are first.

	while (b_op > 0) {
	    if (s_op > maxch)
		return (gstrcpy ("**********", str, maxch))
	    str[s_op] = buf[b_op]
	    s_op = s_op + 1
	    b_op = b_op - 1
	}

	str[s_op] = EOS
	return (s_op - 1)
end
