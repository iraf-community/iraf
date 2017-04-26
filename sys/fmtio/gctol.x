# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

define	OCTAL		8
define	DECIMAL		10
define	HEX		16

.help gctol
.nf _______________________________________________________________________
GCTOL -- Convert string to long integer (any radix).  The long integer
value is returned in LVAL, and the pointer IP is left pointing at the
first character following the number.  IP must be set to the index of
the character at which conversion is to start before calling GCTOL.

If the conversion radix is octal (hex), and the number is immediately
followed by the suffix "b|B" ("x|X"), IP will be advanced past the suffix
character, which is considered to be part of the number.
.endhelp __________________________________________________________________


int procedure gctol (str, ip, lval, radix)

char	str[ARB]		# string to be decoded
int	ip			# pointer within string
int	radix			# radix of number
long	lval			# output variable

int	digit, base, ip_save, first_char
char	ch
bool	neg

begin
	while (IS_WHITE (str[ip]))
	    ip = ip + 1
	ip_save = ip
	
	neg = (str[ip] == '-')
	if (neg || str[ip] == '+')		# eat the +/-
	    ip = ip + 1

	first_char = ip
	base = abs (radix)

	# The first character (following than the sign character) must be
	# a digit, regardless of the radix.

	for (lval=0;  str[ip] != EOS;  ip=ip+1) {
	    ch = str[ip]

	    if (IS_DIGIT (ch))			# cvt char to binary
		digit = TO_INTEG (ch)
	    else if (base > DECIMAL) {
		if (IS_UPPER (ch))
		    ch = TO_LOWER (ch)
		else if (! IS_LOWER (ch))
		    break
		digit = ch - 'a' + 10		# for radices > 10
	    } else
		break

	    if (digit < 0 || digit >= base)
		break
	    lval = lval * base + digit
	}
	
 	if (neg)
	    lval = -lval

	if (ip == first_char)			# not a number ?
	    ip = ip_save			# restore pointer
	else if (radix == OCTAL && ch == 'b' || ch == 'B')
	    ip = ip + 1
	else if (radix == HEX && ch == 'x' || ch == 'X')
	    ip = ip + 1				# eat suffix char

	return (ip - first_char)
end
