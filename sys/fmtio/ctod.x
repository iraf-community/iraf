# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <ctype.h>

define	DECIMAL		10

.help
.nf _________________________________________________________________________
Attempt to convert a string to a number:  nchar = ctod (str, ip, dval)
The index IP must be set to the first character to be scanned upon entry
to CTOD, and will be left pointing at the first untranslated character.

If the string is successfully converted, the number of characters used
is returned as the function argument.  If the string (or the first few
characters of the string) cannot be interpreted as a number, zero will be
returned.  Note that even if no numeric characters are encountered, the
index IP may be incremented, if leading whitespace is encountered (but the
return value N will still be zero).

The upper case string "INDEF" is a legal real number, as is "." (. == 0.0).
Sexagesimal numbers are permitted.  Excess digits of precision are ignored.
Out of range exponents are detected, and result in the value INDEF being
returned (this is not considered an ERROR condition).  Any number with an
exponent greater than or equal to MAX_EXPONENT is interpreted as INDEF,
regardless of the mantissa.  The number need not contain a decimal point.

Lexical form of a sexagesimal number:

	D :==	[0-9]		numeric digit
	E :==	[eEdD]		exponent symbol

	({D}*:)+{D}*(".")?{D}*({E}("+"|"-")?{D}+)?

The format for sexagesimal numbers is fairly permissive.  Any number of
colon fields are permitted, with any number of digits (including zero) in
each field.  An exponent may occur at the end of a sexagesimal number.
Leading zeros may be omitted in the fields.
.endhelp ____________________________________________________________________


# CTOD -- Convert a string to double precision real.

int procedure ctod (str, ip, dval)

char	str[ARB]		# string to be converted
int	ip			# pointer into str
double	dval			# receives binary value

bool	neg
char	dig[MAX_DIGITS]
int	j, e, vexp, ip_start
long	expon
double	value, scalar
int	strncmp(), gctol(), stridx()

begin
	while (IS_WHITE (str[ip]))			# skip whitespace
	    ip = ip + 1
	ip_start = ip
	dval = INDEFD
				
	if (strncmp (str[ip], "INDEF", 5) == 0) {	# check for "INDEF"
	    for (ip=ip+5;  IS_ALPHA (str[ip]) || str[ip] == '_';  ip=ip+1)
		;
	    return (ip - ip_start)
	}

	neg = (str[ip] == '-')				# check for sign
	if (neg || str[ip] == '+')
	    ip = ip + 1

	while (str[ip] == '0')				# ignore leading zeros
	    ip = ip + 1

	dval = 0.0
	scalar = 60.0

	repeat {					# accumulate digits
	    for (j=1;  j <= MAX_DIGITS && IS_DIGIT(str[ip]);  j=j+1) {
		dig[j] = str[ip]
		ip = ip + 1
	    }

	    for (e=0;  IS_DIGIT(str[ip]);  e=e+1)	# ignore the rest
		ip = ip + 1

	    scalar = scalar / 60.0
	    if (ip > 1 && stridx(str[ip], "'\":dDhHmMsS")>0) {  # sexagesimal?
		ip = ip + 1
		dig[j] = EOS
		value = 0.0				# convert digits
		for (j=1;  dig[j] != EOS;  j=j+1)
		    value = value * 10.0D0 + TO_INTEG (dig[j])
		dval = dval + value * scalar * (10.0 ** e)

		while (str[ip] != EOS && 		# multiple spaces etc
		    stridx(str[ip]," '\":dDhHmMsS")>0)
		        ip = ip + 1
	    } else
		break
	}

	if (str[ip] == '.') {				# check for a fraction
	    ip = ip + 1
	    if (j == 1)					# skip leading zeros
		while (str[ip] == '0') {		# if str = "0.00ddd"
		    ip = ip + 1
		    e = e - 1
		}
	    for (;  j <= MAX_DIGITS && IS_DIGIT(str[ip]);  j=j+1) {
		dig[j] = str[ip]
		e = e - 1				# adjust scale factor
		ip = ip + 1
	    }						# discard insignificant
	    while (IS_DIGIT (str[ip]))			# fractional digits
		ip = ip + 1
	}

	dig[j] = EOS					# no more digits
	vexp = e + j - 1				# save for ovfl check
	if (ip == ip_start)				# not a number?
	    return (0)

	value = 0.0					# convert the mantissa
	for (j=1;  dig[j] != EOS;  j=j+1)
	    value = value * 10.0D0 + TO_INTEG (dig[j])
	if (e != 0)
	    value = value * (10.0D0 ** e)		# scale by e


	# Check for exponent.

	j = ip
	expon = 0
	if (stridx (str[ip], "eEdD") > 0) {		# exponent?
	    ip = ip + 1
	    if (gctol (str, ip, expon, DECIMAL) <= 0) {
		ip = j					# return chars
		expon = 0
	    }
	}

	if (abs(vexp+expon) > MAX_EXPONENTD)		# check for overflow
	    return (ip - ip_start)

	dval = dval + value * scalar
	if (expon != 0)
	    dval = dval * (10.0D0 ** expon)		# apply exponent

	if (neg)
	    dval = -dval
	return (ip - ip_start)
end
