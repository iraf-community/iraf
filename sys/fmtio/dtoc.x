# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<printf.h>

# DTOC -- Format and output a floating point number, in any of the formats
# F,E,G,H, or M (H and M are hours-minutes-seconds and minutes-seconds formats,
# respectively).

int procedure dtoc (dval, outstr, maxch, decpl, a_fmt, width)

double	dval			# number to be output
char	outstr[ARB]		# output string
int	maxch			# size of the output string
int	decpl			# number of decimal places or precision
int	a_fmt, fmt		# format type (feghm)
int	width			# field width

int	op
double	val
long	lval
int	dtoc3(), ltoc(), gstrcpy()
define	output {outstr[op]=$1;op=op+1;if(op>maxch)goto retry_}
define	retry_ 91

begin
	# If HMS format is not desired, simply call DTOC3.  Control also
	# returns to this point in the event of overflow.

	fmt = a_fmt
	if (IS_UPPER (fmt))
	    fmt = TO_LOWER (fmt)

	if (fmt == FMT_FIXED || fmt == FMT_EXPON || fmt == FMT_GENERAL ||
	    IS_INDEFD(dval)) {
retry_	
	    return (dtoc3 (dval, outstr, maxch, decpl, fmt, width))
	}

	# HMS format is implemented using calls to DTOC3, LTOC.  Use zero
	# fill to get two chars for the second and third fields, if necessary.
	# The second field is omitted for "m" format.  No whitespace is
	# permitted in an HMS (or other) number.

	if (dval < 0.0D0 && long (dval) == 0)
	    op = gstrcpy ("-0", outstr, maxch) + 1
	else
	    op = ltoc (long(dval), outstr, maxch) + 1
	output (':')						# "+/-nnn:..."

	val = abs (dval)
	val = val - long (val)					# abs fraction

	if (fmt == FMT_HMS) {					# "...nn:..."
	    val = val * 60.0D0
	    lval = long (val)
	    if (lval < 10)
		output ('0')
	    op = op + ltoc (lval, outstr[op], maxch-op+1)
	    output (':')
	    val = val - lval
	}

	val = val * 60.0D0					# "...nn.nnn"
	lval = long (val)
	if (lval < 10)
	    output ('0')

	if (decpl <= 0)						# no decimal?
	    op = op + ltoc (lval, outstr[op], maxch-op+1)
	else
	    op = op + dtoc3 (val, outstr[op], maxch-op+1, decpl, FMT_FIXED, ARB)

	# If the HMS format does not fit, go try a more compact format.
	if (op-1 > abs(width) || op > maxch) {
	    fmt = FMT_GENERAL
	    goto retry_
	}

	return (op-1)
end
