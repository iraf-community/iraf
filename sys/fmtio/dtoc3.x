# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <ctype.h>
include <printf.h>

.help dtoc3
.nf ___________________________________________________________________________
This routine is based on the TOOLS routine of the same name by J. Chong.
The major changes are translation to the IRAF pp language, implementation
of the "G" format, and some restructuring to avoid duplicated code.
This procedure is called by the more general routine DTOC, which adds the
additional format type "H" (sexageximal, or hms format).

Formats:	f	Fixed format.  D == number of decimal places.  If D
			is -1 (or less), the "." will not be printed.  If the
			number being printed is too large to fit in F format,
			"E" format will be used instead.  Nonsignificant
			digits are returned as zeros.

		e	Exponential format.  D == number of significant digits.

		g	General format.  D == number of significant digits.
			The actual format used may be either F or E, depending
			on which is smaller.  If D is negative, the "." will
			be omitted if possible (abs(D) remains the precision).

If the field width is too small to convert the number, the field will be
filled with asterisks instead.  If the number being converted is INDEF, the
string "INDEF" will be returned.  The number is rounded to the desired precision
before being printed.
.endhelp ______________________________________________________________________

# DTOC3 --- Convert double precision real to string.

int procedure dtoc3 (val, out, maxch, decpl, a_fmt, width)

double	val			# value to be encoded
char	out[ARB]		# output string
int	maxch			# max chars out
int	decpl			# precision
int	a_fmt			# type of encoding ('f', 'e', etc.)
int	width			# field width

double	v
char	digits[MAX_DIGITS]
bool	neg, small, exp_format, squeeze
int	i, w, d, e, j, len, no_digits, max_size, e_size, f_size, fmt
int	itoc(), gstrcpy()

begin
	# Set flags indicating whether the number is greater or less that zero,
	# and whether its absolute value is greater or less than 1.

	v = abs (val)
	w = abs (width)

	fmt = a_fmt
	if (IS_UPPER(a_fmt))
	    fmt = TO_LOWER(a_fmt)
	squeeze = (fmt == FMT_GENERAL)
	neg = (val < 0.0)
	small = (v < 0.1)

	if (squeeze)
	    d = abs (decpl)
	else
	    d = max (0, decpl)

	if (IS_INDEFD (val))			# INDEF is a special case
	    return (gstrcpy ("INDEF", out, w))

	# Scale number to 0.1 <= v < 1.0
	call dtcscl (v, e, 1)

	# Start tally for the maximum size of the number to determine if an
	# error should be returned.
	if (neg)				# 1 for neg, plus 1 for .
	    max_size = 2
	else
	    max_size = 1
	no_digits = min (MAX_DIGITS, d)


	# Determine exact format for printing.

	len = abs (e)				# base size of E format
	e_size = 1
	for (i=10;  i <= 10000;  i=i*10) {
	    if (len < i)
		break
	    e_size = e_size + 1
	}
	e_size = e_size + max_size + 1
	if (e < 0)
	    e_size = e_size + 1			# allow space for leading '0'

	if (squeeze) {				# G-format: find best format
	    e_size = e_size + d
	    if (e > 0)
	        f_size = max (d, e + 1)
	    else
		f_size = d - e
	    f_size = f_size + max_size
	} else if (fmt == FMT_FIXED)
	    f_size = max (e, 1) + max_size + d


	if (squeeze) {				# 'G' format
	    if (f_size <= e_size) {
		exp_format = false
		if (e > 0)
		    no_digits = f_size - max_size
	        max_size = f_size
	    } else {
		exp_format = true
		max_size = e_size
	    }
	    d = w				# deactivate dec-places count

	} else if (fmt == FMT_FIXED) {		# Fortran 'F' format
	    exp_format = f_size > w

	    if (exp_format) {			# is there too little space?
		no_digits = max (1, w - e_size)
		max_size = no_digits + e_size
	    } else {
		no_digits = e + d + 1		# negative e is OK here
		max_size = f_size
	    }

	} else { 				# Fortran 'E' format
	    exp_format = true
	    max_size = e_size + d
	    d = w
	}

	# Round the number at digit (no_digits + 1).
	if (no_digits >= 0)
	    v = v + 0.5 * 10. ** (-no_digits)

	# Be sure the number of digits is in range.
	no_digits = max(1, min(MAX_DIGITS, no_digits))

	# Handle the unusual situation of rounding from .999..  up to 1.000.
	if (v >= 1.0) {
	    v = v / 10.0
	    e = e + 1
	    if (!exp_format) {
		max_size = max_size + 1
		no_digits = min (MAX_DIGITS, no_digits + 1)
	    }
	}

	# See if the number will fit in 'w' characters
	if (max_size > w) {
	    for (i=1;  i <= w;  i=i+1)
		out[i] = OVFL_CHAR
	    out[i] = EOS
	    return (w)
	}

	# Extract the first <no_digits> digits.  At the start V is normalized
	# to a value less than 1.0.  The algorithm is to multiply by ten and
	# take the integer part to get each digit.  

	do i = 1, no_digits {
	    v = v * 10.0d0
	    j = int (v + EPSILOND)		# truncate to integer
	    v = v - j				# lop off the integral part

	    # Make sure the next iteration will produce a decimal J in the
	    # range 1-9.  On some systems, due to precision problems J=int(V)
	    # can be off by one compared to V-J and this will result in a J
	    # of 10 in the next iteration.  The expression below attempts to
	    # look ahead for J>=10 and adjusts the J for the current iteration
	    # up by one if this will occur.

	    if (int (v * 10.0d0 + EPSILOND) >= 10) {
		j = j + 1
		v = v - 1
		if (v < 0)
		    v = 0
	    }

	    digits[i] = TO_DIGIT(j)
	}

	# Take digit string and exponent and arrange into desired format.
	len = 1
	if (neg) {
	    out[1] = '-'
	    len = len + 1
	}

	if (exp_format) {			# set up exponential format
	    out[len] = digits[1]
	    out[len+1] = '.'
	    len = len + 2
	    for (i=2;  i <= no_digits;  i=i+1) {
	        out[len] = digits[i]
	        len = len + 1
	    }
	    out[len] = 'E'
	    len = len + 1
	    if (e < 0) {
		out[len] = '-'
		len = len + 1
		e = -e
	    }
	    len = len + itoc (e, out[len], w - len + 1)

	} else if (e >= no_digits) {
	    # Handle numbers >= 1 with dp after figures.
	    for (i=1;  i <= no_digits;  i=i+1) {
		out[len] = digits[i]
		len = len + 1
	    }
    	    for (i=no_digits;  i <= e;  i=i+1) {
		out[len] = '0'
		len = len + 1
	    }
	    if (decpl > 0) {
	        out[len] = '.'
	        len = len + 1
	        if (!squeeze) {
		    for (i=1;  i <= d;  i=i+1) {
		        out[len] = '0'
		        len = len + 1
		    }
	        }
	    }

	} else {
	    if (e < 0) {
		# Handle fixed numbers < 1.
	        if (d == 0 && e == -1 && digits[1] >= '5') 
		    out[len] = '1'
	        else
		    out[len] = '0'
	        out[len + 1] = '.'
	        len = len + 2
	        for (i=1;  i < -e && d > 0;  i=i+1) {
		    out[len] = '0'
		    len = len + 1
		    d = d - 1
	        }
		i = 1
	    } else {
		# Handle numbers > 1 with dp inside figures.
	        e = e + 2				# one more zero below
	        for (i=1;  i < e;  i=i+1) {
		    out[len] = digits[i]
		    len = len + 1
	        }
		if (decpl > 0) {
	            out[len] = '.'
	            len = len + 1
		}
	    }

	    for (j=1;  i <= no_digits && j <= d;  j=j+1) {
		out[len] = digits[i]
		i = i + 1
		len = len + 1
	    }
	    if (squeeze) {
	        while (len > 2) {			# skip trailing zeroes
		    len = len - 1
		    if (out[len] != '0') {
		        len = len + 1			# non-digit -- keep it
		        break
		    }
		}
	    } else {
		for (i=1;  i < d+e-no_digits && i <= d;  i=i+1) {
		    out[len] = '0'
		    len = len + 1
		}
	    }
	}

	out[len] = EOS
	return (len - 1)
end
