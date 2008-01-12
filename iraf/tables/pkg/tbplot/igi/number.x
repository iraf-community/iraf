procedure number (var, n, label, notation, iascii, trailz, fmt)

# Converts the dp floating point number VAR to a string of characters.
# NOTATION = 0 for decimal, 1 for exponential notation
# IASCII = 1 for 1E-7, 0 for superscripts, etc with escapes

## 28 May 1992  Remove extra zero drawn between ones place and decimal
## for exponentials.
## Remove test for exponential outside range.
## Extract procedure to separate file.  ZGL
## 1 June 1992  Fix bugs trashing numbers between 1 and 10 and exponent
## when mantissa=1.  ZGL


double	var		# Data value
int	n		# Number of characters in label
char	label[ARB]	# Output formatted number
int	notation	# Exponential?
int	iascii		# Superscripts?
int	trailz		# Trailing zero?
char	fmt[ARB]	# SPP label format

double	value
string	letter "0123456789-+.E"
bool	wrote_decimal
int	ndecimal
int	maxdigit
data	maxdigit /10/
int	nsign
double	expo
int	iexp, iexp1, iexp2, nexp
double	amant
int	iexit
int	j

int	strlen()

begin
	if (fmt[1] != EOS) {
	    # If the user has supplied the format, all we need to do is write
	    # the string to be plotted in that format with an internal write 

	    call sprintf (label, SZ_LINE, fmt)
		call pargd (var)

	    n = strlen (label)
	    return
	}

        wrote_decimal = false
        ndecimal = 0
	n = 0
	label[1] = EOS
	value = var
	nsign = 1

	# Check for negative or zero number
	if (value == 0) {
	    # Write a zero only
	    n = n + 1
	    label[n] = letter[1]

	    if (trailz == YES) {
		# Write a decimal point
		n = n + 1
		label[n] = '.'

		# Write a trailing zero
		n = n + 1
		label [n] = '0'
	    }

	    label[n+1] = EOS
	    return

	} else if (value < 0) {
	    # Start out with a minus
	    n = n + 1
	    label[n] = '-'
	    label[n+1] = EOS
	    nsign = n
	    value = -value
	}

	# Pull off the exponent and get the mantissa
	expo = log10 (value)
	iexp = int (abs (expo) + 1d-5)

	if (expo < -1d-5) 
	    iexp = -iexp - 1

	amant = value * 10.d0 ** (-iexp) + 1d-10

	if (amant >= 10d0) {
	    amant = amant / 10d0
	    iexp  = iexp + 1
	}

	# Pull off the various digits, decrementing NEXP
	nexp = iexp

#	if (abs (iexp) > 4 || notation == 1) 
	if (notation == 1) 
	    nexp = 0

	# For numbers to be written as .0xxx, change the exponent to -1
#	if (iexp < -1 && iexp >= -4 && notation == 0) {
	if (iexp < -1 && notation == 0) {
	    amant = amant * 10d0**(iexp + 1)
	    nexp = -1
	}
        
	iexit = 0

	do j = 1, 10 {
	    # This controls how many decimal digits are plotted
	    if (ndecimal == maxdigit)
		break

	    if (amant <= 1d-4) 
		# There is very little left
		iexit = 1

	    if (iexit != 0 && nexp < 0 && maxdigit == 10) 
		break

	    if (nexp == -1) {
		if (value < 1 && notation == 0) {
		    # Leading 0 for values < 1
		    n = n + 1
		    label[n] = '0'
		}

		# Write a decimal point
		n = n + 1
		label[n] = '.'
	        label[n+1] = EOS
		wrote_decimal = true
	    }

	    n = n + 1
	    if (wrote_decimal) 
		ndecimal = ndecimal + 1

	    label[n] = letter[int (amant) + 1]
	    label[n+1] = EOS
	    amant = 10 * (amant - int(amant))
	    nexp = nexp - 1
	}

	if (wrote_decimal) {
	    if (ndecimal == 0) {
		# Write a trailing zero
		n = n + 1
		label [n] = '0'
		label[n+1] = EOS
	    }

	} else {
	    if (trailz == YES) {
		# Write a decimal point
		n = n + 1
		label[n] = '.'

		# Write a trailing zero
		n = n + 1
		label [n] = '0'
		label[n+1] = EOS
	    }
	}

#	if ((abs (iexp) > 4 && notation == 0) || notation == 1) {
	if (notation == 1) {
	    # Get exponent
	    if (iascii == 1) {
	        n = n + 1
	        label[n] = 'E'
		n = n + 1
	        label[n] = ' '
	        label[n+1] = EOS
	    } else {
	        if (n > 1 || label[1] != '1') {
	            call strcat ("\\G*10\\\\U ", label, 40)
	            n = n + 9
	        } else {
	            call strcpy ("10\\\\U ", label, 40)
	            n = 6
	        }
	    }

	    # If the exponent is < 0 or >= 10 we don't want to
	    # put a space between the exponent and the
	    # mantissa, since this will make the numbers not
	    # line up 

	    if (iexp < 0) {
	    	label[n] = '-'
	        label[n+1] = EOS
	    } else if (iexp > 9)
		n = n - 1

	    iexp1 = abs (iexp) / 10
	    iexp2 = abs (iexp) - iexp1 * 10

	    if (iexp1 != 0) {
	    	n = n + 1
	    	label[n] = letter[iexp1+1]
	        label[n+1] = EOS
	    }

	    n = n + 1
	    label[n] = letter[iexp2+1]
	    label[n+1] = EOS
	}

	label[n+1] = EOS
end
