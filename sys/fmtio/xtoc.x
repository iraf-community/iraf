# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# XTOC -- Encode a complex number as a character string in a field width of
# at most WIDTH characters.

int procedure xtoc (xval, outstr, maxch, decpl, fmt, width)

complex	xval			# value to be formatted
char	outstr[ARB]		# output string
int	fmt			# format encoding (f,e,etc.)
int	maxch			# max chars out
int	decpl			# precision
int	width			# field width

int	op, dtoc()
double	real_part, imag_part
define	output {outstr[op]=$1;op=op+1;if(op>maxch)goto overflow_}
define	overflow_ 91

begin
	if (IS_INDEFX (xval)) {
	    real_part = INDEFD
	    imag_part = INDEFD
	} else {
	    real_part = real (xval)
	    imag_part = aimag (xval)
	}

	op = 1
	output ('(')
	op = op + dtoc (real_part, outstr[op], maxch-op+1, decpl, fmt, width)
	output (',')
	op = op + dtoc (imag_part, outstr[op], maxch-op+1, decpl, fmt, width)
	output (')')

overflow_
	outstr[op] = EOS
	return (op-1)
end
