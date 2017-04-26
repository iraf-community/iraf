# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"glabax.h"

# GLB_ENCODE -- Encode a floating point number as a character string for a
# tick label.  We have to be careful how we do this, since on the one hand
# we want the most concise label possible (e.g., 500 not 500.00) but on the
# other we must provide enough precision to discriminate between ticks that
# are close together (e.g., 500.02 and 500.04).  The extra information is
# given by the "ndigits" argument, which was calculated knowing the range
# and step at setup time.

procedure glb_encode (x, out, maxch, format, step)

real	x			# number to be encoded
char	out[ARB]		# output string
int	maxch			# max chars out
char	format[ARB]		# sprintf format
real	step			# tick spacing

int	ip, op
real	nicex
define	trim_ 91

begin
	# Test for the zero tick, to avoid tick labels that look like the
	# machine epsilon.

	if (abs (x / step) < TOL)
	    nicex = 0
	else
	    nicex = x

	# Encode number.
	call sprintf (out, maxch, format)
	    call pargr (nicex)

	# Lop off any insignificant trailing zeros or periods.  Watch out for
	# trailing zeros in exponential format, e.g., "1.0E10".

	for (ip=1;  out[ip] != EOS;  ip=ip+1)
	    if (out[ip] == 'E' || out[ip] == 'D')
		goto trim_

	for (ip=ip-1;  ip > 1 && out[ip] == '0';  ip=ip-1)
	    ;
	if (ip > 1 && out[ip] == '.')
	    ip = ip - 1
	if (ip >= 1)
	    out[ip+1] = EOS

	# Lop off any insignificant leading zeros, but be sure to leave at
	# least one digit.
trim_
	for (op=1;  out[op] == '-' || out[op] == '+';  op=op+1)
	    ;
	for (ip=op;  out[ip] == '0' && out[ip+1] != EOS;  ip=ip+1)
	    ;
	while (out[ip] != EOS) {
	    out[op] = out[ip]
	    op = op + 1
	    ip = ip + 1
	}
	out[op] = EOS
end
