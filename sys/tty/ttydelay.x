# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "tty.h"

# TTYDELAY -- Output a sequence of pad characters to create a delay, giving
# the terminal time to complete some operation before being passed the next
# request.

procedure ttydelay (fd, tty, delay)

int	fd			# output file
pointer	tty			# tty descriptor
int	delay			# desired milliseconds of delay

int	padchar, npadchars
real	msec_per_char
int	and()

begin
	# Add padding if needed to generate delay.  (8 = nbits per char,
	# baud is in units of bits per second).

	if (delay > 0 && T_BAUD(tty) > 0) {
	    padchar = and (T_PADCHAR(tty), 177B)
	    msec_per_char = real(8 * 1000) / real(T_BAUD(tty)) 
	    npadchars = int (delay / msec_per_char + 0.5)

	    for (;  npadchars > 0;  npadchars = npadchars - 1)
		call putci (fd, padchar)
	}
end
