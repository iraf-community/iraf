# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# BITMOV -- Move a sequence of bits in a bit array of arbitrary length.

procedure bitmov (a, a_off, b, b_off, nbits)

int	a[ARB]		# input bit array
int	a_off		# first bit to be moved
int	b[ARB]		# output bit array
int	b_off		# first bit to be written
int	nbits		# number of bits to be moved

int	ip, op, ip_top, nbits_left
int	bitupk()

begin
	ip_top = a_off + nbits - NBITS_INT
	op = b_off

	for (ip = a_off;  ip <= ip_top;  ip = ip + NBITS_INT) {
	    call bitpak (bitupk(a,ip,NBITS_INT), b, op, NBITS_INT)
	    op = op + NBITS_INT
	}

	nbits_left = (a_off + nbits) - ip
	if (nbits_left > 0)
	    call bitpak (bitupk(a,ip,nbits_left), b, op, nbits_left)
end
