# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# ISHIFT -- integer shift.  To be used for calls to ISHIFT in NCAR routines.

int procedure ishift (in_word, n)

int	in_word, n
int	new_word, bit, index, i
int	bitupk(), imod()

begin
	if (n > NBITS_INT)
	    call error (0, "n > NBITS_INT in ishift")
	if (n < 0)
	    # Right end-off shift
	    new_word = bitupk (in_word, iabs(n) + 1, NBITS_INT - iabs(n))
	else {
	    # Left circular shift (rotate)
	    do i = 1, NBITS_INT {
		index = n + i
		if (index > NBITS_INT)
		    index = imod ((n + i), NBITS_INT)
		bit = bitupk (in_word, i, 1)
		call bitpak (bit, new_word, index, 1)
	    }
	}

	return (new_word)
end


# IAND -- AND two integers.

int procedure iand (a, b)

int	a, b
int	and()

begin
	return (and (a, b))
end


# IOR -- OR two integers.  

int procedure ior (a, b)

int	a, b
int	or()

begin
	return (or (a, b))
end
