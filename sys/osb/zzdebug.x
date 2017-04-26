# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

task	sbit, tbit

define	NWORDS		1	# limited to 1 longword at present


# SBIT, TBIT -- Test the bitpak and bitupk primitives.

procedure sbit()

int	b[NWORDS]
int	offset, nbits, value, i
int	bitupk(), clgeti()

begin
	offset = clgeti ("offset")
	nbits  = clgeti ("nbits")
	value  = clgeti ("value")

	if (offset < 1 || offset > NWORDS * NBITS_INT)
	    call error (1, "bit offset out of range")

	call bitpak (value, b, offset, nbits)

	call printf ("\n")
	call printf ("\t21098765432109876543210987654321\n")
	call printf ("\t  3         2         1        0\n")
	do i = 1, NWORDS {
	    call printf ("%4d\t%032r2  (%011oB)\n")
		call pargi ((i-1) * 32 + 1)
		call pargi (b[i])
		call pargi (b[i])
	}
	return

entry	tbit()
	offset = clgeti ("offset")
	nbits  = clgeti ("nbits")

	call printf ("bitfield=%d\n")
	    call pargi (bitupk (b, offset, nbits))
end
