# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<nspp.h>

# PACKUM -- Pack an integer array containing 16 bit quantities into a buffer.
# Each 16 bit input datum occupies one integer; the input integers may be
# any size.  This implementation will work on most byte oriented machines,
# but will generate a fatal error on machines with 24, 60, etc. bit words.

procedure packum (a, npix, bp)

int	a[ARB]			# input array, one 16-bit datum per word
int	npix			# number of mc words
int	bp			# LOC pointer to output buffer

int	offset, dummy[1]
int	loci()
include	"nspp.com"

begin
	offset = bp - loci (dummy) + 1

	# It is necessary to swap the order of the metacode words on some
	# machines.  Npix is always an even number.  The swapping must be
	# done here because the NSPP and MCTR code assumes that the bytes
	# are ordered in a certain manner (most significant first).  Thus,
	# when the buffer is flushed FLUSHB will set the magic bits, and
	# if we wait and swap upon output rather than here, it will set the
	# bits in the wrong word.

	if (mbswap == YES)		# flag set from graphcap in nsppkern
	    call mcswap (a, npix)

	switch (NBITS_MCWORD) {
	case NBITS_SHORT:
	    call achtis (a, dummy[offset], npix)
	case NBITS_INT:
	    call amovi (a, dummy[offset], npix)
	default:
	    call fatal (1, "gio.ncar.packum: cannot pack metacode")
	}
end
