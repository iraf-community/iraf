# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# LOCI -- Return the zero-indexed offset of the argument in the user address
# space, in integer units.  In other words, if A is an integer array,
# { loci(a[2]) - loci(a[1]) } is exactly one.
#
# NOTE -- The original NSPP (portlib) code called this function LOC, however,
# the Sun-4 Fortran compiler has an intrinsic function of the same name which
# behaves slightly differently, hence the name was changed to LOCI.

int procedure loci (x)

int	x
int	xaddr

begin
	# ZLOCVA returns the address of the variable in units of XCHAR.

	call zlocva (x, xaddr)
	return (xaddr / SZ_INT)
end
