# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIPKSIZE -- Determine the size in SPP chars of the array required to store
# nelems of type mii_type in MII packed form.  The mii_type codes are defined
# in mii.h; we assume here that the integer codes are the sizes of the MII
# types in bits.

int procedure miipksize (nelems, mii_type)

int	nelems			# number of MII elements of type mii_type
int	mii_type		# <mii.h> type code (=8,16,32)

begin
	return ((nelems * mii_type / NBITS_BYTE + SZB_CHAR-1) / SZB_CHAR)
end
