# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIINELEM -- Determine the number of MII elements of the given datatype
# which can be stored in an SPP char array of the indicated length.
# The mii_type codes are defined in mii.h; we assume here that the codes
# used are the number of bits in each MII type.

int procedure miinelem (nchars, mii_type)

int	nchars			# size in chars of packed array
int	mii_type

begin
	return ((nchars * SZB_CHAR * NBITS_BYTE + mii_type-1) / mii_type)
end
