# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMINELEM -- Determine the number of NMI elements of the given datatype
# which can be stored in an SPP char array of the indicated length.
# The nmi_type codes are defined in nmi.h; we assume here that the codes
# used are the number of bits in each NMI type.

int procedure nminelem (nchars, nmi_type)

int	nchars			#I size in chars of packed array
int	nmi_type		#I NMI type of packed data

int	nbits

begin
	nbits = abs (nmi_type)
	return ((nchars * SZB_CHAR * NBITS_BYTE) / nbits)
end
