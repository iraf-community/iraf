# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMIPKSIZE -- Determine the size in SPP chars of the array required to store
# nelems of type nmi_type in NMI packed form.  The nmi_type codes are defined
# in nmi.h; we assume here that the integer codes are the sizes of the NMI
# types in bits.

int procedure nmipksize (nelems, nmi_type)

int	nelems			#I number of NMI elements of type nmi_type
int	nmi_type		#I <nmi.h> type code (=8,16,32,-32,-64)

begin
	return ((nelems * abs(nmi_type) / NBITS_BYTE + SZB_CHAR-1) / SZB_CHAR)
end
