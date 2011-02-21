# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMIUPK32 -- Unpack a 32 bit signed NMI array into an SPP array of the
# indicated datatype.

procedure nmiupk32 (nmi, spp, nelems, spp_datatype)

int	nmi[ARB]		#I input NMI format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

int	nmi_bytes
int	spp_bytes
int	sizeof()

begin
	nmi_bytes = 32 / NBITS_BYTE
	spp_bytes = sizeof(spp_datatype) * SZB_CHAR

	# for 64bit integer
	if ( 2 * nmi_bytes == spp_bytes )
	    call iupk32 (nmi, spp, nelems)

	call achti (nmi, spp, nelems, spp_datatype)
end
