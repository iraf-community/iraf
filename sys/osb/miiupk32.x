# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIUPK32 -- Unpack a 32 bit signed MII array into an SPP array of the
# indicated datatype.

procedure miiupk32 (mii, spp, nelems, spp_datatype)

int	mii[ARB]		#I input MII format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	if (BYTE_SWAP4 == YES) {
	    call bswap4 (mii, 1, spp, 1, nelems * (32 / NBITS_BYTE))
	    call achtl (spp, spp, nelems, spp_datatype)
	} else if (BYTE_SWAP2 == YES) {
	    call bswap2 (mii, 1, spp, 1, nelems * (32 / NBITS_BYTE))
	    call achtl (spp, spp, nelems, spp_datatype)
	} else
	    call achtl (mii, spp, nelems, spp_datatype)
end
