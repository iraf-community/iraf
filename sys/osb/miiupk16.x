# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIUPK16 -- Unpack a 16 bit signed MII array into an SPP array of the
# indicated datatype.

procedure miiupk16 (mii, spp, nelems, spp_datatype)

int	mii[ARB]		#I input MII format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	if (BYTE_SWAP2 == YES) {
	    call bswap2 (mii, 1, spp, 1, nelems * (16 / NBITS_BYTE))    
	    call achts (spp, spp, nelems, spp_datatype)
	} else
	    call achts (mii, spp, nelems, spp_datatype)
end
