# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIUPK32 -- Unpack a 32 bit signed MII array into an SPP array of the
# indicated datatype.

procedure miiupk32 (mii, spp, nelems, spp_datatype)

int	mii[ARB]		#I input MII format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

int	mii_bytes
int	spp_bytes
int	sizeof()

begin
	mii_bytes = 32 / NBITS_BYTE
	spp_bytes = sizeof(spp_datatype) * SZB_CHAR

        if ( mii_bytes == spp_bytes ) {
	    if (BYTE_SWAP4 == YES) {
		call bswap4 (mii, 1, spp, 1, nelems * (mii_bytes))
		call achtl (spp, spp, nelems, spp_datatype)
	    } else if (BYTE_SWAP2 == YES) {
		call bswap2 (mii, 1, spp, 1, nelems * (mii_bytes))
		call achtl (spp, spp, nelems, spp_datatype)
	    } else
		call achtl (mii, spp, nelems, spp_datatype)

	} else if ( 2 * mii_bytes == spp_bytes ) {
	    call i32to64 (mii, spp, nelems) 		# for 64bit integer
	    if (BYTE_SWAP8 == YES) {
		call bswap8 (spp, 1, spp, 1, nelems * (spp_bytes))
		call achtl (spp, spp, nelems, spp_datatype)
	    } else if (BYTE_SWAP4 == YES) {
		call bswap4 (spp, 1, spp, 1, nelems * (spp_bytes))
		call achtl (spp, spp, nelems, spp_datatype)
	    } else if (BYTE_SWAP2 == YES) {
		call bswap2 (spp, 1, spp, 1, nelems * (spp_bytes))
		call achtl (spp, spp, nelems, spp_datatype)
	    } else
		call achtl (spp, spp, nelems, spp_datatype)

	} else {
	    call eprintf("[ERROR] miiupk32.x: unexpected integer size\n")
	}
end
