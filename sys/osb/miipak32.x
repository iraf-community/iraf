# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIPAK32 -- Pack an SPP array of the indicated datatype into an 32 bit
# signed MII array.

procedure miipak32 (spp, mii, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
int	mii[ARB]		#O output MII format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

int	mii_bytes
int	spp_bytes
int	sizeof()
pointer	tmpp

begin
	call malloc (tmpp, nelems, TY_LONG)

	mii_bytes = 32 / NBITS_BYTE
	spp_bytes = sizeof(spp_datatype) * SZB_CHAR

	switch (spp_datatype) {
	case TY_UBYTE:
	    call achtbl (spp, Meml[tmpp], nelems)
	case TY_USHORT:
	    call achtul (spp, Meml[tmpp], nelems)
	case TY_CHAR:
	    call achtcl (spp, Meml[tmpp], nelems)
	case TY_SHORT:
	    call achtsl (spp, Meml[tmpp], nelems)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtil (spp, Meml[tmpp], nelems)
	case TY_LONG:
	    call achtll (spp, Meml[tmpp], nelems)
	case TY_REAL:
	    call achtrl (spp, Meml[tmpp], nelems)
	case TY_DOUBLE:
	    call achtdl (spp, Meml[tmpp], nelems)
	case TY_COMPLEX:
	    call achtxl (spp, Meml[tmpp], nelems)
	}

        if ( mii_bytes == spp_bytes ) {
	    if (BYTE_SWAP4 == YES)
		call bswap4 (Meml[tmpp], 1, mii, 1, nelems * (mii_bytes))
	    else if (BYTE_SWAP2 == YES)
		call bswap2 (Meml[tmpp], 1, mii, 1, nelems * (mii_bytes))
	}
	else if ( 2 * mii_bytes == spp_bytes ) {
	    if (BYTE_SWAP8 == YES)
		call bswap8 (Meml[tmpp], 1, Meml[tmpp], 1, nelems * (spp_bytes))
	    else if (BYTE_SWAP4 == YES)
		call bswap4 (Meml[tmpp], 1, Meml[tmpp], 1, nelems * (spp_bytes))
	    else if (BYTE_SWAP2 == YES)
		call bswap2 (Meml[tmpp], 1, Meml[tmpp], 1, nelems * (spp_bytes))
	    call i64to32 ( Meml[tmpp], mii, nelems )
	}
	else {
	    call eprintf("[ERROR] miipak32.x: unexpected integer size\n")
	}

        call mfree (tmpp, TY_LONG)
end
