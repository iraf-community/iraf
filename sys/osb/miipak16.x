# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIPAK16 -- Pack an SPP array of the indicated datatype into an 16 bit
# signed MII array.

procedure miipak16 (spp, mii, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
int	mii[ARB]		#O output MII format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	switch (spp_datatype) {
	case TY_UBYTE:
	    call achtbs (spp, mii, nelems)
	case TY_USHORT:
	    call achtus (spp, mii, nelems)
	case TY_CHAR:
	    call achtcs (spp, mii, nelems)
	case TY_SHORT:
	    call achtss (spp, mii, nelems)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtis (spp, mii, nelems)
	case TY_LONG:
	    call achtls (spp, mii, nelems)
	case TY_REAL:
	    call achtrs (spp, mii, nelems)
	case TY_DOUBLE:
	    call achtds (spp, mii, nelems)
	case TY_COMPLEX:
	    call achtxs (spp, mii, nelems)
	}

	if (BYTE_SWAP2 == YES)
	    call bswap2 (mii, 1, mii, 1, nelems * (16 / NBITS_BYTE))
end
