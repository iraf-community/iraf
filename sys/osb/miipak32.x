# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIPAK32 -- Pack an SPP array of the indicated datatype into an 32 bit
# signed MII array.

procedure miipak32 (spp, mii, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
int	mii[ARB]		#O output MII format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	switch (spp_datatype) {
	case TY_UBYTE:
	    call achtbl (spp, mii, nelems)
	case TY_USHORT:
	    call achtul (spp, mii, nelems)
	case TY_CHAR:
	    call achtcl (spp, mii, nelems)
	case TY_SHORT:
	    call achtsl (spp, mii, nelems)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtil (spp, mii, nelems)
	case TY_LONG:
	    call achtll (spp, mii, nelems)
	case TY_REAL:
	    call achtrl (spp, mii, nelems)
	case TY_DOUBLE:
	    call achtdl (spp, mii, nelems)
	case TY_COMPLEX:
	    call achtxl (spp, mii, nelems)
	}

	if (BYTE_SWAP4 == YES)
	    call bswap4 (mii, 1, mii, 1, nelems * (32 / NBITS_BYTE))    
	else if (BYTE_SWAP2 == YES)
	    call bswap2 (mii, 1, mii, 1, nelems * (32 / NBITS_BYTE))    
end
