# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIPAKR -- Pack an SPP array of the indicated datatype into an 32 bit
# IEEE floating format.

procedure miipakr (spp, mii, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
real	mii[ARB]		#O output MII format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	if (spp_datatype == TY_REAL)
	    call ieevpakr (spp, mii, nelems)
	else {
	    switch (spp_datatype) {
	    case TY_UBYTE:
		call achtbr (spp, mii, nelems)
	    case TY_USHORT:
		call achtur (spp, mii, nelems)
	    case TY_CHAR:
		call achtcr (spp, mii, nelems)
	    case TY_SHORT:
		call achtsr (spp, mii, nelems)
	    case TY_INT, TY_POINTER, TY_STRUCT:
		call achtir (spp, mii, nelems)
	    case TY_LONG:
		call achtlr (spp, mii, nelems)
	    case TY_DOUBLE:
		call achtdr (spp, mii, nelems)
	    case TY_COMPLEX:
		call achtxr (spp, mii, nelems)
	    default:
		call amovr (spp, mii, nelems)
	    }

	    call ieevpakr (mii, mii, nelems)
	}
end
