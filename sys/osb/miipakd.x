# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIPAKD -- Pack an SPP array of the indicated datatype into an 64 bit
# IEEE floating format.

procedure miipakd (spp, mii, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
double	mii[ARB]		#O output MII format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	if (spp_datatype == TY_DOUBLE)
	    call ieevpakd (spp, mii, nelems)
	else {
	    switch (spp_datatype) {
	    case TY_UBYTE:
		call achtbd (spp, mii, nelems)
	    case TY_USHORT:
		call achtud (spp, mii, nelems)
	    case TY_CHAR:
		call achtcd (spp, mii, nelems)
	    case TY_SHORT:
		call achtsd (spp, mii, nelems)
	    case TY_INT, TY_POINTER, TY_STRUCT:
		call achtid (spp, mii, nelems)
	    case TY_LONG:
		call achtld (spp, mii, nelems)
	    case TY_REAL:
		call achtrd (spp, mii, nelems)
	    case TY_COMPLEX:
		call achtxd (spp, mii, nelems)
	    default:
		call amovd (spp, mii, nelems)
	    }

	    call ieevpakd (mii, mii, nelems)
	}
end
