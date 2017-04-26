# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMIPAKR -- Pack an SPP array of the indicated datatype into an 32 bit
# IEEE floating format.

procedure nmipakr (spp, nmi, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
real	nmi[ARB]		#O output NMI format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	if (spp_datatype == TY_REAL)
	    call ieevpakr (spp, nmi, nelems)
	else {
	    switch (spp_datatype) {
	    case TY_UBYTE:
		call achtbr (spp, nmi, nelems)
	    case TY_USHORT:
		call achtur (spp, nmi, nelems)
	    case TY_CHAR:
		call achtcr (spp, nmi, nelems)
	    case TY_SHORT:
		call achtsr (spp, nmi, nelems)
	    case TY_INT, TY_POINTER, TY_STRUCT:
		call achtir (spp, nmi, nelems)
	    case TY_LONG:
		call achtlr (spp, nmi, nelems)
	    case TY_DOUBLE:
		call achtdr (spp, nmi, nelems)
	    case TY_COMPLEX:
		call achtxr (spp, nmi, nelems)
	    default:
		call amovr (spp, nmi, nelems)
	    }

	    call ieevpakr (nmi, nmi, nelems)
	}
end
