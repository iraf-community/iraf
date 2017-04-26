# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMIPAKD -- Pack an SPP array of the indicated datatype into an 64 bit
# IEEE floating format.

procedure nmipakd (spp, nmi, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
double	nmi[ARB]		#O output NMI format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	if (spp_datatype == TY_DOUBLE)
	    call ieevpakd (spp, nmi, nelems)
	else {
	    switch (spp_datatype) {
	    case TY_UBYTE:
		call achtbd (spp, nmi, nelems)
	    case TY_USHORT:
		call achtud (spp, nmi, nelems)
	    case TY_CHAR:
		call achtcd (spp, nmi, nelems)
	    case TY_SHORT:
		call achtsd (spp, nmi, nelems)
	    case TY_INT, TY_POINTER, TY_STRUCT:
		call achtid (spp, nmi, nelems)
	    case TY_LONG:
		call achtld (spp, nmi, nelems)
	    case TY_REAL:
		call achtrd (spp, nmi, nelems)
	    case TY_COMPLEX:
		call achtxd (spp, nmi, nelems)
	    default:
		call amovd (spp, nmi, nelems)
	    }

	    call ieevpakd (nmi, nmi, nelems)
	}
end
