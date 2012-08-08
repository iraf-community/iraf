# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMIPAK16 -- Pack an SPP array of the indicated datatype into an 16 bit
# signed NMI array.

procedure nmipak16 (spp, nmi, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
int	nmi[ARB]		#O output NMI format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	switch (spp_datatype) {
	case TY_UBYTE:
	    call achtbs (spp, nmi, nelems)
	case TY_USHORT:
	    call achtus (spp, nmi, nelems)
	case TY_CHAR:
	    call achtcs (spp, nmi, nelems)
	case TY_SHORT:
	    call achtss (spp, nmi, nelems)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtis (spp, nmi, nelems)
	case TY_LONG:
	    call achtls (spp, nmi, nelems)
	case TY_REAL:
	    call achtrs (spp, nmi, nelems)
	case TY_DOUBLE:
	    call achtds (spp, nmi, nelems)
	case TY_COMPLEX:
	    call achtxs (spp, nmi, nelems)
	}
end
