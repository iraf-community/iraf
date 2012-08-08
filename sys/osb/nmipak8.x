# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# NMIPAK8 -- Pack an SPP array of the indicated datatype into an 8 bit
# unsigned NMI array.

procedure nmipak8 (spp, nmi, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
int	nmi[ARB]		#O output NMI format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	switch (spp_datatype) {
	case TY_UBYTE:
	    call achtbb (spp, nmi, nelems)
	case TY_USHORT:
	    call achtub (spp, nmi, nelems)
	case TY_CHAR:
	    call achtcb (spp, nmi, nelems)
	case TY_SHORT:
	    call achtsb (spp, nmi, nelems)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtib (spp, nmi, nelems)
	case TY_LONG:
	    call achtlb (spp, nmi, nelems)
	case TY_REAL:
	    call achtrb (spp, nmi, nelems)
	case TY_DOUBLE:
	    call achtdb (spp, nmi, nelems)
	case TY_COMPLEX:
	    call achtxb (spp, nmi, nelems)
	}
end
