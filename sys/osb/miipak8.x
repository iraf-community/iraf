# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MIIPAK8 -- Pack an SPP array of the indicated datatype into an 8 bit
# unsigned MII array.

procedure miipak8 (spp, mii, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
int	mii[ARB]		#O output MII format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	switch (spp_datatype) {
	case TY_UBYTE:
	    call achtbb (spp, mii, nelems)
	case TY_USHORT:
	    call achtub (spp, mii, nelems)
	case TY_CHAR:
	    call achtcb (spp, mii, nelems)
	case TY_SHORT:
	    call achtsb (spp, mii, nelems)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtib (spp, mii, nelems)
	case TY_LONG:
	    call achtlb (spp, mii, nelems)
	case TY_REAL:
	    call achtrb (spp, mii, nelems)
	case TY_DOUBLE:
	    call achtdb (spp, mii, nelems)
	case TY_COMPLEX:
	    call achtxb (spp, mii, nelems)
	}
end
