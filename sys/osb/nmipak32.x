# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMIPAK32 -- Pack an SPP array of the indicated datatype into an 32 bit
# signed NMI array.

procedure nmipak32 (spp, nmi, nelems, spp_datatype)

int	spp[ARB]		#I input array of SPP integers
int	nmi[ARB]		#O output NMI format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

int	nmi_bytes
int	spp_bytes
int	sizeof()
pointer	tmpp

begin
	call malloc (tmpp, nelems, TY_INT)

	nmi_bytes = 32 / NBITS_BYTE
	spp_bytes = sizeof(spp_datatype) * SZB_CHAR

	switch (spp_datatype) {
	case TY_UBYTE:
	    call achtbi (spp, Memi[tmpp], nelems)
	case TY_USHORT:
	    call achtui (spp, Memi[tmpp], nelems)
	case TY_CHAR:
	    call achtci (spp, Memi[tmpp], nelems)
	case TY_SHORT:
	    call achtsi (spp, Memi[tmpp], nelems)
	case TY_INT, TY_POINTER, TY_STRUCT:
	    call achtii (spp, Memi[tmpp], nelems)
	case TY_LONG:
	    call achtli (spp, Memi[tmpp], nelems)
	case TY_REAL:
	    call achtri (spp, Memi[tmpp], nelems)
	case TY_DOUBLE:
	    call achtdi (spp, Memi[tmpp], nelems)
	case TY_COMPLEX:
	    call achtxi (spp, Memi[tmpp], nelems)
	}

	if ( 2 * nmi_bytes == spp_bytes )
	    call ipak32 (Memi[tmpp], nmi, nelems)

        call mfree (tmpp, TY_INT)
end
