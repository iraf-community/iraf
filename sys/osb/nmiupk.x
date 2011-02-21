# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<nmi.h>

# NMIUPK -- Unpack a NMI array of type nmi_type into a SPP array of type
# spp_type.  The nmi_types are defined in nmi.h.

procedure nmiupk (nmi, spp, nelems, nmi_datatype, spp_datatype)

int	nmi[ARB]		#I input NMI format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	nmi_datatype		#I NMI datatype code
int	spp_datatype		#I SPP datatype code

begin
	switch (nmi_datatype) {
	case NMI_BYTE:
	    call nmiupk8  (nmi, spp, nelems, spp_datatype)
	case NMI_SHORT:
	    call nmiupk16 (nmi, spp, nelems, spp_datatype)
	case NMI_LONG:
	    call nmiupk32 (nmi, spp, nelems, spp_datatype)
	case NMI_REAL:
	    call nmiupkr (nmi, spp, nelems, spp_datatype)
	case NMI_DOUBLE:
	    call nmiupkd (nmi, spp, nelems, spp_datatype)
	}
end
