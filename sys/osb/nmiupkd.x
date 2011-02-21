# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# NMIUPKD -- Unpack a 64 bit IEEE floating array into an SPP array of the
# indicated datatype.

procedure nmiupkd (nmi, spp, nelems, spp_datatype)

double	nmi[ARB]		#I input NMI format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	call ieevupkd (nmi, spp, nelems)
	if (spp_datatype != TY_DOUBLE)
	    call achtd (spp, spp, nelems, spp_datatype)
end
