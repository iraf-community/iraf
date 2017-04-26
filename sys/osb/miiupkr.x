# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

# MIIUPKR -- Unpack a 32 bit IEEE floating array into an SPP array of the
# indicated datatype.

procedure miiupkr (mii, spp, nelems, spp_datatype)

real	mii[ARB]		#I input MII format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	call ieevupkr (mii, spp, nelems)
	if (spp_datatype != TY_REAL)
	    call achtr (spp, spp, nelems, spp_datatype)
end
