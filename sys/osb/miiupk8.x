# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MIIUPK8 -- Unpack an 8 bit unsigned MII array into an SPP array of the
# indicated datatype.

procedure miiupk8 (mii, spp, nelems, spp_datatype)

int	mii[ARB]		#I input MII format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	call achtb (mii, spp, nelems, spp_datatype)
end
