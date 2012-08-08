# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# NMIUPK8 -- Unpack an 8 bit unsigned NMI array into an SPP array of the
# indicated datatype.

procedure nmiupk8 (nmi, spp, nelems, spp_datatype)

int	nmi[ARB]		#I input NMI format array
int	spp[ARB]		#O output SPP format array
int	nelems			#I number of integers to be converted
int	spp_datatype		#I SPP datatype code

begin
	call achtb (nmi, spp, nelems, spp_datatype)
end
