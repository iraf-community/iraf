# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STG_ESCAPE -- Pass a device dependent instruction on to the kernel.  
# The stdgraph kernel does not have any escape functions at present.

procedure stg_escape (fn, instruction, nwords)

int	fn			# function code
short	instruction[ARB]	# instruction data words
int	nwords			# length of instruction

begin
end
