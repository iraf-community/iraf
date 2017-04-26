# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMD_ESCAPE -- Pass a device dependent instruction on to the kernel.  
# The IDK kernel does not have any escape functions at present.

procedure imd_escape (fn, instruction, nwords)

int	fn			# function code
short	instruction[ARB]	# instruction data words
int	nwords			# length of instruction

begin
end
