# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# SGI_ESCAPE -- Pass a device dependent instruction on to the kernel.  
# The SGK kernel does not have any escape functions at present.

procedure sgi_escape (fn, instruction, nwords)

int	fn			# function code
short	instruction[ARB]	# instruction data words
int	nwords			# length of instruction

begin
end
