# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stdgraph.h"

# STG_OUTPUT2 -- Encode two arguments using the program given and write the
# encoded character string to the output file.

procedure stg_output2 (fd, program, arg1, arg2)

int	fd			# output file
char	program[ARB]		# encoder program defining encoding
int	arg1			# argument to be placed in register 1
int	arg2			# argument to be placed in register 2

int	stg_encode()
include	"stdgraph.com"

begin
	# Set up encoder.
	g_reg[1] = arg1
	g_reg[2] = arg2
	g_reg[E_IOP] = 1

	# Encode the output string and write the encoded string to the output
	# file.
	if (stg_encode (g_xy, g_mem, g_reg) == OK)
	    call write (fd, g_mem, g_reg[E_IOP] - 1)
end
