# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GESCAPE -- Pass a device dependent instruction on to the graphics kernel.
# A unique function code should be assigned each escape function.  The graphics
# kernel will ignore escapes with unrecognized function codes.

procedure gescape (gp, fn, instruction, nwords)

pointer	gp			# graphics descriptor
int	fn			# function code (1 - 32767)
short	instruction[ARB]	# instruction to be transmitted
int	nwords			# length of instruction

begin
	call gpl_flush()
	call gki_escape (GP_FD(gp), fn, instruction, nwords)
end
