# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GFLUSH -- Flush the graphics output.  A simple call to the FIO flush does not
# suffice since the graphics data stream may be spread over three or more
# processes.  Flush any buffered polyline output, append the GKI_FLUSH metacode
# instruction, and flush the FIO buffered metacode output.  The other processes
# will take similar actions upon receipt of the GKI_FLUSH instruction.

procedure gflush (gp)

pointer	gp			# graphics descriptor

begin
	call gpl_flush()
	call gki_flush (GP_FD(gp))
end
