include	<gio.h>

# APGFLAGS -- Procedure to get the value of the flags variable.

int procedure apgflags (gp)

pointer	gp			# graphics descriptor

begin
	return (GP_GFLAGS(gp))
end
