include	<gio.h>

# APGFLAGS -- Fetch the value of the graphics flags variable.

int procedure apgflags (gp)

pointer	gp			# graphics descriptor

begin
	return (GP_GFLAGS(gp))
end
