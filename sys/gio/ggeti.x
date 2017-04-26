# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GGETI -- Get the integer value of an device parameter from the graphcap
# entry for the device.  Zero is returned if the device does not have the
# named parameter.

int procedure ggeti (gp, cap)

pointer	gp			# graphics descriptor
char	cap[ARB]		# name of device capability
int	ttygeti()

begin
	return (ttygeti (GP_TTY(gp), cap))
end
