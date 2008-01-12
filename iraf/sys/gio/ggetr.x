# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GGETR -- Get the real value of an device parameter from the graphcap entry
# for the device.  Zero is returned if the device does not have the
# named parameter.

real procedure ggetr (gp, cap)

pointer	gp			# graphics descriptor
char	cap[ARB]		# name of device capability
real	ttygetr()

begin
	return (ttygetr (GP_TTY(gp), cap))
end
