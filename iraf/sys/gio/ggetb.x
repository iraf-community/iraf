# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GGETB -- Get a boolean device parameter from the graphcap entry for the
# device.  A boolean graphcap query tests if the named parameter exists.
# Boolean queries are permitted for any capability, regardless of its actual
# datatype.

bool procedure ggetb (gp, cap)

pointer	gp			# graphics descriptor
char	cap[ARB]		# name of device capability
bool	ttygetb()

begin
	return (ttygetb (GP_TTY(gp), cap))
end
