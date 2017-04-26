# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GGETS -- Get the string value of a device parameter from the graphcap entry
# for the device.  The null string is returned if no entry is found for the
# named capability, or if the capability exists but the value field is null.
# The value of any parameter may be returned as a string, regardless of its
# datatype.  Escape sequences are converted to control codes in the output
# string.

int procedure ggets (gp, cap, outstr, maxch)

pointer	gp			# graphics descriptor
char	cap[ARB]		# name of device capability
char	outstr[ARB]		# output string
int	maxch
int	ttygets()

begin
	return (ttygets (GP_TTY(gp), cap, outstr, maxch))
end
