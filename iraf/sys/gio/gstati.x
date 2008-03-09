# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<gset.h>
include	<gio.h>

# GSTAT[IP] -- Get any GIO parameter of type integer or real.  Precision may be
# lost if the actual parameter is of type real (call GSTATR instead in such
# a case).

int procedure gstati (gp, param)

pointer	gp			# graphics descriptor
int	param			# parameter to be inspected

real	gstatr()

begin
	return (gstatr (gp, param))
end


pointer procedure gstatp (gp, param)

pointer	gp			# graphics descriptor
int	param			# parameter to be set

begin
	switch (param) {
	case G_TTY:
	    return (GP_TTY(gp))
	default:
	    call syserr (SYS_GSTAT)
	}
	return (NULL);
end
