# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<gset.h>
include	<gio.h>

# GSETS -- Set a string valued GIO parameter.

procedure gsets (gp, param, value)

pointer	gp			# graphics descriptor
int	param			# parmeter to be set
char	value[ARB]		# new value of parameter
int	i
pointer	gl[2]

begin
	gl[1] = GP_XAP(gp)
	gl[2] = GP_YAP(gp)

	switch (param) {
	case G_XTICKFORMAT:
	    call strcpy (value, GL_TICKFORMAT(gl[1]), SZ_TICKFORMAT)
	case G_YTICKFORMAT:
	    call strcpy (value, GL_TICKFORMAT(gl[2]), SZ_TICKFORMAT)
	case G_TICKFORMAT:
	    do i = 1, 2
		call strcpy (value, GL_TICKFORMAT(gl[i]), SZ_TICKFORMAT)
	default:
	    call syserr (SYS_GSET)
	}
end
