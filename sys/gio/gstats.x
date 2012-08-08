# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<gset.h>
include	<gio.h>

# GSTATS -- Get the value of a string valued GIO parameter.

int procedure gstats (gp, param, outstr, maxch)

pointer	gp			# graphics descriptor
int	param			# parmeter to be set
char	outstr[ARB]		# output string
int	maxch
int	gstrcpy()

int	i, value
pointer	p[2]

begin
	p[1] = GP_XAP(gp)
	p[2] = GP_XAP(gp)

	switch (param) {
	case G_XTICKFORMAT:
	    return (gstrcpy (GL_TICKFORMAT(p[1]), value, maxch))
	case G_YTICKFORMAT:
	    return (gstrcpy (GL_TICKFORMAT(p[2]), value, maxch))
	case G_TICKFORMAT:
	    do i = 1, 2
		return (gstrcpy (GL_TICKFORMAT(p[i]), value, maxch))
	default:
	    call syserr (SYS_GSTAT)
	}
end
