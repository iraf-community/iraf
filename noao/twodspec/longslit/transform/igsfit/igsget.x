include	<pkg/igsfit.h>

# IGS_GETI -- Get the value of an integer parameter.

int procedure igs_geti (param)

int	param			# IGS parameter

include	"igsfit.com"

begin
	switch (param) {
	case IGS_XORDER:
	    return (xorder)
	case IGS_YORDER:
	    return (yorder)
	default:
	    call error (0, "igs_geti: Unknown parameter")
	}
end


# IGS_GETS -- Get the value of a string parameter.

procedure igs_gets (param, str, maxchar)

int	param			# IGS parameter
char	str[maxchar]		# String
int	maxchar			# Maximum number of characters

include	"igsfit.com"

begin
	switch (param) {
	case IGS_FUNCTION:
	    call strcpy (function, str, maxchar)
	default:
	    call error (0, "igs_gets: Unknown parameter")
	}
end


# IGS_GETR -- Get the values of real valued fitting parameters.

real procedure igs_getr (param)

int	param			# Parameter to be get

include	"igsfit.com"

begin
	switch (param) {
	case IGS_XMIN:
	    return (xmin)
	case IGS_XMAX:
	    return (xmax)
	case IGS_YMIN:
	    return (ymin)
	case IGS_YMAX:
	    return (ymax)
	}
end
