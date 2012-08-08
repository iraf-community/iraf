include	<pkg/igsfit.h>

# IGS_SETS -- Set the values of string valued fitting parameters.

procedure igs_sets (param, str)

int	param			# Parameter to be set
char	str[ARB]		# String value

include	"igsfit.com"

begin
	switch (param) {
	case IGS_FUNCTION:
	    call strcpy (str, function, SZ_LINE)
	}
end


# IGS_SETI -- Set the values of integer valued fitting parameters.

procedure igs_seti (param, ival)

int	param			# Parameter to be set
int	ival			# Integer value

include	"igsfit.com"

begin
	switch (param) {
	case IGS_XORDER:
	    xorder = ival
	case IGS_YORDER:
	    yorder = ival
	}
end


# IGS_SETR -- Set the values of real valued fitting parameters.

procedure igs_setr (param, rval)

int	param			# Parameter to be set
real	rval			# Real value

include	"igsfit.com"

begin
	switch (param) {
	case IGS_XMIN:
	    xmin = rval
	case IGS_XMAX:
	    xmax = rval
	case IGS_YMIN:
	    ymin = rval
	case IGS_YMAX:
	    ymax = rval
	}
end
