# ECF_GETI -- Get the value of an integer parameter.

int procedure ecf_geti (param)

char	param[ARB]		# ECF parameter

int	i, strdic()
include	"ecffit.com"

begin
	i = strdic (param, ecfstr, SZ_LINE,
	    "|slope|offset|xorder|yorder|niterate|")
	switch (i) {
	case 1:
	    return (slope)
	case 2:
	    return (offset)
	case 3:
	    return (xorder)
	case 4:
	    return (yorder)
	case 5:
	    return (niterate)
	default:
	    call error (0, "ecf_geti: Unknown parameter")
	}
end


# ECF_GETS -- Get the value of a string parameter.

procedure ecf_gets (param, str, maxchar)

char	param[ARB]		# ECF parameter
char	str[maxchar]		# String
int	maxchar			# Maximum number of characters

int	i, strdic()
include	"ecffit.com"

begin
	i = strdic (param, ecfstr, SZ_LINE, "|function|")
	switch (i) {
	case 1:
	    call strcpy (function, str, maxchar)
	default:
	    call error (0, "ecf_gets: Unknown parameter")
	}
end


# ECF_GETD -- Get the values of double valued fitting parameters.

double procedure ecf_getd (param)

char	param[ARB]		# ECF parameter

int	i, strdic()
include	"ecffit.com"

begin
	i = strdic (param, ecfstr, SZ_LINE,
	    "|xmin|xmax|ymin|ymax|shift|rms|low|high|")
	switch (i) {
	case 1:
	    return (xmin)
	case 2:
	    return (xmax)
	case 3:
	    return (ymin)
	case 4:
	    return (ymax)
	case 5:
	    return (shift)
	case 6:
	    return (rms)
	case 7:
	    return (low)
	case 8:
	    return (high)
	default:
	    call error (0, "ecf_gets: Unknown parameter")
	}
end
