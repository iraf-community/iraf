# ECF_SETS -- Set the values of string valued fitting parameters.

procedure ecf_sets (param, str)

char	param[ARB]		# Parameter to be set
char	str[ARB]		# String value

char	temp[10]
int	i, strdic()
include	"ecffit.com"

begin
	i = strdic (param, temp, 10, "|function|")
	switch (i) {
	case 1:
	    i = strdic (str, str, SZ_FNAME, "|chebyshev|legendre|")
	    if (i == 0)
		call error (0, "Unknown function type")
	    call strcpy (str, function, SZ_LINE)
	    gstype = i
	default:
	    call error (0, "ecf_sets: Unknown parameter")
	}
end


# ECF_SETI -- Set the values of integer valued fitting parameters.

procedure ecf_seti (param, ival)

char	param[ARB]		# Parameter to be set
int	ival			# Integer value

int	i, strdic()
include	"ecffit.com"

begin
	i = strdic (param, ecfstr, SZ_LINE,
	    "|slope|offset|xorder|yorder|xtype|ytype|niterate|")
	switch (i) {
	case 1:
	    slope = ival
	case 2:
	    offset = ival
	case 3:
	    xorder = ival
	case 4:
	    yorder = ival
	case 5:
	    xtype = ival
	case 6:
	    ytype = ival
	case 7:
	    niterate = max (0, ival)
	default:
	    call error (0, "ecf_seti: Unknown parameter")
	}
end


# ECF_SETD -- Set the values of double valued fitting parameters.

procedure ecf_setd (param, dval)

char	param[ARB]		# Parameter to be set
double	dval			# Double value

int	i, strdic()
include	"ecffit.com"

begin
	i = strdic (param, ecfstr, SZ_LINE,
	    "|xmin|xmax|ymin|ymax|shift|low|high|")
	switch (i) {
	case 1:
	    xmin = dval
	case 2:
	    xmax = dval
	case 3:
	    ymin = dval
	case 4:
	    ymax = dval
	case 5:
	    shift = dval
	case 6:
	    low = max (0.D0, dval)
	case 7:
	    high = max (0.D0, dval)
	default:
	    call error (0, "ecf_setd: Unknown parameter")
	}
end
