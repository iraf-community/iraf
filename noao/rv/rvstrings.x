include "rvpackage.h"
include "rvflags.h"
include "rvfilter.h"
include "rvcont.h"
include "rvplots.h"

# RVSTRINGS - A file containing utility routines to convert between the
# strdic() strings and their integer code equivalents.

# CODES - A series of routines to get the correlation function,
# fitting function, or filter function given the string name of that
# function.  Returns the integer code used by the tasks.

int procedure cod_aptype (ap)

char	ap[SZ_FNAME]
bool	streq()

begin
	if (streq(ap, "echelle"))
	    return (ECHELLE)
	else if (streq(ap,"multispec"))
	    return (MULTISPEC)
	else if (streq(ap, "twodspec"))
	    return (TWODSPEC)
	else if (streq(ap, "onedspec") ||
		 streq(ap, "sum") ||
		 streq(ap, "average") ||
		 streq(ap, "maximum"))
	    return (ONEDSPEC)
end


int procedure cod_cninterp (mode)

char	mode[SZ_FNAME]
int	strdic()

begin
	return (strdic(mode, mode, SZ_FNAME, CN_INTERP_MODE))
end


int procedure cod_color (color)

char	color[SZ_FNAME]
int	strdic()

begin
	return (strdic(color, color, SZ_FNAME, C_COLOR_NAMES))
end


int procedure cod_ccftype (ccf)

char	ccf[SZ_FNAME]
int	strdic()

begin
	return (strdic(ccf, ccf, SZ_FNAME, CCF_TYPES))
end


int procedure cod_filttype (filt)

char	filt[SZ_FNAME]				#I Function name
int	strdic()

begin
	return (strdic(filt, filt, SZ_FNAME, RV_FTYPES))
end


int procedure cod_fitfunc (func)

char	func[SZ_FNAME]				#I Function name
int	strdic()

begin
	return (strdic(func, func, SZ_FNAME, RV_CFTYPES))
end


int procedure cod_plotype (plot)

char	plot[SZ_FNAME]
int	strdic()

begin
	return (strdic(plot, plot, SZ_FNAME, RV_PTYPES))
end


int procedure cod_rebin (rebin)

char	rebin[SZ_FNAME]
int	strdic()

begin
	return (strdic(rebin, rebin, SZ_FNAME, RB_WHICH))
end


int procedure cod_verbose (str)

char	str[SZ_FNAME]				#I Parameter string		
int	strdic()

begin
	return (strdic(str, str, SZ_FNAME, RV_OFTYPES))
end


int procedure cod_which (which)

char	which[SZ_FNAME]
int	strdic()

begin
	return (strdic(which, which, SZ_FNAME, RV_SPTODO))
end


# NAMES - A series of routines to get the correlation function,
# fitting function, or filter function given the string name of that
# function.

procedure nam_cninterp (rv, mode)

pointer	rv					#I RV struct pointer
char	mode[SZ_FNAME]				#O Function name

begin
	switch (CON_CNFUNC(rv)) {
	case CN_SPLINE3:
	    call strcpy ("spline3", mode, SZ_FNAME)
	case CN_LEGENDRE:
	    call strcpy ("legendre", mode, SZ_FNAME)
	case CN_CHEBYSHEV:
	    call strcpy ("chebyshev", mode, SZ_FNAME)
	case CN_SPLINE1:
	    call strcpy ("spline1", mode, SZ_FNAME)
	default:
	    call strcpy ("", mode, SZ_FNAME)
	}
end


procedure nam_color (code, name)

int	code					#I Color code
char	name[SZ_FNAME]				#O Color name

begin
	switch (code) {
	case C_BACKGROUND:
	    call strcpy ("background", name, SZ_FNAME)
	case C_FOREGROUND:
	    call strcpy ("foreground", name, SZ_FNAME)
	case C_RED:
	    call strcpy ("red", name, SZ_FNAME)
	case C_GREEN:
	    call strcpy ("green", name, SZ_FNAME)
	case C_BLUE:
	    call strcpy ("blue", name, SZ_FNAME)
	case C_CYAN:
	    call strcpy ("cyan", name, SZ_FNAME)
	case C_YELLOW:
	    call strcpy ("yellow", name, SZ_FNAME)
	case C_MAGENTA:
	    call strcpy ("magenta", name, SZ_FNAME)
	case C_PUPLE:
	    call strcpy ("purple", name, SZ_FNAME)
	case C_DARKSLATEGREY:
	    call strcpy ("slategrey", name, SZ_FNAME)
	default:
	    call strcpy ("", name, SZ_FNAME)
	}
end


procedure nam_fitfunc (rv, func)

pointer	rv					#I RV struct pointer
char	func[SZ_FNAME]				#O Function name

begin
	if (IS_DBLSTAR(rv) == YES) {
	    call strcpy ("deblend", func, SZ_FNAME)
	} else {
	    switch (RV_FITFUNC(rv)) {
	    case GAUSSIAN:
	        call strcpy ("gaussian", func, SZ_FNAME)
	    case PARABOLA:
	        call strcpy ("parabola", func, SZ_FNAME)
	    case LORENTZIAN:
	        call strcpy ("lorentzian", func, SZ_FNAME)
	    case CENTER1D:
	        call strcpy ("center1d", func, SZ_FNAME)
	    case SINC:
	        call strcpy ("sinc", func, SZ_FNAME)
	    default: 
	        call strcpy ("", func, SZ_FNAME)
	    }
	}
end


procedure nam_filttype (rv, filt)

pointer	rv					#I RV struct pointer
char	filt[SZ_FNAME]				#O Function name

begin
	switch (RVF_FILTTYPE(rv)) {
	case SQUARE:
	    call strcpy ("square", filt, SZ_FNAME)
	case RAMP:
	    call strcpy ("ramp", filt, SZ_FNAME)
	case HANNING:
	    call strcpy ("hanning", filt, SZ_FNAME)
	case WELCH:
	    call strcpy ("welch", filt, SZ_FNAME)
	default: 
	    call strcpy ("", filt, SZ_FNAME)
	}
end


procedure nam_plotype (rv, plot)

pointer	rv					#I RV struct pointer
char	plot[SZ_FNAME]				#O Plot type

begin
	switch (RVP_PLOT(rv)) {
	case AMPLITUDE_PLOT:
	    call strcpy ("amplitude", plot, SZ_FNAME)
	case PHASE_PLOT:
	    call strcpy ("phase", plot, SZ_FNAME)
	case POWER_PLOT:
	    call strcpy ("power", plot, SZ_FNAME)
	default:
	    call strcpy ("", plot, SZ_FNAME)
	}
end


procedure nam_rebin (rv, rebin)

pointer	rv					#I RV struct pointer
char	rebin[SZ_FNAME]				#O Plot type

begin
	switch (RV_REBIN(rv)) {
	case RB_OBJ:
	    call strcpy ("object", rebin, SZ_FNAME)
	case RB_TEMP:
	    call strcpy ("template", rebin, SZ_FNAME)
	case RB_SMALL:
	    call strcpy ("smallest", rebin, SZ_FNAME)
	case RB_BIG:
	    call strcpy ("largest", rebin, SZ_FNAME)
	default:
	    call strcpy ("", rebin, SZ_FNAME)
	}
end


procedure nam_tempcode (tnum, cod)

int	tnum					#I Template number
char	cod[SZ_FNAME]				#O Template code string

begin
	if (tnum <= 26) { 			# Get the simple case first.
	    cod[1] = ' '
	    cod[2] = 'A' + tnum - 1
	} else {
	    cod[1] = 'A' + int ((tnum-1)/26) - 1
	    cod[2] = 'A' + mod (tnum-1,26)
	}
	cod[3] = '\0'
end


procedure nam_verbose (rv, str)

pointer	rv					#I RV struct pointer
char	str[SZ_FNAME]				#O Output string

begin
        switch (RV_VERBOSE(rv)) {
        case OF_SHORT:
            call strcpy ("short", str, SZ_FNAME)
        case OF_LONG:
            call strcpy ("long", str, SZ_FNAME)
        case OF_NOLOG:
            call strcpy ("nolog", str, SZ_FNAME)
        case OF_NOGKI:
            call strcpy ("nogki", str, SZ_FNAME)
        case OF_TXTONLY:
            call strcpy ("txtonly", str, SZ_FNAME)
        case OF_STXTONLY:
            call strcpy ("stxtonly", str, SZ_FNAME)
        default:
            call strcpy ("", str, SZ_FNAME)
        }
end


procedure nam_which (param, str)

int	param					#I Param to be tested
char	str[SZ_FNAME]				#O Param string type

begin
	switch (param) {
	case OBJ_ONLY:
	    call strcpy ("object", str, SZ_FNAME)
	case TEMP_ONLY:
	    call strcpy ("template", str, SZ_FNAME)
	case BOTH:
	    call strcpy ("both", str, SZ_FNAME)
	case NONE:
	    call strcpy ("none", str, SZ_FNAME)
	default:
	    call strcpy ("", str, SZ_FNAME)
	}
end
