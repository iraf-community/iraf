include "hdicfit.h"

define	FUNCTIONS	"|chebyshev|legendre|spline3|spline1|power|"
define	TRANSFORMS	"|none|logopacitance|k50|k75|"

# IC_OPEN -- Open ICFIT parameter structure.

procedure ic_open (ic)

pointer	ic		# ICFIT pointer
errchk	malloc

begin
	# Allocate memory for the package parameter structure.
	call malloc (ic, IC_LENSTRUCT, TY_STRUCT)
	call malloc (IC_SAMPLE(ic), SZ_LINE, TY_CHAR)
	call malloc (IC_LABELS(ic,1), SZ_LINE, TY_CHAR)
	call malloc (IC_LABELS(ic,2), SZ_LINE, TY_CHAR)
	call malloc (IC_UNITS(ic,1), SZ_LINE, TY_CHAR)
	call malloc (IC_UNITS(ic,2), SZ_LINE, TY_CHAR)

	# Set defaults.
	call ic_pstr (ic, "function", "spline3")
	call ic_puti (ic, "order", 1)
	call ic_pstr (ic, "sample", "*")
	call ic_puti (ic, "naverage", 1)
	call ic_puti (ic, "niterate", 0)
	call ic_putr (ic, "low", 0.)
	call ic_putr (ic, "high", 0.)
	call ic_putr (ic, "grow", 0.)
	call ic_pstr (ic, "xlabel", "X")
	call ic_pstr (ic, "ylabel", "Y")
	call ic_pstr (ic, "xunits", "")
	call ic_pstr (ic, "yunits", "")
	call ic_puti (ic, "key", 1)
	call ic_pkey (ic, 1, 'x', 'y')
	call ic_pkey (ic, 2, 'y', 'x')
	call ic_pkey (ic, 3, 'x', 'r')
	call ic_pkey (ic, 4, 'x', 'd')
	call ic_pkey (ic, 5, 'x', 'n')

	# Initialize other parameters
	IC_RG(ic) = NULL
	IC_XFIT(ic) = NULL
	IC_YFIT(ic) = NULL
	IC_WTSFIT(ic) = NULL
	IC_REJPTS(ic) = NULL
end


# IC_CLOSER -- Close ICFIT parameter structure.

procedure ic_closer (ic)

pointer	ic		# ICFIT pointer

begin
	# Free memory for the package parameter structure.
	call rg_free (IC_RG(ic))
	call mfree (IC_XFIT(ic), TY_REAL)
	call mfree (IC_YFIT(ic), TY_REAL)
	call mfree (IC_WTSFIT(ic), TY_REAL)
	call mfree (IC_REJPTS(ic), TY_INT)
	call mfree (IC_SAMPLE(ic), TY_CHAR)
	call mfree (IC_LABELS(ic,1), TY_CHAR)
	call mfree (IC_LABELS(ic,2), TY_CHAR)
	call mfree (IC_UNITS(ic,1), TY_CHAR)
	call mfree (IC_UNITS(ic,2), TY_CHAR)
	call mfree (ic, TY_STRUCT)
end


# IC_CLOSED -- Close ICFIT parameter structure.

procedure ic_closed (ic)

pointer	ic		# ICFIT pointer

begin
	# Free memory for the package parameter structure.
	call rg_free (IC_RG(ic))
	call mfree (IC_XFIT(ic), TY_DOUBLE)
	call mfree (IC_YFIT(ic), TY_DOUBLE)
	call mfree (IC_WTSFIT(ic), TY_DOUBLE)
	call mfree (IC_REJPTS(ic), TY_INT)
	call mfree (IC_SAMPLE(ic), TY_CHAR)
	call mfree (IC_LABELS(ic,1), TY_CHAR)
	call mfree (IC_LABELS(ic,2), TY_CHAR)
	call mfree (IC_UNITS(ic,1), TY_CHAR)
	call mfree (IC_UNITS(ic,2), TY_CHAR)
	call mfree (ic, TY_STRUCT)
end


# IC_PSTR -- Put string valued parameters.

procedure ic_pstr (ic, param, str)

pointer	ic			# ICFIT pointer
char	param[ARB]		# Parameter to be put
char	str[ARB]		# String value

int	i
pointer	ptr
int	strdic()
bool	streq()

begin
	if (streq (param, "sample"))
	    call strcpy (str, Memc[IC_SAMPLE(ic)], SZ_LINE)
	else if (streq (param, "function")) {
	    call malloc (ptr, SZ_LINE, TY_CHAR)
	    i = strdic (str, Memc[ptr], SZ_LINE, FUNCTIONS)
	    if (i > 0)
		IC_FUNCTION(ic) = i
	    call mfree (ptr, TY_CHAR)
	} else if (streq (param, "transform")) {
	    call malloc (ptr, SZ_LINE, TY_CHAR)
	    i = strdic (str, Memc[ptr], SZ_LINE, TRANSFORMS)
	    if (i > 0)
		IC_TRANSFORM(ic) = i
	    call mfree (ptr, TY_CHAR)
	} else if (streq (param, "xlabel"))
	    call strcpy (str, Memc[IC_LABELS(ic,1)], SZ_LINE)
	else if (streq (param, "ylabel"))
	    call strcpy (str, Memc[IC_LABELS(ic,2)], SZ_LINE)
	else if (streq (param, "xunits"))
	    call strcpy (str, Memc[IC_UNITS(ic,1)], SZ_LINE)
	else if (streq (param, "yunits"))
	    call strcpy (str, Memc[IC_UNITS(ic,2)], SZ_LINE)
	else
	    call error (0, "ICFIT: Unknown parameter")
end


# IC_PUTI -- Put integer valued parameters.

procedure ic_puti (ic, param, ival)

pointer	ic			# ICFIT pointer
char	param[ARB]		# Parameter to be put
int	ival			# Integer value

bool	streq()

begin
	if (streq (param, "naverage"))
	    IC_NAVERAGE(ic) = ival
	else if (streq (param, "order"))
	    IC_ORDER(ic) = ival
	else if (streq (param, "niterate"))
	    IC_NITERATE(ic) = ival
	else if (streq (param, "key"))
	    IC_GKEY(ic) = ival
	else if (streq (param, "transform"))
	    IC_TRANSFORM(ic) = ival
	else
	    call error (0, "ICFIT: Unknown parameter")
end


# IC_PKEY -- Put key parameters.
# Note the key types must be integers not characters.

procedure ic_pkey (ic, key, xaxis, yaxis)

pointer	ic			# ICFIT pointer
int	key			# Key to be defined
int	xaxis			# X axis type
int	yaxis			# Y axis type

begin
	IC_AXES(ic, key, 1) = xaxis
	IC_AXES(ic, key, 2) = yaxis
end


# IC_GKEY -- Get key parameters.

procedure ic_gkey (ic, key, xaxis, yaxis)

pointer	ic			# ICFIT pointer
int	key			# Key to be gotten
int	xaxis			# X axis type
int	yaxis			# Y axis type

begin
	xaxis = IC_AXES(ic, key, 1)
	yaxis = IC_AXES(ic, key, 2)
end


# IC_PUTR -- Put real valued parameters.

procedure ic_putr (ic, param, rval)

pointer	ic			# ICFIT pointer
char	param[ARB]		# Parameter to be put
real	rval			# Real value

bool	streq()

begin
	if (streq (param, "xmin"))
	    IC_XMIN(ic) = rval
	else if (streq (param, "xmax"))
	    IC_XMAX(ic) = rval
	else if (streq (param, "low"))
	    IC_LOW(ic) = rval
	else if (streq (param, "high"))
	    IC_HIGH(ic) = rval
	else if (streq (param, "grow"))
	    IC_GROW(ic) = rval
	else if (streq (param, "fog"))
	    IC_FOG(ic) = rval
	else if (streq (param, "rfog"))
	    IC_RFOG(ic) = rval
	else
	    call error (0, "ICFIT: Unknown parameter")
end


# IC_GSTR -- Get string valued parameters.

procedure ic_gstr (ic, param, str, maxchars)

pointer	ic			# ICFIT pointer
char	param[ARB]		# Parameter to be put
char	str[maxchars]		# String value
int	maxchars		# Maximum number of characters

bool	streq()

begin
	if (streq (param, "sample"))
	    call strcpy (Memc[IC_SAMPLE(ic)], str, maxchars)
	else if (streq (param, "xlabel"))
	    call strcpy (Memc[IC_LABELS(ic,1)], str, maxchars)
	else if (streq (param, "ylabel"))
	    call strcpy (Memc[IC_LABELS(ic,2)], str, maxchars)
	else if (streq (param, "xunits"))
	    call strcpy (Memc[IC_UNITS(ic,1)], str, maxchars)
	else if (streq (param, "yunits"))
	    call strcpy (Memc[IC_UNITS(ic,2)], str, maxchars)
	else if (streq (param, "function")) {
	    switch (IC_FUNCTION(ic)) {
	    case 1:
		call strcpy ("chebyshev", str, maxchars)
	    case 2:
		call strcpy ("legendre", str, maxchars)
	    case 3:
		call strcpy ("spline3", str, maxchars)
	    case 4:
		call strcpy ("spline1", str, maxchars)
	    case 5:
		call strcpy ("power", str, maxchars)
	    }
	} else if (streq (param, "transform")) {
	    switch (IC_TRANSFORM(ic)) {
	    case 1:
		call strcpy ("none", str, maxchars)
	    case 2:
		call strcpy ("logopacitance", str, maxchars)
	    case 3:
		call strcpy ("k50", str, maxchars)
	    case 4:
		call strcpy ("k75", str, maxchars)
	    }
	} else
	    call error (0, "ICFIT: Unknown parameter")
end


# IC_GETI -- Get integer valued parameters.

int procedure ic_geti (ic, param)

pointer	ic			# ICFIT pointer
char	param[ARB]		# Parameter to be gotten

bool	streq()

begin
	if (streq (param, "naverage"))
	    return (IC_NAVERAGE(ic))
	else if (streq (param, "order"))
	    return (IC_ORDER(ic))
	else if (streq (param, "niterate"))
	    return (IC_NITERATE(ic))
	else if (streq (param, "key"))
	    return (IC_GKEY(ic))
	else if (streq (param, "transform"))
	    return (IC_TRANSFORM(ic))

	call error (0, "ICFIT: Unknown parameter")
end


# IC_GETR -- Get real valued parameters.

real procedure ic_getr (ic, param)

pointer	ic			# ICFIT pointer
char	param[ARB]		# Parameter to be put

bool	streq()

begin
	if (streq (param, "xmin"))
	    return (IC_XMIN(ic))
	else if (streq (param, "xmax"))
	    return (IC_XMAX(ic))
	else if (streq (param, "low"))
	    return (IC_LOW(ic))
	else if (streq (param, "high"))
	    return (IC_HIGH(ic))
	else if (streq (param, "grow"))
	    return (IC_GROW(ic))
	else if (streq (param, "fog"))
	    return (IC_FOG(ic))

	call error (0, "ICFIT: Unknown parameter")
end
