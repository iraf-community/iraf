# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "icfit.h"

define	FUNCTIONS	"|chebyshev|legendre|spline3|spline1|user|"

# IC_OPEN -- Open ICFIT parameter structure.

procedure ic_open (ic)

pointer	ic		# ICFIT pointer

begin
	# Allocate memory for the package parameter structure.
	call malloc (ic, IC_LENSTRUCT, TY_STRUCT)
	call malloc (IC_SAMPLE(ic), IC_SZSAMPLE, TY_CHAR)
	call malloc (IC_LABELS(ic,1), SZ_LINE, TY_CHAR)
	call malloc (IC_LABELS(ic,2), SZ_LINE, TY_CHAR)
	call malloc (IC_UNITS(ic,1), SZ_LINE, TY_CHAR)
	call malloc (IC_UNITS(ic,2), SZ_LINE, TY_CHAR)
	call malloc (IC_HELP(ic), SZ_FNAME, TY_CHAR)

	# Initialize parameters
	IC_OVERPLOT(ic) = NO
	IC_RG(ic) = NULL
	IC_XFIT(ic) = NULL
	IC_YFIT(ic) = NULL
	IC_WTSFIT(ic) = NULL
	IC_REJPTS(ic) = NULL
	IC_GP(ic) = NULL
	IC_GT(ic) = NULL

	# Set defaults
	call ic_pstr (ic, "function", "spline3")
	call ic_puti (ic, "order", 1)
	call ic_pstr (ic, "sample", "*")
	call ic_puti (ic, "naverage", 1)
	call ic_puti (ic, "niterate", 0)
	call ic_putr (ic, "low", 3.)
	call ic_putr (ic, "high", 3.)
	call ic_putr (ic, "grow", 0.)
	call ic_puti (ic, "markrej", YES)
	call ic_pstr (ic, "xlabel", "X")
	call ic_pstr (ic, "ylabel", "Y")
	call ic_pstr (ic, "xunits", "")
	call ic_pstr (ic, "yunits", "")
	call ic_puti (ic, "color", 1)
	call ic_pstr (ic, "help", IC_DEFHELP)
	call ic_puti (ic, "key", 1)
	call ic_pkey (ic, 1, 'x', 'y')
	call ic_pkey (ic, 2, 'y', 'x')
	call ic_pkey (ic, 3, 'x', 'r')
	call ic_pkey (ic, 4, 'x', 'd')
	call ic_pkey (ic, 5, 'x', 'n')
end


# IC_COPY -- Copy an ICFIT structure.
# The output pointer must be allocated already.

procedure ic_copy (icin, icout)

pointer	icin		# Input ICFIT pointer to copy
pointer	icout		# Ouput ICFIT pointer

begin
	IC_FUNCTION(icout) = IC_FUNCTION(icin)
	IC_ORDER(icout) = IC_ORDER(icin)
	IC_NAVERAGE(icout) = IC_NAVERAGE(icin)
	IC_NITERATE(icout) = IC_NITERATE(icin)
	IC_XMIN(icout) = IC_XMIN(icin)
	IC_XMAX(icout) = IC_XMAX(icin)
	IC_LOW(icout) = IC_LOW(icin)
	IC_HIGH(icout) = IC_HIGH(icin)
	IC_GROW(icout) = IC_GROW(icin)
	IC_COLOR(icout) = IC_COLOR(icin)
	IC_MARKREJ(icout) = IC_MARKREJ(icin)
	IC_GKEY(icout) = IC_GKEY(icin)

	call strcpy (Memc[IC_SAMPLE(icin)], Memc[IC_SAMPLE(icout)], IC_SZSAMPLE)
	call strcpy (Memc[IC_LABELS(icin,1)], Memc[IC_LABELS(icout,1)], SZ_LINE)
	call strcpy (Memc[IC_LABELS(icin,2)], Memc[IC_LABELS(icout,2)], SZ_LINE)
	call strcpy (Memc[IC_UNITS(icin,1)], Memc[IC_UNITS(icout,1)], SZ_LINE)
	call strcpy (Memc[IC_UNITS(icin,2)], Memc[IC_UNITS(icout,2)], SZ_LINE)
	call strcpy (Memc[IC_HELP(icin)], Memc[IC_HELP(icout)], SZ_LINE)

	call amovi (IC_AXES(icin,1,1), IC_AXES(icout,1,1), 10)

	IC_RG(icout) = NULL
	IC_XFIT(icout) = NULL
	IC_YFIT(icout) = NULL
	IC_WTSFIT(icout) = NULL
	IC_REJPTS(icout) = NULL
end


# IC_CLOSER -- Close ICFIT parameter structure.

procedure ic_closer (ic)

pointer	ic		# ICFIT pointer

begin
	if (ic != NULL) {
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
	    call mfree (IC_HELP(ic), TY_CHAR)
	    call mfree (ic, TY_STRUCT)
	}
end


# IC_CLOSED -- Close ICFIT parameter structure.

procedure ic_closed (ic)

pointer	ic		# ICFIT pointer

begin
	if (ic != NULL) {
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
	    call mfree (IC_HELP(ic), TY_CHAR)
	    call mfree (ic, TY_STRUCT)
	}
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
	    call strcpy (str, Memc[IC_SAMPLE(ic)], IC_SZSAMPLE)
	else if (streq (param, "function")) {
	    call malloc (ptr, SZ_LINE, TY_CHAR)
	    i = strdic (str, Memc[ptr], SZ_LINE, FUNCTIONS)
	    if (i > 0)
		IC_FUNCTION(ic) = i
	    call mfree (ptr, TY_CHAR)
	} else if (streq (param, "xlabel"))
	    call strcpy (str, Memc[IC_LABELS(ic,1)], SZ_LINE)
	else if (streq (param, "ylabel"))
	    call strcpy (str, Memc[IC_LABELS(ic,2)], SZ_LINE)
	else if (streq (param, "xunits"))
	    call strcpy (str, Memc[IC_UNITS(ic,1)], SZ_LINE)
	else if (streq (param, "yunits"))
	    call strcpy (str, Memc[IC_UNITS(ic,2)], SZ_LINE)
	else if (streq (param, "help"))
	    call strcpy (str, Memc[IC_HELP(ic)], SZ_LINE)
	else
	    call error (0, "ICFIT: Unknown parameter")

	call ic_gui (ic, "params")
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
	    IC_ORDER(ic) = max (1, ival)
	else if (streq (param, "niterate"))
	    IC_NITERATE(ic) = ival
	else if (streq (param, "key"))
	    IC_GKEY(ic) = ival
	else if (streq (param, "color"))
	    IC_COLOR(ic) = ival
	else if (streq (param, "markrej"))
	    IC_MARKREJ(ic) = ival
	else
	    call error (0, "ICFIT: Unknown parameter")

	call ic_gui (ic, "params")
end


# IC_PKEY -- Put key parameters.
# Note the key types must be integers not characters.

procedure ic_pkey (ic, key, xaxis, yaxis)

pointer	ic			# ICFIT pointer
int	key			# Key to be defined
int	xaxis			# X axis type
int	yaxis			# Y axis type

begin
	if (key >= 1 && key <= 5) {
	    IC_AXES(ic, key, 1) = xaxis
	    IC_AXES(ic, key, 2) = yaxis

	    if (key == IC_GKEY(ic))
		call ic_gui (ic, "graph")
	}
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
	else
	    call error (0, "ICFIT: Unknown parameter")

	call ic_gui (ic, "params")
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
	else if (streq (param, "help"))
	    call strcpy (Memc[IC_HELP(ic)], str, maxchars)
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
		call strcpy ("user", str, maxchars)
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
	else if (streq (param, "nfit"))
	    return (IC_NFIT(ic))
	else if (streq (param, "nreject"))
	    return (IC_NREJECT(ic))
	else if (streq (param, "rejpts"))
	    return (IC_REJPTS(ic))
	else if (streq (param, "color"))
	    return (IC_COLOR(ic))
	else if (streq (param, "markrej"))
	    return (IC_MARKREJ(ic))
	else if (streq (param, "nmin")) {
	    switch (IC_FUNCTION(ic)) {
	    case 3:
		return (IC_ORDER(ic) + 3)
	    case 4:
		return (IC_ORDER(ic) + 1)
	    default:
		return (IC_ORDER(ic))
	    }
	}

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

	call error (0, "ICFIT: Unknown parameter")
end
