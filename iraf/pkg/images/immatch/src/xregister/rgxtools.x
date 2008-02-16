include "xregister.h"

#  RG_XINIT -- Initialize the cross-correlation code fitting structure. 

procedure rg_xinit (xc, cfunc)

pointer	xc		#O pointer to the cross-correlation structure
int	cfunc		#I the input cross-correlation function

begin
	call malloc (xc, LEN_XCSTRUCT, TY_STRUCT)

	# Initialize the regions pointers.
	XC_RC1(xc) = NULL
	XC_RC2(xc) = NULL
	XC_RL1(xc) = NULL
	XC_RL2(xc) = NULL
	XC_RZERO(xc) = NULL
	XC_RXSLOPE(xc) = NULL
	XC_RYSLOPE(xc) = NULL
	XC_XSHIFTS(xc) = NULL
	XC_YSHIFTS(xc) = NULL
	XC_TXSHIFT(xc) = 0.0
	XC_TYSHIFT(xc) = 0.0
	XC_NREGIONS(xc) = 0
	XC_CREGION(xc) = 1

	# Set up transformation parameters.
	XC_NREFPTS(xc) = 0
	call malloc (XC_XREF(xc), MAX_NREF, TY_REAL)
	call malloc (XC_YREF(xc), MAX_NREF, TY_REAL)
	call malloc (XC_TRANSFORM(xc), MAX_NTRANSFORM, TY_REAL)

	# Initialize the region offsets
	XC_IXLAG(xc) = DEF_IXLAG
	XC_IYLAG(xc) = DEF_IYLAG
	XC_XLAG(xc) = DEF_IXLAG
	XC_YLAG(xc) = DEF_IYLAG
	XC_DXLAG(xc) = DEF_DXLAG
	XC_DYLAG(xc) = DEF_DYLAG
		
	# Define the background fitting parameters.
	XC_BACKGRD(xc) = XC_BNONE
	call strcpy ("none", XC_BSTRING(xc), SZ_FNAME)
	XC_BVALUER(xc) = 0.0
	XC_BVALUE(xc) = 0.0
	XC_BORDER(xc) = DEF_BORDER
	XC_LOREJECT(xc) = DEF_LOREJECT
	XC_HIREJECT(xc) = DEF_HIREJECT
	XC_APODIZE(xc) = 0.0
	XC_FILTER(xc) = XC_FNONE
	call strcpy ("none", XC_FSTRING(xc), SZ_FNAME)

	# Get the correlation parameters.
	XC_CFUNC(xc) = cfunc
	switch (cfunc) {
	case XC_DISCRETE:
	    call strcpy ("discrete", XC_CSTRING(xc), SZ_FNAME)
	case XC_FOURIER:
	    call strcpy ("fourier", XC_CSTRING(xc), SZ_FNAME)
	case XC_FILE:
	    call strcpy ("file", XC_CSTRING(xc), SZ_FNAME)
	case XC_DIFFERENCE:
	    call strcpy ("difference", XC_CSTRING(xc), SZ_FNAME)
	default:
	    call strcpy ("unknown", XC_CSTRING(xc), SZ_FNAME)
	}
	XC_XWINDOW(xc) = DEF_XWINDOW
	XC_YWINDOW(xc) = DEF_YWINDOW
	XC_XCOR(xc) = NULL

	# Define the peak fitting function.
	XC_PFUNC(xc) = DEF_PFUNC
	call sprintf (XC_PSTRING(xc), SZ_FNAME, "%s")
	    call pargstr ("centroid")
	XC_XCBOX(xc) = DEF_XCBOX
	XC_YCBOX(xc) = DEF_YCBOX

	# Initialize the strings.
	XC_IMAGE(xc) = EOS
	XC_REFIMAGE(xc) = EOS
	XC_REGIONS(xc) = EOS
	XC_DATABASE(xc) = EOS
	XC_OUTIMAGE(xc) = EOS
	XC_REFFILE(xc) = EOS
	XC_RECORD(xc) = EOS

	# Initialize the buffers.
	call rg_xrinit (xc)

end


#  RG_XRINIT -- Initialize the regions definition portion of the 
# cross correlation code fitting structure. 

procedure rg_xrinit (xc)

pointer	xc		#I pointer to crosscor structure

begin
	call rg_xrfree (xc)

	XC_NREGIONS(xc) = 0
	XC_CREGION(xc) = 1

	call malloc (XC_RC1(xc), MAX_NREGIONS, TY_INT)
	call malloc (XC_RC2(xc), MAX_NREGIONS, TY_INT)
	call malloc (XC_RL1(xc), MAX_NREGIONS, TY_INT)
	call malloc (XC_RL2(xc), MAX_NREGIONS, TY_INT)
	call malloc (XC_RZERO(xc), MAX_NREGIONS, TY_REAL)
	call malloc (XC_RXSLOPE(xc), MAX_NREGIONS, TY_REAL)
	call malloc (XC_RYSLOPE(xc), MAX_NREGIONS, TY_REAL)
	call malloc (XC_XSHIFTS(xc), MAX_NREGIONS, TY_REAL)
	call malloc (XC_YSHIFTS(xc), MAX_NREGIONS, TY_REAL)

	call amovki (INDEFI, Memi[XC_RC1(xc)], MAX_NREGIONS)
	call amovki (INDEFI, Memi[XC_RC2(xc)], MAX_NREGIONS)
	call amovki (INDEFI, Memi[XC_RL1(xc)], MAX_NREGIONS)
	call amovki (INDEFI, Memi[XC_RL2(xc)], MAX_NREGIONS)
	call amovkr (INDEFR, Memr[XC_RZERO(xc)], MAX_NREGIONS)
	call amovkr (INDEFR, Memr[XC_RXSLOPE(xc)], MAX_NREGIONS)
	call amovkr (INDEFR, Memr[XC_RYSLOPE(xc)], MAX_NREGIONS)
	call amovkr (INDEFR, Memr[XC_XSHIFTS(xc)], MAX_NREGIONS)
	call amovkr (INDEFR, Memr[XC_YSHIFTS(xc)], MAX_NREGIONS)

	XC_TXSHIFT(xc) = 0.0
	XC_TYSHIFT(xc) = 0.0
end


# RG_XCINDEFR -- Re-initialize the background and answers regions portion of
# the cross-correlation fitting structure

procedure rg_xcindefr (xc, creg)

pointer	xc		#I pointer to the cross-correlation structure
int     creg            #I the current region

int	nregions
int	rg_xstati()

begin
	nregions = rg_xstati (xc, NREGIONS)
	if (creg < 1 || creg > nregions)
	    return

	if (nregions > 0) {
	    Memr[XC_RZERO(xc)+creg-1] = INDEFR
	    Memr[XC_RXSLOPE(xc)+creg-1] = INDEFR
	    Memr[XC_RYSLOPE(xc)+creg-1] = INDEFR
	    Memr[XC_XSHIFTS(xc)+creg-1] = INDEFR
	    Memr[XC_YSHIFTS(xc)+creg-1] = INDEFR
	}

	XC_TXSHIFT(xc) = 0.0
	XC_TYSHIFT(xc) = 0.0
end


# RG_XINDEFR -- Re-initialize the background and answers regions portion of
# the cross-correlation fitting structure for all regions and reset the
# current region to 1.

procedure rg_xindefr (xc)

pointer xc              #I pointer to the cross-correlation structure

int     nregions
int     rg_xstati()

begin
        nregions = rg_xstati (xc, NREGIONS)

        if (nregions > 0) {
            call amovkr (INDEFR, Memr[XC_RZERO(xc)], nregions)
            call amovkr (INDEFR, Memr[XC_RXSLOPE(xc)], nregions)
            call amovkr (INDEFR, Memr[XC_RYSLOPE(xc)], nregions)
            call amovkr (INDEFR, Memr[XC_XSHIFTS(xc)], nregions)
            call amovkr (INDEFR, Memr[XC_YSHIFTS(xc)], nregions)
        }

        XC_CREGION(xc) = 1
        XC_TXSHIFT(xc) = 0.0
        XC_TYSHIFT(xc) = 0.0
end


#  RG_XREALLOC -- Reallocate the regions bufffers and initialize if necessary.

procedure rg_xrealloc (xc, nregions)

pointer	xc		#I pointer to crosscor structure
int	nregions	#I number of regions

int	nr
int	rg_xstati()

begin
	nr = rg_xstati (xc, NREGIONS)

	call realloc (XC_RC1(xc), nregions, TY_INT)
	call realloc (XC_RC2(xc), nregions, TY_INT)
	call realloc (XC_RL1(xc), nregions, TY_INT)
	call realloc (XC_RL2(xc), nregions, TY_INT)
	call realloc (XC_RZERO(xc), nregions, TY_REAL)
	call realloc (XC_RXSLOPE(xc), nregions, TY_REAL)
	call realloc (XC_RYSLOPE(xc), nregions, TY_REAL)
	call realloc (XC_XSHIFTS(xc), nregions, TY_REAL)
	call realloc (XC_YSHIFTS(xc), nregions, TY_REAL)

	call amovki (INDEFI, Memi[XC_RC1(xc)+nr], nregions - nr)
	call amovki (INDEFI, Memi[XC_RC2(xc)+nr], nregions - nr)
	call amovki (INDEFI, Memi[XC_RL1(xc)+nr], nregions - nr)
	call amovki (INDEFI, Memi[XC_RL2(xc)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[XC_RZERO(xc)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[XC_RXSLOPE(xc)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[XC_RYSLOPE(xc)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[XC_XSHIFTS(xc)+nr], nregions - nr)
	call amovkr (INDEFR, Memr[XC_YSHIFTS(xc)+nr], nregions - nr)
end


# RG_XFREE -- Free the cross-correlation fitting structure.

procedure rg_xfree (xc)

pointer	xc		#I pointer to the cross-correlation structure

begin
	# Free the region descriptors.
	call rg_xrfree (xc)

	# Free the transformation descriptors.
	if (XC_XREF(xc) != NULL)
	    call mfree (XC_XREF(xc), TY_REAL)
	if (XC_YREF(xc) != NULL)
	    call mfree (XC_YREF(xc), TY_REAL)
	if (XC_TRANSFORM(xc) != NULL)
	    call mfree (XC_TRANSFORM(xc), TY_REAL)

	# Free the correlation function.
	if (XC_XCOR(xc) != NULL)
	    call mfree (XC_XCOR(xc), TY_REAL)

	call mfree (xc, TY_STRUCT)
end


# RG_XRFREE -- Free the regions portion of the cross-correlation structure.

procedure rg_xrfree (xc)

pointer	xc		#I pointer to the cross-correlation structure

begin
	call rg_xseti (xc, NREGIONS, 0)
	if (XC_RC1(xc) != NULL)
	    call mfree (XC_RC1(xc), TY_INT)
	XC_RC1(xc) = NULL
	if (XC_RC2(xc) != NULL)
	    call mfree (XC_RC2(xc), TY_INT)
	XC_RC2(xc) = NULL
	if (XC_RL1(xc) != NULL)
	    call mfree (XC_RL1(xc), TY_INT)
	XC_RL1(xc) = NULL
	if (XC_RL2(xc) != NULL)
	    call mfree (XC_RL2(xc), TY_INT)
	XC_RL2(xc) = NULL
	if (XC_RZERO(xc) != NULL)
	    call mfree (XC_RZERO(xc), TY_REAL)
	XC_RZERO(xc) = NULL
	if (XC_RXSLOPE(xc) != NULL)
	    call mfree (XC_RXSLOPE(xc), TY_REAL)
	XC_RXSLOPE(xc) = NULL
	if (XC_RYSLOPE(xc) != NULL)
	    call mfree (XC_RYSLOPE(xc), TY_REAL)
	XC_RYSLOPE(xc) = NULL
	if (XC_XSHIFTS(xc) != NULL)
	    call mfree (XC_XSHIFTS(xc), TY_REAL)
	XC_XSHIFTS(xc) = NULL
	if (XC_YSHIFTS(xc) != NULL)
	    call mfree (XC_YSHIFTS(xc), TY_REAL)
	XC_YSHIFTS(xc) = NULL
end


# RG_XSTATI -- Fetch the value of a cross-correlation fitting structure
# integer parameter.

int procedure rg_xstati (xc, param)

pointer	xc		#I pointer to the cross-correlation fitting structure
int	param		#I parameter to be fetched

begin
	switch (param) {
	case CFUNC:
	    return (XC_CFUNC(xc))
	case IXLAG:
	    return (XC_IXLAG(xc))
	case IYLAG:
	    return (XC_IYLAG(xc))
	case XLAG:
	    return (XC_XLAG(xc))
	case YLAG:
	    return (XC_YLAG(xc))
	case DXLAG:
	    return (XC_DXLAG(xc))
	case DYLAG:
	    return (XC_DYLAG(xc))
	case XWINDOW:
	    return (XC_XWINDOW(xc))
	case YWINDOW:
	    return (XC_YWINDOW(xc))
	case CREGION:
	    return (XC_CREGION(xc))
	case NREGIONS:
	    return (XC_NREGIONS(xc))
	case BACKGRD:
	    return (XC_BACKGRD(xc))
	case BORDER:
	    return (XC_BORDER(xc))
	case FILTER:
	    return (XC_FILTER(xc))
	case XCBOX:
	    return (XC_XCBOX(xc))
	case YCBOX:
	    return (XC_YCBOX(xc))
	case PFUNC:
	    return (XC_PFUNC(xc))
	case NREFPTS:
	    return (XC_NREFPTS(xc))
	default:
	    call error (0, "RG_XSTATI: Undefined integer parameter.")
	}
end


# RG_XSTATP -- Fetch the value of a pointer parameter.

pointer procedure rg_xstatp (xc, param)

pointer	xc		#I pointer to the cross-correlation structure
int	param		#I parameter to be fetched

begin
	switch (param) {
	case RC1:
	    return (XC_RC1(xc))
	case RC2:
	    return (XC_RC2(xc))
	case RL1:
	    return (XC_RL1(xc))
	case RL2:
	    return (XC_RL2(xc))
	case RZERO:
	    return (XC_RZERO(xc))
	case RXSLOPE:
	    return (XC_RXSLOPE(xc))
	case RYSLOPE:
	    return (XC_RYSLOPE(xc))
	case XSHIFTS:
	    return (XC_XSHIFTS(xc))
	case YSHIFTS:
	    return (XC_YSHIFTS(xc))
	case XCOR:
	    return (XC_XCOR(xc))
	case XREF:
	    return (XC_XREF(xc))
	case YREF:
	    return (XC_YREF(xc))
#	case CORAPODIZE:
#	    return (XC_CORAPODIZE(xc))
	case TRANSFORM:
	    return (XC_TRANSFORM(xc))
	default:
	    call error (0, "RG_XSTATP: Undefined pointer parameter.")
	}
end


# RG_XSTATR -- Fetch the value of a real parameter.

real procedure rg_xstatr (xc, param)

pointer	xc		#I pointer to the cross-correlation structure
int	param		#I parameter to be fetched

begin
	switch (param) {
	case BVALUER:
	    return (XC_BVALUER(xc))
	case BVALUE:
	    return (XC_BVALUE(xc))
	case LOREJECT:
	    return (XC_LOREJECT(xc))
	case HIREJECT:
	    return (XC_HIREJECT(xc))
	case APODIZE:
	    return (XC_APODIZE(xc))
	case TXSHIFT:
	    return (XC_TXSHIFT(xc))
	case TYSHIFT:
	    return (XC_TYSHIFT(xc))
	default:
	    call error (0, "RG_XSTATR: Undefined real parameter.")
	}
end


# RG_XSTATS -- Fetch the value of a string parameter.

procedure rg_xstats (xc, param, str, maxch)

pointer	xc		#I pointer to the cross-correlation structure
int	param		#I parameter to be fetched
char	str[ARB]	#O output value of string parameter
int	maxch		#I maximum number of characters in output string

begin
	switch (param) {
	case BSTRING:
	    call strcpy (XC_BSTRING(xc), str, maxch)
	case FSTRING:
	    call strcpy (XC_FSTRING(xc), str, maxch)
	case CSTRING:
	    call strcpy (XC_CSTRING(xc), str, maxch)
	case PSTRING:
	    call strcpy (XC_PSTRING(xc), str, maxch)
	case REFIMAGE:
	    call strcpy (XC_REFIMAGE(xc), str, maxch)
	case IMAGE:
	    call strcpy (XC_IMAGE(xc), str, maxch)
	case OUTIMAGE:
	    call strcpy (XC_OUTIMAGE(xc), str, maxch)
	case REGIONS:
	    call strcpy (XC_REGIONS(xc), str, maxch)
	case DATABASE:
	    call strcpy (XC_DATABASE(xc), str, maxch)
	case RECORD:
	    call strcpy (XC_RECORD(xc), str, maxch)
	case REFFILE:
	    call strcpy (XC_REFFILE(xc), str, maxch)
	default:
	    call error (0, "RG_XSTATS: Undefined string parameter.")
	}
end


# RG_XSETI -- Set the value of an integer parameter.

procedure rg_xseti (xc, param, value)

pointer	xc		#I pointer to the cross-correlation structure
int	param		#I parameter to be set
int	value		#O value of the integer parameter

begin
	switch (param) {
	case CFUNC:
	    XC_CFUNC(xc) = value
	    switch (value) {
	    case XC_DISCRETE:
		call strcpy ("discrete", XC_CSTRING(xc), SZ_FNAME)
	    case XC_FOURIER:
		call strcpy ("fourier", XC_CSTRING(xc), SZ_FNAME)
	    case XC_FILE:
		call strcpy ("file", XC_CSTRING(xc), SZ_FNAME)
	    case XC_DIFFERENCE:
		call strcpy ("difference", XC_CSTRING(xc), SZ_FNAME)
	    default:
		call strcpy ("unknown", XC_CSTRING(xc), SZ_FNAME)
	    }
	case IXLAG:
	    XC_IXLAG(xc) = value
	case IYLAG:
	    XC_IYLAG(xc) = value
	case XLAG:
	    XC_XLAG(xc) = value
	case YLAG:
	    XC_YLAG(xc) = value
	case DXLAG:
	    XC_DXLAG(xc) = value
	case DYLAG:
	    XC_DYLAG(xc) = value
	case XWINDOW:
	    XC_XWINDOW(xc) = value
	case YWINDOW:
	    XC_YWINDOW(xc) = value
	case BACKGRD:
	    XC_BACKGRD(xc) = value
	    switch (value) {
	    case XC_BNONE:
		call strcpy ("none", XC_BSTRING(xc), SZ_FNAME)
	    case XC_MEAN:
		call strcpy ("mean", XC_BSTRING(xc), SZ_FNAME)
	    case XC_MEDIAN:
		call strcpy ("median", XC_BSTRING(xc), SZ_FNAME)
	    case XC_SLOPE:
		call strcpy ("plane", XC_BSTRING(xc), SZ_FNAME)
	    default:
		call strcpy ("none", XC_BSTRING(xc), SZ_FNAME)
	    }
	case BORDER:
	    XC_BORDER(xc) = value
	case FILTER:
	    XC_FILTER(xc) = value
	    switch (value) {
	    case XC_FNONE:
		call strcpy ("none", XC_FSTRING(xc), SZ_FNAME)
	    case XC_LAPLACE:
		call strcpy ("laplace", XC_FSTRING(xc), SZ_FNAME)
	    default:
		call strcpy ("none", XC_FSTRING(xc), SZ_FNAME)
	    }
	case XCBOX:
	    XC_XCBOX(xc) = value
	case YCBOX:
	    XC_YCBOX(xc) = value
	case PFUNC:
	    XC_PFUNC(xc) = value
	    switch (value) {
	    case XC_PNONE:
		call strcpy ("none", XC_PSTRING(xc), SZ_FNAME)
	    case XC_CENTROID:
		call strcpy ("centroid", XC_PSTRING(xc), SZ_FNAME)
	    case XC_PARABOLA:
		call strcpy ("parabolic", XC_PSTRING(xc), SZ_FNAME)
	    case XC_SAWTOOTH:
		call strcpy ("sawtooth", XC_PSTRING(xc), SZ_FNAME)
#	    case XC_MARK:
#		call strcpy ("mark", XC_PSTRING(xc), SZ_FNAME)
	    default:
		;
	    }
	case NREFPTS:
	    XC_NREFPTS(xc) = value
	case CREGION:
	    XC_CREGION(xc) = value
	case NREGIONS:
	    XC_NREGIONS(xc) = value
	default:
	    call error (0, "RG_XSETI: Undefined integer parameter.")
	}
end


# RG_XSETP -- Set the value of a pointer parameter.

procedure rg_xsetp (xc, param, value)

pointer	xc		#I pointer to the cross-correlation structure
int	param		#I parameter to be set
pointer value		#O value of the pointer parameter

begin
	switch (param) {
	case RC1:
	    XC_RC1(xc) = value
	case RC2:
	    XC_RC2(xc) = value
	case RL1:
	    XC_RL1(xc) = value
	case RL2:
	    XC_RL2(xc) = value
	case RZERO:
	    XC_RZERO(xc) = value
	case RXSLOPE:
	    XC_RXSLOPE(xc) = value
	case RYSLOPE:
	    XC_RYSLOPE(xc) = value
	case XSHIFTS:
	    XC_XSHIFTS(xc) = value
	case YSHIFTS:
	    XC_YSHIFTS(xc) = value
	case XCOR:
	    XC_XCOR(xc) = value
	case XREF:
	    XC_XREF(xc) = value
	case YREF:
	    XC_YREF(xc) = value
	case TRANSFORM:
	    XC_TRANSFORM(xc) = value
#	case CORAPODIZE:
#	    XC_CORAPODIZE(xc) = value
	default:
	    call error (0, "RG_XSETP: Undefined pointer parameter.")
	}
end


# RG_XSETR -- Set the value of a real parameter.

procedure rg_xsetr (xc, param, value)

pointer	xc		#I pointer to the cross-correlation structure
int	param		#I parameter to be set
real	value		#O value of real parameter

begin
	switch (param) {
	case BVALUER:
	    XC_BVALUER(xc) = value
	case BVALUE:
	    XC_BVALUE(xc) = value
	case LOREJECT:
	    XC_LOREJECT(xc) = value
	case HIREJECT:
	    XC_HIREJECT(xc) = value
	case APODIZE:
	    XC_APODIZE(xc) = value
	case TXSHIFT:
	    XC_TXSHIFT(xc) = value
	case TYSHIFT:
	    XC_TYSHIFT(xc) = value
	default:
	    call error (0, "RG_XSETR: Undefined real parameter.")
	}
end


# RG_XSETS -- Set the value of a string parameter.

procedure rg_xsets (xc, param, str)

pointer	xc		#I pointer to the cross-correlation structure
int	param		#I parameter to be set
char	str[ARB]	#O value of string parameter

int	index
pointer	sp, temp
int	strdic(), fnldir()

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)

	switch (param) {
	case BSTRING:
	    index = strdic (str, str, SZ_LINE, XC_BTYPES)
	    if (index > 0) {
	    	call strcpy (str, XC_BSTRING(xc), SZ_FNAME)
		call rg_xseti (xc, BACKGRD, index)
	    }
	case FSTRING:
	    index = strdic (str, str, SZ_LINE, XC_FTYPES)
	    if (index > 0) {
	        call strcpy (str, XC_FSTRING(xc), SZ_FNAME)
		call rg_xseti (xc, FILTER, index)
	    }
	case CSTRING:
	    index = strdic (str, str, SZ_LINE, XC_CTYPES)
	    if (index > 0) {
	        call strcpy (str, XC_CSTRING(xc), SZ_FNAME)
		call rg_xseti (xc, CFUNC, index)
	    }
	case PSTRING:
	    call strcpy (str, XC_PSTRING(xc), SZ_FNAME)
	case REFIMAGE:
	    call imgcluster (str, Memc[temp], SZ_FNAME)
	    index = fnldir (Memc[temp], XC_REFIMAGE(xc), SZ_FNAME)
	    call strcpy (Memc[temp+index], XC_REFIMAGE(xc), SZ_FNAME)
	case IMAGE:
	    call imgcluster (str, Memc[temp], SZ_FNAME)
	    index = fnldir (Memc[temp], XC_IMAGE(xc), SZ_FNAME)
	    call strcpy (Memc[temp+index], XC_IMAGE(xc), SZ_FNAME)
	case OUTIMAGE:
	    call strcpy (str, XC_OUTIMAGE(xc), SZ_FNAME)
	case REGIONS:
	    call strcpy (str, XC_REGIONS(xc), SZ_FNAME)
	case DATABASE:
	    index = fnldir (str, XC_DATABASE(xc), SZ_FNAME)
	    call strcpy (str[index+1], XC_DATABASE(xc), SZ_FNAME)
	case RECORD:
	    call strcpy (str, XC_RECORD(xc), SZ_FNAME)
	case REFFILE:
	    index = fnldir (str, XC_REFFILE(xc), SZ_FNAME)
	    call strcpy (str[index+1], XC_REFFILE(xc), SZ_FNAME)
	default:
	    call error (0, "RG_XSETS: Undefined string parameter.")
	}

	call sfree (sp)
end
