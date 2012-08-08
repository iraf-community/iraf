include <imhdr.h>
include "../../lib/astrom.h"
include "../../lib/aimpars.h"
include <pkg/skywcs.h>


# AT_PARWCS -- Compute a FITS WCS from an image using WCS definitions
# read from the AWCSPARS parameter file and stored in the astromery
# package descriptor.  At the moment I am going to keep this routine simple
# by not worrying about the units of any quantities but the world coordinates
# of the reference point. This routine can be made more sophisticated later
# as time permits. The information is there ...

int procedure at_parwcs (im, at, update, verbose)

pointer	im			#I the input image descriptor
pointer	at			#I the astrometry package descriptor
bool	update			#I update rather than list the wcs
bool	verbose			#I verbose mode ?

double	xref, yref, xmag, ymag, xrot, yrot, lngref, latref, dval
pointer	sp, wfield, wtype, ctype, wcst, sym, coo, mw
int	i, wkey, lngunits, latunits, coostat, stat
double	at_imhms(), imgetd(), at_statd()
pointer	at_statp(), stfind()
int	at_wrdstr(), at_stati(), sk_decwcs()
bool	streq()
errchk	imgetd()

begin
	# Return if the input is not 2D.
	if (IM_NDIM(im) != 2)
	    return (ERR)

	# Return if the wcs pointer is undefined.
	if (at_statp (at, PWCS) == NULL)
	    return (ERR)

	# Return if the keyword symbol table is undefined.
	wcst = at_statp (at, WCST)
	if (wcst == NULL)
	    return (ERR)

	# Allocate working space. 
	call smark (sp)
	call salloc (wfield, SZ_FNAME, TY_CHAR)
	call salloc (wtype, SZ_FNAME, TY_CHAR)
	call salloc (ctype, SZ_FNAME, TY_CHAR)

	# Initialize.
	xref = (1.0d0 + IM_LEN(im,1)) / 2.0d0
	yref = (1.0d0 + IM_LEN(im,2)) / 2.0d0
	xmag = INDEFD
	ymag = INDEFD
	xrot = 180.0d0
	yrot= 0.0d0
	lngref = INDEFD
	latref = INDEFD
	lngunits = 0
	latunits = 0
	call strcpy ("tan", Memc[wtype], SZ_FNAME)
	call strcpy ("J2000", Memc[ctype], SZ_FNAME)

	do i = 1, AT_NWFIELDS {

	    # Which keyword have we got ?
	    wkey = at_wrdstr (i, Memc[wfield], SZ_FNAME, AT_WFIELDS)

	    switch (wkey) {

	    # Get the x reference point in pixels.
	    case WCS_WXREF:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
		        dval = at_statd (at, WXREF)
		    else iferr (dval = imgetd (im, AT_WCSTKVAL(sym)))
		        dval = at_statd (at, WXREF)
		} else 
		    dval = at_statd (at, WXREF)
		if (! IS_INDEFD(dval))
		    xref = dval

	    case WCS_WYREF:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
		        dval = at_statd (at, WYREF)
		    else iferr (dval = imgetd (im, AT_WCSTKVAL(sym)))
		        dval = at_statd (at, WYREF)
		} else 
		    dval = at_statd (at, WYREF)
		if (! IS_INDEFD(dval))
		    yref = dval

	    case WCS_WXMAG:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
		        dval = at_statd (at, WXMAG)
		    else iferr (dval = imgetd (im, AT_WCSTKVAL(sym)))
		        dval = at_statd (at, WXMAG)
		} else 
		    dval = at_statd (at, WXMAG)
		if (! IS_INDEFD(dval))
		    xmag = dval

	    case WCS_WYMAG:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
		        dval = at_statd (at, WYMAG)
		    else iferr (dval = imgetd (im, AT_WCSTKVAL(sym)))
		        dval = at_statd (at, WYMAG)
		} else 
		    dval = at_statd (at, WYMAG)
		if (! IS_INDEFD(dval))
		    ymag = dval

	    case WCS_WXROT:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
		        dval = at_statd (at, WXROT)
		    else iferr (dval = imgetd (im, AT_WCSTKVAL(sym)))
		        dval = at_statd (at, WXROT)
		} else 
		    dval = at_statd (at, WXROT)
		if (! IS_INDEFD(dval))
		    xrot = dval

	    case WCS_WYROT:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
		        dval = at_statd (at, WYROT)
		    else iferr (dval = imgetd (im, AT_WCSTKVAL(sym)))
		        dval = at_statd (at, WYROT)
		} else 
		    dval = at_statd (at, WYROT)
		if (! IS_INDEFD(dval))
		    yrot = dval

	    case WCS_WRAREF:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
		        dval = at_statd (at, WRAREF)
		    else  {
			dval = at_imhms (im, AT_WCSTKVAL(sym))
			if (IS_INDEFD(dval)) {
		            iferr (dval = imgetd (im, AT_WCSTKVAL(sym)))
		                dval = at_statd (at, WRAREF)
			}
		    }
		} else 
		    dval = at_statd (at, WRAREF)
		if (! IS_INDEFD(dval))
		    lngref = dval

	    case WCS_WDECREF:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
		        dval = at_statd (at, WDECREF)
		    else {
			dval = at_imhms (im, AT_WCSTKVAL(sym))
			if (IS_INDEFD(dval)) {
		            iferr (dval = imgetd (im, AT_WCSTKVAL(sym)))
		                dval = at_statd (at, WDECREF)
			}
		    }
		} else 
		    dval = at_statd (at, WDECREF)
		if (! IS_INDEFD(dval))
		    latref = dval

	    case WCS_WRAUNITS:
		lngunits = at_stati (at, WRAUNITS)

	    case WCS_WDECUNITS:
		latunits = at_stati (at, WDECUNITS)

	    case WCS_WPROJ:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
			call at_stats (at, WPROJ, Memc[wtype], SZ_FNAME)
		    else iferr (call imgstr (im, AT_WCSTKVAL(sym), Memc[wtype],
			SZ_FNAME))
			call at_stats (at, WPROJ, Memc[wtype], SZ_FNAME)
		} else 
		    call at_stats (at, WPROJ, Memc[wtype], SZ_FNAME)
		if (streq (Memc[wtype], "INDEF"))
		    call strcpy ("tan", Memc[wtype], SZ_FNAME)

	    case WCS_WSYSTEM:
		sym = stfind (wcst, Memc[wfield])
		if (sym != NULL) {
		    if (streq (AT_WCSTKVAL(sym), "INDEF"))
			call at_stats (at, WSYSTEM, Memc[ctype], SZ_FNAME)
		    else iferr (call imgstr (im, AT_WCSTKVAL(sym), Memc[ctype],
			SZ_FNAME))
			call at_stats (at, WSYSTEM, Memc[ctype], SZ_FNAME)
		} else 
		    call at_stats (at, WSYSTEM, Memc[ctype], SZ_FNAME)
		if (streq (Memc[ctype], "INDEF"))
		    call strcpy ("J2000", Memc[ctype], SZ_FNAME)

	    default:
		;
	    }
	}

	# Update the header.
	if (IS_INDEFD(xmag) || IS_INDEFD(ymag) || IS_INDEFD(lngref) || 
	    IS_INDEFD(latref)) {

	    stat = ERR

	} else {

	    # Open coordinate system struct
	    coostat = sk_decwcs (Memc[ctype], mw, coo, NULL)

	    if (coostat == ERR || mw != NULL) {
		if (mw != NULL)
		    call mw_close (mw)
		stat = ERR
	    } else {
		if (verbose)
		    call printf (
		        "    Writing FITS wcs using default parameters\n")
		if (lngunits > 0)
		    call sk_seti (coo, S_NLNGUNITS, lngunits)
		if (latunits > 0)
		    call sk_seti (coo, S_NLATUNITS, latunits)
		call at_uwcs (im, coo, Memc[wtype], lngref, latref, xref,
		    yref, xmag, ymag, xrot, yrot, false, update)
	        stat = OK
	    }

	    # Close the coordinate structure
	    if (coo != NULL)
	        call sk_close (coo)
	}

	call sfree (sp)

        return (stat)
end
