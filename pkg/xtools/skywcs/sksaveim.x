include "skywcsdef.h"
include "skywcs.h"

# SK_SAVEIM -- Update the image header keywords that describe the
# fundamental coordinate system, CTYPE, RADECSYS, EQUINOX (EPOCH), and
# MJD-WCS.

procedure sk_saveim (coo, mw, im)

pointer	coo			#I pointer to the coordinate structure
pointer	mw			#I pointer to the mwcs structure
pointer	im			#I image descriptor

errchk	imdelf()

begin
	# Move all this to a separate routine
	switch (SKY_CTYPE(coo)) {

	case CTYPE_EQUATORIAL:
	    call mw_swattrs (mw, SKY_PLNGAX(coo), "axtype", "ra")
	    call mw_swattrs (mw, SKY_PLATAX(coo), "axtype", "dec")
	    switch (SKY_RADECSYS(coo)) {
	    case EQTYPE_FK4:
		call imastr (im, "radecsys", "FK4")
		call imaddd (im, "equinox", SKY_EQUINOX(coo))
		call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))
	    case EQTYPE_FK4NOE:
		call imastr (im, "radecsys", "FK4NOE")
		call imaddd (im, "equinox", SKY_EQUINOX(coo))
		call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))
	    case EQTYPE_FK5:
		call imastr (im, "radecsys", "FK5")
		call imaddd (im, "equinox", SKY_EQUINOX(coo))
	        iferr (call imdelf (im, "mjd-wcs"))
		    ;
	    case EQTYPE_ICRS:
		call imastr (im, "radecsys", "ICRS")
		call imaddd (im, "equinox", SKY_EQUINOX(coo))
	        iferr (call imdelf (im, "mjd-wcs"))
		    ;
	    case EQTYPE_GAPPT:
		call imastr (im, "radecsys", "GAPPT")
		iferr (call imdelf (im, "equinox"))
		    ;
		call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))
	    }

	case CTYPE_ECLIPTIC:
	    call mw_swattrs (mw, SKY_PLNGAX(coo), "axtype", "elon")
	    call mw_swattrs (mw, SKY_PLATAX(coo), "axtype", "elat")
	    iferr (call imdelf (im, "radecsys"))
		;
	    iferr (call imdelf (im, "equinox"))
		;
	    call imaddd (im, "mjd-wcs", SKY_EPOCH(coo))

	case CTYPE_GALACTIC:
	    call mw_swattrs (mw, SKY_PLNGAX(coo), "axtype", "glon")
	    call mw_swattrs (mw, SKY_PLATAX(coo), "axtype", "glat")
	    iferr (call imdelf (im, "radecsys"))
		;
	    iferr (call imdelf (im, "equinox"))
		;
	    iferr (call imdelf (im, "mjd-wcs"))
		;

	case CTYPE_SUPERGALACTIC:
	    call mw_swattrs (mw, SKY_PLNGAX(coo), "axtype", "slon")
	    call mw_swattrs (mw, SKY_PLATAX(coo), "axtype", "slat")
	    iferr (call imdelf (im, "radecsys"))
		;
	    iferr (call imdelf (im, "equinox"))
		;
	    iferr (call imdelf (im, "mjd-wcs"))
		;
	}
end


# SK_CTYPEIM -- Modify the CTYPE keywords appropriately. This step will
# become unnecessary when MWCS is updated to deal with non-equatorial celestial
# coordinate systems.

procedure sk_ctypeim (coo, im)

pointer	coo			#I pointer to the coordinate structure
pointer	im			#I image descriptor

pointer	sp, wtype, key1, key2, attr
int	sk_wrdstr()

begin
	call smark (sp)
	call salloc (key1, 8, TY_CHAR)
	call salloc (key2, 8, TY_CHAR)
	call salloc (wtype, 3, TY_CHAR)
	call salloc (attr, 8, TY_CHAR)

	call sprintf (Memc[key1], 8, "CTYPE%d")
	    call pargi (SKY_PLNGAX(coo))
	call sprintf (Memc[key2], 8, "CTYPE%d")
	    call pargi (SKY_PLATAX(coo))

	if (SKY_WTYPE(coo) <= 0 || SKY_WTYPE(coo) == WTYPE_LIN) {
	    call imastr (im, Memc[key1], "LINEAR")
	    call imastr (im, Memc[key2], "LINEAR")
	    call sfree (sp)
	    return
	}

	if (sk_wrdstr (SKY_WTYPE(coo), Memc[wtype], 3, WTYPE_LIST) <= 0)
	    call strcpy ("tan", Memc[wtype], 3)
	call strupr (Memc[wtype])

	# Move all this to a separate routine
	switch (SKY_CTYPE(coo)) {

	case CTYPE_EQUATORIAL:
	    call sprintf (Memc[attr], 8, "RA---%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key1], Memc[attr])
	    call sprintf (Memc[attr], 8, "DEC--%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key2], Memc[attr])

	case CTYPE_ECLIPTIC:
	    call sprintf (Memc[attr], 8, "ELON-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key1], Memc[attr])
	    call sprintf (Memc[attr], 8, "ELAT-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key2], Memc[attr])

	case CTYPE_GALACTIC:
	    call sprintf (Memc[attr], 8, "GLON-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key1], Memc[attr])
	    call sprintf (Memc[attr], 8, "GLAT-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key2], Memc[attr])

	case CTYPE_SUPERGALACTIC:
	    call sprintf (Memc[attr], 8, "SLON-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key1], Memc[attr])
	    call sprintf (Memc[attr], 8, "SLAT-%3s") 
		call pargstr (Memc[wtype])
	    call imastr (im, Memc[key2], Memc[attr])

	default:
	    call imastr (im, Memc[key1], "LINEAR")
	    call imastr (im, Memc[key2], "LINEAR")
	}

	call sfree (sp)
end
