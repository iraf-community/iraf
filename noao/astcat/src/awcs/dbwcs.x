include <imhdr.h>
include <math.h>
include <mwset.h>
include <pkg/skywcs.h>
include <pkg/cq.h>

# These should probably go into aimpars.h.

define	IMDB_WCSDICT	"|wxref|wyref|wxmag|wymag|wxrot|wyrot|wraref|wdecref|\
wproj|wsystem|"

define	IMDB_WCS_WXREF		1
define	IMDB_WCS_WYREF		2
define	IMDB_WCS_WXMAG		3
define	IMDB_WCS_WYMAG		4
define	IMDB_WCS_WXROT		5
define	IMDB_WCS_WYROT		6
define	IMDB_WCS_WLNGREF	7
define	IMDB_WCS_WLATREF	8
define	IMDB_WCS_WPROJ		9
define	IMDB_WCS_WSYSTEM	10


# AT_DBWCS -- Compute a FITS WCS from an image using WCS definitions
# stored in the image surveys configuration file and transferred to the
# image query results structure. At the moment I am going to keep this
# routine simple by not worrying about the units of any quantities but the
# world coordinates of the reference point. This routine can be made more
# sophisticated later as time permits. The information is there ...

int procedure at_dbwcs (im, res, update, verbose)

pointer	im			#I the input image descriptor
pointer	res			#I the image query results descriptor
bool	update			#I update rather than list the wcs
bool	verbose			#I verbose mode

double	xref, yref, xmag, ymag, xrot, yrot, lngref, latref, dval
pointer	sp, kfield, kname, kvalue, kunits, wtype, ctype, coo, mw
int	i, ip, stat, coostat, ktype, nwcs, wkey, lngunits, latunits
double	imgetd(), at_imhms()
int	cq_istati(), cq_winfon(), strdic(), ctod(), ctowrd(), sk_decwcs()
bool	streq()
errchk	imgetd()

begin
	# Return if the input is not 2D.

	if (IM_NDIM(im) != 2)
	    return (ERR)

	# Allocate working space. 

	call smark (sp)
        call salloc (kfield, CQ_SZ_QPNAME, TY_CHAR) 
        call salloc (kname, CQ_SZ_QPNAME, TY_CHAR) 
        call salloc (kvalue, CQ_SZ_QPVALUE, TY_CHAR) 
        call salloc (kunits, CQ_SZ_QPUNITS, TY_CHAR) 
	call salloc (wtype, SZ_FNAME, TY_CHAR)
	call salloc (ctype, SZ_FNAME, TY_CHAR)

	# Assume some sensible defaults, e.g. the reference point is at
	# the center of the image, the orientation is the standard astronomical
	# orientation with ra increasing to the left and declination increasing
	# to the top, the projection is tan, the coordinate system is J2000.

	xref = (IM_LEN(im,1) + 1.0d0)/ 2.0d0
	yref = (IM_LEN(im,2) + 1.0d0)/ 2.0d0
	xmag = INDEFD
	ymag = INDEFD
	xrot = 180.0d0
	yrot = 0.0d0 
	lngref = INDEFD
	latref = INDEFD
	call strcpy ("tan", Memc[wtype], SZ_FNAME)
	call strcpy ("J2000", Memc[ctype], SZ_FNAME)

	# Loop over the mwcs database quantities.

	nwcs = cq_istati (res, CQNWCS)
	do i = 1, nwcs {

            # Get the keyword information.
            if (cq_winfon (res, i, Memc[kfield], CQ_SZ_QPNAME, Memc[kname],
                CQ_SZ_QPNAME, Memc[kvalue], CQ_SZ_QPVALUE, ktype, Memc[kunits],
                CQ_SZ_QPUNITS) != i)
                next

	    # Which keyword have we got ?
	    wkey = strdic (Memc[kfield], Memc[kfield], CQ_SZ_QPNAME,
	        IMDB_WCSDICT)
	    ip = 1
	    switch (wkey) {

	    # Get the x reference point in pixels.
	    case IMDB_WCS_WXREF:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			dval = INDEFD
		    else if (ctod (Memc[kvalue], ip, dval) <= 0)
			dval = INDEFD
		} else iferr (dval = imgetd (im, Memc[kname]))
		    dval = INDEFD
		if (! IS_INDEFD(dval))
		    xref = dval

	    case IMDB_WCS_WYREF:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			dval = INDEFD
		    else if (ctod (Memc[kvalue], ip, dval) <= 0)
			dval = INDEFD
		} else iferr (dval = imgetd (im, Memc[kname]))
		    dval = INDEFD
		if (! IS_INDEFD(dval))
		    yref = dval

	    case IMDB_WCS_WXMAG:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			dval = INDEFD
		    else if (ctod (Memc[kvalue], ip, dval) <= 0)
			dval = INDEFD
		} else iferr (dval = imgetd (im, Memc[kname]))
		    dval = INDEFD
		if (! IS_INDEFD(dval))
		    xmag = dval

	    case IMDB_WCS_WYMAG:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			dval = INDEFD
		    else if (ctod (Memc[kvalue], ip, dval) <= 0)
			dval = INDEFD
		} else iferr (dval = imgetd (im, Memc[kname]))
		    dval = INDEFD
		if (! IS_INDEFD(dval))
		    ymag = dval

	    case IMDB_WCS_WXROT:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			dval = INDEFD
		    else if (ctod (Memc[kvalue], ip, dval) <= 0)
			dval = INDEFD
		} else iferr (dval = imgetd (im, Memc[kname]))
		    dval = INDEFD
		if (! IS_INDEFD(dval))
		    xrot = dval

	    case IMDB_WCS_WYROT:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			dval = INDEFD
		    else if (ctod (Memc[kvalue], ip, dval) <= 0)
			dval = INDEFD
		} else iferr (dval = imgetd (im, Memc[kname]))
		    dval = INDEFD
		if (! IS_INDEFD(dval))
		    yrot = dval

	    case IMDB_WCS_WLNGREF:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			dval = INDEFD
		    else if (ctod (Memc[kvalue], ip, dval) <= 0)
			dval = INDEFD
		} else {
		    dval = at_imhms (im, Memc[kname])
		    if (IS_INDEFD(dval)) {
			iferr (dval = imgetd (im, Memc[kname]))
		    	    dval = INDEFD
		    }
		}
		if (! IS_INDEFD(dval))
		    lngref = dval
		lngunits = strdic (Memc[kunits], Memc[kunits], CQ_SZ_QPUNITS,
		    SKY_LNG_UNITLIST)

	    case IMDB_WCS_WLATREF:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			dval = INDEFD
		    else if (ctod (Memc[kvalue], ip, dval) <= 0)
			dval = INDEFD
		} else {
		    dval = at_imhms (im, Memc[kname])
		    if (IS_INDEFD(dval)) {
			iferr (dval = imgetd (im, Memc[kname]))
		    	    dval = INDEFD
		    }
		}
		if (! IS_INDEFD(dval))
		    latref = dval
		latunits = strdic (Memc[kunits], Memc[kunits], CQ_SZ_QPUNITS,
		    SKY_LAT_UNITLIST)

	    case IMDB_WCS_WPROJ:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			call strcpy ("tan", Memc[wtype], SZ_FNAME)
		    else if (ctowrd (Memc[kvalue], ip, Memc[wtype],
		        SZ_FNAME) <= 0)
			call strcpy ("tan", Memc[wtype], SZ_FNAME)
		} else iferr (call imgstr (im, Memc[kname], Memc[wtype],
			SZ_FNAME))
		    call strcpy ("tan", Memc[wtype], SZ_FNAME)

	    case IMDB_WCS_WSYSTEM:
		if (streq (Memc[kname], "INDEF")) {
		    if (streq (Memc[kvalue], "INDEF"))
			call strcpy ("J2000", Memc[ctype], SZ_FNAME)
		    else if (ctowrd (Memc[kvalue], ip, Memc[ctype],
		        SZ_FNAME) <= 0)
			call strcpy ("J2000", Memc[ctype], SZ_FNAME)
		} else iferr (call imgstr (im, Memc[kname], Memc[ctype],
			SZ_FNAME))
		    call strcpy ("J2000", Memc[ctype], SZ_FNAME)

	    default:
		;
	    }
	}

	# Check to see of the critical quantities image scale and reference
	# point are defined. Quit if they are not, otherwise update the
	# header.

	if (IS_INDEFD(xmag) || IS_INDEFD(ymag) || IS_INDEFD(lngref) || 
	    IS_INDEFD(latref)) {

	    stat = ERR

	} else {

	    # Open the coordinate system structure.
	    coostat = sk_decwcs (Memc[ctype], mw, coo, NULL)

	    # Update hte header.
	    if (coostat == ERR || mw != NULL) {
		if (mw != NULL)
		    call mw_close (mw)
		stat = ERR
	    } else {
		if (lngunits > 0)
		    call sk_seti (coo, S_NLNGUNITS, lngunits)
		if (latunits > 0)
		    call sk_seti (coo, S_NLATUNITS, latunits)
		if (verbose)
		    call printf ("    Writing FITS wcs using image survey db\n")
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


define  NEWCD     Memd[ncd+(($2)-1)*ndim+($1)-1]

# AT_UWCS -- Compute the image wcs from the user parameters.

procedure at_uwcs (im, coo, projection, lngref, latref, xref, yref,
        xscale, yscale, xrot, yrot, transpose, update)

pointer im                      #I pointer to the input image
pointer coo                     #I pointer to the coordinate structure
char    projection[ARB]         #I the sky projection geometry
double  lngref, latref          #I the world coordinates of the reference point
double  xref, yref              #I the reference point in pixels
double  xscale, yscale          #I the x and y scale in arcsec / pixel
double  xrot, yrot              #I the x and y axis rotation angles in degrees
bool    transpose               #I transpose the wcs
bool    update	                #I update rather than list the wcs


double  tlngref, tlatref
int     l, i, ndim, naxes, axmap, wtype, ax1, ax2, szatstr
pointer mw, sp, r, w, cd, ltm, ltv, iltm, nr, ncd, axes, axno, axval
pointer projstr, projpars, wpars, mwnew, atstr
int     mw_stati(), sk_stati(), strdic(), strlen(), itoc()
pointer mw_openim(), mw_open()
errchk  mw_newsystem(), mw_gwattrs()

begin
        mw = mw_openim (im)
        ndim = mw_stati (mw, MW_NPHYSDIM)

        # Allocate working memory for the vectors and matrices.
        call smark (sp)
        call salloc (projstr, SZ_FNAME, TY_CHAR)
        call salloc (projpars, SZ_LINE, TY_CHAR)
        call salloc (wpars, SZ_LINE, TY_CHAR)
        call salloc (r, ndim, TY_DOUBLE)
        call salloc (w, ndim, TY_DOUBLE)
        call salloc (cd, ndim * ndim, TY_DOUBLE)
        call salloc (ltm, ndim * ndim, TY_DOUBLE)
        call salloc (ltv, ndim, TY_DOUBLE)
        call salloc (iltm, ndim * ndim, TY_DOUBLE)
        call salloc (nr, ndim, TY_DOUBLE)
        call salloc (ncd, ndim * ndim, TY_DOUBLE)
        call salloc (axes, IM_MAXDIM, TY_INT)
        call salloc (axno, IM_MAXDIM, TY_INT)
        call salloc (axval, IM_MAXDIM, TY_INT)

        # Open the new wcs
        mwnew = mw_open (NULL, ndim)
        call mw_gsystem (mw, Memc[projstr], SZ_FNAME)
        iferr {
            call mw_newsystem (mw, "image", ndim)
        } then {
            call mw_newsystem (mwnew, Memc[projstr], ndim)
        } else {
            call mw_newsystem (mwnew, "image", ndim)
        }

        # Set the LTERM.
        call mw_gltermd (mw, Memd[ltm], Memd[ltv], ndim)
        call mw_sltermd (mwnew, Memd[ltm], Memd[ltv], ndim)

        # Store the old axis map for later use.
        call mw_gaxmap (mw, Memi[axno], Memi[axval], ndim)

        # Get the 2 logical axes.
        call mw_gaxlist (mw, 03B, Memi[axes], naxes)
        axmap = mw_stati (mw, MW_USEAXMAP)
        ax1 = Memi[axes]
        ax2 = Memi[axes+1]

        # Set the axes and projection type.
        if (projection[1] == EOS) {
            call mw_swtype (mwnew, Memi[axes], ndim, "linear", "")
        } else {
            call sscan (projection)
                call gargwrd (Memc[projstr], SZ_FNAME)
                call gargstr (Memc[projpars], SZ_LINE)
            call sprintf (Memc[wpars], SZ_LINE,
                "axis 1: axtype = ra %s axis 2: axtype = dec %s")
                call pargstr (Memc[projpars])
                call pargstr (Memc[projpars])
            call mw_swtype (mwnew, Memi[axes], ndim, Memc[projstr], Memc[wpars])
        }

        # Copy in the atrributes of the other axes.
        szatstr = SZ_LINE
        call malloc (atstr, szatstr, TY_CHAR)
        do l = 1, ndim {
            if (l == ax1 || l == ax2)
                next
            iferr {
                call mw_gwattrs (mw, l, "wtype", Memc[projpars], SZ_LINE)
            } then {
                call mw_swtype (mwnew, l, 1, "linear", "")
            } else {
                call mw_swtype (mwnew, l, 1, Memc[projpars], "")
            }
            for (i = 1; ; i = i + 1) {
                if (itoc (i, Memc[projpars], SZ_LINE) <= 0)
                    Memc[atstr] = EOS
                repeat {
                    iferr (call mw_gwattrs (mw, l, Memc[projpars],
                        Memc[atstr], szatstr))
                        Memc[atstr] = EOS
                    if (strlen (Memc[atstr]) < szatstr)
                        break
                    szatstr = szatstr + SZ_LINE
                    call realloc (atstr, szatstr, TY_CHAR)
                }
                if (Memc[atstr] == EOS)
                    break
                call mw_swattrs (mwnew, 1, Memc[projpars], Memc[atstr])
            }
        }
        call mfree (atstr, TY_CHAR)

        # Compute the referemce point world coordinates.
        switch (sk_stati(coo, S_NLNGUNITS)) {
        case SKY_DEGREES:
            tlngref = lngref
        case SKY_RADIANS:
            tlngref = RADTODEG(lngref)
        case SKY_HOURS:
            tlngref = 15.0d0 * lngref
        default:
            tlngref = lngref
        }
        switch (sk_stati(coo, S_NLATUNITS)) {
        case SKY_DEGREES:
            tlatref = latref
        case SKY_RADIANS:
            tlatref = RADTODEG(latref)
        case SKY_HOURS:
            tlatref = 15.0d0 * latref
        default:
            tlatref = latref
        }

        if (! transpose) {
            Memd[w+ax1-1] = tlngref
            Memd[w+ax2-1] = tlatref
        } else {
            Memd[w+ax2-1] = tlngref
            Memd[w+ax1-1] = tlatref
        }

        # Compute the reference point pixel coordinates.
        Memd[nr+ax1-1] = xref
        Memd[nr+ax2-1] = yref

        # Compute the new CD matrix.
        if (! transpose) {
            NEWCD(ax1,ax1) = xscale * cos (DEGTORAD(xrot)) / 3600.0d0
            NEWCD(ax2,ax1) = -yscale * sin (DEGTORAD(yrot)) / 3600.0d0
            NEWCD(ax1,ax2) = xscale * sin (DEGTORAD(xrot)) / 3600.0d0
            NEWCD(ax2,ax2) = yscale * cos (DEGTORAD(yrot)) / 3600.0d0
        } else {
            NEWCD(ax1,ax1) = xscale * sin (DEGTORAD(xrot)) / 3600.0d0
            NEWCD(ax2,ax1) = yscale * cos (DEGTORAD(yrot)) / 3600.0d0
            NEWCD(ax1,ax2) = xscale * cos (DEGTORAD(xrot)) / 3600.0d0
            NEWCD(ax2,ax2) = -yscale * sin (DEGTORAD(yrot)) / 3600.0d0
        }

	if (! update)
	    call at_mwshow (mwnew, Memd[ltv], Memd[ltm], Memd[w], Memd[nr],
		Memd[ncd], ndim)

        # Reset the axis map.
        call mw_seti (mw, MW_USEAXMAP, axmap)

        # Recompute and store the new wcs if update is enabled.
        call mw_saxmap (mwnew, Memi[axno], Memi[axval], ndim)
        if (sk_stati (coo, S_PIXTYPE) == PIXTYPE_PHYSICAL) {
            call mw_swtermd (mwnew, Memd[nr], Memd[w], Memd[ncd], ndim)
        } else {
            call mwmmuld (Memd[ncd], Memd[ltm], Memd[cd], ndim)
            call mwinvertd (Memd[ltm], Memd[iltm], ndim)
            call asubd (Memd[nr], Memd[ltv], Memd[r], ndim)
            call mwvmuld (Memd[iltm], Memd[r], Memd[nr], ndim)
            call mw_swtermd (mwnew, Memd[nr], Memd[w], Memd[cd], ndim)
        }

        # Save the fit.
        if (! transpose) {
            call sk_seti (coo, S_PLNGAX, ax1)
            call sk_seti (coo, S_PLATAX, ax2)
        } else {
            call sk_seti (coo, S_PLNGAX, ax2)
            call sk_seti (coo, S_PLATAX, ax1)
        }
	if (update) {
            call sk_saveim (coo, mwnew, im)
            call mw_saveim (mwnew, im)
	}

	# Close the wcs,
        call mw_close (mwnew)
        call mw_close (mw)

        # Force the CDELT keywords to update. This will be unecessary when
        # mwcs is updated to deal with non-quoted and / or non left-justified
        # CTYPE keywords..
        wtype = strdic (Memc[projstr], Memc[projstr], SZ_FNAME, WTYPE_LIST)
        if (wtype > 0)
            call sk_seti (coo, S_WTYPE, wtype)
        call sk_ctypeim (coo, im)

        # Reset the fit. This will be unecessary when wcs is updated to deal
        # with non-quoted and / or non left-justified CTYPE keywords.
        call sk_seti (coo, S_WTYPE, 0)
        call sk_seti (coo, S_PLNGAX, 0)
        call sk_seti (coo, S_PLATAX, 0)

        call sfree (sp)
end


# AT_IMHMS -- Fetch a quantity form the image header that is in hms or dms
# format, e.g. in the form "+/-hh mm ss.x" or "+/-dd mm ss.s".

double procedure at_imhms (im, kname)

pointer	im			#I the image descriptor
char	kname[ARB]		#I the image keyword name

double	dval, hours, minutes, seconds
pointer	sp, value
int	nscan()
errchk	imgstr()

begin
	call smark (sp)
	call salloc (value, SZ_FNAME, TY_CHAR)

	iferr {
	    call imgstr (im, kname, Memc[value], SZ_FNAME)
	} then {
	    dval = INDEFD
	} else {
	    call sscan (Memc[value])
	        call gargd (hours)
	        call gargd (minutes)
	        call gargd (seconds)
	    if (nscan() != 3)
		dval = INDEFD
	    else if (hours >= 0.0d0)
		dval = hours + (minutes / 60.0d0) + (seconds / 3600.0d0)
	    else
		dval = -(abs(hours) + (minutes / 60.0d0) + (seconds / 3600.0d0))

	}
	    
	call sfree (sp)

	return (dval)
end
