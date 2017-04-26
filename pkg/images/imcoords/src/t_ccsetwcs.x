include <imhdr.h>
include <math.h>
include <mwset.h>
include <pkg/skywcs.h>

# Define the possible pixel types

define  CC_PIXTYPESTR   "|logical|physical|"
define  CC_LOGICAL      1
define  CC_PHYSICAL     2


# T_CCSETWCS -- Create a wcs and write it to the image header. The wcs may
# be read from a database file written by CCMAP or it may be input by the
# user.

procedure t_ccsetwcs ()

bool	transpose, verbose, update
double	xref, yref, xscale, yscale, xrot, yrot, lngref, latref
double	txref, tyref, txscale, tyscale, txrot, tyrot, tlngref, tlatref
int	imlist, reclist, lngunits, latunits, coostat, recstat, proj, pixsys, pfd
pointer	sp, image, database, record, insystem, projstr, str
pointer	dt, im, coo, tcoo, mw, sx1, sy1, sx2, sy2
bool	clgetb()
double	clgetd()
int	imtopenp(), clgwrd(), sk_decwcs(), sk_stati(), imtlen()
int	imtgetim(), cc_dtwcs(), strdic(), cc_rdproj(), open()
pointer	dtmap(), immap()
errchk	open()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (record, SZ_FNAME, TY_CHAR)
	call salloc (insystem, SZ_FNAME, TY_CHAR)
	call salloc (projstr, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	imlist = imtopenp ("images")
	call clgstr ("database", Memc[database], SZ_FNAME)

	# Fetch the celestial coordinate system parameters.
	if (Memc[database] == EOS) {
	    dt = NULL
	    reclist = NULL
	    xref = clgetd ("xref")
	    yref = clgetd ("yref")
	    xscale = clgetd ("xmag")
	    yscale = clgetd ("ymag")
	    xrot = clgetd ("xrotation")
	    yrot = clgetd ("yrotation")
	    lngref = clgetd ("lngref")
	    latref = clgetd ("latref")
	    iferr (lngunits = clgwrd ("lngunits", Memc[str], SZ_FNAME,
	        SKY_LNG_UNITLIST))
		lngunits = 0
	    iferr (latunits = clgwrd ("latunits", Memc[str], SZ_FNAME,
	        SKY_LAT_UNITLIST))
		latunits = 0
	    call clgstr ("coosystem", Memc[insystem], SZ_FNAME)
	    coostat = sk_decwcs (Memc[insystem], mw, coo, NULL)
	    if (coostat == ERR || mw != NULL) {
		call eprintf ("Error decoding the coordinate system %s\n")
		    call pargstr (Memc[insystem])
		if (mw != NULL)
		    call mw_close (mw)
		if (coo != NULL)
		    #call mfree (coo, TY_STRUCT)
		    call sk_close (coo)
		call imtclose (imlist)
		call sfree (sp)
		return
	    }
	    if (lngunits <= 0)
		lngunits = sk_stati (coo, S_NLNGUNITS)
	    call sk_seti (coo, S_NLNGUNITS, lngunits)
	    if (latunits <= 0)
		latunits = sk_stati (coo, S_NLATUNITS)
	    call sk_seti (coo, S_NLATUNITS, latunits)

            call clgstr ("projection", Memc[projstr], SZ_LINE)
            iferr {
                pfd = open (Memc[projstr], READ_ONLY, TEXT_FILE)
            } then {
                proj = strdic (Memc[projstr], Memc[projstr], SZ_LINE,
                    WTYPE_LIST)
                if (proj <= 0 || proj == WTYPE_LIN)
                    Memc[projstr] = EOS
            } else {
                proj = cc_rdproj (pfd, Memc[projstr], SZ_LINE)
                call close (pfd)
            }

            iferr (pixsys = clgwrd ("pixsystem", Memc[str], SZ_FNAME,
                CC_PIXTYPESTR))
                pixsys = PIXTYPE_LOGICAL
            else if (pixsys == CC_PHYSICAL)
                pixsys = PIXTYPE_PHYSICAL
            else
                pixsys = PIXTYPE_LOGICAL
            call sk_seti (coo, S_PIXTYPE, pixsys)
	} else {
	    dt = dtmap (Memc[database], READ_ONLY)
	    reclist = imtopenp ("solutions")
	    if ((imtlen (reclist) > 1) && (imtlen (imlist) !=
	        imtlen (reclist))) {
		call eprintf (
		    " The image and record list lengths are different\n")
		call imtclose (reclist)
		call dtunmap (dt)
		call imtclose (imlist)
		call sfree (sp)
		return
	    }
	    coo = NULL
	}

	transpose = clgetb ("transpose")
	verbose = clgetb ("verbose")
	update = clgetb ("update")

	# Loop over the images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    if (update)
	        im = immap (Memc[image], READ_WRITE, 0)
	    else
	        im = immap (Memc[image], READ_ONLY, 0)
	    if (IM_NDIM(im) != 2) {
		call printf ("Skipping non 2D image %s\n")
		    call pargstr (Memc[image])
		call imunmap (im)
		next
	    }

	    if (dt == NULL) {

		if (verbose) {
		    call printf ("Image: %s\n")
			call pargstr (Memc[image])
		}

		# Compute the linear transformation parameters.
		if (IS_INDEFD(lngref))
		    tlngref = 0.0d0
		else
		    tlngref = lngref
		if (IS_INDEFD(latref))
		    tlatref = 0.0d0
		else
		    tlatref = latref
		if (IS_INDEFD(xref))
		    txref = (1.0d0 + IM_LEN(im,1)) / 2.0
		else
		    txref = xref
		if (IS_INDEFD(yref))
		    tyref = (1.0d0 + IM_LEN(im,2)) / 2.0
		else
		    tyref = yref
		if (IS_INDEFD(xscale))
		    txscale = 1.0d0
		else
		    txscale = xscale
		if (IS_INDEFD(yscale))
		    tyscale = 1.0d0
		else
		    tyscale = yscale
		if (IS_INDEFD(xrot))
		    txrot = 0.0d0
		else
		    txrot = xrot
		if (IS_INDEFD(yrot))
		    tyrot = 0.0d0
		else
		    tyrot = yrot

		if (verbose)
		    call cc_usershow (coo, Memc[projstr], tlngref, tlatref,
		        txref, tyref, txscale, tyscale, txrot, tyrot,
			transpose)

		if (update) {
		    call cc_userwcs (im, coo, Memc[projstr], tlngref, tlatref,
		        txref, tyref, txscale, tyscale, txrot, tyrot,
			transpose)
		    if (verbose)
			call printf ("Updating image header wcs\n")
		}

	    } else {
		if (imtgetim (reclist, Memc[record], SZ_FNAME) == EOF)
		    #call strcpy (Memc[image], Memc[record], SZ_FNAME)
		    ;
	        if (verbose) {
		    call printf ("Image: %s  Database: %s  Solution: %d\n")
		        call pargstr (Memc[image])
		        call pargstr (Memc[database])
			call pargstr (Memc[record])
	        }
		sx1 = NULL; sx2 = NULL
		sy1 = NULL; sy2 = NULL
		tcoo = NULL
		recstat = cc_dtwcs (dt, Memc[record], tcoo, Memc[projstr],
		    tlngref, tlatref, sx1, sy1, sx2, sy2, txref, tyref, txscale,
		    tyscale, txrot, tyrot)
		if (recstat == ERR) {
                    call printf ("    Cannot find or decode ")
                    call printf ("record %s in database file %s\n")
                        call pargstr (Memc[record])
                        call pargstr (Memc[database])
		} else {
		    call sscan (Memc[projstr])
			call gargwrd (Memc[str], SZ_FNAME)
	    	    proj = strdic (Memc[str], Memc[str], SZ_FNAME,
		        WTYPE_LIST)
	    	    if (proj <= 0 || proj == WTYPE_LIN)
			Memc[projstr] = EOS
		    if (verbose)
		        call cc_usershow (tcoo, Memc[projstr], tlngref,
			    tlatref, txref, tyref, txscale, tyscale, txrot,
			    tyrot, transpose)
		    if (update) {
		        call cc_nwcsim (im, tcoo, Memc[projstr], tlngref,
			    tlatref, sx1, sy1, sx2, sy2, transpose)
		        if (verbose)
			    call printf ("Updating image header wcs\n")
		    }
		}
		if (tcoo != NULL)
		    #call mfree (tcoo, TY_STRUCT)
		    call sk_close (tcoo)
		if (sx1 != NULL)
		    call dgsfree (sx1)
		if (sy1 != NULL)
		    call dgsfree (sy1)
	    }

	    call imunmap (im)
	}

	# Close up memory.
	if (coo != NULL)
	    #call mfree (coo, TY_STRUCT)
	    call sk_close (coo)
	if (dt != NULL)
	    call dtunmap (dt)
	if (reclist != NULL)
	    call imtclose (reclist)
	call imtclose (imlist)

	call sfree (sp)
end


define  NEWCD     Memd[ncd+(($2)-1)*ndim+($1)-1]

# CC_USERWCS -- Compute the image wcs from the user parameters.

procedure cc_userwcs (im, coo, projection, lngref, latref, xref, yref,
        xscale, yscale, xrot, yrot, transpose)

pointer im                      #I pointer to the input image
pointer coo                     #I pointer to the coordinate structure
char    projection[ARB]         #I the sky projection geometry
double  lngref, latref          #I the world coordinates of the reference point
double  xref, yref              #I the reference point in pixels
double  xscale, yscale          #I the x and y scale in arcsec / pixel
double  xrot, yrot              #I the x and y axis rotation angles in degrees
bool    transpose               #I transpose the wcs

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
        call sk_saveim (coo, mwnew, im)
        call mw_saveim (mwnew, im)
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


# CC_USERSHOW -- Print the image wcs parameters in user friendly format.

procedure cc_usershow (coo, projection, lngref, latref, xref, yref, xscale,
	yscale, xrot, yrot, transpose)

pointer	coo			#I pointer to the coordinate structure
char	projection[ARB]		#I the sky projection geometry
double	lngref, latref		#I the world coordinates of the reference point
double	xref, yref		#I the reference point in pixels
double	xscale, yscale		#I the x and y scale in arcsec / pixel
double	xrot, yrot		#I the x and y axis rotation angles in degrees
bool	transpose		#I transpose the wcs

pointer	sp, str, keyword, value
int	sk_stati()

begin
        # Allocate temporary space.
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)
        call salloc (keyword, SZ_FNAME, TY_CHAR)
        call salloc (value, SZ_FNAME, TY_CHAR)

        call printf ("Coordinate mapping parameters\n")
        call printf ("    Sky projection geometry: %s\n")
	if (projection[1] == EOS)
	    call pargstr ("lin")
	else {
            call sscan (projection)
            call gargwrd (Memc[str], SZ_LINE)
            call pargstr (Memc[str])
            repeat {
                call gargwrd (Memc[keyword], SZ_FNAME)
                if (Memc[keyword] == EOS)
                    break
                call gargwrd (Memc[value], SZ_FNAME)
                if (Memc[value] != '=')
                    break
                call gargwrd (Memc[value], SZ_FNAME)
                if (Memc[value] == EOS)
                    break
                call printf ("    Projection parameter %s: %s\n")
                    call pargstr (Memc[keyword])
                    call pargstr (Memc[value])
            }
	}

       # Output the reference point.
        call sprintf (Memc[str], SZ_LINE,
            "    Reference point: %s  %s  (%s   %s)\n")
        switch (sk_stati (coo, S_NLNGUNITS)) {
        case SKY_DEGREES:
            call pargstr ("%0.2h")
        case SKY_RADIANS:
            call pargstr ("%0.7g")
        case SKY_HOURS:
            call pargstr ("%0.3h")
        }
        switch (sk_stati (coo, S_NLATUNITS)) {
        case SKY_DEGREES:
            call pargstr ("%0.2h")
        case SKY_RADIANS:
            call pargstr ("%0.7g")
        case SKY_HOURS:
            call pargstr ("%0.3h")
        }
        switch (sk_stati (coo, S_NLNGUNITS)) {
        case SKY_DEGREES:
            call pargstr ("degrees")
        case SKY_RADIANS:
            call pargstr ("radians")
        case SKY_HOURS:
            call pargstr ("hours")
        }
        switch (sk_stati (coo, S_NLATUNITS)) {
        case SKY_DEGREES:
            call pargstr ("degrees")
        case SKY_RADIANS:
            call pargstr ("radians")
        case SKY_HOURS:
            call pargstr ("hours")
        }
        call printf (Memc[str])
            call pargd (lngref)
            call pargd (latref)

	# Output the logical axes.
	if (sk_stati (coo, S_CTYPE) == CTYPE_EQUATORIAL)
	    call printf ("    Ra/Dec logical image axes: %d  %d\n")
	else
	    call printf ("    Long/Lat logical image axes: %d  %d\n")
	if (! transpose) {
	    call pargi (1)
	    call pargi (2)
	} else {
	    call pargi (2)
	    call pargi (1)
	}

	# Output the reference point in pixels.
        call printf ("    Reference point: %0.3f  %0.3f  (pixels  pixels)\n")
            call pargd (xref)
            call pargd (yref)

        # Output the scale factors.
        call printf (
        "    X and Y scale: %0.3f  %0.3f  (arcsec/pixel  arcsec/pixel)\n")
            call pargd (xscale)
            call pargd (yscale)

	# Output the rotation angles.
        call printf (
        "    X and Y coordinate rotation: %0.3f  %0.3f  (degrees  degrees)\n")
            call pargd (xrot)
            call pargd (yrot)

	call sfree (sp)
end


# CC_DTWCS -- Read the wcs from the database records written by CCMAP.

int procedure cc_dtwcs (dt, record, coo, projection, lngref, latref, sx1, sy1,
        sx2, sy2, xref, yref, xscale, yscale, xrot, yrot)

pointer dt                      #I pointer to the database
char    record[ARB]             #I the database records to be read
pointer coo                     #O pointer to the coordinate structure
char    projection[ARB]         #O the sky projection geometry
double  lngref, latref          #O the reference point world coordinates
pointer sx1, sy1                #O pointer to the linear x and y fits
pointer sx2, sy2                #O pointer to the distortion x and y fits
double  xref, yref              #O the reference point in pixels
double  xscale, yscale          #O the x and y scale factors
double  xrot, yrot              #O the x and y axis rotation angles

int     i, op, ncoeff, junk, rec, coostat, lngunits, latunits, pixsys
double  xshift, yshift, a, b, c, d, denom
pointer sp, xcoeff, ycoeff, nxcoeff, nycoeff, mw, projpar, projvalue
bool    fp_equald()
double  dtgetd()
int     dtlocate(), dtgeti(), dtscan(), sk_decwcs(), strdic(), strlen()
int     gstrcpy()
errchk  dtgstr(), dtgetd(), dtgeti(), dgsrestore()

begin
        # Locate the appropriate records.
        iferr (rec = dtlocate (dt, record))
            return (ERR)

        # Open the coordinate structure.
        iferr (call dtgstr (dt, rec, "coosystem", projection, SZ_FNAME))
            return (ERR)
        coostat = sk_decwcs (projection, mw, coo, NULL)
        if (coostat == ERR || mw != NULL) {
            if (mw != NULL)
                call mw_close (mw)
            projection[1] = EOS
            return (ERR)
        }

        # Get the pixel coordinate system.
        iferr (call dtgstr (dt, rec, "pixsystem", projection, SZ_FNAME)) {
            pixsys = PIXTYPE_LOGICAL
        } else {
            pixsys = strdic (projection, projection, SZ_FNAME, PIXTYPE_LIST)
            if (pixsys != PIXTYPE_PHYSICAL)
                pixsys = PIXTYPE_LOGICAL
        }
        call sk_seti (coo, S_PIXTYPE, pixsys)


        # Get the reference point units.
        iferr (call dtgstr (dt, rec, "lngunits", projection, SZ_FNAME))
            return (ERR)
        lngunits = strdic (projection, projection, SZ_FNAME, SKY_LNG_UNITLIST)
        if (lngunits > 0)
            call sk_seti (coo, S_NLNGUNITS, lngunits)
        iferr (call dtgstr (dt, rec, "latunits", projection, SZ_FNAME))
            return (ERR)
        latunits = strdic (projection, projection, SZ_FNAME, SKY_LAT_UNITLIST)
        if (latunits > 0)
            call sk_seti (coo, S_NLATUNITS, latunits)

        # Get the reference point.
        iferr (call dtgstr (dt, rec, "projection", projection, SZ_FNAME))
            return (ERR)
        iferr (lngref = dtgetd (dt, rec, "lngref"))
            return (ERR)
        iferr (latref = dtgetd (dt, rec, "latref"))
            return (ERR)

        # Read in the coefficients.
        iferr (ncoeff = dtgeti (dt, rec, "surface1"))
            return (ERR)
        call smark (sp)
        call salloc (xcoeff, ncoeff, TY_DOUBLE)
        call salloc (ycoeff, ncoeff, TY_DOUBLE)
        do i = 1, ncoeff {
            junk = dtscan(dt)
            call gargd (Memd[xcoeff+i-1])
            call gargd (Memd[ycoeff+i-1])
        }

        # Restore the linear part of the fit.
        call dgsrestore (sx1, Memd[xcoeff])
        call dgsrestore (sy1, Memd[ycoeff])

        # Get and restore the distortion part of the fit.
        ncoeff = dtgeti (dt, rec, "surface2")
        if (ncoeff > 0) {
            call salloc (nxcoeff, ncoeff, TY_DOUBLE)
            call salloc (nycoeff, ncoeff, TY_DOUBLE)
            do i = 1, ncoeff {
                junk = dtscan(dt)
                call gargd (Memd[nxcoeff+i-1])
                call gargd (Memd[nycoeff+i-1])
            }
            iferr {
                call dgsrestore (sx2, Memd[nxcoeff])
            } then {
                call mfree (sx2, TY_STRUCT)
                sx2 = NULL
            }
            iferr {
                call dgsrestore (sy2, Memd[nycoeff])
            } then {
                call mfree (sy2, TY_STRUCT)
                sy2 = NULL
            }
        } else {
            sx2 = NULL
            sy2 = NULL
        }
        # Compute the coefficients.
        call geo_gcoeffd (sx1, sy1, xshift, yshift, a, b, c, d)

        # Compute the reference point.
        denom = a * d - c * b
        if (denom == 0.0d0)
            xref = INDEFD
        else
            xref = (b * yshift - d * xshift) / denom
        if (denom == 0.0d0)
            yref = INDEFD
        else
            yref =  (c * xshift - a * yshift) / denom

        # Compute the scale factors.
        xscale = sqrt (a * a + c * c)
        yscale = sqrt (b * b + d * d)

        # Compute the rotation angles.
        if (fp_equald (a, 0.0d0) && fp_equald (c, 0.0d0))
            xrot = 0.0d0
        else
            xrot = RADTODEG (atan2 (-c, a))
        if (xrot < 0.0d0)
            xrot = xrot + 360.0d0
        if (fp_equald (b, 0.0d0) && fp_equald (d, 0.0d0))
            yrot = 0.0d0
        else
            yrot = RADTODEG (atan2 (b, d))
        if (yrot < 0.0d0)
            yrot = yrot + 360.0d0

        # Read in up to 10 projection parameters.
        call salloc (projpar, SZ_FNAME, TY_CHAR)
        call salloc (projvalue, SZ_FNAME, TY_CHAR)
        op = strlen (projection) + 1
        do i = 0, 9 {
            call sprintf (Memc[projpar], SZ_FNAME, "projp%d")
                call pargi (i)
            iferr (call dtgstr (dt, rec, Memc[projpar], Memc[projvalue],
                SZ_FNAME))
                next
            op = op + gstrcpy (" ", projection[op], SZ_LINE - op + 1)
            op = op + gstrcpy (Memc[projpar], projection[op],
                SZ_LINE - op + 1)
            op = op + gstrcpy (" = ", projection[op], SZ_LINE - op + 1)
            op = op + gstrcpy (Memc[projvalue], projection[op],
                SZ_LINE - op + 1)
        }

        call sfree (sp)

        return (OK)
end
