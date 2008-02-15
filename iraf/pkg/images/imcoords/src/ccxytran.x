include <math.h>
include <pkg/skywcs.h>

# Define the transform geometries
define	GEO_LINEAR	1
define	GEO_DISTORTION	2
define	GEO_GEOMETRIC	3

# CC_INIT_TRANSFORM -- Get the parameter values relevant to the
# transformation from the cl. 

procedure cc_init_transform (dt, record, geometry, lngunits, latunits, sx1,
	sy1, sx2, sy2, mw, coo)

pointer	dt			#I pointer to database file produced by geomap
char	record[ARB]		#I the name of the database record
int	geometry		#I the type of geometry to be computed
int     lngunits                #I the input ra / longitude units
int     latunits                #I the input dec / latitude units
pointer	sx1, sy1		#O pointers to the linear x and y surfaces
pointer	sx2, sy2		#O pointers to the x and y distortion surfaces
pointer	mw			#O pointer to the mwcs structure
pointer	coo			#O pointer to the coordinate structure

double	lngref, latref
int	recstat, proj
pointer	sp, projstr, projpars
int	cc_dtrecord(), strdic()
pointer	cc_geowcs(), cc_celwcs()

begin
	call smark (sp)
	call salloc (projstr, SZ_FNAME, TY_CHAR)
	call salloc (projpars, SZ_LINE, TY_CHAR)

	if (dt == NULL) {

	    sx1 = NULL
	    sy1 = NULL
	    sx2 = NULL
	    sy2 = NULL
	    call cc_linit (lngunits, latunits, mw, coo)

	} else {

	    recstat = cc_dtrecord (dt, record, geometry, coo, Memc[projpars],
	        lngref, latref, sx1, sy1, sx2, sy2)
	    if (recstat == ERR) {
		coo = NULL
		sx1 = NULL
		sy1 = NULL
		sx2 = NULL
		sy2 = NULL
    		mw = NULL
	    } else {
                call sscan (Memc[projpars])
                    call gargwrd (Memc[projstr], SZ_FNAME)
                proj = strdic (Memc[projstr], Memc[projstr], SZ_FNAME,
                    WTYPE_LIST)
                if (proj <= 0 || proj == WTYPE_LIN)
                    Memc[projpars] = EOS
                if (sx2 == NULL && sy2 == NULL)
                    mw = cc_geowcs (coo, Memc[projpars], lngref, latref,
                        sx1, sy1, false)
                else
                    mw = cc_celwcs (coo, Memc[projpars], lngref, latref)
	    }
	}

	call sfree (sp)
end


# CC_FREE_TRANSFORM -- Free the previously defined transformation.

procedure cc_free_transform (sx1, sy1, sx2, sy2, mw, coo)

pointer	sx1, sy1		#U pointers to the linear x and y surfaces
pointer	sx2, sy2		#U pointers to the x and y distortion surfaces
pointer	mw			#U pointer to the mwcs structure
pointer	coo			#U pointer to the celestial coordinate structure

begin
	if (sx1 != NULL)
	    call dgsfree (sx1)
	if (sy1 != NULL)
	    call dgsfree (sy1)
	if (sx2 != NULL)
	    call dgsfree (sx2)
	if (sy2 != NULL)
	    call dgsfree (sy2)
	if (mw != NULL)
	    call mw_close (mw)
	if (coo != NULL)
	    call sk_close (coo)
end


# CC_LINIT -- Compute the required wcs structure from the input parameters.

procedure cc_linit (lngunits, latunits, mw, coo)

int     lngunits                #I the input ra / longitude units
int     latunits                #I the input dec / latitude units
pointer	mw			#O pointer to the mwcs structure
pointer	coo			#O pointer to the celestial coordinate structure

double	xref, yref, xscale, yscale, xrot, yrot, lngref, latref
int	coostat, proj, tlngunits, tlatunits, pfd
pointer	sp, projstr
double	clgetd()
int	sk_decwcs(), sk_stati(), open(), strdic(), cc_rdproj()
pointer	cc_mkwcs()
errchk	open()

begin
	# Allocate some workin space.
	call smark (sp)
	call salloc (projstr, SZ_LINE, TY_CHAR)

	# Get the reference point pixel coordinates.
	xref = clgetd ("xref")
	if (IS_INDEFD(xref))
	    xref = 0.0d0
	yref = clgetd ("yref")
	if (IS_INDEFD(yref))
	    yref = 0.0d0

	xscale = clgetd ("xmag")
	if (IS_INDEFD(xscale))
	    xscale = 1.0d0
	yscale = clgetd ("ymag")
	if (IS_INDEFD(yscale))
	    yscale = 1.0d0

	xrot = clgetd ("xrotation")
	if (IS_INDEFD(xrot))
	    xrot = 0.0d0
	yrot = clgetd ("yrotation")
	if (IS_INDEFD(yrot))
	    yrot = 0.0d0

	lngref = clgetd ("lngref")
	if (IS_INDEFD(lngref))
	    lngref = 0.0d0
	latref = clgetd ("latref")
	if (IS_INDEFD(latref))
	    latref = 0.0d0

        coostat = sk_decwcs ("j2000", mw, coo, NULL)
        if (coostat == ERR || mw != NULL) {
            if (mw != NULL)
                call mw_close (mw)
        }
        if (lngunits <= 0)
            tlngunits = sk_stati (coo, S_NLNGUNITS)
	else
	    tlngunits = lngunits
        call sk_seti (coo, S_NLNGUNITS, tlngunits)
        if (latunits <= 0)
            tlatunits = sk_stati (coo, S_NLATUNITS)
	else
	    tlatunits = latunits
        call sk_seti (coo, S_NLATUNITS, tlatunits)

        call clgstr ("projection", Memc[projstr], SZ_LINE)
        iferr {
            pfd = open (Memc[projstr], READ_ONLY, TEXT_FILE)
        } then {
            proj = strdic (Memc[projstr], Memc[projstr], SZ_LINE, WTYPE_LIST)
            if (proj <= 0 || proj == WTYPE_LIN)
                Memc[projstr] = EOS
        } else {
            proj = cc_rdproj (pfd, Memc[projstr], SZ_LINE)
            call close (pfd)
        }


	mw = cc_mkwcs (coo, Memc[projstr], lngref, latref, xref, yref,
	    xscale, yscale, xrot, yrot, false)

	call sfree (sp)
end


# CC_DTRECORD -- Read the transform from the database records written by
# CCMAP.

int procedure cc_dtrecord (dt, record, geometry, coo, projection,
	lngref, latref, sx1, sy1, sx2, sy2)

pointer dt                      #I pointer to the database
char    record[ARB]             #I the database records to be read
int	geometry		#I the transform geometry
pointer coo                     #O pointer to the coordinate structure
char    projection[ARB]         #O the sky projection geometry
double  lngref, latref          #O the reference point world coordinates
pointer sx1, sy1                #O pointer to the linear x and y fits
pointer sx2, sy2                #O pointer to the distortion x and y fits

int     i, op, ncoeff, junk, rec, coostat, lngunits, latunits
pointer mw, xcoeff, ycoeff, sp, projpar, projvalue
double  dtgetd()
int     dtlocate(), dtgeti(), dtscan(), sk_decwcs(), strdic(), strlen()
int	gstrcpy()
errchk	dgsrestore(), dtgstr(), dtdgetd(), dtgeti()

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
        call malloc (xcoeff, ncoeff, TY_DOUBLE)
        call malloc (ycoeff, ncoeff, TY_DOUBLE)
        do i = 1, ncoeff {
            junk = dtscan(dt)
            call gargd (Memd[xcoeff+i-1])
            call gargd (Memd[ycoeff+i-1])
        }

        # Restore the fit.
        call dgsrestore (sx1, Memd[xcoeff])
        call dgsrestore (sy1, Memd[ycoeff])

        # Get distortion part of fit.
        ncoeff = dtgeti (dt, rec, "surface2")
        if (ncoeff > 0 && (geometry == GEO_GEOMETRIC ||
            geometry == GEO_DISTORTION)) {
            call realloc (xcoeff, ncoeff, TY_DOUBLE)
            call realloc (ycoeff, ncoeff, TY_DOUBLE)
            do i = 1, ncoeff {
                junk = dtscan (dt)
                call gargd (Memd[xcoeff+i-1])
                call gargd (Memd[ycoeff+i-1])
            }

            # Restore distortion part of fit.
	    iferr {
                call dgsrestore (sx2, Memd[xcoeff])
	    } then {
		call mfree (sx2, TY_STRUCT)
		sx2 = NULL
	    }
	    iferr {
                call dgsrestore (sy2, Memd[ycoeff])
	    } then {
		call mfree (sy2, TY_STRUCT)
		sy2 = NULL
	    }

        } else {
            sx2 = NULL
            sy2 = NULL
        }

        # Get the projection parameters if any.
        call smark (sp)
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


	call mfree (xcoeff, TY_DOUBLE)
	call mfree (ycoeff, TY_DOUBLE)

	return (OK)
end


define	MAX_NITER	20

# CC_DO_TRANSFORM -- Transform the coordinates using the full transformation
# computed by CCMAP and the MWCS celestial coordinate wcs.

procedure cc_do_transform (x, y, xt, yt, ct, sx1, sy1, sx2, sy2, forward)

double  x, y                    #I initial positions
double  xt, yt                  #O transformed positions
pointer	ct			#I pointer to the mwcs transform
pointer sx1, sy1                #I pointer to linear surfaces
pointer sx2, sy2                #I pointer to distortion surfaces
bool	forward			#I forward transform

double	xm, ym, f, fx, fy, g, gx, gy, denom, dx, dy
int	niter
pointer	sumsx, sumsy, newsx, newsy
double  dgseval()

begin

	if (forward) {

            xm = dgseval (sx1, x, y)
            if (sx2 != NULL)
                xm = xm + dgseval (sx2, x, y)
            ym = dgseval (sy1, x, y)
            if (sy2 != NULL)
                ym = ym + dgseval (sy2, x, y)
	    xm = xm / 3600.0d0
	    ym = ym / 3600.0d0

	    call mw_c2trand (ct, xm, ym, xt, yt)

	} else {

	    # Use a value of 1.0 for an initial guess at the plate scale. 
	    call mw_c2trand (ct, x, y, xm, ym)
	    xm = xm * 3600.0d0
	    ym = ym * 3600.0d0

	    call dgsadd (sx1, sx2, sumsx)
	    call dgsadd (sy1, sy2, sumsy)

	    niter = 0
	    xt = xm 
	    yt = ym
	    repeat {

		if (niter == 0) {
		    newsx = sx1
		    newsy = sy1
		} else if (niter == 1) {
		    newsx = sumsx
		    newsy = sumsy
		}

	        f = dgseval (newsx, xt, yt) - xm
	        call dgsder (newsx, xt, yt, fx, 1, 1, 0)
	        call dgsder (newsx, xt, yt, fy, 1, 0, 1)

	        g = dgseval (newsy, xt, yt) - ym
	        call dgsder (newsy, xt, yt, gx, 1, 1, 0)
	        call dgsder (newsy, xt, yt, gy, 1, 0, 1)

		denom = fx * gy - fy * gx
		if (denom == 0.0d0)
		    break
		dx = (-f * gy + g * fy) / denom
		dy = (-g * fx + f * gx) / denom
		xt = xt + dx
		yt = yt + dy
		if (max (abs (dx), abs (dy), abs(f), abs(g)) < 1.0e-5)
		    break

		niter = niter + 1

	    } until (niter >= MAX_NITER)

	    call dgsfree (sumsx)
	    call dgsfree (sumsy)
	}
end

define  NEWCD     Memd[cd+(($2)-1)*ndim+($1)-1]

# CC_MKWCS -- Compute the wcs from the user parameters.

pointer procedure cc_mkwcs (coo, projection, lngref, latref, xref, yref,
        xscale, yscale, xrot, yrot, transpose)

pointer coo                     #I pointer to the coordinate structure
char    projection[ARB]         #I the sky projection geometry
double  lngref, latref          #I the world coordinates of the reference point
double  xref, yref              #I the reference point in pixels
double  xscale, yscale          #I the x and y scale in arcsec / pixel
double  xrot, yrot              #I the x and y axis rotation angles in degrees
bool    transpose               #I transpose the wcs

int     ndim
double  tlngref, tlatref
pointer sp, axes, ltm, ltv, r, w, cd, mw, projstr, projpars, wpars
int     sk_stati()
pointer mw_open()

begin
	# Open the wcs.
        ndim = 2
        mw = mw_open (NULL, ndim)

        # Allocate working space.
        call smark (sp)
        call salloc (projstr, SZ_FNAME, TY_CHAR)
        call salloc (projpars, SZ_LINE, TY_CHAR)
        call salloc (wpars, SZ_LINE, TY_CHAR)
        call salloc (axes, ndim, TY_INT)
        call salloc (ltm, ndim * ndim, TY_DOUBLE)
        call salloc (ltv, ndim, TY_DOUBLE)
        call salloc (r, ndim, TY_DOUBLE)
        call salloc (w, ndim, TY_DOUBLE)
        call salloc (cd, ndim * ndim, TY_DOUBLE)

        # Set the wcs.
        iferr (call mw_newsystem (mw, "image", ndim))
            ;

        # Set the axes.
        Memi[axes] = 1
        Memi[axes+1] = 2

        # Set the axes and projection type.
        if (projection[1] == EOS) {
            call mw_swtype (mw, Memi[axes], ndim, "linear", "")
        } else {
            call sscan (projection)
                call gargwrd (Memc[projstr], SZ_FNAME)
                call gargstr (Memc[projpars], SZ_LINE)
            call sprintf (Memc[wpars], SZ_LINE,
                "axis 1: axtype = ra %s axis 2: axtype = dec %s")
                call pargstr (Memc[projpars])
                call pargstr (Memc[projpars])
            call mw_swtype (mw, Memi[axes], ndim, Memc[projstr], Memc[wpars])
        }

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
            Memd[w] = tlngref
            Memd[w+1] = tlatref
        } else {
            Memd[w+1] = tlngref
            Memd[w] = tlatref
        }

        # Compute the reference point pixel coordinates.
        Memd[r] = xref
        Memd[r+1] = yref

        # Compute the new CD matrix.
        if (! transpose) {
            NEWCD(1,1) = xscale * cos (DEGTORAD(xrot)) / 3600.0d0
            NEWCD(2,1) = -yscale * sin (DEGTORAD(yrot)) / 3600.0d0
            NEWCD(1,2) = xscale * sin (DEGTORAD(xrot)) / 3600.0d0
            NEWCD(2,2) = yscale * cos (DEGTORAD(yrot)) / 3600.0d0
        } else {
            NEWCD(1,1) = xscale * sin (DEGTORAD(xrot)) / 3600.0d0
            NEWCD(2,1) = yscale * cos (DEGTORAD(yrot)) / 3600.0d0
            NEWCD(1,2) = xscale * cos (DEGTORAD(xrot)) / 3600.0d0
            NEWCD(2,2) = -yscale * sin (DEGTORAD(yrot)) / 3600.0d0
        }

        # Compute the Lterm.
        call aclrd (Memd[ltv], ndim)
        call mw_mkidmd (Memd[ltm], ndim)

        # Store the wcs.
        call mw_sltermd (mw, Memd[ltm], Memd[ltv], ndim)
        call mw_swtermd (mw, Memd[r], Memd[w], Memd[cd], ndim)

        call sfree (sp)

        return (mw)
end

# CC_GEOWCS -- Create the wcs from the geometric transformation computed
# by CCMAP

pointer procedure cc_geowcs (coo, projection, lngref, latref, sx1, sy1,
        transpose)

pointer coo                     #I the pointer to the coordinate structure
char    projection[ARB]         #I the sky projection geometry
double  lngref, latref          #I the coordinates of the reference point
pointer sx1, sy1                #I pointer to linear surfaces
bool    transpose               #I transpose the wcs

int     ndim
double  xshift, yshift, a, b, c, d, denom, xpix, ypix, tlngref, tlatref
pointer mw, sp, projstr, projpars, wpars, r, w, cd, ltm, ltv, axes
int     sk_stati()
pointer mw_open()

begin
        ndim = 2
        mw = mw_open (NULL, ndim)

        # Allocate working memory for the vectors and matrices.
        call smark (sp)
        call salloc (projstr, SZ_FNAME, TY_CHAR)
        call salloc (projpars, SZ_LINE, TY_CHAR)
        call salloc (wpars, SZ_LINE, TY_CHAR)
        call salloc (axes, 2, TY_INT)
        call salloc (r, ndim, TY_DOUBLE)
        call salloc (w, ndim, TY_DOUBLE)
        call salloc (cd, ndim * ndim, TY_DOUBLE)
        call salloc (ltm, ndim * ndim, TY_DOUBLE)
        call salloc (ltv, ndim, TY_DOUBLE)

        # Set the wcs.
        iferr (call mw_newsystem (mw, "image", ndim))
            ;

        # Set the axes.
        Memi[axes] = 1
        Memi[axes+1] = 2

        # Set the axes and projection type.
        if (projection[1] == EOS) {
            call mw_swtype (mw, Memi[axes], ndim, "linear", "")
        } else {
            call sscan (projection)
                call gargwrd (Memc[projstr], SZ_FNAME)
                call gargstr (Memc[projpars], SZ_LINE)
            call sprintf (Memc[wpars], SZ_LINE,
                "axis 1: axtype = ra %s axis 2: axtype = dec %s")
                call pargstr (Memc[projpars])
                call pargstr (Memc[projpars])
            call mw_swtype (mw, Memi[axes], ndim, Memc[projstr], Memc[wpars])
        }

        # Compute the new referemce point.
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
            Memd[w] = tlngref
            Memd[w+1] = tlatref
        } else {
            Memd[w] = tlatref
            Memd[w+1] = tlngref
        }


        # Fetch the linear coefficients of the fit.
        call geo_gcoeffd (sx1, sy1, xshift, yshift, a, b, c, d)

        # Compute the new reference pixel.
        denom = a * d - c * b
        if (denom == 0.0d0)
            xpix = INDEFD
        else
            xpix = (b * yshift - d * xshift) / denom
        if (denom == 0.0d0)
            ypix = INDEFD
        else
            ypix =  (c * xshift - a * yshift) / denom
        Memd[r] = xpix
        Memd[r+1] = ypix

        # Compute the new CD matrix.
        if (! transpose) {
            NEWCD(1,1) = a / 3600.0d0
            NEWCD(1,2) = c / 3600.0d0
            NEWCD(2,1) = b / 3600.0d0
            NEWCD(2,2) = d / 3600.0d0
        } else {
            NEWCD(1,1) = c / 3600.0d0
            NEWCD(1,2) = a / 3600.0d0
            NEWCD(2,1) = d / 3600.0d0
            NEWCD(2,2) = b / 3600.0d0
        }

        # Compute the Lterm.
        call aclrd (Memd[ltv], ndim)
        call mw_mkidmd (Memd[ltm], ndim)

        # Recompute and store the new wcs if update is enabled.
        call mw_sltermd (mw, Memd[ltm], Memd[ltv], ndim)
        call mw_swtermd (mw, Memd[r], Memd[w], Memd[cd], ndim)

        call sfree (sp)

        return (mw)
end




# CC_CELWCS -- Create a wcs which compute the projection part of the
# transformation only

pointer procedure cc_celwcs (coo, projection, lngref, latref)

pointer coo                     #I the pointer to the coordinate structure
char    projection[ARB]         #I the sky projection geometry
double  lngref, latref          #I the position of the reference point.

int     ndim
pointer sp, projstr, projpars, wpars, ltm, ltv, cd, r, w, axes, mw
int     sk_stati()
pointer mw_open()

begin
        # Open the wcs.
        ndim = 2
        mw = mw_open (NULL, ndim)

        # Allocate working space.
        call smark (sp)
        call salloc (projstr, SZ_FNAME, TY_CHAR)
        call salloc (projpars, SZ_LINE, TY_CHAR)
        call salloc (wpars, SZ_LINE, TY_CHAR)
        call salloc (ltm, ndim * ndim, TY_DOUBLE)
        call salloc (ltv, ndim, TY_DOUBLE)
        call salloc (cd, ndim * ndim, TY_DOUBLE)
        call salloc (r, ndim, TY_DOUBLE)
        call salloc (w, ndim, TY_DOUBLE)
        call salloc (axes, 2, TY_INT)


        # Set the wcs.
        iferr (call mw_newsystem (mw, "image", ndim))
            ;

        # Set the axes and projection type.
        Memi[axes] = 1
        Memi[axes+1] = 2
        if (projection[1] == EOS) {
            call mw_swtype (mw, Memi[axes], ndim, "linear", "")
        } else {
            call sscan (projection)
                call gargwrd (Memc[projstr], SZ_FNAME)
                call gargstr (Memc[projpars], SZ_LINE)
            call sprintf (Memc[wpars], SZ_LINE,
                "axis 1: axtype = ra %s axis 2: axtype = dec %s")
                call pargstr (Memc[projpars])
                call pargstr (Memc[projpars])
            call mw_swtype (mw, Memi[axes], ndim, Memc[projstr], Memc[wpars])
        }

        # Set the lterm.
        call mw_mkidmd (Memd[ltm], ndim)
        call aclrd (Memd[ltv], ndim)
        call mw_sltermd (mw, Memd[ltm], Memd[ltv], ndim)

        # Set the wterm.
        call mw_mkidmd (Memd[cd], ndim)
        call aclrd (Memd[r], ndim)
        switch (sk_stati(coo, S_NLNGUNITS)) {
        case SKY_DEGREES:
            Memd[w] = lngref
        case SKY_RADIANS:
            Memd[w] = RADTODEG(lngref)
        case SKY_HOURS:
            Memd[w] = 15.0d0 * lngref
        default:
            Memd[w] = lngref
        }
        switch (sk_stati(coo, S_NLATUNITS)) {
        case SKY_DEGREES:
            Memd[w+1] = latref
        case SKY_RADIANS:
            Memd[w+1] = RADTODEG(latref)
        case SKY_HOURS:
            Memd[w+1] = 15.0d0 * latref
        default:
            Memd[w+1] = latref
        }
        call mw_swtermd (mw, Memd[r], Memd[w], Memd[cd], ndim)

        call sfree (sp)

        return (mw)
end


