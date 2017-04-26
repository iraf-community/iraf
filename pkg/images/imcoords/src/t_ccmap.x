# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <ctype.h>
include <math.h>
include <math/gsurfit.h>
include <imhdr.h>
include <pkg/skywcs.h>
include "../../lib/geomap.h"

# Define the source of the reference point.
define	CC_REFPOINTSTR		"|coords|user|tweak|"
define	CC_COORDS		1
define	CC_USER			2
define	CC_TWEAK		3

# Define the possible pixel types.
define  CC_PIXTYPESTR           "|logical|physical|"
define  CC_LOGICAL              1
define  CC_PHYSICAL             2

# Define some limits on the input file
define	MAX_FIELDS		100     # the max number of fields in the list
define	TABSIZE			8       # the spacing of the tab stops

# Define the default data buffer size
define	CC_DEFBUFSIZE	1000		# the default buffer size

# T_CCMAP -- Compute the linear portion of the transformation required
# to convert image  x and y coordinates to ra / longitude and dec / latitude
# coordinates.  This version allows combining multiple inputs with different
# tangent points (as in a dither set) to create a single solution.

procedure t_ccmap ()

pointer	in, im, tdxref, tdyref, tdlngref, tdlatref
pointer	sp, infile, image, database, insystem, refsystem, str
pointer	xref, yref, lngref, latref
pointer	graphics, coo, refcoo, tcoo, mw, fit, out, gd, projstr
double	dxref, dyref, dlngref, dlatref, xmin, xmax, ymin, ymax, reject
int	i, inlist, ninfiles, nin, imlist, nimages, coostat, refstat, nchars, ip 
int	xreflist, yreflist, lngreflist, latreflist
int	xcolumn, ycolumn, lngcolumn, latcolumn, lngunits, latunits, res, pfd
int	lngrefunits, latrefunits, refpoint_type, tweak, projection
int	reslist, nresfiles
int	geometry, function, xxorder, xyorder, xxterms, yxorder, yyorder, yxterms
int	reclist, nrecords, pixsys, maxiter
bool	verbose, update, interactive

double	clgetd()
pointer	dtmap(), immap(), gopen(), cc_utan(), cc_imtan()
int	clpopnu(), clplen(), imtopenp(), imtlen(), clgeti(), clgwrd(), strlen()
int	sk_decwcs(), sk_stati(), imtgetim(), clgfil(), open(), ctod()
int	errget(), imtopen(), strncmp(), cc_rdproj(), strdic()
bool	clgetb()
errchk	open(), cc_map()

begin
	# Get some working space.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (insystem, SZ_FNAME, TY_CHAR)
	call salloc (xref, SZ_FNAME, TY_CHAR)
	call salloc (yref, SZ_FNAME, TY_CHAR)
	call salloc (lngref, SZ_FNAME, TY_CHAR)
	call salloc (latref, SZ_FNAME, TY_CHAR)
	call salloc (refsystem, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (projstr, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the input data file list.
	inlist = clpopnu ("input")
	ninfiles = clplen (inlist)
	if (ninfiles <= 0) {
	    call eprintf ("Error: The input coordinate file list is empty\n")
	    call clpcls (inlist)
	    call sfree (sp)
	    return
	}

	# Open the database output file.
	call clgstr ("database", Memc[database], SZ_FNAME)
	out = dtmap (Memc[database], APPEND)

        # Open the record list.
        call clgstr ("solutions", Memc[str], SZ_FNAME)
        if (Memc[str] == EOS) {
            reclist = NULL
            nrecords = 0
        } else {
            reclist = imtopen (Memc[str])
            nrecords = imtlen (reclist)
        }
        if (nrecords > 1 && nrecords != ninfiles) {
            call eprintf ("Error: List of record names does not match input\n")
            call clpcls (inlist)
            call dtunmap (out)
            if (reclist != NULL)
                call imtclose (reclist)
            call sfree (sp)
            return
        }

	# Get the input image list.
	imlist = imtopenp ("images")
	nimages = imtlen (imlist)
	if (nimages > 1 && nimages != ninfiles) {
	    call eprintf ("Error: Coordinate files and images don't match\n")
	    call imtclose (imlist)
	    call clpcls (inlist)
            call dtunmap (out)
            if (reclist != NULL)
                call imtclose (reclist)
            call sfree (sp)
            return
	}

	# Get the output results lists.
	reslist = clpopnu ("results")
	nresfiles = clplen (reslist)
	if (nresfiles > 1 && nresfiles != ninfiles) {
	    call eprintf ("Error: List of results files does not match input\n")
	    call imtclose (imlist)
	    call clpcls (inlist)
	    call clpcls (reslist)
	    call dtunmap (out)
            if (reclist != NULL)
                call imtclose (reclist)
	    call sfree (sp)
	    return
	}

	# Get the coordinates file format.
	xcolumn = clgeti ("xcolumn")
	ycolumn = clgeti ("ycolumn")
	lngcolumn = clgeti ("lngcolumn")
	latcolumn = clgeti ("latcolumn")
	call clgstr ("insystem", Memc[insystem], SZ_FNAME)
	iferr (lngunits = clgwrd ("lngunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    lngunits = 0
	iferr (latunits = clgwrd ("latunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    latunits = 0

	# Get the reference point parameters.
	refpoint_type = clgwrd ("refpoint", Memc[str], SZ_FNAME,
	    CC_REFPOINTSTR)
	tweak = refpoint_type
	xreflist = clpopnu ("xref")
	yreflist = clpopnu ("yref")
	lngreflist = clpopnu ("lngref")
	latreflist = clpopnu ("latref")
	call clgstr ("refsystem", Memc[refsystem], SZ_FNAME)
	if (strncmp (Memc[refsystem], "INDEF", 5) == 0)
	    Memc[refsystem] = EOS
	iferr (lngrefunits = clgwrd ("lngrefunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    lngrefunits = 0
	iferr (latrefunits = clgwrd ("latrefunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    latrefunits = 0

	# Get the minimum and maximum reference values.
	xmin = clgetd ("xmin")
	xmax = clgetd ("xmax")
	ymin = clgetd ("ymin")
	ymax = clgetd ("ymax")

	# Get the coordinate mapping parameters.
        call clgstr ("projection", Memc[str], SZ_LINE)
        iferr {
            pfd = open (Memc[str], READ_ONLY, TEXT_FILE)
        } then {
            projection = strdic (Memc[str], Memc[str], SZ_LINE, GM_PROJLIST)
            if (projection <= 0 || projection == WTYPE_LIN)
                Memc[projstr] = EOS
            else
                call strcpy (Memc[str], Memc[projstr], SZ_LINE)
        } else {
            projection = cc_rdproj (pfd, Memc[projstr], SZ_LINE)
            call close (pfd)
        }
	geometry = clgwrd ("fitgeometry", Memc[str], SZ_LINE, GM_GEOMETRIES)
	function = clgwrd ("function", Memc[str], SZ_LINE, GM_FUNCS)
	xxorder = clgeti ("xxorder")
	xyorder = clgeti ("xyorder")
	xxterms = clgwrd ("xxterms", Memc[str], SZ_LINE, GM_XFUNCS) - 1
	yxorder = clgeti ("yxorder")
	yyorder = clgeti ("yyorder")
	yxterms = clgwrd ("yxterms", Memc[str], SZ_LINE, GM_XFUNCS) - 1
	maxiter = clgeti ("maxiter")
	reject = clgetd ("reject")

	# Get the input and output parameters.
	update = clgetb ("update")
        iferr (pixsys = clgwrd ("pixsystem", Memc[str], SZ_FNAME,
            CC_PIXTYPESTR))
            pixsys = PIXTYPE_LOGICAL
        else if (pixsys == CC_PHYSICAL)
            pixsys = PIXTYPE_PHYSICAL
        else
            pixsys = PIXTYPE_LOGICAL
	verbose = clgetb ("verbose")

	# Open the input coordinate system.
	coostat = sk_decwcs (Memc[insystem], mw, coo, NULL) 
	if (coostat == ERR || mw != NULL) {
	    call eprintf ("Error: Cannot decode the input coordinate system\n")
	    if (mw != NULL)
		call mw_close (mw)
	    call imtclose (imlist)
	    call clpcls (inlist)
	    call clpcls (reslist)
	    call dtunmap (out)
	    call sfree (sp)
	    return
	}

	# Determine the units of the input coordinate system.
	if (lngunits <= 0)
	    lngunits = sk_stati (coo, S_NLNGUNITS)
	call sk_seti (coo, S_NLNGUNITS, lngunits)
	if (latunits <= 0)
	    latunits = sk_stati (coo, S_NLATUNITS)
	call sk_seti (coo, S_NLATUNITS, latunits)
        call sk_seti (coo, S_PIXTYPE, pixsys)

	# Set default reference coordinate.
	Memc[xref] = EOS
	Memc[yref] = EOS
	Memc[lngref] = EOS
	Memc[latref] = EOS

	# Open the reference coordinate system if possible.
	refstat = sk_decwcs (Memc[refsystem], mw, refcoo, NULL)
	if (refstat == ERR || mw != NULL) {
	    if (mw != NULL)
	        call mw_close (mw)
	    refcoo = NULL
	    if (lngrefunits <= 0)
	        lngrefunits = sk_stati (coo, S_NLNGUNITS)
	    if (latrefunits <= 0)
	        latrefunits = sk_stati (coo, S_NLATUNITS)
	} else {
	    if (lngrefunits <= 0)
	        lngrefunits = sk_stati (refcoo, S_NLNGUNITS)
	    call sk_seti (refcoo, S_NLNGUNITS, lngrefunits)
	    if (latrefunits <= 0)
	        latrefunits = sk_stati (refcoo, S_NLATUNITS)
	    call sk_seti (refcoo, S_NLATUNITS, latrefunits)
	}

	# Get the graphics parameters.
	interactive = clgetb ("interactive")
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)

	# Flush standard output on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize the coordinate mapping structure.
	call geo_minit (fit, projection, geometry, function, xxorder, xyorder,
	    xxterms, yxorder, yyorder, yxterms, maxiter, reject)
	call strcpy (Memc[projstr], GM_PROJSTR(fit), SZ_LINE)

	# Process the input.
	call calloc (in, ninfiles, TY_INT)
	call calloc (im, ninfiles, TY_POINTER)
	call calloc (tdxref, ninfiles, TY_DOUBLE)
	call calloc (tdyref, ninfiles, TY_DOUBLE)
	call calloc (tdlngref, ninfiles, TY_DOUBLE)
	call calloc (tdlatref, ninfiles, TY_DOUBLE)
	call amovkd (INDEFD, Memd[tdxref], ninfiles)
	call amovkd (INDEFD, Memd[tdyref], ninfiles)
	call amovkd (INDEFD, Memd[tdlngref], ninfiles)
	call amovkd (INDEFD, Memd[tdlatref], ninfiles)

	# Loop over the files.  This is a little messy in order to allow
	# both the case where all inputs are combined or separately done.
	repeat {

	    nin = 0
	    while (clgfil (inlist, Memc[infile], SZ_FNAME) != EOF) {

		# Open text file of coordinates.
		Memi[in+nin] = open (Memc[infile], READ_ONLY, TEXT_FILE)

		# Open the input image.
		if (nimages > 0) {
		    if (imtgetim (imlist, Memc[image], SZ_FNAME) == EOF) {
			Memi[im+nin] = NULL
		    } else if (update) {
			Memi[im+nin] = immap (Memc[image], READ_WRITE, 0)
		    } else {
			Memi[im+nin] = immap (Memc[image], READ_ONLY, 0)
		    }
		    if (Memi[im+nin] != NULL) {
			if (IM_NDIM(Memi[im+nin]) != 2) {
			    call printf ("Skipping file: %s Image: %s is not 2D\n")
				call pargstr (Memc[infile])
				call pargstr (Memc[image])
			    call imunmap (Memi[im+nin])
			    next
			}
		    } else
			Memc[image] = EOS
		} else {
		    Memi[im+nin] = NULL
		    Memc[image] = EOS
		}

		if (nin == 0) {

		    # Open the results file.
		    if (nresfiles <= 0)
			res = NULL
		    else if (clgfil (reslist, Memc[str], SZ_FNAME) != EOF)
			res = open (Memc[str], NEW_FILE, TEXT_FILE)
		    else
		        res = NULL

		    # Set the output file record name.
		    if (nrecords > 0) {
			if (imtgetim (reclist, GM_RECORD(fit), SZ_FNAME) != EOF)
			    ;
		    } else if (Memi[im] == NULL) {
			call strcpy (Memc[infile], GM_RECORD(fit), SZ_FNAME)
		    } else {
			#call imgimage (Memc[image], Memc[str], SZ_FNAME)
			call strcpy (Memc[image], GM_RECORD(fit), SZ_FNAME)
		    }
		}

		# Determine the coordinates of the reference point if possible.
	    	if (clgfil (xreflist, Memc[xref], SZ_FNAME) == EOF)
		    ;
	    	if (clgfil (yreflist, Memc[yref], SZ_FNAME) == EOF)
		    ;
	    	if (clgfil (lngreflist, Memc[lngref], SZ_FNAME) == EOF)
		    ;
	    	if (clgfil (latreflist, Memc[latref], SZ_FNAME) == EOF)
		    ;
		ip = 1
		nchars = ctod (Memc[xref], ip, dxref)
		if (nchars <= 0 || nchars != strlen (Memc[xref]))
		    dxref = INDEFD
		ip = 1
		nchars = ctod (Memc[yref], ip, dyref)
		if (nchars <= 0 || nchars != strlen (Memc[yref]))
		    dyref = INDEFD
		ip = 1
		nchars = ctod (Memc[lngref], ip, dlngref)
		if (nchars <= 0 || nchars != strlen (Memc[lngref]))
		    dlngref = INDEFD
		if (dlngref < 0.0d0 || dlngref > 360.0d0)
		    dlngref = INDEFD
		ip = 1
		nchars = ctod (Memc[latref], ip, dlatref)
		if (nchars <= 0 || nchars != strlen (Memc[latref]))
		    dlatref = INDEFD
		if (dlatref < -90.0d0 || dlatref > 90.0d0)
		    dlatref = INDEFD

		Memd[tdxref+nin] = dxref
		Memd[tdyref+nin] = dyref
		Memd[tdlngref+nin] = dlngref
		Memd[tdlatref+nin] = dlatref

		# Determine the tangent points and convert them to the
		# celestial coordinate system of the input data, 

		# The tangent point will be determined directly from
		# the input coordinates.
		if (refpoint_type == CC_COORDS) {

		    if (nin == 0) {
			if (verbose && res != STDOUT)
			    call sk_iiprint ("Refsystem", Memc[insystem],
				NULL, coo)
			if (res != NULL)
			    call sk_iiwrite (res, "Refsystem", Memc[insystem],
				NULL, coo)
		    }
		    Memd[tdxref+nin] = INDEFD
		    Memd[tdyref+nin] = INDEFD
		    Memd[tdlngref+nin] = INDEFD
		    Memd[tdlatref+nin] = INDEFD

		# The tangent point was set by the user and a tangent point
		# reference system may or may not have been defined.
		} else if (! IS_INDEFD(dlngref) && ! IS_INDEFD (dlatref)) {

		    tcoo = cc_utan (refcoo, coo, dxref, dyref, dlngref, dlatref,
		        Memd[tdlngref+nin], Memd[tdlatref+nin],
			lngrefunits, latrefunits)
		    call sk_stats (tcoo, S_COOSYSTEM, Memc[str], SZ_FNAME) 
		    if (nin == 0) {
			if (verbose && res != STDOUT)
			    call sk_iiprint ("Refsystem", Memc[str], NULL, tcoo)
			if (res != NULL)
			    call sk_iiwrite (res, "Refsystem", Memc[str],
				NULL, tcoo)
			call sk_close (tcoo)
		    }

		} else if (Memi[im+nin] != NULL) {

		    tcoo = cc_imtan (Memi[im+nin], Memc[xref], Memc[yref],
			Memc[lngref], Memc[latref], Memc[refsystem],
			refcoo, coo, Memd[tdxref+nin], Memd[tdyref+nin],
			Memd[tdlngref+nin], Memd[tdlatref+nin],
			lngrefunits, latrefunits)
		    call sk_stats (tcoo, S_COOSYSTEM, Memc[str], SZ_FNAME) 
		    if (nin == 0) {
			if (verbose && res != STDOUT)
			    call sk_iiprint ("Refsystem", Memc[str], NULL, tcoo)
			if (res != NULL)
			    call sk_iiwrite (res, "Refsystem", Memc[str],
				NULL, tcoo)
			call sk_close (tcoo)
		    }

		# The tangent point will be determined directly from
		# the input coordinates.
		} else {

		    if (nin == 0) {
			if (verbose && res != STDOUT)
			    call sk_iiprint ("Refsystem", Memc[insystem],
				NULL, coo)
			if (res != NULL)
			    call sk_iiwrite (res, "Refsystem", Memc[insystem],
				NULL, coo)
		    }
		    Memd[tdxref+nin] = INDEFD
		    Memd[tdyref+nin] = INDEFD
		    Memd[tdlngref+nin] = INDEFD
		    Memd[tdlatref+nin] = INDEFD

		}

		if (nin == 0) {
		    # Print information about the input coordinate system.
		    if (verbose && res != STDOUT)
			call sk_iiprint ("Insystem", Memc[insystem], NULL, coo)
		    if (res != NULL)
			call sk_iiwrite (res, "Insystem", Memc[insystem],
			    NULL, coo)
		}

		# Print the input and out file information.
		if (verbose && res != STDOUT) {
		    call printf ("\nCoords File: %s  Image: %s\n")
			call pargstr (Memc[infile])
			call pargstr (Memc[image])
		    call printf ("    Database: %s  Solution: %s\n")
			call pargstr (Memc[database])
			call pargstr (GM_RECORD(fit))
		}
		if (res != NULL) {
		    call fprintf (res, "\n# Coords File: %s  Image: %s\n")
			call pargstr (Memc[infile])
			call pargstr (Memc[image])
		    call fprintf (res, "#     Database: %s  Solution: %s\n")
			call pargstr (Memc[database])
			call pargstr (GM_RECORD(fit))
		}

		nin = nin + 1
		if (nrecords > 1 || nresfiles > 1)
		    break
	    }
	    if (nin == 0)
	        break

	    iferr { 
		if (interactive)
	            gd = gopen (Memc[graphics], NEW_FILE, STDGRAPH)
		else
	            gd = NULL
		call cc_map (gd, nin, Memi[in], out, Memi[im], res, coo, fit,
		    xcolumn, ycolumn, lngcolumn, latcolumn, tweak,
		    Memd[tdxref], Memd[tdyref], Memd[tdlngref], Memd[tdlatref],
		    xmin, xmax, ymin, ymax, update, verbose)
		if (gd != NULL)
	    	    call gclose (gd)
	    } then {
		if (verbose && res != STDOUT) {
		    if (nin == 1) {
			call printf ("Error fitting coordinate list: %s\n")
			    call pargstr (Memc[infile])
		    } else
			call printf ("Error fitting coordinate lists\n")
		    call flush (STDOUT)
		    Memc[str] = EOS
		    if (errget (Memc[str], SZ_LINE) == 0)
			;
		    call printf ("    %s\n")
			call pargstr (Memc[str])
		}
		if (res != NULL) {
		    if (nin == 1) {
			call fprintf (res,
			    "# Error fitting coordinate list: %s\n")
			    call pargstr (Memc[infile])
		    } else
			call fprintf (res,
			    "# Error fitting coordinate lists\n")
		    call flush (STDOUT)
		    if (errget (Memc[str], SZ_LINE) == 0)
			;
		    call fprintf (res, "#     %s\n")
			call pargstr (Memc[str])
		}
		if (gd != NULL)
		    call gclose (gd)
	    }


	    if (nresfiles == ninfiles)
		call close (res)
	    do i = 1, nin {
		call close (Memi[in+i-1])
		if (Memi[im+i-1] != NULL)
		    call imunmap (Memi[im+i-1])
	    }
	}

	call mfree (in, TY_INT)
	call mfree (im, TY_POINTER)
	call mfree (tdxref, TY_DOUBLE)
	call mfree (tdyref, TY_DOUBLE)
	call mfree (tdlngref, TY_DOUBLE)
	call mfree (tdlatref, TY_DOUBLE)

	call geo_free (fit)
	call sk_close (coo)
	call clpcls (xreflist)
	call clpcls (yreflist)
	call clpcls (latreflist)
	call clpcls (lngreflist)
	if (nresfiles < ninfiles)
	    call close (res)
	call dtunmap (out)
        if (reclist != NULL)
            call imtclose (reclist)
	call imtclose (imlist)
	call clpcls (inlist)
	call clpcls (reslist)
	call sfree (sp)
end


# CC_UTAN -- Convert the user defined tangent point from the reference
# point celestial coordinate system to the input coordinate celestial
# coordinate system.

pointer	procedure cc_utan (refcoo, coo, idxref, idyref, idlngref, idlatref, odlngref, odlatref,
	lngrefunits, latrefunits)

pointer	refcoo			#I pointer to the reference point system
pointer	coo			#I pointer to the input coordinate system
double	idxref			#I the input x reference point
double	idyref			#I the input y reference point
double	idlngref		#I the input reference point ra / longitude
double	idlatref		#I the input reference point dec / latitude
double	odxref			#O the output x reference point
double	odyref			#O the output y reference point
double	odlngref		#O the output reference point ra / longitude
double	odlatref		#O the output reference point dec / latitude
int	lngrefunits		#I the input reference ra / longitude units
int	latrefunits		#I the input reference dec / latitude units

pointer	trefcoo
pointer	sk_copy()

begin
	odxref = idxref
	odyref = idyref
	if (refcoo != NULL) {
	    trefcoo = sk_copy (refcoo)
	} else {
	    trefcoo = sk_copy (coo)
	    call sk_seti (trefcoo, S_NLNGUNITS, lngrefunits)
	    call sk_seti (trefcoo, S_NLATUNITS, latrefunits)
	}
	call sk_ultran (trefcoo, coo, idlngref, idlatref, odlngref, odlatref, 1)

	return (trefcoo)
end


# CC_IMTAN -- Read the tangent point from the image and convert it from the
# reference point celestial coordinate system to the input coordinate celestial
# coordinate system.

pointer	procedure cc_imtan (im, xref, yref, lngref, latref, refsystem, refcoo,
	coo, odxref, odyref, odlngref, odlatref, lngrefunits, latrefunits)

pointer	im			#I pointer to the input image
char	xref[ARB]		#I the x reference keyword
char	yref[ARB]		#I the y reference keyword
char	lngref[ARB]		#I the ra / longitude keyword
char	latref[ARB]		#I the dec / latitude keyword
char	refsystem[ARB]		#I the reference point coordinate system
pointer	refcoo			#I pointer to the reference point system
pointer	coo			#I pointer to the input coordinate system
double	odxref			#O the output x reference point
double	odyref			#O the output y reference point
double	odlngref		#O the output reference point ra / longitude
double	odlatref		#O the output reference point dec / latitude
int	lngrefunits		#I the input reference ra / longitude units
int	latrefunits		#I the input reference dec / latitude units

double	idxref, idyref, idlngref, idlatref, idepoch
pointer	sp, str, tcoo, mw
double	imgetd()
pointer	sk_copy()
int	sk_decwcs()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	iferr (idxref = imgetd (im, xref))
	    idxref = INDEFD
	iferr (idyref = imgetd (im, yref))
	    idyref = INDEFD
	iferr (idlngref = imgetd (im, lngref))
	    idlngref = INDEFD
	if (idlngref < 0.0d0 || idlngref > 360.0d0)
	    idlngref = INDEFD
	iferr (idlatref = imgetd (im, latref))
	    idlatref = INDEFD
	if (idlatref < -90.0d0 || idlatref > 90.0d0)
	    idlatref = INDEFD

	if (!IS_INDEFD(idxref))
	    odxref = idxref
	if (!IS_INDEFD(idyref))
	    odyref = idyref

	if (IS_INDEFD(idlngref) || IS_INDEFD(idlatref))
	    tcoo = sk_copy (coo)
	else if (refcoo != NULL) {
	    tcoo = sk_copy (refcoo)
	    call sk_ultran (tcoo, coo, idlngref, idlatref, odlngref,
	        odlatref, 1) 
	} else {
	    iferr (idepoch = imgetd (im, refsystem))
		idepoch = INDEFD
	    if (IS_INDEFD(idepoch))
		tcoo = sk_copy (coo)
	    else {
		call sprintf (Memc[str], SZ_FNAME, "fk4 b%g")
		    call pargd (idepoch)
		if (sk_decwcs (Memc[str], mw, tcoo, NULL) == ERR) {
		    call sk_close (tcoo)
		    tcoo = sk_copy (coo)
		}
		if (mw != NULL)
		    call mw_close (mw)
	    }
	    call sk_seti (tcoo, S_NLNGUNITS, lngrefunits)
	    call sk_seti (tcoo, S_NLATUNITS, latrefunits)
	    call sk_ultran (tcoo, coo, idlngref, idlatref, odlngref,
		odlatref, 1) 
	}

	call sfree (sp)

	return (tcoo)
end


# CC_MAP -- Compute the required coordinate transformation.
#
# This version uses the nin variable.

procedure cc_map (gd, nin, in, out, im, res, coo, fit,
	xcolumn, ycolumn, lngcolumn, latcolumn, tweak,
	xtan, ytan, ratan, dectan,
	xmin, xmax, ymin, ymax, update, verbose)

pointer	gd			#I graphics stream pointer
int	nin			#I number of input files
int	in[ARB]			#I the input file descriptors
pointer	out			#I the output file descriptor
pointer	im[ARB]			#I the input image pointers
int	res			#I the results file descriptor
pointer	coo			# pointer to the input coordinate system
pointer	fit			#I pointer to fit parameters
int	xcolumn, ycolumn	#I the x and y column numbers
int	lngcolumn, latcolumn	#I the longitude and latitude column numbers
int	tweak			#I tweak flag
double	xtan[ARB], ytan[ARB]	#I the input x and y of the tangent point
double	ratan[ARB], dectan[ARB]	#I the input ra and dec of the tangent point
double	xmin, xmax		#I max and min xref values
double	ymin, ymax		#I max and min yref values
bool	update			#I update the image wcs
bool	verbose			#I verbose mode

double	mintemp, maxtemp, lngrms, latrms, lngmean, latmean
pointer	sp, str, projstr
pointer	n, xref, yref, xifit, etafit, lngfit, latfit, wts
pointer	lngref, latref, xi, eta, lngref1, latref1, xi1, eta1
pointer	sx1, sy1, sx2, sy2, xerrmsg, yerrmsg
int	i, npts, npts1
double	asumd()
int	cc_rdxyrd(), sk_stati(), rg_wrdstr()
bool	streq()

errchk	geo_fitd, geo_mgfitd()

begin
	# Get working space.
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (projstr, SZ_LINE, TY_CHAR)
	call salloc (xerrmsg, SZ_LINE, TY_CHAR)
	call salloc (yerrmsg, SZ_LINE, TY_CHAR)

	# Initialize the pointers.
	xref = NULL
	yref = NULL
	lngref = NULL
	latref = NULL
	xi = NULL
	eta = NULL
	xifit = NULL
	etafit = NULL
	lngfit = NULL
	latfit = NULL
	wts = NULL

	# Read in data and check that it is in range.
	if (gd != NULL)
	    call gdeactivate (gd, 0)
	npts = cc_rdxyrd (in, im, xtan, ytan, ratan, dectan, nin,
	    coo, xcolumn, ycolumn, lngcolumn, latcolumn, tweak,
	    n, xref, yref, lngref, latref, xmin, xmax, ymin, ymax)
	if (gd != NULL)
	    call greactivate (gd, 0)
	if (npts == 0)
	    return

	# Compute the mean of the reference and input coordinates.
	GM_XOREF(fit) = asumd (Memd[xref], npts) / npts
	GM_YOREF(fit) = asumd (Memd[yref], npts) / npts
	GM_XOIN(fit) = asumd (Memd[lngref], npts) / npts
	GM_YOIN(fit) = asumd (Memd[latref], npts) / npts

	# Set the sky projection str.
	if (rg_wrdstr (GM_PROJECTION(fit), Memc[projstr], SZ_LINE,
	    GM_PROJLIST) <= 0 || GM_PROJECTION(fit) == GM_LIN)
	    Memc[projstr] = EOS
	else
	    call strcpy (GM_PROJSTR(fit), Memc[projstr], SZ_LINE)

	# Compute the position of the reference point for the solution.
	if (IS_INDEFD(ratan[1]) || IS_INDEFD(dectan[1])) {
	    call cc_refpt (coo, Memd[lngref], Memd[latref], npts,
	        lngmean, latmean)
	    if (IS_INDEFD(ratan[1]))
		GM_XREFPT(fit) = lngmean
	    else
		GM_XREFPT(fit) = ratan[1]
	    if (IS_INDEFD(dectan[1]))
		GM_YREFPT(fit) = latmean
	    else
		GM_YREFPT(fit) = dectan[1]
	} else {
	    GM_XREFPT(fit) = ratan[1]
	    GM_YREFPT(fit) = dectan[1]
	}

	# Allocate space for and compute the weights.
	call malloc (wts, npts, TY_DOUBLE)
	call amovkd (double(1.), Memd[wts], npts)

	# Determine the x max and min.
	if (IS_INDEFD(xmin) || IS_INDEFD(xmax)) {
	    call alimd (Memd[xref], npts, mintemp, maxtemp)
	    if (! IS_INDEFD(xmin))
		GM_XMIN(fit) = xmin
	    else
		GM_XMIN(fit) = mintemp
	    if (! IS_INDEFD(xmax))
		GM_XMAX(fit) = xmax
	    else
		GM_XMAX(fit) = maxtemp
	} else {
	    GM_XMIN(fit) = xmin
	    GM_XMAX(fit) = xmax
	}

	# Determine the y max and min.
	if (IS_INDEFD(ymin) || IS_INDEFD(ymax)) {
	    call alimd (Memd[yref], npts, mintemp, maxtemp)
	    if (! IS_INDEFD(ymin))
		GM_YMIN(fit) = ymin
	    else
		GM_YMIN(fit) = mintemp
	    if (! IS_INDEFD(ymax))
		GM_YMAX(fit) = ymax
	    else
		GM_YMAX(fit) = maxtemp
	} else {
	    GM_YMIN(fit) = ymin
	    GM_YMAX(fit) = ymax
	}

	# Convert the ra / longitude and dec / latitude values to standard
	# coordinates in arc seconds before fitting.
	call malloc (xi, npts, TY_DOUBLE)
	call malloc (eta, npts, TY_DOUBLE)
	lngref1 = lngref; latref1 = latref; xi1 = xi; eta1 = eta
	do i = 1, nin {
	    npts1 = Memi[n+i-1]
	    if (npts1 == 0)
	        next
	    if (IS_INDEFD(ratan[i]) || IS_INDEFD(dectan[i]))
		call rg_celtostd (Memc[projstr], Memd[lngref1], Memd[latref1],
		    Memd[xi1], Memd[eta1], npts1, lngmean, latmean,
		    sk_stati(coo, S_NLNGUNITS), sk_stati(coo, S_NLATUNITS))
	    else
		call rg_celtostd (Memc[projstr], Memd[lngref1], Memd[latref1],
		    Memd[xi1], Memd[eta1], npts1, ratan[i], dectan[i],
		    sk_stati(coo, S_NLNGUNITS), sk_stati(coo, S_NLATUNITS))
	    lngref1 = lngref1 + npts1
	    latref1 = latref1 + npts1
	    xi1 = xi1 + npts1
	    eta1 = eta1 + npts1
	}
	call amulkd (Memd[xi], 3600.0d0, Memd[xi], npts)
	call amulkd (Memd[eta], 3600.0d0, Memd[eta], npts)

	# Initalize surface pointers.
	sx1 = NULL
	sy1 = NULL
	sx2 = NULL
	sy2 = NULL

	# Fit the data.
	if (! (IS_INDEFD(xtan[1]) || IS_INDEFD(ytan[1]))) {
	    call geo_setd (fit, GMXO, xtan[1])
	    call geo_setd (fit, GMYO, ytan[1])
	    call geo_setd (fit, GMXOREF, 0D0)
	    call geo_setd (fit, GMYOREF, 0D0)
	}
	if (gd != NULL) {
	    iferr {
	        call geo_mgfitd (gd, fit, sx1, sy1, sx2, sy2, Memd[xref],
	            Memd[yref], Memd[xi], Memd[eta], Memd[wts], npts,
		    Memc[xerrmsg], Memc[yerrmsg], SZ_LINE)
	    } then {
	        call gdeactivate (gd, 0)
		call mfree (xi, TY_DOUBLE)
		call mfree (eta, TY_DOUBLE)
		call mfree (wts, TY_DOUBLE)
		call geo_mmfreed (sx1, sy1, sx2, sy2)
		call sfree (sp)
		call error (3, "Too few data points in XI or ETA fits.")
	    }
	    call gdeactivate (gd, 0)
	    if (verbose && res != STDOUT) {
	        call printf ("Coordinate mapping status\n")
		call flush (STDOUT)
	    }
	    if (res != NULL)
	        call fprintf (res, "# Coordinate mapping status\n")
	} else {
	    if (verbose && res != STDOUT) {
	        call printf ("Coordinate mapping status\n    ")
		call flush (STDOUT)
	    }
	    if (res != NULL) {
	        call fprintf (res, "# Coordinate mapping status\n#     ")
	    }
	    iferr {
	        call geo_fitd (fit, sx1, sy1, sx2, sy2, Memd[xref], Memd[yref],
		    Memd[xi], Memd[eta], Memd[wts], npts, Memc[xerrmsg],
		    Memc[yerrmsg], SZ_LINE)
	    } then {
		#call printf ("%s  %s\n")
		    #call pargstr (Memc[xerrmsg])
		    #call pargstr (Memc[yerrmsg])
		#call flush (STDOUT)
		call mfree (xi, TY_DOUBLE)
		call mfree (eta, TY_DOUBLE)
		call mfree (wts, TY_DOUBLE)
		call geo_mmfreed (sx1, sy1, sx2, sy2)
		call sfree (sp)
		call error (3, "Too few data points in XI or ETA fits.")
	    }
	    if (verbose && res != STDOUT) {
		call printf ("%s  %s\n")
		    call pargstr (Memc[xerrmsg])
		    call pargstr (Memc[yerrmsg])
		call flush (STDOUT)
	    }
	    if (res != NULL) {
		call fprintf (res, "%s  %s\n")
		    call pargstr (Memc[xerrmsg])
		    call pargstr (Memc[yerrmsg])
	    }
	}

	# Allocate fitting arrays.
	call malloc (xifit, npts, TY_DOUBLE)
	call malloc (etafit, npts, TY_DOUBLE)
	call malloc (lngfit, npts, TY_DOUBLE)
	call malloc (latfit, npts, TY_DOUBLE)

	# Compute the fitted ra / dec or longitude latitude,
	if (res != NULL || verbose) {
	    call cc_eval (sx1, sy1, sx2, sy2, Memd[xref], Memd[yref],
	        Memd[xifit], Memd[etafit], npts)
	    call cc_rms (fit, Memd[xi], Memd[eta], Memd[xifit], Memd[etafit],
	        Memd[wts], npts, lngrms, latrms) 
	    call adivkd (Memd[xifit], 3600.0d0, Memd[xifit], npts)
	    call adivkd (Memd[etafit], 3600.0d0, Memd[etafit], npts)
	    call rg_stdtocel (Memc[projstr], Memd[xifit], Memd[etafit],
	        Memd[lngfit], Memd[latfit], npts, GM_XREFPT(fit),
		GM_YREFPT(fit), sk_stati(coo, S_NLNGUNITS), sk_stati(coo,
		S_NLATUNITS))
	}

	# Print some detailed info about the fit.
	if (verbose && res != STDOUT) {
	    call printf (
	    "    Ra/Dec or Long/Lat fit rms: %0.3g  %0.3g   (arcsec  arcsec)\n")
		call pargd (lngrms)
		call pargd (latrms)
	    call cc_show (STDOUT, coo, Memc[projstr], GM_XREFPT(fit),
	        GM_YREFPT(fit), sx1, sy1, NO)
	}
	if (res != NULL) {
	    call fprintf (res,
	"#     Ra/Dec or Long/Lat fit rms: %0.3g  %0.3g   (arcsec  arcsec)\n")
		call pargd (lngrms)
		call pargd (latrms)
	    call cc_show (res, coo, Memc[projstr], GM_XREFPT(fit),
	        GM_YREFPT(fit), sx1, sy1, YES)
	}

	# Compute the wcs mapping rms.
	if (! streq (GM_PROJSTR(fit), "tnx") && ! streq (GM_PROJSTR(fit),
	    "zpx")) {
	    call cc_eval (sx1, sy1, NULL, NULL, Memd[xref], Memd[yref],
	        Memd[xifit], Memd[etafit], npts)
	    call cc_rms (fit, Memd[xi], Memd[eta], Memd[xifit],
	        Memd[etafit], Memd[wts], npts, lngrms, latrms)
	}

	# Update the image wcs.
	do i = 1, nin {
	    if (im[i] != NULL) {
		if (i == 1) {
		    if (verbose && res != STDOUT) {
			call printf ("Wcs mapping status\n")
			call printf (
	   "    Ra/Dec or Long/Lat wcs rms: %0.3g  %0.3g   (arcsec  arcsec)\n")
			    call pargd (lngrms)
			    call pargd (latrms)
		    }
		    if (res != NULL) {
			call fprintf (res, "# Wcs mapping status\n")
			call fprintf (res,
	  "#     Ra/Dec or Long/Lat wcs rms: %0.3g  %0.3g   (arcsec  arcsec)\n")
			    call pargd (lngrms)
			    call pargd (latrms)
		    }
		}
		if (update) {
		    if (IS_INDEFD(ratan[i]) || IS_INDEFD(dectan[i]))
			call cc_nwcsim (im[i], coo, Memc[projstr], lngmean,
			    latmean, sx1, sy1, sx2, sy2, false)
		    else
			call cc_nwcsim (im[i], coo, Memc[projstr], ratan[i],
			    dectan[i], sx1, sy1, sx2, sy2, false)
		    if (i == 1) {
			if (verbose && res != STDOUT)
			    call printf ("Updating image header wcs\n\n")
			if (res != NULL)
			    call fprintf (res,
			        "# Updating image header wcs\n\n")
		    }
		}
	    }
	}

	# Write the database file.
	call cc_out (fit, coo, out, sx1, sy1, sx2, sy2, lngrms, latrms)

	# List results for individual objects.
	if (res != NULL)
	    call cc_plist (res, fit, coo, Memd[xref], Memd[yref], Memd[lngref],
		Memd[latref], Memd[lngfit], Memd[latfit], Memd[wts],
		npts)

	# Free the space and close files.
	call geo_mmfreed (sx1, sy1, sx2, sy2)

	if (n != NULL)
	    call mfree (n, TY_INT)
	if (xref != NULL)
	    call mfree (xref, TY_DOUBLE)
	if (yref != NULL)
	    call mfree (yref, TY_DOUBLE)
	if (lngref != NULL)
	    call mfree (lngref, TY_DOUBLE)
	if (latref != NULL)
	    call mfree (latref, TY_DOUBLE)
	if (xi != NULL)
	    call mfree (xi, TY_DOUBLE)
	if (eta != NULL)
	    call mfree (eta, TY_DOUBLE)
	if (xifit != NULL)
	    call mfree (xifit, TY_DOUBLE)
	if (etafit != NULL)
	    call mfree (etafit, TY_DOUBLE)
	if (wts != NULL)
	    call mfree (wts, TY_DOUBLE)
	if (lngfit != NULL)
	    call mfree (lngfit, TY_DOUBLE)
	if (latfit != NULL)
	    call mfree (latfit, TY_DOUBLE)

	call sfree (sp)
end


# CC_RDXYRD -- Read in the x, y, ra, and dec values from the input file(s).
#
# Adjust the tangent points if there is an image WCS.

int procedure cc_rdxyrd (in, im, xtan, ytan, ratan, dectan, nin,
        coo, xcolumn, ycolumn, lngcolumn, latcolumn, tweak,
	n, xref, yref, lngref, latref, xmin, xmax, ymin, ymax)

int	in[nin]			#I the input file file descriptors
pointer	im[nin]			#I the input image pointers
double	xtan[ARB], ytan[ARB]	#I the input x and y of the tangent point
double	ratan[ARB], dectan[ARB]	#I the input ra and dec of the tangent point
int	nin			#I number of input files
pointer	coo			#I the input coordinate system
int	xcolumn, ycolumn	#I the columns containing the x / y values
int	lngcolumn, latcolumn	#I the columns containing the lng / lat values
int	tweak			#I tweak flag
pointer	n			#U pointer to the number of points
pointer	xref, yref		#I pointers to the x / y value arrays
pointer	lngref, latref		#I pointers to the lng / lat value arrays
double	xmin, xmax		#U the min and max x values
double	ymin, ymax		#U the min and max y values

int	i, npts, npts1
pointer	xref1, yref1, lngref1, latref1

int	cc_rdxyrd1()

begin
	call calloc (n, nin, TY_INT)

	npts = 0
	do i = 1, nin {
	    npts1 = cc_rdxyrd1 (in[i], im[i], xtan[i], ytan[i], ratan[i],
	        dectan[i], coo, xcolumn, ycolumn, lngcolumn, latcolumn, tweak,
		xref1, yref1, lngref1, latref1, xmin, xmax, ymin, ymax)
	    Memi[n+i-1] = npts1
	    if (npts1 == 0)
	        next
	    if (npts == 0) {
	        xref = xref1
	        yref = yref1
	        lngref = lngref1
	        latref = latref1
	    } else {
		call realloc (xref, npts+npts1, TY_DOUBLE)
		call realloc (yref, npts+npts1, TY_DOUBLE)
		call realloc (lngref, npts+npts1, TY_DOUBLE)
		call realloc (latref, npts+npts1, TY_DOUBLE)
		call amovd (Memd[xref1], Memd[xref+npts], npts1)
		call amovd (Memd[yref1], Memd[yref+npts], npts1)
		call amovd (Memd[lngref1], Memd[lngref+npts], npts1)
		call amovd (Memd[latref1], Memd[latref+npts], npts1)
		call mfree (xref1, TY_DOUBLE)
		call mfree (yref1, TY_DOUBLE)
		call mfree (lngref1, TY_DOUBLE)
		call mfree (latref1, TY_DOUBLE)
	    }
	    npts = npts + npts1
	}

	if (npts == 0) {
	    call mfree (n, TY_INT)

	    if (i > 1)
		call printf ("Coordinate lists have no data in range.\n")
	}

	return (npts)
end


# CC_RDXYRD1 -- Read in the x, y, ra, and dec values from the input file.
#
# If a reference point (both pixel and value) and an image (with a
# valid celestial WCS) are defined then the WCS is reset to the reference
# point and the reference point value is then shifted to make the
# the WCS coordinates evaluated at the input pixel coordinates agree
# if the input celestial coordinates on average.

int procedure cc_rdxyrd1 (in, im, xtan, ytan, ratan, dectan, icoo,
	xcolumn, ycolumn, lngcolumn, latcolumn, tweak,
	xref, yref, lngref, latref, xmin, xmax, ymin, ymax)

int	in			#I the input file file descriptor
pointer	im			#I the input image pointer
double	xtan, ytan		#I the input x and y of the tangent point
double	ratan, dectan		#I the input ra and dec of the tangent point
pointer	icoo			#I the input coordinate system
int	xcolumn, ycolumn	#I the columns containing the x / y values
int	lngcolumn, latcolumn	#I the columns containing the lng / lat values
int	tweak			#I tweak flag
pointer	xref, yref		#I pointers to the input x / y values
pointer	lngref, latref		#I pointers to the input lng / lat values
double	xmin, xmax		#U the min and max x values
double	ymin, ymax		#U the min and max y values

int	nline, i, npts, bufsize, nfields, max_fields, nsig, offset
double	lng1, lat1, lng2, lat2, x, y, z, sumx, sumy, sumz, r, pa, wterm[8]
pointer	sp, inbuf, linebuf, field_pos
pointer	mw, ct, coo
int	getline(), li_get_numd(), sk_decim()
pointer	mw_ctrand(), mw_sctran()

int	sk_stati()

begin
	call smark (sp)
	call salloc (inbuf, SZ_LINE, TY_CHAR)
	call salloc (linebuf, SZ_LINE, TY_CHAR)
	call salloc (field_pos, MAX_FIELDS, TY_INT)

	bufsize = CC_DEFBUFSIZE
	call malloc (xref, bufsize, TY_DOUBLE)
	call malloc (yref, bufsize, TY_DOUBLE)
	call malloc (lngref, bufsize, TY_DOUBLE)
	call malloc (latref, bufsize, TY_DOUBLE)

	# Check whether to adjust the reference value based on the
	# current image WCS.
	mw = NULL; ct = NULL; coo = NULL
	if (tweak == 3 && im != NULL && !IS_INDEFD(xtan) && !IS_INDEFD(ytan) &&
	    !IS_INDEFD(ratan) && !IS_INDEFD(dectan)) {
	    if (sk_decim (im, "logical", mw, coo) != ERR && mw != NULL) {
	        call sk_seti (coo, S_NLNGUNITS, SKY_DEGREES)
		call sk_ultran (icoo, coo, ratan, dectan, lng1, lat1, 1)
		call mw_gwtermd (mw, wterm[1], wterm[3], wterm[5], 2)
		wterm[1] = xtan; wterm[2] = ytan 
		wterm[3] = lng1; wterm[4] = lat1
		call mw_swtermd (mw, wterm[1], wterm[3], wterm[5], 2)
	        ct = mw_sctran (mw, "logical", "world", 03B)
		sumx = 0d0; sumy = 0d0; sumz = 0d0
	    } else {
	        if (mw != NULL) 
		    call mw_close (mw)
		call sk_close (coo)
		mw = NULL; coo = NULL
	    }
	}

	npts = 0
	max_fields = MAX_FIELDS
	for (nline = 1; getline (in, Memc[inbuf]) != EOF; nline = nline + 1) {

	    # Skip over leading white space.
            for (i = inbuf; IS_WHITE(Memc[i]); i = i + 1)
                ;

            # Skip comment and blank lines.
            if (Memc[i] == '#')
                next
            else if (Memc[i] == '\n' || Memc[i] == EOS)
                next

            # Expand tabs into blanks, determine field offsets.
            call strdetab (Memc[inbuf], Memc[linebuf], SZ_LINE, TABSIZE)
            call li_find_fields (Memc[linebuf], Memi[field_pos], max_fields,
                nfields)

	    # Decode the x coordinate.
	    if (xcolumn > nfields)
		next
	    offset = Memi[field_pos+xcolumn-1]
	    if (li_get_numd (Memc[linebuf+offset-1], Memd[xref+npts],
	        nsig) == 0)
		next

	    # Decode the y coordinate.
	    if (ycolumn > nfields)
		next
	    offset = Memi[field_pos+ycolumn-1]
	    if (li_get_numd (Memc[linebuf+offset-1], Memd[yref+npts],
		nsig) == 0)
		next

	    # Decode the ra / longitude coordinate.
	    if (lngcolumn > nfields)
		next
	    offset = Memi[field_pos+lngcolumn-1]
	    if (li_get_numd (Memc[linebuf+offset-1], Memd[lngref+npts],
		nsig) == 0)
		next

	    # Decode the dec / latitude coordinate.
	    if (latcolumn > nfields)
		next
	    offset = Memi[field_pos+latcolumn-1]
	    if (li_get_numd (Memc[linebuf+offset-1], Memd[latref+npts],
		nsig) == 0)
		next

	    # Accumulate cartisian shifts from image WCS coordinates.
	    if (ct != NULL) {
		call mw_c2trand (ct, Memd[xref+npts], Memd[yref+npts],
		    lng1, lat1)
		call sk_ultran (icoo, coo, Memd[lngref+npts], Memd[latref+npts],
		    lng2, lat2, 1)
		lng1 = DDEGTORAD(lng1); lat1 = DDEGTORAD(lat1)
		lng2 = DDEGTORAD(lng2); lat2 = DDEGTORAD(lat2)
		x = sin (lat2) - sin(lat1)
		y = cos (lat2) * sin (lng2) - cos (lat1) * sin (lng1)
		z = cos (lat2) * cos (lng2) - cos (lat1) * cos (lng1)
		sumx = sumx + x; sumy = sumy + y; sumz = sumz + z
	    }

	    npts = npts + 1

	    if (npts >= bufsize) {
		bufsize = bufsize + CC_DEFBUFSIZE
	        call realloc (xref, bufsize, TY_DOUBLE)
	        call realloc (yref, bufsize, TY_DOUBLE)
	        call realloc (lngref, bufsize, TY_DOUBLE)
	        call realloc (latref, bufsize, TY_DOUBLE)
	    }
	}

	# Adjust the tangent point value.
	if (npts > 0 && ct != NULL) {
	    sumx = sumx / npts; sumy = sumy / npts; sumz = sumz / npts
	    r = sqrt (sumx**2 + sumy**2 + sumz**2) / 2
	    r = 2 * atan2 (r, sqrt(max(0d0,1d0-r)))
	    r = 3600 * DRADTODEG (r)
	    call eprintf ("Tangent point shift = %.2f\n")
	        call pargd (r)

	    call sk_ultran (icoo, coo, ratan, dectan, lng1, lat1, 1)
	    lng2 = DDEGTORAD(lng1); lat2 = DDEGTORAD(lat1)
	    x = sin (lat2) + sumx
	    y = cos (lat2) * sin (lng2) + sumy
	    z = cos (lat2) * cos (lng2) + sumz
	    pa = atan2 (y, x)
	    if (pa < 0d0)
		pa = pa + DTWOPI
	    if (pa >= DTWOPI)
		pa = pa - DTWOPI
	    r = z
	    if (abs(r) > 0.99d0) {
		if (r < 0d0)
		   r = DPI - asin (sqrt (x * x + y * y))
		else
		   r = asin (sqrt (x * x + y * y))
	    } else
		r = acos (r)
	    x = sin (r) * cos (pa)
	    y = sin (r) * sin (pa)
	    z = cos (r)
	    lng2 = atan2 (y, z)
	    if (lng2 < 0d0)
		lng2 = lng2 + DTWOPI
	    if (lng2 >= DTWOPI)
		lng2 = lng2 - DTWOPI
	    lat2 = x
	    if (abs (lat2) > 0.99d0) {
		if (lat2 < 0d0)
		   lat2 = -acos (sqrt (y * y + z * z))
		else
		   lat2 = acos (sqrt (y * y + z * z))
	    } else
		lat2 = asin (lat2)
	    lng2 = DRADTODEG (lng2); lat2 = DRADTODEG (lat2)
	    call sk_ultran (coo, icoo, lng2, lat2, ratan, dectan, 1)
	}
	        
	# Finish up.

	if (npts <= 0) {
	    call mfree (xref, TY_DOUBLE)
	    call mfree (yref, TY_DOUBLE)
	    call mfree (lngref, TY_DOUBLE)
	    call mfree (latref, TY_DOUBLE)

	    call fstats (in, F_FILENAME, Memc[linebuf], SZ_LINE)
	    call printf ("Coordinate list: %s has no data in range.\n")
		call pargstr (Memc[linebuf])
	} else if (npts < bufsize) {
	    call realloc (xref, npts, TY_DOUBLE)
	    call realloc (yref, npts, TY_DOUBLE)
	    call realloc (lngref, npts, TY_DOUBLE)
	    call realloc (latref, npts, TY_DOUBLE)
	}

	if (ct != NULL)
	    call mw_ctfree (ct)
	if (mw != NULL)
	    call mw_close (mw)
	if (coo != NULL)
	    call sk_close (coo)
	call sfree (sp)

	return (npts)
end


# CC_REFPT -- Compute the coordinates of the reference point by averaging
# the celestial coordinates.


procedure cc_refpt (coo, lngref, latref, npts, lngmean, latmean)

pointer	coo			#I the input coordinate system descriptor
double	lngref[ARB]		#I the input longitude coordinates
double	latref[ARB]		#I the input latitude coordinates
int	npts			#I the number of input coordinates
double	lngmean			#O the output mean longitude
double	latmean			#O the output mean latitude

double	sumx, sumy, sumz, sumdx, sumdy, sumdz
double  tlng, tlat
double	x, y, z, tr, tpa
int	i
int	sk_stati()

begin
	sumx  = 0.0d0; sumy  = 0.0d0; sumz  = 0.0d0
	sumdx = 0.0d0; sumdy = 0.0d0; sumdz = 0.0d0

	# Loop over the data points.
	do i = 1, npts {

	    # Convert to radians.
	    switch (sk_stati(coo, S_NLNGUNITS)) {
	    case SKY_HOURS:
		tlng = DDEGTORAD (15.0d0 * lngref[i])
	    case SKY_DEGREES:
		tlng = DDEGTORAD (lngref[i])
	    case SKY_RADIANS:
		tlng = lngref[i]
	    }
	    switch (sk_stati(coo, S_NLATUNITS)) {
	    case SKY_HOURS:
		tlat = DDEGTORAD (15.0d0 * latref[i])
	    case SKY_DEGREES:
		tlat = DDEGTORAD (latref[i])
	    case SKY_RADIANS:
		tlat = latref[i]
	    }

	    x = sin (tlat)
	    y = cos (tlat) * sin (tlng)
	    z = cos (tlat) * cos (tlng)

	    sumx = sumx + x
	    sumy = sumy + y
	    sumz = sumz + z
	}

	# Compute the average vector components.
	sumx = sumx / npts
	sumy = sumy / npts
	sumz = sumz / npts

	# Now compute the average distance and position angle.
	tpa = atan2 (sumy, sumx)
        if (tpa < 0.0d0)
            tpa = tpa + DTWOPI
        if (tpa >= DTWOPI)
            tpa = tpa - DTWOPI
	tr = sumz 
	if (abs(tr) > 0.99d0) {
	    if (tr < 0.0d0)
	        tr = DPI - asin (sqrt (sumx * sumx + sumy * sumy))
	    else
	        tr = asin (sqrt (sumx * sumx + sumy * sumy))
	} else
	    tr = acos (tr)

	# Solve for the average longitude and latitude.
	sumx = sin (tr) * cos (tpa)
	sumy = sin (tr) * sin (tpa)
	sumz = cos (tr)
	lngmean = atan2 (sumy, sumz)
        if (lngmean < 0.0d0)
            lngmean = lngmean + DTWOPI
        if (lngmean >= DTWOPI)
            lngmean = lngmean - DTWOPI
	latmean = sumx
	if (abs (latmean) > 0.99d0) {
	    if (latmean < 0.0d0)
		latmean = -acos (sqrt(sumy ** 2 + sumz ** 2))
	    else
		latmean = acos (sqrt(sumy ** 2 + sumz ** 2))
	} else
	    latmean = asin (latmean)

	# Convert back to appropriate units.
	switch (sk_stati(coo, S_NLNGUNITS)) {
	case SKY_HOURS:
	    lngmean = DRADTODEG (lngmean) / 15.0d0
	case SKY_DEGREES:
	    lngmean = DRADTODEG (lngmean)
	case SKY_RADIANS:
	    ;
	}
	switch (sk_stati(coo, S_NLATUNITS)) {
	case SKY_HOURS:
	    latmean = DRADTODEG (latmean) / 15.0d0
	case SKY_DEGREES:
	    latmean = DRADTODEG (latmean)
	case SKY_RADIANS:
	    ;
	}
end


# CC_EVAL -- Compute the fitted standard coordinates.

procedure cc_eval (sx1, sy1, sx2, sy2, xref, yref, xi, eta, npts)

pointer	sx1, sy1		#I pointer to linear surfaces
pointer	sx2, sy2		#I pointer to higher order surfaces
double	xref[ARB]		#I the x reference coordinates
double	yref[ARB]		#I the y reference coordinates
double	xi[ARB]			#O the fitted xi coordinates
double	eta[ARB]		#O the fitted eta coordinates
int	npts			#I the number of points

pointer	sp, temp

begin
	call smark (sp)
	call salloc (temp, npts, TY_DOUBLE)

	call dgsvector (sx1, xref, yref, xi, npts)
	if (sx2 != NULL) {
	    call dgsvector (sx2, xref, yref, Memd[temp], npts)
	    call aaddd (Memd[temp], xi, xi, npts)
	}
	call dgsvector (sy1, xref, yref, eta, npts)
	if (sy2 != NULL) {
	    call dgsvector (sy2, xref, yref, Memd[temp], npts)
	    call aaddd (Memd[temp], eta, eta, npts)
	}

	call sfree (sp)
end


# CC_RMS -- Compute the rms of the fit in arcseconds.

procedure cc_rms (fit, xi, eta, xifit, etafit, wts, npts, xirms, etarms) 

pointer	fit			#I pointer to the fit structure
double	xi[ARB]			#I the input xi coordinates
double	eta[ARB]		#I the input eta coordinates
double	xifit[ARB]		#I the fitted chi coordinates
double	etafit[ARB]		#I the fitted eta coordinates
double	wts[ARB]		#I the input weights array
int	npts			#I the number of points
double	xirms			#O the output xi rms
double	etarms			#O the output eta rms

int	i, index, ngood
pointer	sp, twts

begin
	# Allocate working space.
	call smark (sp)
	call salloc (twts, npts, TY_DOUBLE)

	# Compute the weights.
	call amovd (wts, Memd[twts], npts)
	do i = 1, GM_NREJECT(fit) {
	    index = Memi[GM_REJ(fit)+i-1]
	    if (wts[index] > 0.0d0) 
		Memd[twts+index-1] = 0.0d0
	}

	# Accumulate the squares.
	xirms = 0.0d0
	etarms = 0.0d0
	do i = 1, npts {
	    xirms = xirms + Memd[twts+i-1] * (xi[i] - xifit[i]) ** 2
	    etarms = etarms + Memd[twts+i-1] * (eta[i] - etafit[i]) ** 2
	}

	# Compute the rms.
	#ngood = max (0, GM_NPTS(fit) - GM_NREJECT(fit) - GM_NWTS0(fit))
	ngood = max (0, GM_NPTS(fit) - GM_NWTS0(fit))
	if (ngood > 1) {
	    xirms = sqrt (xirms / (ngood - 1))
	    etarms = sqrt (etarms / (ngood - 1))
	} else {
	    xirms = 0.0d0
	    etarms = 0.0d0
	}
	xirms = xirms
	etarms = etarms

	call sfree (sp)
end


# CC_SHOW -- Print the coodinate mapping parameters.

procedure cc_show (fd, coo, projection, lngref, latref, sx1, sy1, comment)

int	fd			#I the output file descriptor
pointer	coo			#I pointer to the coordinate structure
char	projection[ARB]		#I the sky projection geometry
double	lngref, latref		#I the coordinates of the reference point
pointer	sx1, sy1		#I pointer to linear surfaces
int	comment			#I comment the output ?

double	xshift, yshift, a, b, c, d, denom
double	xpix, ypix, xscale, yscale, xrot, yrot
pointer sp, str, keyword, value
bool	fp_equald()
int	sk_stati()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)

	# Compute the geometric parameters.
	call geo_gcoeffd (sx1, sy1, xshift, yshift, a, b, c, d)

	# Compute the position of the reference pixel from the geometric
	# parameters.
	denom = a * d - c * b
	if (denom == 0.0d0)
	    xpix = INDEFD
	else
	    xpix = (b * yshift - d * xshift) / denom 
	if (denom == 0.0d0)
	    ypix = INDEFD
	else
	    ypix =  (c * xshift - a * yshift) / denom

	if (comment == NO) {
	    call fprintf (fd, "Coordinate mapping parameters\n")
	    call fprintf (fd, "    Sky projection geometry: %s\n")
	} else {
	    call fprintf (fd, "# Coordinate mapping parameters\n")
	    call fprintf (fd, "#     Sky projection geometry: %s\n")
	}
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
                if (comment == NO) {
                    call fprintf (fd, "    Projection parameter %s: %s\n")
                } else {
                    call fprintf (fd, "#     Projection parameter %s: %s\n")
                }
                    call pargstr (Memc[keyword])
                    call pargstr (Memc[value])
            }

	}

	# Output the reference point.
	if (comment == NO) {
	    call sprintf (Memc[str], SZ_LINE,
	        "    Reference point: %s  %s  (%s  %s)\n")
	} else {
	    call sprintf (Memc[str], SZ_LINE,
	        "#     Reference point: %s  %s  (%s  %s)\n")
	}
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
	if (comment == NO) {
	    call printf (Memc[str])
	        call pargd (lngref)
	        call pargd (latref)
	} else {
	    call fprintf (fd, Memc[str])
	        call pargd (lngref)
	        call pargd (latref)
	}

	if (comment == NO) {
	    call fprintf (fd,
	        "    Reference point: %0.3f  %0.3f  (pixels  pixels)\n")
	        call pargd (xpix)
	        call pargd (ypix)
	} else {
	    call fprintf (fd,
	        "#     Reference point: %0.3f  %0.3f  (pixels  pixels)\n")
	        call pargd (xpix)
	        call pargd (ypix)
	}

	# Output the scale factors.
	xscale = sqrt (a * a + c * c)
	yscale = sqrt (b * b + d * d) 
	if (comment == NO) {
	    call fprintf (fd,
	    "    X and Y scale: %0.3f  %0.3f  (arcsec/pixel  arcsec/pixel)\n")
	        call pargd (xscale)
	        call pargd (yscale)
	} else {
	    call fprintf (fd,
	    "#     X and Y scale: %0.3f  %0.3f  (arcsec/pixel  arcsec/pixel)\n")
	        call pargd (xscale)
	        call pargd (yscale)
	}

	# Output the rotation factors.
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
	if (comment == NO) {
	    call fprintf (fd,
	    "    X and Y axis rotation: %0.3f  %0.3f  (degrees  degrees)\n")
	        call pargd (xrot)
	        call pargd (yrot)
	} else {
	    call fprintf (fd,
	    "#     X and Y axis rotation: %0.3f  %0.3f  (degrees  degrees)\n")
	        call pargd (xrot)
	        call pargd (yrot)
	}

	call sfree (sp)
end


# CC_OUT -- Write the output database file record.

procedure cc_out (fit, coo, out, sx1, sy1, sx2, sy2, lxrms, lyrms)

pointer	fit		#I pointer to fitting structure
pointer	coo		#I pointer to the coordinate system structure
int	out		#I pointer to database file
pointer	sx1, sy1	#I pointer to linear surfaces
pointer	sx2, sy2	#I pointer to distortion surfaces
double	lxrms, lyrms	#I the input wcs x and y rms

double	xshift, yshift, a, b, c, d, denom, xrms, yrms
double	xpixref, ypixref, xscale, yscale, xrot, yrot
int	i, npts, ncoeff
pointer	sp, str, xcoeff, ycoeff, keyword, value
bool	fp_equald()
int	dgsgeti(), rg_wrdstr(), sk_stati()

begin
	# Allocate some working memory.
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)

	# Compute the rms.
	#npts = max (0, GM_NPTS(fit) - GM_NREJECT(fit) - GM_NWTS0(fit))
	npts = max (0, GM_NPTS(fit) - GM_NWTS0(fit))
        xrms = max (0.0d0, GM_XRMS(fit))
        yrms = max (0.0d0, GM_YRMS(fit))
        if (npts > 1) {
            xrms = sqrt (xrms / (npts - 1))
            yrms = sqrt (yrms / (npts - 1))
        } else {
            xrms = 0.0d0
            yrms = 0.0d0
        }

	# Compute the geometric parameters.
	call geo_gcoeffd (sx1, sy1, xshift, yshift, a, b, c, d)
	denom = a * d - c * b
	if (denom == 0.0d0)
	    xpixref = INDEFD
	else
	    xpixref = (b * yshift - d * xshift) / denom 
	if (denom == 0.0d0)
	    ypixref = INDEFD
	else
	    ypixref =  (c * xshift - a * yshift) / denom
	xscale = sqrt (a * a + c * c)
	yscale = sqrt (b * b + d * d)
	if (fp_equald (a, 0.0d0) && fp_equald (c, 0.0d0))
	    xrot = 0.0d0
	else
	    xrot = RADTODEG(atan2 (-c, a))
	if (xrot < 0.0d0)
	    xrot = xrot + 360.0d0
	if (fp_equald (b, 0.0d0) && fp_equald (d, 0.0d0))
	    yrot = 0.0d0
	else
	    yrot = RADTODEG(atan2 (b, d))
	if (yrot < 0.0d0)
	    yrot = yrot + 360.0d0

	# Print title.
	call dtptime (out)
	call dtput (out, "begin\t%s\n")
	    call pargstr (GM_RECORD(fit))

	# Print out some information about the data.
	call dtput (out, "\txrefmean\t%g\n")
	    call pargd (GM_XOREF(fit))
	call dtput (out, "\tyrefmean\t%g\n")
	    call pargd (GM_YOREF(fit))
	call dtput (out, "\tlngmean\t\t%g\n")
	    call pargd (GM_XOIN(fit))
	call dtput (out, "\tlatmean\t\t%g\n")
	    call pargd (GM_YOIN(fit))

	# Print out information about the tangent point.
        if (rg_wrdstr(sk_stati(coo, S_PIXTYPE), Memc[str], SZ_FNAME,
            PIXTYPE_LIST) <= 0)
            call strcpy ("logical", Memc[str], SZ_FNAME)
        call dtput (out, "\tpixsystem\t%s\n")
            call pargstr (Memc[str])
	call sk_stats (coo, S_COOSYSTEM, Memc[str], SZ_FNAME)
	call dtput (out, "\tcoosystem\t%g\n")
	    call pargstr (Memc[str])

        if (rg_wrdstr (GM_PROJECTION(fit), Memc[str], SZ_FNAME,
            GM_PROJLIST) <= 0)
            call strcpy ("tan", Memc[str], SZ_FNAME)
        call dtput (out, "\tprojection\t%s\n")
            call pargstr (Memc[str])
        call sscan (GM_PROJSTR(fit))
            call gargwrd (Memc[str], SZ_FNAME)
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
            call dtput (out, "\t%s\t\t%s\n")
                call pargstr (Memc[keyword])
                call pargstr (Memc[value])
        }

	call dtput (out, "\tlngref\t\t%g\n")
	    call pargd (GM_XREFPT(fit))
	call dtput (out, "\tlatref\t\t%g\n")
	    call pargd (GM_YREFPT(fit))
	if (rg_wrdstr (sk_stati(coo, S_NLNGUNITS), Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST) <= 0)
	    ;
	call dtput (out, "\tlngunits\t%s\n")
	    call pargstr (Memc[str])
	if (rg_wrdstr (sk_stati(coo, S_NLATUNITS), Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST) <= 0)
	    ;
	call dtput (out, "\tlatunits\t%s\n")
	    call pargstr (Memc[str])
	call dtput (out, "\txpixref\t\t%g\n")
	    call pargd (xpixref)
	call dtput (out, "\typixref\t\t%g\n")
	    call pargd (ypixref)

	# Print out information about the fit.
	if (rg_wrdstr (GM_FIT(fit), Memc[str], SZ_FNAME, GM_GEOMETRIES) <= 0)
	    call strcpy ("general", Memc[str], SZ_FNAME)
	call dtput (out, "\tgeometry\t%s\n")
	    call pargstr (Memc[str])
	if (rg_wrdstr (GM_FUNCTION(fit), Memc[str], SZ_FNAME, GM_FUNCS) <= 0)
	    call strcpy ("polynomial", Memc[str], SZ_FNAME)
	call dtput (out, "\tfunction\t%s\n")
	    call pargstr (Memc[str])
	call dtput (out, "\txishift\t\t%g\n")
	    call pargd (xshift)
	call dtput (out, "\tetashift\t%g\n")
	    call pargd (yshift)
	call dtput (out, "\txmag\t\t%g\n")
	    call pargd (xscale)
	call dtput (out, "\tymag\t\t%g\n")
	    call pargd (yscale)
	call dtput (out, "\txrotation\t%g\n")
	    call pargd (xrot)
	call dtput (out, "\tyrotation\t%g\n")
	    call pargd (yrot)

	# Output the rms of the fit.
	call dtput (out, "\twcsxirms\t%g\n")
	    call pargd (lxrms)
	call dtput (out, "\twcsetarms\t%g\n")
	    call pargd (lyrms)
	call dtput (out, "\txirms\t\t%g\n")
	    call pargd (xrms)
	call dtput (out, "\tetarms\t\t%g\n")
	    call pargd (yrms)

	# Allocate memory for linear coefficients.
	ncoeff = max (dgsgeti (sx1, GSNSAVE), dgsgeti (sy1, GSNSAVE))
	call calloc (xcoeff, ncoeff, TY_DOUBLE)
	call calloc (ycoeff, ncoeff, TY_DOUBLE)

	# Encode the linear coefficients.
	call dgssave (sx1, Memd[xcoeff])
	call dgssave (sy1, Memd[ycoeff])

	# Output the linear coefficients.
	call dtput (out, "\tsurface1\t%d\n")
	    call pargi (ncoeff)
	do i = 1, ncoeff {
	    call dtput (out, "\t\t\t%g\t%g\n")
		call pargd (Memd[xcoeff+i-1])
		call pargd (Memd[ycoeff+i-1])
	}

	# Free the linear coefficient memory.
	call mfree (xcoeff, TY_DOUBLE)
	call mfree (ycoeff, TY_DOUBLE)

	# Allocate memory for higer order coefficients.
	if (sx2 == NULL)
	    ncoeff = 0
	else
	    ncoeff = dgsgeti (sx2, GSNSAVE)
	if (sy2 == NULL)
	    ncoeff = max (0, ncoeff)
	else
	    ncoeff = max (dgsgeti (sy2, GSNSAVE), ncoeff)
	call calloc (xcoeff, ncoeff, TY_DOUBLE)
	call calloc (ycoeff, ncoeff, TY_DOUBLE)

	# Encode the coefficients.
	call dgssave (sx2, Memd[xcoeff])
	call dgssave (sy2, Memd[ycoeff])

	# Output the coefficients.
	call dtput (out, "\tsurface2\t%d\n")
	    call pargi (ncoeff)
	do i = 1, ncoeff {
	    call dtput (out, "\t\t\t%g\t%g\n")
		call pargd (Memd[xcoeff+i-1])
		call pargd (Memd[ycoeff+i-1])
	}

	# Cleanup.
	call mfree (xcoeff, TY_DOUBLE)
	call mfree (ycoeff, TY_DOUBLE)
	call sfree (sp)
end


# CC_PLIST -- List the coordinates and the residuals.

procedure cc_plist (fd, fit, coo, xref, yref, lngref, latref, lngfit, latfit,
        wts, npts)

int     fd                      #I the results file descriptor
pointer fit                     #I pointer to the fit structure
pointer coo                     #I pointer to the coordinate structure
double  xref[ARB]               #I the input x coordinates
double  yref[ARB]               #I the input y coordinates
double  lngref[ARB]             #I the input ra / longitude coordinates
double  latref[ARB]             #I the input dec / latitude coordinates
double  lngfit[ARB]             #I the fitted ra / longitude coordinates
double  latfit[ARB]             #I the fitted dec / latitude coordinates
double  wts[ARB]                #I the weights array
int     npts                    #I the number of data points

double  diflng, diflat
int     i, index
pointer sp, fmtstr, lngunits, latunits, twts
int     sk_stati()

begin
        # Allocate working space.
        call smark (sp)
        call salloc (fmtstr, SZ_LINE, TY_CHAR)
        call salloc (lngunits, SZ_FNAME, TY_CHAR)
        call salloc (latunits, SZ_FNAME, TY_CHAR)
        call salloc (twts, npts, TY_DOUBLE)

        # Get the unit strings.
        switch (sk_stati (coo, S_NLNGUNITS)) {
        case SKY_HOURS:
            call strcpy ("hours", Memc[lngunits], SZ_FNAME)
        case SKY_DEGREES:
            call strcpy ("degrees", Memc[lngunits], SZ_FNAME)
        default:
            call strcpy ("radians", Memc[lngunits], SZ_FNAME)
        }
        switch (sk_stati (coo, S_NLATUNITS)) {
        case SKY_HOURS:
            call strcpy ("hours", Memc[latunits], SZ_FNAME)
        case SKY_DEGREES:
            call strcpy ("degrees", Memc[latunits], SZ_FNAME)
        default:
            call strcpy ("radians", Memc[latunits], SZ_FNAME)
        }

        # Compute the weights.
        call amovd (wts, Memd[twts], npts)
        do i = 1, GM_NREJECT(fit) {
            index = Memi[GM_REJ(fit)+i-1]
            if (wts[index] > 0.0d0)
                Memd[twts+index-1] = 0.0d0
        }

        # Print banner.
        call fprintf (fd, "\n# Input Coordinate Listing\n")
        call fprintf (fd, "#     Column 1: X (pixels)\n")
        call fprintf (fd, "#     Column 2: Y (pixels)\n")
        call fprintf (fd, "#     Column 3: Ra / Longitude (%s)\n")
            call pargstr (Memc[lngunits])
        call fprintf (fd, "#     Column 4: Dec / Latitude (%s)\n")
            call pargstr (Memc[latunits])
        call fprintf (fd, "#     Column 5: Fitted Ra / Longitude (%s)\n")
            call pargstr (Memc[lngunits])
        call fprintf (fd, "#     Column 6: Fitted Dec / Latitude (%s)\n")
            call pargstr (Memc[latunits])
        call fprintf (fd,
	    "#     Column 7: Residual Ra / Longitude (arcseconds)\n")
        call fprintf (fd,
            "#     Column 8: Residual Dec / Latitude (arcseconds)\n\n")

        # Create format string.
        call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s  %s %s  %s %s\n")
            call pargstr ("%10.3f")
            call pargstr ("%10.3f")
        switch (sk_stati (coo, S_NLNGUNITS)) {
        case SKY_HOURS:
            call pargstr ("%12.3h")
        case SKY_DEGREES:
            call pargstr ("%12.2h")
        default:
            call pargstr ("%12.7g")
        }
        switch (sk_stati (coo, S_NLATUNITS)) {
        case SKY_HOURS:
            call pargstr ("%12.3h")
        case SKY_DEGREES:
            call pargstr ("%12.2h")
        default:
            call pargstr ("%12.7g")
        }
        switch (sk_stati (coo, S_NLNGUNITS)) {
        case SKY_HOURS:
            call pargstr ("%12.3h")
        case SKY_DEGREES:
            call pargstr ("%12.2h")
        default:
            call pargstr ("%12.7g")
        }
        switch (sk_stati (coo, S_NLATUNITS)) {
        case SKY_HOURS:
            call pargstr ("%12.3h")
        case SKY_DEGREES:
            call pargstr ("%12.2h")
        default:
            call pargstr ("%12.7g")
        }
            call pargstr ("%6.3f")
            call pargstr ("%6.3f")

        do i = 1, npts {
            switch (sk_stati (coo, S_NLNGUNITS)) {
            case SKY_DEGREES:
                diflng = (lngref[i] - lngfit[i]) * 3600.0d0
            case SKY_HOURS:
                diflng = 15.0d0 * (lngref[i] - lngfit[i]) * 3600.0d0 *
                    cos (DEGTORAD(latref[i]))
            case SKY_RADIANS:
                diflng = RADTODEG ((lngref[i] - lngfit[i])) * 3600.0d0
            default:
                diflng = lngref[i] - lngfit[i]
            }
            switch (sk_stati (coo, S_NLATUNITS)) {
            case SKY_DEGREES:
                diflat = (latref[i] - latfit[i]) * 3600.0d0
            case SKY_HOURS:
                diflat = 15.0d0 * (latref[i] - latfit[i]) * 3600.0d0
            case SKY_RADIANS:
                diflat = RADTODEG ((latref[i] - latfit[i])) * 3600.0d0
            default:
                diflat = latref[i] - latfit[i]
            }
            call fprintf (fd, Memc[fmtstr])
                call pargd (xref[i])
                call pargd (yref[i])
                call pargd (lngref[i])
                call pargd (latref[i])
            if (Memd[twts+i-1] > 0.0d0) {
                call pargd (lngfit[i])
                call pargd (latfit[i])
                call pargd (diflng)
                call pargd (diflat)
            } else {
                call pargd (INDEFD)
                call pargd (INDEFD)
                call pargd (INDEFD)
                call pargd (INDEFD)
            }
        }

        call fprintf (fd, "\n")

        call sfree (sp)
end
