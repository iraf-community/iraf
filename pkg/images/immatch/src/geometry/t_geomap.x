# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <error.h>
include <mach.h>
include <math.h>
include <math/gsurfit.h>
include "../../../lib/geomap.h"

define	GM_REAL		1	# computation type is real
define	GM_DOUBLE	2	# computation type is double



# T_GEOMAP -- Procedure to calculate the transformation required to transform
# the coordinate system of a reference image to the coordinate system of
# an input image. The transformation is of the following form.
#
#		xin = f (xref, yref)
#		yin = g (xref, yref)

procedure t_geomap ()

bool	verbose, interactive
double	xmin, xmax, ymin, ymax, reject
int	geometry, function, calctype, nfiles, list, in, reclist, nrecords
int	xxorder, xyorder, xxterms, yxorder, yyorder, yxterms, maxiter
int	reslist, nresfiles, res
pointer	sp, in_name, str, out, fit, gd, graphics
real	rxmin, rxmax, rymin, rymax

bool	clgetb()
double	clgetd()
int	clgeti(), clgwrd(), clplen(), errget(), imtopenp(), imtlen()
int	imtgetim()
pointer	clpopnu(), clgfil(), dtmap(), gopen(), open()

errchk	geo_mapr(), geo_mapd()

begin
	# Get working space.
	call smark (sp)
	call salloc (in_name, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (str, max(SZ_LINE, SZ_FNAME), TY_CHAR)

	# Get input data file(s).
	list = clpopnu ("input")
	nfiles = clplen (list)

	# Open database output file.
	call clgstr ("database", Memc[str], SZ_FNAME)
	out = dtmap (Memc[str], APPEND)

	# Get minimum and maximum reference values.
	xmin = clgetd ("xmin")
	if (IS_INDEFD(xmin))
	    rxmin = INDEFR
	else
	    rxmin = xmin
	xmax = clgetd ("xmax")
	if (IS_INDEFD(xmax))
	    rxmax = INDEFR
	else
	    rxmax = xmax
	ymin = clgetd ("ymin")
	if (IS_INDEFD(ymin))
	    rymin = INDEFR
	else
	    rymin = ymin
	ymax = clgetd ("ymax")
	if (IS_INDEFD(ymax))
	    rymax = INDEFR
	else
	    rymax = ymax

	# Get the records list.
	reclist = imtopenp ("transforms")
	nrecords = imtlen (reclist)
	if ((nrecords > 0) && (nrecords != nfiles)) {
	    call eprintf (
	    "The number of records is not equal to the number of input files")
	    call clpcls (list)
	    call dtunmap (out)
	    call imtclose (reclist)
	    call sfree (sp)
	    return
	}

	# Get the results file list.
	reslist = clpopnu ("results")
	nresfiles = clplen (reslist)
	if (nresfiles > 1 && nresfiles !=  nfiles) {
	    call eprintf ("Error: there are too few results files\n")
	    call clpcls (list)
	    call dtunmap (out)
	    call imtclose (reclist)
	    call clpcls (reslist)
	    call sfree (sp)
	    return
	}

	# Get the surface fitting parameters.
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
	calctype = clgwrd ("calctype", Memc[str], SZ_LINE, ",real,double,")

	# Get the graphics parameters.
	verbose = clgetb ("verbose")
	interactive = clgetb ("interactive")
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)

	# Flush standard output on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize the fit structure.
	call geo_minit (fit, GM_NONE, geometry, function, xxorder, xyorder,
	    xxterms, yxorder, yyorder, yxterms, maxiter, reject)

	# Loop over the files.
	while (clgfil (list, Memc[in_name], SZ_FNAME) != EOF) {

	    # Open text file of coordinates.
	    in = open (Memc[in_name], READ_ONLY, TEXT_FILE)

	    # Open the results files.
	    if (nresfiles <= 0)
		res = NULL
	    else if (clgfil (reslist, Memc[str], SZ_FNAME) != EOF)
		res = open (Memc[str], NEW_FILE, TEXT_FILE)

	    # Set file name in structure.
	    if (nrecords > 0) {
		if (imtgetim (reclist, GM_RECORD(fit), SZ_FNAME) != EOF)
		    ;
	    } else
	        call strcpy (Memc[in_name], GM_RECORD(fit), SZ_FNAME)

	    if (verbose && res != STDOUT) {
	        call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	        call printf ("\nCoordinate list: %s  Transform: %s\n")
		    call pargstr (Memc[str])
		    call pargstr (GM_RECORD(fit))
		if (res != NULL) 
	            call fstats (res, F_FILENAME, Memc[str], SZ_FNAME)
		else
		    call strcpy ("", Memc[str], SZ_FNAME)
	        call printf ("    Results file: %s\n")
		    call pargstr (Memc[str])
		call flush (STDOUT)
	    }
	    if (res != NULL) {
	        call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	        call fprintf (res, "\n# Coordinate list: %s  Transform: %s\n")
		    call pargstr (Memc[str])
		    call pargstr (GM_RECORD(fit))
		if (res != NULL) 
	            call fstats (res, F_FILENAME, Memc[str], SZ_FNAME)
		else
		    call strcpy ("", Memc[str], SZ_FNAME)
	        call fprintf (res, "#     Results file: %s\n")
		    call pargstr (Memc[str])
		call flush (STDOUT)
	    }

	    if (interactive) {
	        gd = gopen (Memc[graphics], NEW_FILE, STDGRAPH)
	    } else
	        gd = NULL

	    iferr { 
		if (calctype == GM_REAL) 
	            call geo_mapr (gd, in, out, res, fit, rxmin, rxmax, rymin,
		        rymax, verbose)
		else
		    call geo_mapd (gd, in, out, res, fit, xmin, xmax, ymin,
		        ymax, verbose)
	    } then {
		if (verbose && res != STDOUT) {
		    call printf ("Error fitting coordinate list: %s\n")
		        call pargstr (Memc[in_name])
		    call flush (STDOUT)
		    if (errget (Memc[str], SZ_LINE) == 0)
			;
		    call printf ("\t%s\n")
			call pargstr (Memc[str))
		}
		if (res != NULL) {
		    call fprintf (res, "# Error fitting coordinate list: %s\n")
		        call pargstr (Memc[in_name])
		    call flush (STDOUT)
		    if (errget (Memc[str], SZ_LINE) == 0)
			;
		    call fprintf (res, "#     %s\n")
			call pargstr (Memc[str))
		}
	    }

	    call close (in)
	    if (nresfiles == nfiles)
	        call close ( res)

	    if (gd != NULL)
	        call gclose (gd)
	}

	# Close up.
	call geo_free (fit)
	if (nresfiles < nfiles)
	    call close ( res)
	call dtunmap (out)
	call imtclose (reclist)
	call clpcls (list)
	call sfree (sp)
end





# GEO_MAP -- Procedure to calculate the coordinate transformations

procedure geo_mapr (gd, in, out, res, fit, xmin, xmax, ymin, ymax, verbose)

pointer	gd			#I the graphics stream
int	in			#I the input file descriptor
pointer	out			#I the output file descriptor
int	res			#I the results file descriptor
pointer	fit			#I pointer to fit parameters
real	xmin, xmax		#I max and min xref values
real	ymin, ymax		#I max and min yref values
bool	verbose			#I verbose mode

int	npts, ngood
pointer	sp, str, xref, yref, xin, yin, wts, xfit, yfit, xerrmsg, yerrmsg
pointer	sx1, sy1, sx2, sy2
real	mintemp, maxtemp

real	asumr()
int	geo_rdxyr()
errchk	geo_fitr, geo_mgfitr()

begin
	# Get working space.
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (xerrmsg, SZ_LINE, TY_CHAR)
	call salloc (yerrmsg, SZ_LINE, TY_CHAR)

	# Initialize pointers.
	xref = NULL
	yref = NULL
	xin = NULL
	yin = NULL
	wts = NULL

	# Read in data and check that data is in range.
	npts = geo_rdxyr (in, xref, yref, xin, yin, xmin, xmax, ymin, ymax)
	if (npts <= 0) {
	    call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	    call printf ("Coordinate list: %s has no data in range.\n")
		call pargstr (Memc[str])
	    call sfree (sp)
	    return
	}

	# Compute the mean of the reference and input coordinates.
	GM_XOREF(fit) = double (asumr (Memr[xref], npts) / npts)
	GM_YOREF(fit) = double (asumr (Memr[yref], npts) / npts)
	GM_XOIN(fit) = double (asumr (Memr[xin], npts) / npts)
	GM_YOIN(fit) = double (asumr (Memr[yin], npts) / npts)

	# Set the reference point for the projections to INDEF.
	GM_XREFPT(fit) = INDEFD
	GM_YREFPT(fit) = INDEFD

	# Compute the weights.
	call malloc (xfit, npts, TY_REAL)
	call malloc (yfit, npts, TY_REAL)
	call malloc (wts, npts, TY_REAL)
	call amovkr (real(1.), Memr[wts], npts)

	# Determine the x max and min.
	if (IS_INDEFR(xmin) || IS_INDEFR(xmax)) {
	    call alimr (Memr[xref], npts, mintemp, maxtemp)
	    if (! IS_INDEFR(xmin))
		GM_XMIN(fit) = double (xmin)
	    else
		GM_XMIN(fit) = double (mintemp)
	    if (! IS_INDEFR(xmax))
		GM_XMAX(fit) = double (xmax)
	    else
		GM_XMAX(fit) = double (maxtemp)
	} else {
	    GM_XMIN(fit) = double (xmin)
	    GM_XMAX(fit) = double (xmax)
	}

	# Determine the y max and min.
	if (IS_INDEFR(ymin) || IS_INDEFR(ymax)) {
	    call alimr (Memr[yref], npts, mintemp, maxtemp)
	    if (! IS_INDEFR(ymin))
		GM_YMIN(fit) = double (ymin)
	    else
		GM_YMIN(fit) = double (mintemp)
	    if (! IS_INDEFR(ymax))
		GM_YMAX(fit) = double (ymax)
	    else
		GM_YMAX(fit) = double (maxtemp)
	} else {
	    GM_YMIN(fit) = double (ymin)
	    GM_YMAX(fit) = double (ymax)
	}

	# Initalize surface pointers.
	sx1 = NULL
	sy1 = NULL
	sx2 = NULL
	sy2 = NULL

	# Fit the data.
	if (gd != NULL) {
	    iferr {
	        call geo_mgfitr (gd, fit, sx1, sy1, sx2, sy2, Memr[xref],
	            Memr[yref], Memr[xin], Memr[yin], Memr[wts], npts,
		    Memc[xerrmsg], Memc[yerrmsg], SZ_LINE)
	    } then {
		call gdeactivate (gd, 0)
		call mfree (xfit, TY_REAL)
		call mfree (yfit, TY_REAL)
		call mfree (wts, TY_REAL)
		call geo_mmfreer (sx1, sy1, sx2, sy2)
		call sfree (sp)
		call error (3, "Too few points for X or Y fits.")
	    }
	    call gdeactivate (gd, 0)
	    if (verbose && res != STDOUT) {
		call printf ("Coordinate mapping status\n")
		call flush (STDOUT)
	    }
	    if (res != NULL) {
		call fprintf (res, "# Coordinate mapping status\n")
	    }
	} else {
	    if (verbose && res != STDOUT) {
		call printf ("Coordinate mapping status\n    ")
		call flush (STDOUT)
	    }
	    if (res != NULL) {
		call fprintf (res, "# Coordinate mapping status\n#     ")
	    }
	    iferr {
	        call geo_fitr (fit, sx1, sy1, sx2, sy2, Memr[xref],
		    Memr[yref], Memr[xin], Memr[yin], Memr[wts], npts,
		    Memc[xerrmsg], Memc[yerrmsg], SZ_LINE)
	    } then {
		call mfree (xfit, TY_REAL)
		call mfree (yfit, TY_REAL)
		call mfree (wts, TY_REAL)
		call geo_mmfreer (sx1, sy1, sx2, sy2)
		call sfree (sp)
		call error (3, "Too few points for X or Y fits.")
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
		call flush (STDOUT)
	    }
	}
	ngood = GM_NPTS(fit) - GM_NWTS0(fit)
	if (verbose && res != STDOUT) {
	    call printf ("    Xin and Yin fit rms: %0.7g  %0.7g\n")
	    if (ngood <= 1) {
		call pargd (0.0d0)
		call pargd (0.0d0)
	    } else {
		call pargd (sqrt (GM_XRMS(fit) / (ngood - 1)))
		call pargd (sqrt (GM_YRMS(fit) / (ngood - 1)))
	    }
	    call geo_showr (STDOUT, fit, sx1, sy1, NO)
	}
	if (res != NULL) {
	    call fprintf (res, "#     Xin and Yin fit rms: %0.7g  %0.7g\n")
	    if (ngood <= 1) {
		call pargd (0.0)
		call pargd (0.0)
	    } else {
		call pargd (sqrt (GM_XRMS(fit) / (ngood - 1)))
		call pargd (sqrt (GM_YRMS(fit) / (ngood - 1)))
	    }
	    call geo_showr (res, fit, sx1, sy1, YES)
	}

	# Compute and print the fitted x and y values.
	if (res != NULL) {
	    call geo_evalr (sx1, sy1, sx2, sy2, Memr[xref], Memr[yref],
		Memr[xfit], Memr[yfit], npts)
	    call geo_plistr (res, fit, Memr[xref], Memr[yref], Memr[xin],
		Memr[yin], Memr[xfit], Memr[yfit], Memr[wts], npts)
	}

	# Free the data
	if (xref != NULL)
	    call mfree (xref, TY_REAL)
	if (yref != NULL)
	    call mfree (yref, TY_REAL)
	if (xin != NULL)
	    call mfree (xin, TY_REAL)
	if (yin != NULL)
	    call mfree (yin, TY_REAL)
	if (xfit != NULL)
	    call mfree (xfit, TY_REAL)
	if (yfit != NULL)
	    call mfree (yfit, TY_REAL)
	if (wts != NULL)
	    call mfree (wts, TY_REAL)

	# Output the data.
	call geo_moutr (fit, out, sx1, sy1, sx2, sy2)

	# Free the space and close files.
	call geo_mmfreer (sx1, sy1, sx2, sy2)
	call sfree (sp)
end


define	GEO_DEFBUFSIZE	1000	# default data buffer sizes

# GEO_RDXY -- Read in the data points.

int procedure geo_rdxyr (fd, xref, yref, xin, yin, xmin, xmax, ymin, ymax)

int	fd			# the input file descriptor
pointer	xref			# the x reference coordinates
pointer	yref			# the y reference coordinates
pointer	xin			# the x coordinates
pointer	yin			# the y coordinates
real	xmin, xmax		# the range of the x coordinates
real	ymin, ymax		# the range of the y coordinates

int	npts, bufsize
int	fscan(), nscan()

begin
	bufsize = GEO_DEFBUFSIZE
	call malloc (xref, bufsize, TY_REAL)
	call malloc (yref, bufsize, TY_REAL)
	call malloc (xin, bufsize, TY_REAL)
	call malloc (yin, bufsize, TY_REAL)

	npts = 0
	while (fscan (fd) != EOF) {

	    # Decode the data.
	    call gargr (Memr[xref+npts])
	    call gargr (Memr[yref+npts])
	    call gargr (Memr[xin+npts])
	    call gargr (Memr[yin+npts])
	    if (nscan() < 4)
		next

	    # Check the data limits.
	    if (! IS_INDEFR(xmin)) {
	        if (Memr[xref+npts] < xmin)
		    next
	    }
	    if (! IS_INDEFR(xmax)) {
	        if (Memr[xref+npts] > xmax)
		    next
	    }
	    if (! IS_INDEFR(ymin)) {
	        if (Memr[yref+npts] < ymin)
		    next
	    }
	    if (! IS_INDEFR(ymax)) {
		if (Memr[yref+npts] > ymax)
		    next
	    }

	    npts = npts + 1
	    if (npts >= bufsize) {
		bufsize = bufsize + GEO_DEFBUFSIZE
		call realloc (xref, bufsize, TY_REAL)
		call realloc (yref, bufsize, TY_REAL)
		call realloc (xin, bufsize, TY_REAL)
		call realloc (yin, bufsize, TY_REAL)
	    }
	}

	if (npts <= 0) {
	    call mfree (xref, TY_REAL)
	    call mfree (yref, TY_REAL)
	    call mfree (xin, TY_REAL)
	    call mfree (yin, TY_REAL)
	    xref = NULL
	    yref = NULL
	    xin = NULL
	    yin = NULL
	} else if (npts < bufsize) {
	    call realloc (xref, npts, TY_REAL)
	    call realloc (yref, npts, TY_REAL)
	    call realloc (xin, npts, TY_REAL)
	    call realloc (yin, npts, TY_REAL)
	}

	return (npts)
end


# GEO_EVAL -- Evalute the fit.

procedure geo_evalr (sx1, sy1, sx2, sy2, xref, yref, xi, eta, npts)

pointer sx1, sy1                #I pointer to linear surfaces
pointer sx2, sy2                #I pointer to higher order surfaces
real   xref[ARB]               #I the x reference coordinates
real   yref[ARB]               #I the y reference coordinates
real   xi[ARB]                 #O the fitted xi coordinates
real   eta[ARB]                #O the fitted eta coordinates
int     npts                    #I the number of points

pointer sp, temp

begin
        call smark (sp)
        call salloc (temp, npts, TY_REAL)

        call gsvector (sx1, xref, yref, xi, npts)
        if (sx2 != NULL) {
            call gsvector (sx2, xref, yref, Memr[temp], npts)
            call aaddr (Memr[temp], xi, xi, npts)
        }
        call gsvector (sy1, xref, yref, eta, npts)
        if (sy2 != NULL) {
            call gsvector (sy2, xref, yref, Memr[temp], npts)

            call aaddr (Memr[temp], eta, eta, npts)
        }

        call sfree (sp)
end


# GEO_MOUT -- Write the output database file.

procedure geo_moutr (fit, out, sx1, sy1, sx2, sy2)

pointer	fit		#I pointer to fitting structure
int	out		#I pointer to database file
pointer	sx1, sy1	#I pointer to linear surfaces
pointer	sx2, sy2	#I pointer to distortion surfaces

int	i, npts, ncoeff
pointer	sp, str, xcoeff, ycoeff
real	xrms, yrms, xshift, yshift, xscale, yscale, xrot, yrot
int	gsgeti()
int	rg_wrdstr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Compute the x and y fit rms.
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

	# Print title.
	call dtptime (out)
	call dtput (out, "begin\t%s\n")
	    call pargstr (GM_RECORD(fit))

	# Print the x and y mean values.
	call dtput (out, "\txrefmean\t%g\n")
	    call pargd (GM_XOREF(fit))
	call dtput (out, "\tyrefmean\t%g\n")
	    call pargd (GM_YOREF(fit))
	call dtput (out, "\txmean\t\t%g\n")
	    call pargd (GM_XOIN(fit))
	call dtput (out, "\tymean\t\t%g\n")
	    call pargd (GM_YOIN(fit))

	# Print some of the fitting parameters.
	if (rg_wrdstr (GM_FIT(fit), Memc[str], SZ_FNAME, GM_GEOMETRIES) <= 0)
	    call strcpy ("general", Memc[str], SZ_FNAME)
	call dtput (out, "\tgeometry\t%s\n")
	    call pargstr (Memc[str])
	if (rg_wrdstr (GM_FUNCTION(fit), Memc[str], SZ_FNAME, GM_FUNCS) <= 0)
	    call strcpy ("polynomial", Memc[str], SZ_FNAME)
	call dtput (out, "\tfunction\t%s\n")
	    call pargstr (Memc[str])

	# Output the geometric parameters.
	call geo_lcoeffr (sx1, sy1, xshift, yshift, xscale, yscale, xrot, yrot)
	call dtput (out, "\txshift\t\t%g\n")
	    call pargr (xshift)
	call dtput (out, "\tyshift\t\t%g\n")
	    call pargr (yshift)
	call dtput (out, "\txmag\t\t%g\n")
	    call pargr (xscale)
	call dtput (out, "\tymag\t\t%g\n")
	    call pargr (yscale)
	call dtput (out, "\txrotation\t%g\n")
	    call pargr (xrot)
	call dtput (out, "\tyrotation\t%g\n")
	    call pargr (yrot)

	# Out the rms values.
	call dtput (out, "\txrms\t\t%g\n")
	    call pargr (real(xrms))
	call dtput (out, "\tyrms\t\t%g\n")
	    call pargr (real(yrms))

	# Allocate memory for linear coefficients.
	ncoeff = max (gsgeti (sx1, GSNSAVE), gsgeti (sy1, GSNSAVE))
	call calloc (xcoeff, ncoeff, TY_REAL)
	call calloc (ycoeff, ncoeff, TY_REAL)

	# Output the linear coefficients.
	call gssave (sx1, Memr[xcoeff])
	call gssave (sy1, Memr[ycoeff])
	call dtput (out, "\tsurface1\t%d\n")
	    call pargi (ncoeff)
	do i = 1, ncoeff {
	    call dtput (out, "\t\t\t%g\t%g\n")
		call pargr (Memr[xcoeff+i-1])
		call pargr (Memr[ycoeff+i-1])
	}

	call mfree (xcoeff, TY_REAL)
	call mfree (ycoeff, TY_REAL)

	# Allocate memory for higer order coefficients.
	if (sx2 == NULL)
	    ncoeff = 0
	else
	    ncoeff = gsgeti (sx2, GSNSAVE)
	if (sy2 == NULL)
	    ncoeff = max (0, ncoeff)
	else
	    ncoeff = max (gsgeti (sy2, GSNSAVE), ncoeff)
	call calloc (xcoeff, ncoeff, TY_REAL)
	call calloc (ycoeff, ncoeff, TY_REAL)

	# Save the coefficients.
	call gssave (sx2, Memr[xcoeff])
	call gssave (sy2, Memr[ycoeff])

	# Output the coefficients.
	call dtput (out, "\tsurface2\t%d\n")
	    call pargi (ncoeff)
	do i = 1, ncoeff {
	    call dtput (out, "\t\t\t%g\t%g\n")
		call pargr (Memr[xcoeff+i-1])
		call pargr (Memr[ycoeff+i-1])
	}

	# Cleanup.
	call mfree (xcoeff, TY_REAL)
	call mfree (ycoeff, TY_REAL)
	call sfree (sp)
end


# GEO_PLIST -- Print the input, output, and fitted data and the residuals.

procedure geo_plistr (fd, fit, xref, yref, xin, yin, xfit, yfit, wts, npts)

int     fd                      #I the results file descriptor
pointer fit                     #I pointer to the fit structure
real   xref[ARB]               #I the input x coordinates
real   yref[ARB]               #I the input y coordinates
real   xin[ARB]                #I the input ra / longitude coordinates
real   yin[ARB]                #I the input dec / latitude coordinates
real   xfit[ARB]               #I the fitted ra / longitude coordinates
real   yfit[ARB]               #I the fitted dec / latitude coordinates
real   wts[ARB]                #I the weights array
int     npts                    #I the number of data points

int     i, index
pointer sp, fmtstr, twts

begin
        # Allocate working space.
        call smark (sp)
        call salloc (fmtstr, SZ_LINE, TY_CHAR)
        call salloc (twts, npts, TY_REAL)

        # Compute the weights.
        call amovr (wts, Memr[twts], npts)
        do i = 1, GM_NREJECT(fit) {
            index = Memi[GM_REJ(fit)+i-1]
            if (wts[index] > real(0.0))
                Memr[twts+index-1] = real(0.0)
        }

        # Print banner.
        call fprintf (fd, "\n# Input Coordinate Listing\n")
        call fprintf (fd, "#     Column 1: X (reference) \n")
        call fprintf (fd, "#     Column 2: Y (reference)\n")
        call fprintf (fd, "#     Column 3: X (input)\n")
        call fprintf (fd, "#     Column 4: Y (input)\n")
        call fprintf (fd, "#     Column 5: X (fit)\n")
        call fprintf (fd, "#     Column 6: Y (fit)\n")
        call fprintf (fd, "#     Column 7: X (residual)\n")
        call fprintf (fd, "#     Column 8: Y (residual)\n\n")

        # Create the format string.
        call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s  %s %s  %s %s\n")
	    call pargstr ("%9.7g")
	    call pargstr ("%9.7g")
	    call pargstr ("%9.7g")
	    call pargstr ("%9.7g")
	    call pargstr ("%9.7g")
	    call pargstr ("%9.7g")
	    call pargstr ("%9.7g")
	    call pargstr ("%9.7g")

	# Print the data.
	do i = 1, npts {
	    call fprintf (fd, Memc[fmtstr])
		call pargr (xref[i])
		call pargr (yref[i])
		call pargr (xin[i])
		call pargr (yin[i])
           if (Memr[twts+i-1] > 0.0d0) {
                call pargr (xfit[i])
                call pargr (yfit[i])
                call pargr (xin[i] - xfit[i])
                call pargr (yin[i] - yfit[i])
            } else {
                call pargr (INDEFR)
                call pargr (INDEFR)
                call pargr (INDEFR)
                call pargr (INDEFR)
            }

	}

        call fprintf (fd, "\n")

	call sfree (sp)

end

# GEO_SHOW -- Print the coordinate mapping parameters.

procedure geo_showr (fd, fit, sx1, sy1, comment)

int     fd                      #I the output file descriptor
pointer	fit			#I pointer to the fit structure
pointer sx1, sy1                #I pointer to linear surfaces
int     comment                 #I comment the output ?

real   xshift, yshift, a, b, c, d
real   xscale, yscale, xrot, yrot
pointer sp, str
bool    fp_equalr()

begin
        # Allocate temporary space.
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)

        # Compute the geometric parameters.
        call geo_gcoeffr (sx1, sy1, xshift, yshift, a, b, c, d)

        if (comment == NO) {
            call fprintf (fd, "Coordinate mapping parameters\n")
        } else {
            call fprintf (fd, "# Coordinate mapping parameters\n")
        }

        if (comment == NO) {
	    call fprintf (fd,
		"    Mean Xref and Yref: %0.7g  %0.7g\n")
		call pargd (GM_XOREF(fit))
		call pargd (GM_YOREF(fit))
	    call fprintf (fd,
		"    Mean Xin and Yin: %0.7g  %0.7g\n")
		call pargd (GM_XOIN(fit))
		call pargd (GM_YOIN(fit))
            call fprintf (fd,
                "    X and Y shift: %0.7g  %0.7g  (xin  yin)\n")
                call pargr (xshift)
                call pargr (yshift)
        } else {
	    call fprintf (fd,
		"#     Mean Xref and Yref: %0.7g  %0.7g\n")
		call pargd (GM_XOREF(fit))
		call pargd (GM_YOREF(fit))
	    call fprintf (fd,
		"#     Mean Xin and Yin: %0.7g  %g0.7\n")
		call pargd (GM_XOIN(fit))
		call pargd (GM_YOIN(fit))
            call fprintf (fd,
                "#     X and Y shift: %0.7g  %0.7g  (xin  yin)\n")
                call pargr (xshift)
                call pargr (yshift)
        }

        # Output the scale factors.
        xscale = sqrt (a * a + c * c)
        yscale = sqrt (b * b + d * d)
        if (comment == NO) {
            call fprintf (fd,
            "    X and Y scale: %0.7g  %0.7g  (xin / xref  yin / yref)\n")
                call pargr (xscale)
                call pargr (yscale)
        } else {
            call fprintf (fd,
        "#     X and Y scale: %0.7g  %0.7g  (xin / xref  yin / yref)\n")
                call pargr (xscale)
                call pargr (yscale)
        }

        # Output the rotation factors.
        if (fp_equalr (a, real(0.0)) && fp_equalr (c, real(0.0)))
            xrot = real(0.0)
        else
            xrot = RADTODEG (atan2 (-c, a))
        if (xrot < real(0.0))
            xrot = xrot + real(360.0)
        if (fp_equalr (b, real(0.0)) && fp_equalr (d, real(0.0)))
            yrot = real(0.0)
        else
            yrot = RADTODEG (atan2 (b, d))
        if (yrot < real(0.0))
            yrot = yrot + real(360.0)
        if (comment == NO) {
            call fprintf (fd,
            "    X and Y axis rotation: %0.5f  %0.5f  (degrees  degrees)\n")
                call pargr (xrot)
                call pargr (yrot)
        } else {
            call fprintf (fd,
            "#     X and Y axis rotation: %0.5f  %0.5f  (degrees  degrees)\n")
                call pargr (xrot)
                call pargr (yrot)
        }

	call sfree (sp)
end



# GEO_MAP -- Procedure to calculate the coordinate transformations

procedure geo_mapd (gd, in, out, res, fit, xmin, xmax, ymin, ymax, verbose)

pointer	gd			#I the graphics stream
int	in			#I the input file descriptor
pointer	out			#I the output file descriptor
int	res			#I the results file descriptor
pointer	fit			#I pointer to fit parameters
double	xmin, xmax		#I max and min xref values
double	ymin, ymax		#I max and min yref values
bool	verbose			#I verbose mode

int	npts, ngood
pointer	sp, str, xref, yref, xin, yin, wts, xfit, yfit, xerrmsg, yerrmsg
pointer	sx1, sy1, sx2, sy2
double	mintemp, maxtemp

double	asumd()
int	geo_rdxyd()
errchk	geo_fitd, geo_mgfitd()

begin
	# Get working space.
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (xerrmsg, SZ_LINE, TY_CHAR)
	call salloc (yerrmsg, SZ_LINE, TY_CHAR)

	# Initialize pointers.
	xref = NULL
	yref = NULL
	xin = NULL
	yin = NULL
	wts = NULL

	# Read in data and check that data is in range.
	npts = geo_rdxyd (in, xref, yref, xin, yin, xmin, xmax, ymin, ymax)
	if (npts <= 0) {
	    call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	    call printf ("Coordinate list: %s has no data in range.\n")
		call pargstr (Memc[str])
	    call sfree (sp)
	    return
	}

	# Compute the mean of the reference and input coordinates.
	GM_XOREF(fit) = double (asumd (Memd[xref], npts) / npts)
	GM_YOREF(fit) = double (asumd (Memd[yref], npts) / npts)
	GM_XOIN(fit) = double (asumd (Memd[xin], npts) / npts)
	GM_YOIN(fit) = double (asumd (Memd[yin], npts) / npts)

	# Set the reference point for the projections to INDEF.
	GM_XREFPT(fit) = INDEFD
	GM_YREFPT(fit) = INDEFD

	# Compute the weights.
	call malloc (xfit, npts, TY_DOUBLE)
	call malloc (yfit, npts, TY_DOUBLE)
	call malloc (wts, npts, TY_DOUBLE)
	call amovkd (double(1.), Memd[wts], npts)

	# Determine the x max and min.
	if (IS_INDEFD(xmin) || IS_INDEFD(xmax)) {
	    call alimd (Memd[xref], npts, mintemp, maxtemp)
	    if (! IS_INDEFD(xmin))
		GM_XMIN(fit) = double (xmin)
	    else
		GM_XMIN(fit) = double (mintemp)
	    if (! IS_INDEFD(xmax))
		GM_XMAX(fit) = double (xmax)
	    else
		GM_XMAX(fit) = double (maxtemp)
	} else {
	    GM_XMIN(fit) = double (xmin)
	    GM_XMAX(fit) = double (xmax)
	}

	# Determine the y max and min.
	if (IS_INDEFD(ymin) || IS_INDEFD(ymax)) {
	    call alimd (Memd[yref], npts, mintemp, maxtemp)
	    if (! IS_INDEFD(ymin))
		GM_YMIN(fit) = double (ymin)
	    else
		GM_YMIN(fit) = double (mintemp)
	    if (! IS_INDEFD(ymax))
		GM_YMAX(fit) = double (ymax)
	    else
		GM_YMAX(fit) = double (maxtemp)
	} else {
	    GM_YMIN(fit) = double (ymin)
	    GM_YMAX(fit) = double (ymax)
	}

	# Initalize surface pointers.
	sx1 = NULL
	sy1 = NULL
	sx2 = NULL
	sy2 = NULL

	# Fit the data.
	if (gd != NULL) {
	    iferr {
	        call geo_mgfitd (gd, fit, sx1, sy1, sx2, sy2, Memd[xref],
	            Memd[yref], Memd[xin], Memd[yin], Memd[wts], npts,
		    Memc[xerrmsg], Memc[yerrmsg], SZ_LINE)
	    } then {
		call gdeactivate (gd, 0)
		call mfree (xfit, TY_DOUBLE)
		call mfree (yfit, TY_DOUBLE)
		call mfree (wts, TY_DOUBLE)
		call geo_mmfreed (sx1, sy1, sx2, sy2)
		call sfree (sp)
		call error (3, "Too few points for X or Y fits.")
	    }
	    call gdeactivate (gd, 0)
	    if (verbose && res != STDOUT) {
		call printf ("Coordinate mapping status\n")
		call flush (STDOUT)
	    }
	    if (res != NULL) {
		call fprintf (res, "# Coordinate mapping status\n")
	    }
	} else {
	    if (verbose && res != STDOUT) {
		call printf ("Coordinate mapping status\n    ")
		call flush (STDOUT)
	    }
	    if (res != NULL) {
		call fprintf (res, "# Coordinate mapping status\n#     ")
	    }
	    iferr {
	        call geo_fitd (fit, sx1, sy1, sx2, sy2, Memd[xref],
		    Memd[yref], Memd[xin], Memd[yin], Memd[wts], npts,
		    Memc[xerrmsg], Memc[yerrmsg], SZ_LINE)
	    } then {
		call mfree (xfit, TY_DOUBLE)
		call mfree (yfit, TY_DOUBLE)
		call mfree (wts, TY_DOUBLE)
		call geo_mmfreed (sx1, sy1, sx2, sy2)
		call sfree (sp)
		call error (3, "Too few points for X or Y fits.")
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
		call flush (STDOUT)
	    }
	}
	ngood = GM_NPTS(fit) - GM_NWTS0(fit)
	if (verbose && res != STDOUT) {
	    call printf ("    Xin and Yin fit rms: %0.7g  %0.7g\n")
	    if (ngood <= 1) {
		call pargd (0.0d0)
		call pargd (0.0d0)
	    } else {
		call pargd (sqrt (GM_XRMS(fit) / (ngood - 1)))
		call pargd (sqrt (GM_YRMS(fit) / (ngood - 1)))
	    }
	    call geo_showd (STDOUT, fit, sx1, sy1, NO)
	}
	if (res != NULL) {
	    call fprintf (res, "#     Xin and Yin fit rms: %0.7g  %0.7g\n")
	    if (ngood <= 1) {
		call pargd (0.0)
		call pargd (0.0)
	    } else {
		call pargd (sqrt (GM_XRMS(fit) / (ngood - 1)))
		call pargd (sqrt (GM_YRMS(fit) / (ngood - 1)))
	    }
	    call geo_showd (res, fit, sx1, sy1, YES)
	}

	# Compute and print the fitted x and y values.
	if (res != NULL) {
	    call geo_evald (sx1, sy1, sx2, sy2, Memd[xref], Memd[yref],
		Memd[xfit], Memd[yfit], npts)
	    call geo_plistd (res, fit, Memd[xref], Memd[yref], Memd[xin],
		Memd[yin], Memd[xfit], Memd[yfit], Memd[wts], npts)
	}

	# Free the data
	if (xref != NULL)
	    call mfree (xref, TY_DOUBLE)
	if (yref != NULL)
	    call mfree (yref, TY_DOUBLE)
	if (xin != NULL)
	    call mfree (xin, TY_DOUBLE)
	if (yin != NULL)
	    call mfree (yin, TY_DOUBLE)
	if (xfit != NULL)
	    call mfree (xfit, TY_DOUBLE)
	if (yfit != NULL)
	    call mfree (yfit, TY_DOUBLE)
	if (wts != NULL)
	    call mfree (wts, TY_DOUBLE)

	# Output the data.
	call geo_moutd (fit, out, sx1, sy1, sx2, sy2)

	# Free the space and close files.
	call geo_mmfreed (sx1, sy1, sx2, sy2)
	call sfree (sp)
end


define	GEO_DEFBUFSIZE	1000	# default data buffer sizes

# GEO_RDXY -- Read in the data points.

int procedure geo_rdxyd (fd, xref, yref, xin, yin, xmin, xmax, ymin, ymax)

int	fd			# the input file descriptor
pointer	xref			# the x reference coordinates
pointer	yref			# the y reference coordinates
pointer	xin			# the x coordinates
pointer	yin			# the y coordinates
double	xmin, xmax		# the range of the x coordinates
double	ymin, ymax		# the range of the y coordinates

int	npts, bufsize
int	fscan(), nscan()

begin
	bufsize = GEO_DEFBUFSIZE
	call malloc (xref, bufsize, TY_DOUBLE)
	call malloc (yref, bufsize, TY_DOUBLE)
	call malloc (xin, bufsize, TY_DOUBLE)
	call malloc (yin, bufsize, TY_DOUBLE)

	npts = 0
	while (fscan (fd) != EOF) {

	    # Decode the data.
	    call gargd (Memd[xref+npts])
	    call gargd (Memd[yref+npts])
	    call gargd (Memd[xin+npts])
	    call gargd (Memd[yin+npts])
	    if (nscan() < 4)
		next

	    # Check the data limits.
	    if (! IS_INDEFD(xmin)) {
	        if (Memd[xref+npts] < xmin)
		    next
	    }
	    if (! IS_INDEFD(xmax)) {
	        if (Memd[xref+npts] > xmax)
		    next
	    }
	    if (! IS_INDEFD(ymin)) {
	        if (Memd[yref+npts] < ymin)
		    next
	    }
	    if (! IS_INDEFD(ymax)) {
		if (Memd[yref+npts] > ymax)
		    next
	    }

	    npts = npts + 1
	    if (npts >= bufsize) {
		bufsize = bufsize + GEO_DEFBUFSIZE
		call realloc (xref, bufsize, TY_DOUBLE)
		call realloc (yref, bufsize, TY_DOUBLE)
		call realloc (xin, bufsize, TY_DOUBLE)
		call realloc (yin, bufsize, TY_DOUBLE)
	    }
	}

	if (npts <= 0) {
	    call mfree (xref, TY_DOUBLE)
	    call mfree (yref, TY_DOUBLE)
	    call mfree (xin, TY_DOUBLE)
	    call mfree (yin, TY_DOUBLE)
	    xref = NULL
	    yref = NULL
	    xin = NULL
	    yin = NULL
	} else if (npts < bufsize) {
	    call realloc (xref, npts, TY_DOUBLE)
	    call realloc (yref, npts, TY_DOUBLE)
	    call realloc (xin, npts, TY_DOUBLE)
	    call realloc (yin, npts, TY_DOUBLE)
	}

	return (npts)
end


# GEO_EVAL -- Evalute the fit.

procedure geo_evald (sx1, sy1, sx2, sy2, xref, yref, xi, eta, npts)

pointer sx1, sy1                #I pointer to linear surfaces
pointer sx2, sy2                #I pointer to higher order surfaces
double   xref[ARB]               #I the x reference coordinates
double   yref[ARB]               #I the y reference coordinates
double   xi[ARB]                 #O the fitted xi coordinates
double   eta[ARB]                #O the fitted eta coordinates
int     npts                    #I the number of points

pointer sp, temp

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


# GEO_MOUT -- Write the output database file.

procedure geo_moutd (fit, out, sx1, sy1, sx2, sy2)

pointer	fit		#I pointer to fitting structure
int	out		#I pointer to database file
pointer	sx1, sy1	#I pointer to linear surfaces
pointer	sx2, sy2	#I pointer to distortion surfaces

int	i, npts, ncoeff
pointer	sp, str, xcoeff, ycoeff
double	xrms, yrms, xshift, yshift, xscale, yscale, xrot, yrot
int	dgsgeti()
int	rg_wrdstr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Compute the x and y fit rms.
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

	# Print title.
	call dtptime (out)
	call dtput (out, "begin\t%s\n")
	    call pargstr (GM_RECORD(fit))

	# Print the x and y mean values.
	call dtput (out, "\txrefmean\t%g\n")
	    call pargd (GM_XOREF(fit))
	call dtput (out, "\tyrefmean\t%g\n")
	    call pargd (GM_YOREF(fit))
	call dtput (out, "\txmean\t\t%g\n")
	    call pargd (GM_XOIN(fit))
	call dtput (out, "\tymean\t\t%g\n")
	    call pargd (GM_YOIN(fit))

	# Print some of the fitting parameters.
	if (rg_wrdstr (GM_FIT(fit), Memc[str], SZ_FNAME, GM_GEOMETRIES) <= 0)
	    call strcpy ("general", Memc[str], SZ_FNAME)
	call dtput (out, "\tgeometry\t%s\n")
	    call pargstr (Memc[str])
	if (rg_wrdstr (GM_FUNCTION(fit), Memc[str], SZ_FNAME, GM_FUNCS) <= 0)
	    call strcpy ("polynomial", Memc[str], SZ_FNAME)
	call dtput (out, "\tfunction\t%s\n")
	    call pargstr (Memc[str])

	# Output the geometric parameters.
	call geo_lcoeffd (sx1, sy1, xshift, yshift, xscale, yscale, xrot, yrot)
	call dtput (out, "\txshift\t\t%g\n")
	    call pargd (xshift)
	call dtput (out, "\tyshift\t\t%g\n")
	    call pargd (yshift)
	call dtput (out, "\txmag\t\t%g\n")
	    call pargd (xscale)
	call dtput (out, "\tymag\t\t%g\n")
	    call pargd (yscale)
	call dtput (out, "\txrotation\t%g\n")
	    call pargd (xrot)
	call dtput (out, "\tyrotation\t%g\n")
	    call pargd (yrot)

	# Out the rms values.
	call dtput (out, "\txrms\t\t%g\n")
	    call pargd (double(xrms))
	call dtput (out, "\tyrms\t\t%g\n")
	    call pargd (double(yrms))

	# Allocate memory for linear coefficients.
	ncoeff = max (dgsgeti (sx1, GSNSAVE), dgsgeti (sy1, GSNSAVE))
	call calloc (xcoeff, ncoeff, TY_DOUBLE)
	call calloc (ycoeff, ncoeff, TY_DOUBLE)

	# Output the linear coefficients.
	call dgssave (sx1, Memd[xcoeff])
	call dgssave (sy1, Memd[ycoeff])
	call dtput (out, "\tsurface1\t%d\n")
	    call pargi (ncoeff)
	do i = 1, ncoeff {
	    call dtput (out, "\t\t\t%g\t%g\n")
		call pargd (Memd[xcoeff+i-1])
		call pargd (Memd[ycoeff+i-1])
	}

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

	# Save the coefficients.
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


# GEO_PLIST -- Print the input, output, and fitted data and the residuals.

procedure geo_plistd (fd, fit, xref, yref, xin, yin, xfit, yfit, wts, npts)

int     fd                      #I the results file descriptor
pointer fit                     #I pointer to the fit structure
double   xref[ARB]               #I the input x coordinates
double   yref[ARB]               #I the input y coordinates
double   xin[ARB]                #I the input ra / longitude coordinates
double   yin[ARB]                #I the input dec / latitude coordinates
double   xfit[ARB]               #I the fitted ra / longitude coordinates
double   yfit[ARB]               #I the fitted dec / latitude coordinates
double   wts[ARB]                #I the weights array
int     npts                    #I the number of data points

int     i, index
pointer sp, fmtstr, twts

begin
        # Allocate working space.
        call smark (sp)
        call salloc (fmtstr, SZ_LINE, TY_CHAR)
        call salloc (twts, npts, TY_DOUBLE)

        # Compute the weights.
        call amovd (wts, Memd[twts], npts)
        do i = 1, GM_NREJECT(fit) {
            index = Memi[GM_REJ(fit)+i-1]
            if (wts[index] > double(0.0))
                Memd[twts+index-1] = double(0.0)
        }

        # Print banner.
        call fprintf (fd, "\n# Input Coordinate Listing\n")
        call fprintf (fd, "#     Column 1: X (reference) \n")
        call fprintf (fd, "#     Column 2: Y (reference)\n")
        call fprintf (fd, "#     Column 3: X (input)\n")
        call fprintf (fd, "#     Column 4: Y (input)\n")
        call fprintf (fd, "#     Column 5: X (fit)\n")
        call fprintf (fd, "#     Column 6: Y (fit)\n")
        call fprintf (fd, "#     Column 7: X (residual)\n")
        call fprintf (fd, "#     Column 8: Y (residual)\n\n")

        # Create the format string.
        call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s  %s %s  %s %s\n")
	    call pargstr ("%16.14g")
	    call pargstr ("%16.14g")
	    call pargstr ("%16.14g")
	    call pargstr ("%16.14g")
	    call pargstr ("%16.14g")
	    call pargstr ("%16.14g")
	    call pargstr ("%16.14g")
	    call pargstr ("%16.14g")

	# Print the data.
	do i = 1, npts {
	    call fprintf (fd, Memc[fmtstr])
		call pargd (xref[i])
		call pargd (yref[i])
		call pargd (xin[i])
		call pargd (yin[i])
           if (Memd[twts+i-1] > 0.0d0) {
                call pargd (xfit[i])
                call pargd (yfit[i])
                call pargd (xin[i] - xfit[i])
                call pargd (yin[i] - yfit[i])
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

# GEO_SHOW -- Print the coordinate mapping parameters.

procedure geo_showd (fd, fit, sx1, sy1, comment)

int     fd                      #I the output file descriptor
pointer	fit			#I pointer to the fit structure
pointer sx1, sy1                #I pointer to linear surfaces
int     comment                 #I comment the output ?

double   xshift, yshift, a, b, c, d
double   xscale, yscale, xrot, yrot
pointer sp, str
bool    fp_equald()

begin
        # Allocate temporary space.
        call smark (sp)
        call salloc (str, SZ_LINE, TY_CHAR)

        # Compute the geometric parameters.
        call geo_gcoeffd (sx1, sy1, xshift, yshift, a, b, c, d)

        if (comment == NO) {
            call fprintf (fd, "Coordinate mapping parameters\n")
        } else {
            call fprintf (fd, "# Coordinate mapping parameters\n")
        }

        if (comment == NO) {
	    call fprintf (fd,
		"    Mean Xref and Yref: %0.7g  %0.7g\n")
		call pargd (GM_XOREF(fit))
		call pargd (GM_YOREF(fit))
	    call fprintf (fd,
		"    Mean Xin and Yin: %0.7g  %0.7g\n")
		call pargd (GM_XOIN(fit))
		call pargd (GM_YOIN(fit))
            call fprintf (fd,
                "    X and Y shift: %0.7g  %0.7g  (xin  yin)\n")
                call pargd (xshift)
                call pargd (yshift)
        } else {
	    call fprintf (fd,
		"#     Mean Xref and Yref: %0.7g  %0.7g\n")
		call pargd (GM_XOREF(fit))
		call pargd (GM_YOREF(fit))
	    call fprintf (fd,
		"#     Mean Xin and Yin: %0.7g  %g0.7\n")
		call pargd (GM_XOIN(fit))
		call pargd (GM_YOIN(fit))
            call fprintf (fd,
                "#     X and Y shift: %0.7g  %0.7g  (xin  yin)\n")
                call pargd (xshift)
                call pargd (yshift)
        }

        # Output the scale factors.
        xscale = sqrt (a * a + c * c)
        yscale = sqrt (b * b + d * d)
        if (comment == NO) {
            call fprintf (fd,
            "    X and Y scale: %0.7g  %0.7g  (xin / xref  yin / yref)\n")
                call pargd (xscale)
                call pargd (yscale)
        } else {
            call fprintf (fd,
        "#     X and Y scale: %0.7g  %0.7g  (xin / xref  yin / yref)\n")
                call pargd (xscale)
                call pargd (yscale)
        }

        # Output the rotation factors.
        if (fp_equald (a, double(0.0)) && fp_equald (c, double(0.0)))
            xrot = double(0.0)
        else
            xrot = RADTODEG (atan2 (-c, a))
        if (xrot < double(0.0))
            xrot = xrot + double(360.0)
        if (fp_equald (b, double(0.0)) && fp_equald (d, double(0.0)))
            yrot = double(0.0)
        else
            yrot = RADTODEG (atan2 (b, d))
        if (yrot < double(0.0))
            yrot = yrot + double(360.0)
        if (comment == NO) {
            call fprintf (fd,
            "    X and Y axis rotation: %0.5f  %0.5f  (degrees  degrees)\n")
                call pargd (xrot)
                call pargd (yrot)
        } else {
            call fprintf (fd,
            "#     X and Y axis rotation: %0.5f  %0.5f  (degrees  degrees)\n")
                call pargd (xrot)
                call pargd (yrot)
        }

	call sfree (sp)
end


