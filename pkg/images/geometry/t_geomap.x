# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <error.h>
include <mach.h>
include <math/gsurfit.h>
include <pkg/dttext.h>
include "geomap.h"

define	GM_REAL		1	# computation type is real
define	GM_DOUBLE	2	# computation type is double

# T_GEOMAP -- Procedure to calculate the transformation required to transform
# the coordinate system of a reference image to the coordinate system of
# an input image. The transformation is of the following form.
#
#		xin = f (xref, yref)
#		yin = g (xref, yref)

procedure t_geomap ()

char	output[SZ_FNAME]		# output file
double	xmin, xmax, ymin, ymax		# minimum and maximum reference values
int	function			# fitting function
int	xxorder, xyorder, xxterms	# x fit fitting parameters
int	yxorder, yyorder, yxterms	# y fit fitting parameters
real	reject				# number of sigma of rejection
int	calctype			# data type of computation
bool	verbose				# verbose mode
bool	interactive			# interactive graphics
char	device[SZ_FNAME]		# graphics device

char	in_name[SZ_FNAME], str[SZ_LINE]
int	nfiles
pointer	list, in, out, fit, gd

bool	clgetb()
double	clgetd()
int	clgeti(), btoi(), clgwrd(), clplen()
pointer	clpopni(), clgfil(), dtmap(), gopen(), open()
real	clgetr()


begin
	# get input data file(s)
	list = clpopni ("input")
	nfiles = clplen (list)

	# open database output file
	call clgstr ("output", output, SZ_FNAME)
	out = dtmap (output, APPEND)

	# minimum and maximum reference values
	xmin = clgetd ("xmin")
	xmax = clgetd ("xmax")
	ymin = clgetd ("ymin")
	ymax = clgetd ("ymax")

	# surface fitting parameters
	function = clgwrd ("function", str, SZ_LINE,
	    ",chebyshev,legendre,polynomial,")
	xxorder = clgeti ("xxorder")
	xyorder = clgeti ("xyorder")
	xxterms = btoi (clgetb ("xxterms"))
	yxorder = clgeti ("yxorder")
	yyorder = clgeti ("yyorder")
	yxterms = btoi (clgetb ("yxterms"))
	reject = clgetr ("reject")
	calctype = clgwrd ("calctype", str, SZ_LINE, ",real,double,")

	# graphics parameters
	verbose = clgetb ("verbose")
	interactive = clgetb ("interactive")
	call clgstr ("graphics", device, SZ_FNAME)

	# flush standard output on newline
	call fseti (STDOUT, F_FLUSHNL, YES)

	# initialize the fit structure
	call geominit (fit, function, xxorder, xyorder, xxterms,
	    yxorder, yyorder, yxterms, reject)

	# open graphics stream
	if (interactive)
	    gd = gopen (device, NEW_FILE, STDGRAPH)
	else
	    gd = NULL

	# loop over the files
	while (clgfil (list, in_name, SZ_FNAME) != EOF) {

	    # open text file of coordinates
	    in = open (in_name, READ_ONLY, TEXT_FILE)

	    # set file name in structure
	    call strcpy (in_name, GM_NAME(fit), SZ_FNAME)

	    iferr { 
		if (calctype == GM_REAL)
	            call geomap (in, out, fit, gd, real(xmin), real(xmax),
		        real(ymin), real(ymax), verbose)
		else
		    call geomapd (in, out, fit, gd, xmin, xmax, ymin, ymax,
			verbose)
	    } then {
		call eprintf ("Error fitting coordinate list: %s\n\t")
		    call pargstr (in_name)
		call flush (STDERR)
		call erract (EA_WARN)
	    }

	    call close (in)
	}

	# close up
	call geofree (fit)
	if (gd != NULL)
	    call gclose (gd)
	call dtunmap (out)
	call clpcls (list)
end

# GEOMAP -- Procedure to calculate the coordinate transformations

procedure geomap (in, out, fit, gd, xmin, xmax, ymin, ymax, verbose)

pointer	in			# pointer to input file
pointer	out			# pointer to output file
pointer	fit			# pointer to fit parameters
pointer	gd			# graphics stream pointer
real	xmin, xmax		# max and min xref values
real	ymin, ymax		# max and min yref values
bool	verbose			# verbose mode

int	npts
pointer	sp, str, xref, yref, xin, yin, wts
pointer	sx1, sy1, sx2, sy2

int	geo_npts(), geo_getpts()
real	asumr()
errchk	geofit(), geomgfit()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# if no data proceed to next file
	npts = geo_npts (in)
	if (npts == 0) {
	    call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	    call eprintf ("Coordinate list: %s is empty.\n")
		call pargstr (Memc[str])
	    call sfree (sp)
	    return
	}

	# allocate space for data
	call salloc (xref, npts, TY_REAL)
	call salloc (yref, npts, TY_REAL)
	call salloc (xin, npts, TY_REAL)
	call salloc (yin, npts, TY_REAL)
	call salloc (wts, npts, TY_REAL)

	# read in data and check that data is in range
	npts = geo_getpts (in, Memr[xref], Memr[yref], Memr[xin], Memr[yin],
	    xmin, xmax, ymin, ymax)
	if (npts == 0) {
	    call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	    call eprintf ("Coordinate list: %s has no data in range.\n")
		call pargstr (Memc[str])
	    call sfree (sp)
	    return
	}

	# compute the mean of the reference and input coordinates.
	GM_XOREF(fit) = asumr (Memr[xref], npts) / npts
	GM_YOREF(fit) = asumr (Memr[yref], npts) / npts
	GM_XOIN(fit) = asumr (Memr[xin], npts) / npts
	GM_YOIN(fit) = asumr (Memr[yin], npts) / npts
	call amovkr (1., Memr[wts], npts)

	# determine x max and min
	if (IS_INDEFR(xmin) || IS_INDEFR(xmax)) {
	    call alimr (Memr[xref], npts, GM_XMIN(fit), GM_XMAX(fit))
	    if (! IS_INDEFR(xmin))
		GM_XMIN(fit) = xmin
	    if (! IS_INDEFR(xmax))
		GM_XMAX(fit) = xmax
	} else {
	    GM_XMIN(fit) = xmin
	    GM_XMAX(fit) = xmax
	}

	# determine y max and min
	if (IS_INDEFR(ymin) || IS_INDEFR(ymax)) {
	    call alimr (Memr[yref], npts, GM_YMIN(fit), GM_YMAX(fit))
	    if (! IS_INDEFR(ymin))
		GM_YMIN(fit) = ymin
	    if (! IS_INDEFR(ymax))
		GM_YMAX(fit) = ymax
	} else {
	    GM_YMIN(fit) = ymin
	    GM_YMAX(fit) = ymax
	}


	# initalize surface pointers
	sx1 = NULL
	sy1 = NULL
	sx2 = NULL
	sy2 = NULL

	# fit the data
	if (gd != NULL) {
	    call geomgfit (gd, fit, sx1, sy1, sx2, sy2, Memr[xref],
	        Memr[yref], Memr[xin], Memr[yin], Memr[wts], npts)
	} else {
	    if (verbose) {
	        call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	        call printf ("Coordinate list: %s\n\t")
		    call pargstr (Memc[str])
		call flush (STDOUT)
	    }
	    call geofit (fit, sx1, sy1, sx2, sy2, Memr[xref], Memr[yref],
		Memr[xin], Memr[yin], Memr[wts], npts, verbose)
	    if (verbose)
		call flush (STDOUT)
	}

	# output data
	call geomout (fit, out, sx1, sy1, sx2, sy2)

	# free space and close files
	call geomfree (sx1, sy1, sx2, sy2)
	call sfree (sp)
end

# GEOMAPD -- Procedure to calculate the coordinate transformations

procedure geomapd (in, out, fit, gd, xmin, xmax, ymin, ymax, verbose)

pointer	in			# pointer to input file
pointer	out			# pointer to output file
pointer	fit			# pointer to fit parameters
pointer	gd			# graphics stream pointer
double	xmin, xmax		# max and min xref values
double	ymin, ymax		# max and min yref values
bool	verbose			# verbose mode

int	npts
double	mintemp, maxtemp
pointer	sp, str, xref, yref, xin, yin, wts
pointer	sx1, sy1, sx2, sy2

int	geo_npts(), geo_getptsd()
double	asumd()
errchk	geofitd(), geomgfitd()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# if no data proceed to next file
	npts = geo_npts (in)
	if (npts == 0) {
	    call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	    call eprintf ("Coordinate list: %s is empty.\n")
		call pargstr (Memc[str])
	    call sfree (sp)
	    return
	}

	# allocate space for data
	call salloc (xref, npts, TY_DOUBLE)
	call salloc (yref, npts, TY_DOUBLE)
	call salloc (xin, npts, TY_DOUBLE)
	call salloc (yin, npts, TY_DOUBLE)
	call salloc (wts, npts, TY_DOUBLE)

	# read in data and check that data is in range
	npts = geo_getptsd (in, Memd[xref], Memd[yref], Memd[xin], Memd[yin],
	    xmin, xmax, ymin, ymax)
	if (npts == 0) {
	    call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	    call eprintf ("Coordinate list: %s has no data in range.\n")
		call pargstr (Memc[str])
	    call sfree (sp)
	    return
	}

	# prepare data for fitting by subtracting a zero point
	GM_XOREF(fit) = asumd (Memd[xref], npts) / npts
	GM_YOREF(fit) = asumd (Memd[yref], npts) / npts
	GM_XOIN(fit) = asumd (Memd[xin], npts) / npts
	GM_YOIN(fit) = asumd (Memd[yin], npts) / npts
	call amovkd (1.0d0, Memd[wts], npts)

	# determine x max and min
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

	# determine y max and min
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


	# initalize surface pointers
	sx1 = NULL
	sy1 = NULL
	sx2 = NULL
	sy2 = NULL

	# fit the data
	if (gd != NULL) {
	    call geomgfitd (gd, fit, sx1, sy1, sx2, sy2, Memd[xref],
	        Memd[yref], Memd[xin], Memd[yin], Memd[wts], npts)
	} else {
	    if (verbose) {
	        call fstats (in, F_FILENAME, Memc[str], SZ_FNAME)
	        call printf ("Coordinate list: %s\n\t")
		    call pargstr (Memc[str])
		call flush (STDOUT)
	    }
	    call geofitd (fit, sx1, sy1, sx2, sy2, Memd[xref], Memd[yref],
		Memd[xin], Memd[yin], Memd[wts], npts, verbose)
	    if (verbose)
		call flush (STDOUT)
	}

	# output data
	call geomoutd (fit, out, sx1, sy1, sx2, sy2)

	# free space and close files
	call geodmfree (sx1, sy1, sx2, sy2)
	call sfree (sp)
end

# GEO_NPTS -- Procedure to find number of lines in file

int procedure geo_npts (fd)

pointer	fd

int	npts

int	fscan()

begin
	npts = 0

	while (fscan (fd) != EOF)
	    npts = npts + 1

	return (npts)
end

# GEO_GETPTS -- Procedure to fetch data points

int procedure geo_getpts (fd, xref, yref, xin, yin, xmin, xmax, ymin, ymax)

pointer	fd			# file descriptor
real	xref[ARB]		# x reference coordinates
real	yref[ARB]		# y reference coordinates
real	xin[ARB]		# x coordinates
real	yin[ARB]		# ycoordinates
real	xmin, xmax		# range of x coords
real	ymin, ymax		# range of y coords

int	npts

int	fscan(), nscan()

begin
	# reset to beginning of file
 	call seek (fd, BOF)

	npts = 0
	while (fscan (fd) != EOF) {

	    call gargr (xref[1+npts])
	    call gargr (yref[1+npts])
	    call gargr (xin[1+npts])
	    call gargr (yin[1+npts])
	    if (nscan() < 4)
		next

	    if (! IS_INDEFR(xmin)) {
	        if (xref[1+npts] < xmin)
		    next
	    }
	    if (! IS_INDEFR(xmax)) {
	        if (xref[1+npts] > xmax)
		    next
	    }
	    if (! IS_INDEFR(ymin)) {
	        if (yref[1+npts] < ymin)
		    next
	    }
	    if (! IS_INDEFR(ymax)) {
		if (yref[1+npts] > ymax)
		    next
	    }

	    npts = npts + 1
	}

	return (npts)
end

# GEO_GETPTSD -- Procedure to fetch data points

int procedure geo_getptsd (fd, xref, yref, xin, yin, xmin, xmax, ymin, ymax)

pointer	fd			# file descriptor
double	xref[ARB]		# x reference coordinates
double	yref[ARB]		# y reference coordinates
double	xin[ARB]		# x coordinates
double	yin[ARB]		# ycoordinates
double	xmin, xmax		# range of x coords
double	ymin, ymax		# range of y coords

int	npts

int	fscan(), nscan()

begin
	# reset to beginning of file
 	call seek (fd, BOF)

	npts = 0
	while (fscan (fd) != EOF) {

	    call gargd (xref[1+npts])
	    call gargd (yref[1+npts])
	    call gargd (xin[1+npts])
	    call gargd (yin[1+npts])
	    if (nscan() < 4)
		next

	    if (! IS_INDEFD(xmin)) {
	        if (xref[1+npts] < xmin)
		    next
	    }
	    if (! IS_INDEFD(xmax)) {
	        if (xref[1+npts] > xmax)
		    next
	    }
	    if (! IS_INDEFD(ymin)) {
	        if (yref[1+npts] < ymin)
		    next
	    }
	    if (! IS_INDEFD(ymax)) {
		if (yref[1+npts] > ymax)
		    next
	    }

	    npts = npts + 1
	}

	return (npts)
end

# GEOMOUT -- Procedure to write the output database file

procedure geomout (fit, out, sx1, sy1, sx2, sy2)

pointer	fit		# pointer to fitting structure
int	out		# pointer to database file
pointer	sx1, sy1	# pointer to linear surfaces
pointer	sx2, sy2	# pointer to distortion surfaces

int	i, ncoeff
pointer	sp, str, xcoeff, ycoeff
real	xshift, yshift, xscale, yscale, xrot, yrot

int	gsgeti()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# print title
	call dtptime (out)
	call dtput (out, "begin\t%s\n")
	    call pargstr (GM_NAME(fit))
	call fstats (DT(out), F_FILENAME, Memc[str], SZ_FNAME)
	call dtput (out, "\toutput\t\t%s\n")
	    call pargstr (Memc[str])

	# output the geometric parameters
	call lincoeff (fit, sx1, sy1, xshift, yshift, xscale, yscale, xrot, 
	    yrot)
	call dtput (out, "\txrefmean\t%g\n")
	    call pargr (GM_XOREF(fit))
	call dtput (out, "\tyrefmean\t%g\n")
	    call pargr (GM_YOREF(fit))
	call dtput (out, "\txmean\t\t%g\n")
	    call pargr (GM_XOIN(fit))
	call dtput (out, "\tymean\t\t%g\n")
	    call pargr (GM_YOIN(fit))
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

	# rebuild the linear coefficients
	#call geomkcof (sx1, sy1, xscale, yscale, xrot, yrot)

	# allocate memory for linear coefficients
	ncoeff = max (gsgeti (sx1, GSNSAVE), gsgeti (sy1, GSNSAVE))
	call calloc (xcoeff, ncoeff, TY_REAL)
	call calloc (ycoeff, ncoeff, TY_REAL)

	# output linear coefficients
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

	# allocate memory for higer order coefficients
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

	# save coefficients
	call gssave (sx2, Memr[xcoeff])
	call gssave (sy2, Memr[ycoeff])

	# output coefficients
	call dtput (out, "\tsurface2\t%d\n")
	    call pargi (ncoeff)
	do i = 1, ncoeff {
	    call dtput (out, "\t\t\t%g\t%g\n")
		call pargr (Memr[xcoeff+i-1])
		call pargr (Memr[ycoeff+i-1])
	}

	call mfree (xcoeff, TY_REAL)
	call mfree (ycoeff, TY_REAL)
	call sfree (sp)
end

# GEOMOUTD -- Procedure to write the output database file

procedure geomoutd (fit, out, sx1, sy1, sx2, sy2)

pointer	fit		# pointer to fitting structure
int	out		# pointer to database file
pointer	sx1, sy1	# pointer to linear surfaces
pointer	sx2, sy2	# pointer to distortion surfaces

int	i, ncoeff
pointer	sp, str, xcoeff, ycoeff
double	xshift, yshift, xscale, yscale, xrot, yrot

int	dgsgeti()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# print title
	call dtptime (out)
	call dtput (out, "begin\t%s\n")
	    call pargstr (GM_NAME(fit))
	call fstats (DT(out), F_FILENAME, Memc[str], SZ_FNAME)
	call dtput (out, "\toutput\t\t%s\n")
	    call pargstr (Memc[str])

	# output the geometric parameters
	call lincoeffd (fit, sx1, sy1, xshift, yshift, xscale, yscale, xrot, 
	    yrot)
	call dtput (out, "\txrefmean\t%g\n")
	    call pargr (GM_XOREF(fit))
	call dtput (out, "\tyrefmean\t%g\n")
	    call pargr (GM_YOREF(fit))
	call dtput (out, "\txmean\t\t%g\n")
	    call pargr (GM_XOIN(fit))
	call dtput (out, "\tymean\t\t%g\n")
	    call pargr (GM_YOIN(fit))
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

	# rebuild the linear coefficients
	#call geomkcofd (sx1, sy1, xscale, yscale, xrot, yrot)

	# allocate memory for linear coefficients
	ncoeff = max (dgsgeti (sx1, GSNSAVE), dgsgeti (sy1, GSNSAVE))
	call calloc (xcoeff, ncoeff, TY_DOUBLE)
	call calloc (ycoeff, ncoeff, TY_DOUBLE)

	# output linear coefficients
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

	# allocate memory for higer order coefficients
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

	# save coefficients
	call dgssave (sx2, Memd[xcoeff])
	call dgssave (sy2, Memd[ycoeff])

	# output coefficients
	call dtput (out, "\tsurface2\t%d\n")
	    call pargi (ncoeff)
	do i = 1, ncoeff {
	    call dtput (out, "\t\t\t%g\t%g\n")
		call pargd (Memd[xcoeff+i-1])
		call pargd (Memd[ycoeff+i-1])
	}

	call mfree (xcoeff, TY_DOUBLE)
	call mfree (ycoeff, TY_DOUBLE)
	call sfree (sp)
end
