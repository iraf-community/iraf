include <fset.h>
include <pkg/skywcs.h>
include "../../lib/xyxymatch.h"

# T_CCXYMATCH -- This task computes the intersection of a set of pixel
# coordinate lists with a reference celestial coordinate list. The output is
# the set of objects common to both lists. In its simplest form CCXYMATCH
# uses a matching tolerance to generate the common list. Alternatively
# CCXYMATCH can use coordinate transformation information derived from the
# positions of one to three stars common to both lists, a sorting algorithm,
# and a matching tolerance to generate the common list. A more sophisticated
# pattern matching algorithm is also available which requires no coordinate
# transformation input from the user but is expensive computationally.

procedure t_ccxymatch()

bool	verbose
double	lngin, latin, tlngin, tlatin
int	ilist, rlist, olist, xcol, ycol, lngcol, latcol, lngunits, latunits
int	match, maxntriangles, nreject, rfd, rpfd, ifd, ofd, pfd
int	ntrefstars, nreftie, nrefstars, nrmaxtri, nreftri, nintie, ntie
int	ntliststars, nliststars, ninter, ninmaxtri, nintri, proj
pointer	sp, inname, refname, outname, refpoints, xreftie, yreftie
pointer	xintie, yintie, coeff, projection, str
pointer	xformat, yformat, lngformat, latformat
pointer	lngref, latref, xref, yref, rlineno, rsindex, reftri, reftrirat
pointer	xtrans, ytrans, listindex, xlist, ylist, ilineno, intri, intrirat
real	tolerance, ptolerance, xin, yin, xmag, ymag, xrot, yrot
real	pseparation, separation, ratio

bool	clgetb()
double	clgetd()
int	fstati(), clpopnu(), clplen(), clgeti(), clgwrd(), open(), clgfil()
int	rg_getrefcel(), rg_rdlli(), rg_sort(), rg_factorial(), rg_triangle()
int	rg_getreftie(), rg_lincoeff(), rg_rdxyi(), rg_llintersect()
int	rg_match(), rg_mlincoeff(), cc_rdproj(), strdic()
real	clgetr()
errchk	open()

begin
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (inname, SZ_FNAME, TY_CHAR)
	call salloc (refname, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (refpoints, SZ_FNAME, TY_CHAR)
	call salloc (xreftie, MAX_NTIE, TY_REAL)
	call salloc (yreftie, MAX_NTIE, TY_REAL)
	call salloc (xintie, MAX_NTIE, TY_REAL)
	call salloc (yintie, MAX_NTIE, TY_REAL)
	call salloc (coeff, MAX_NCOEFF, TY_REAL)
	call salloc (projection, SZ_LINE, TY_CHAR)
	call salloc (xformat, SZ_FNAME, TY_CHAR)
	call salloc (yformat, SZ_FNAME, TY_CHAR)
	call salloc (lngformat, SZ_FNAME, TY_CHAR)
	call salloc (latformat, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the input, output, and reference lists.
	ilist = clpopnu ("input")
	rlist = clpopnu ("reference")
	olist = clpopnu ("output")
	tolerance = clgetr ("tolerance")
	match = clgwrd ("matching", Memc[str], SZ_LINE, RG_MATCHSTR)
	if (match == RG_TRIANGLES)
	    ptolerance = clgetr ("ptolerance")
	else 
	    ptolerance = tolerance

	call clgstr ("refpoints", Memc[refpoints], SZ_FNAME)

	# Check the input and output file lengths.
	if (clplen (rlist) > 1 && clplen (rlist) != clplen (ilist))
	    call error (0,
	        "The number of input and reference lists are not the same")
	if (clplen (ilist) != clplen (olist))
	    call error (0,
	    "The number of input and output lists are not the same") 

	xcol = clgeti ("xcolumn")
	ycol = clgeti ("ycolumn")
	lngcol = clgeti ("lngcolumn")
	latcol = clgeti ("latcolumn")
        lngunits = clgwrd ("lngunits", Memc[str], SZ_FNAME, SKY_LNG_UNITLIST)
        latunits = clgwrd ("latunits", Memc[str], SZ_FNAME, SKY_LAT_UNITLIST)

        call clgstr ("projection", Memc[projection], SZ_LINE)
        iferr {
            pfd = open (Memc[projection], READ_ONLY, TEXT_FILE)
        } then {
            proj = strdic (Memc[projection], Memc[projection], SZ_LINE,
                WTYPE_LIST)
            if (proj <= 0 || proj == WTYPE_LIN)
                Memc[projection] = EOS
        } else {
            proj = cc_rdproj (pfd, Memc[projection], SZ_LINE)
            call close (pfd)
        }

	# Get the matching parameters.
	xin = clgetr ("xin")
	if (IS_INDEFR(xin))
	    xin = 0.0
	yin = clgetr ("yin")
	if (IS_INDEFR(yin))
	    yin = 0.0
	xmag = clgetr ("xmag")
	if (IS_INDEFR(xmag))
	    xmag = 1.0
	ymag = clgetr ("ymag")
	if (IS_INDEFR(ymag))
	    ymag = 1.0
	xrot = clgetr ("xrotation")
	if (IS_INDEFR(xrot))
	    xrot = 0.0
	yrot = clgetr ("yrotation")
	if (IS_INDEFR(yrot))
	    yrot = 0.0
	lngin = clgetd ("lngref")
	latin = clgetd ("latref")

	# Get the algorithm parameters.
	pseparation = clgetr ("pseparation")
	separation = clgetr ("separation")
	maxntriangles = clgeti ("nmatch")
	ratio = clgetr ("ratio")
	nreject = clgeti ("nreject")

	# Get the output formatting parameters.
	call clgstr ("xformat", Memc[xformat], SZ_FNAME)
	call clgstr ("yformat", Memc[yformat], SZ_FNAME)
	call clgstr ("lngformat", Memc[str], SZ_FNAME)
	if (Memc[str] == EOS) {
	    switch (lngunits) {
	    case SKY_HOURS, SKY_DEGREES:
		call strcpy ("%13.3h", Memc[lngformat], SZ_FNAME)
	    case SKY_RADIANS:
		call strcpy ("%13.7g", Memc[lngformat], SZ_FNAME)
	    default:
		call strcpy ("%10.3f", Memc[lngformat], SZ_FNAME)
	    }
	} else 
	    call strcpy (Memc[str], Memc[lngformat], SZ_FNAME)
	call clgstr ("latformat", Memc[str], SZ_FNAME)
	if (Memc[str] == EOS) {
	    switch (latunits) {
	    case SKY_HOURS, SKY_DEGREES:
		call strcpy ("%13.2h", Memc[latformat], SZ_FNAME)
	    case SKY_RADIANS:
		call strcpy ("%13.7g", Memc[latformat], SZ_FNAME)
	    default:
		call strcpy ("%10.3f", Memc[latformat], SZ_FNAME)
	    }
	} else 
	    call strcpy (Memc[str], Memc[latformat], SZ_FNAME)

	verbose = clgetb ("verbose")

	# Open the reference list file if any.
	rfd = NULL
	if (Memc[refpoints] == EOS)
	    rpfd = NULL
	else
	    rpfd = open (Memc[refpoints], READ_ONLY, TEXT_FILE)

	# Initialize.
	lngref = NULL
	latref = NULL
	xref = NULL
	yref = NULL
	rsindex = NULL
	rlineno = NULL

	# Loop over the input lists.
	while (clgfil (ilist, Memc[inname], SZ_FNAME) != EOF &&
	    clgfil (olist, Memc[outname], SZ_FNAME) != EOF) {

	    # Open the input list.
	    ifd = open (Memc[inname], READ_ONLY, TEXT_FILE)

	    # Open the output list.
	    ofd = open (Memc[outname], NEW_FILE, TEXT_FILE)

	    # Open the reference list and get the coordinates.
	    while (clgfil (rlist, Memc[refname], SZ_FNAME) != EOF) {

		# Open the reference file.
		if (rfd != NULL)
		    call close (rfd)
	        rfd = open (Memc[refname], READ_ONLY, TEXT_FILE)

		# Read the reference data.
		if (lngref != NULL)
		    call mfree (lngref, TY_DOUBLE)
		if (latref != NULL)
		    call mfree (latref, TY_DOUBLE)
		if (xref != NULL)
		    call mfree (xref, TY_REAL)
		if (yref != NULL)
		    call mfree (yref, TY_REAL)
		if (rlineno != NULL)
		    call mfree (rlineno, TY_INT)
		if (rsindex != NULL)
		    call mfree (rsindex, TY_INT)
		ntrefstars = rg_rdlli (rfd, lngref, latref, xref, yref, rlineno,
		    tlngin, tlatin, lngcol, latcol, Memc[projection], lngin,
		    latin, lngunits, latunits)

		# Prepare the reference list for the merge algorithm. If a tie
		# point matching algorithm is selected, sort the list in the
		# y and then the x coordinate and remove coincident points.
		# If the pattern matching algorithm is used then construct the
		# triangles used for matching and sort them in order of
		# increasing ratio.

		call malloc (rsindex, ntrefstars, TY_INT)
	    	nrefstars = rg_sort (Memr[xref], Memr[yref], Memi[rsindex],
	            ntrefstars, separation, YES, YES)
		if (match != RG_TRIANGLES) {
	    	    reftri = NULL
	    	    reftrirat = NULL
		    nreftri = nrefstars
		} else if (nrefstars > 2) {
	    	    nrmaxtri = rg_factorial (min (nrefstars, maxntriangles), 3)
	    	    call calloc (reftri, SZ_TRIINDEX * nrmaxtri, TY_INT)
	    	    call calloc (reftrirat, SZ_TRIPAR * nrmaxtri, TY_REAL)
	    	    nreftri = rg_triangle (Memr[xref], Memr[yref],
		        Memi[rsindex], nrefstars, Memi[reftri],
			Memr[reftrirat], nrmaxtri, maxntriangles,
			tolerance, ratio)
		} else {
		    nreftri = 0
		    reftri = NULL
		    reftrirat = NULL
		}


	        # Fetch the reference tie points if any.
	        if (rpfd != NULL)
	            nreftie = rg_getrefcel (rpfd, Memr[xreftie], Memr[yreftie],
		        3, Memc[projection], tlngin, tlatin, lngunits, latunits,
			RG_REFFILE)
	        else
	            nreftie = 0

		break
	    }

	    # Fetch the input tie points and compute the coefficients.
	    if (rpfd != NULL)
	        nintie = rg_getreftie (rpfd, Memr[xintie],
		    Memr[yintie], nreftie, RG_INFILE, false)
	    else
	        nintie = 0
	    ntie = min (nreftie, nintie)
	    if (ntie <= 0)
		call rg_lmkcoeff (xin, yin, xmag, ymag, xrot, yrot,
		    0.0, 0.0, Memr[coeff], MAX_NCOEFF)
	    else if (rg_lincoeff (Memr[xreftie], Memr[yreftie],
	        Memr[xintie], Memr[yintie], ntie, Memr[coeff],
		MAX_NCOEFF) == ERR)
		call rg_lmkcoeff (xin, yin, xmag, ymag, xrot, yrot,
		    0.0, 0.0, Memr[coeff], MAX_NCOEFF)

	    # Print the header.
	    if (verbose) {
		call printf ("\nInput: %s  Reference: %s  ")
		    call pargstr (Memc[inname])
		    call pargstr (Memc[refname])
		call printf ("Number of tie points: %d\n")
		    call pargi (ntie)
	    }
	    call fprintf (ofd, "\n# Input: %s  Reference: %s  ")
		call pargstr (Memc[inname])
		call pargstr (Memc[refname])
	    call fprintf (ofd, "Number of tie points: %d\n")
		call pargi (ntie)

	    # Print the coordinate transformation information.
	    if (verbose)
		call rg_plincoeff ("  xi", " eta", Memr[xreftie],
		    Memr[yreftie], Memr[xintie], Memr[yintie], ntie,
		    Memr[coeff], MAX_NCOEFF)
	    call rg_wlincoeff (ofd, "  xi", " eta", Memr[xreftie],
	        Memr[yreftie], Memr[xintie], Memr[yintie], ntie,
		Memr[coeff], MAX_NCOEFF)

	    # Read in the input list.
	    xtrans = NULL
	    ytrans = NULL
	    listindex = NULL
	    ntliststars = rg_rdxyi (ifd, xlist, ylist, ilineno, xcol, ycol)

	    # Compute the intersection of the two lists using either an
	    # algorithm depending on common tie points or on a more
	    # sophisticated pattern matching algorithm.

	    if (ntrefstars <= 0) {
		if (verbose)
		    call printf ("    The reference coordinate list is empty\n")
		ninter = 0
	    } else if (ntliststars <= 0) {
		if (verbose)
		    call printf ("    The input coordinate list is empty\n")
		ninter = 0
	    } else if (nreftri <= 0) {
		if (verbose)
		    call printf (
		        "    No valid reference triangles can be defined\n")
	    } else {
	        call malloc (xtrans, ntliststars, TY_REAL)
	        call malloc (ytrans, ntliststars, TY_REAL)
	        call malloc (listindex, ntliststars, TY_INT)
	        call rg_compute (Memr[xlist], Memr[ylist], Memr[xtrans],
	            Memr[ytrans], ntliststars, Memr[coeff], MAX_NCOEFF)
	        nliststars = rg_sort (Memr[xtrans], Memr[ytrans],
		    Memi[listindex], ntliststars, separation, YES, YES)
	        if (match != RG_TRIANGLES) {
	    	    intri = NULL
	    	    intrirat = NULL
		    nintri = nliststars
		    call rg_pllcolumns (ofd)
		    ninter = rg_llintersect (ofd, Memd[lngref], Memd[latref],
		        Memr[xref], Memr[yref], Memi[rsindex], Memi[rlineno],
			nrefstars, Memr[xlist], Memr[ylist], Memr[xtrans],
			Memr[ytrans], Memi[listindex], Memi[ilineno],
			nliststars, tolerance, Memc[lngformat],
			Memc[latformat],Memc[xformat], Memc[yformat])
	        } else if (nliststars > 2) {
	    	    ninmaxtri = rg_factorial (min (max(nliststars,nrefstars),
		        maxntriangles), 3)
	    	    call calloc (intri, SZ_TRIINDEX * ninmaxtri, TY_INT)
	    	    call calloc (intrirat, SZ_TRIPAR * ninmaxtri, TY_REAL)
	    	    nintri = rg_triangle (Memr[xtrans], Memr[ytrans],
		        Memi[listindex], nliststars, Memi[intri],
			Memr[intrirat], ninmaxtri, maxntriangles,
			ptolerance, ratio)
	    	    if (nintri <= 0) {
		        if (verbose)
		            call printf (
		            "    No valid input triangles can be defined\n")
		    } else {
		        ninter = rg_match (Memr[xref], Memr[yref], nrefstars,
			    Memr[xtrans], Memr[ytrans], nliststars,
			    Memi[reftri], Memr[reftrirat], nreftri, nrmaxtri,
			    ntrefstars, Memi[intri], Memr[intrirat], nintri,
			    ninmaxtri, ntliststars, tolerance, ptolerance,
			    ratio, nreject)
		    }
		    if (nrefstars <= maxntriangles && nliststars <=
		        maxntriangles) {
		        call rg_pllcolumns (ofd)
			call rg_lmwrite (ofd, Memd[lngref], Memd[latref],
			    Memi[rlineno], Memr[xlist], Memr[ylist],
			    Memi[ilineno], Memi[reftri], nrmaxtri,
			    Memi[intri], ninmaxtri, ninter, Memc[lngformat],
			    Memc[latformat], Memc[xformat], Memc[yformat])
		    } else {
			if (rg_mlincoeff (Memr[xref], Memr[yref], Memr[xlist],
			    Memr[ylist], Memi[reftri], nrmaxtri,
			    Memi[intri], ninmaxtri, ninter, Memr[coeff],
			    MAX_NCOEFF) == ERR)
			    call rg_lmkcoeff (xin, yin, xmag, ymag, xrot, yrot,
			        0.0, 0.0, Memr[coeff], MAX_NCOEFF)
	        	call rg_compute (Memr[xlist], Memr[ylist],
			    Memr[xtrans], Memr[ytrans], ntliststars,
			    Memr[coeff], MAX_NCOEFF)
	        	nliststars = rg_sort (Memr[xtrans], Memr[ytrans],
		    	    Memi[listindex], ntliststars, separation,
			    YES, YES)
	    		if (verbose)
			    call rg_pmlincoeff ("  xi", " eta", Memr[coeff],
			        MAX_NCOEFF)
	    		call rg_wmlincoeff (ofd, "  xi", " eta", Memr[coeff],
			    MAX_NCOEFF)
		        call rg_pllcolumns (ofd)
		        ninter = rg_llintersect (ofd, Memd[lngref],
			    Memd[latref], Memr[xref], Memr[yref], Memi[rsindex],
			    Memi[rlineno], nrefstars, Memr[xlist], Memr[ylist],
			    Memr[xtrans], Memr[ytrans], Memi[listindex],
			    Memi[ilineno], nliststars, tolerance,
			    Memc[lngformat], Memc[latformat], Memc[xformat],
			    Memc[yformat])
		    }
	        } else {
		    if (verbose)
		        call printf (
		            "\tThe input coordinate list has < 3 stars\n")
	    	    intri = NULL
	    	    intrirat = NULL
		    nintri = 0
		    ninter = 0
		}
	    }

	    # Print out the number of stars matched in the two lists.
	    if (verbose) {
		call printf ("%d reference coordinates matched\n")
		    call pargi (ninter)
	    }

	    # Free space used by input list.
	    call mfree (xlist, TY_REAL)
	    call mfree (ylist, TY_REAL)
	    call mfree (ilineno, TY_INT)
	    call mfree (listindex, TY_INT)
	    if (xtrans != NULL)
	        call mfree (xtrans, TY_REAL)
	    if (ytrans != NULL)
	        call mfree (ytrans, TY_REAL)
	    if (intri != NULL)
	        call mfree (intri, TY_INT)
	    if (intrirat != NULL)
	        call mfree (intrirat, TY_REAL)

	    # Close  the input and output lists.
	    call close (ifd)
	    call close (ofd)
	}

	# Release the memory used to store the reference list.
	call mfree (lngref, TY_DOUBLE)
	call mfree (latref, TY_DOUBLE)
	call mfree (xref, TY_REAL)
	call mfree (yref, TY_REAL)
	call mfree (rlineno, TY_INT)
	call mfree (rsindex, TY_INT)
	if (reftri != NULL)
	    call mfree (reftri, TY_INT)
	if (reftrirat != NULL)
	    call mfree (reftrirat, TY_REAL)

	# Close the reference file.
	if (rfd != NULL)
	    call close (rfd)

	# Close the reference points file.
	if (rpfd != NULL)
	    call close (rpfd)

	# Close the file lists.
	call clpcls (ilist)
	call clpcls (rlist)
	call clpcls (olist)

	call sfree (sp)
end


# RG_RDLLI -- Read in the celestial coordinates from a file, convert them
# to standard coordinates, and set the line number index.

int procedure rg_rdlli (fd, lng, lat, x, y, lineno, tlngref, tlatref,
	xcolumn, ycolumn, projection, lngref, latref, lngunits, latunits)

int	fd			#I the input file descriptor
pointer	lng			#U pointer to the x coordinates
pointer	lat			#U pointer to the y coordinates
pointer	x			#U pointer to the x coordinates
pointer	y			#U pointer to the y coordinates
pointer	lineno			#U pointer to the line numbers
double	tlngref			#O the adopted reference ra / longitude
double	tlatref			#O the adopted reference dec / latitude
int	xcolumn			#I column containing the x coordinate
int	ycolumn			#I column containing the y coordinate
char	projection[ARB]		#I the sky projection geometry
double	lngref			#I the input reference ra / longitude
double	latref			#I the input reference dec / latitude
int	lngunits		#I the ra / longitude units
int	latunits		#I the dec / latitude units

int	i, ip, bufsize, npts, lnpts, maxcols
double	xval, yval
pointer	sp, str, tx, ty
int	fscan(), nscan(), ctod()
double	asumd()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	bufsize = DEF_BUFSIZE
	call malloc (lng, bufsize, TY_DOUBLE)
	call malloc (lat, bufsize, TY_DOUBLE)
	call malloc (x, bufsize, TY_REAL)
	call malloc (y, bufsize, TY_REAL)
	call malloc (lineno, bufsize, TY_INT)
	maxcols = max (xcolumn, ycolumn)

	npts = 0
	lnpts = 0
	while (fscan(fd) != EOF) {

	    lnpts = lnpts + 1
	    xval = INDEFD
	    yval = INDEFD
	    do i = 1, maxcols {
		call gargwrd (Memc[str], SZ_LINE)
		if (i != nscan())
		    break
		ip = 1
		if (i == xcolumn) {
		    if (ctod (Memc[str], ip, xval) <= 0)
			xval = INDEFD
		} else if (i == ycolumn) {
		    if (ctod (Memc[str], ip, yval) <= 0)
			yval = INDEFD
		}
	    }
	    if (IS_INDEFD(xval) || IS_INDEFD(yval))
		next

	    Memd[lng+npts] = xval
	    Memd[lat+npts] = yval
	    Memi[lineno+npts] = lnpts
	    npts = npts + 1
	    if (npts >= bufsize) {
		bufsize = bufsize + DEF_BUFSIZE
		call realloc (lng, bufsize, TY_DOUBLE)
		call realloc (lat, bufsize, TY_DOUBLE)
		call realloc (x, bufsize, TY_REAL)
		call realloc (y, bufsize, TY_REAL)
		call realloc (lineno, bufsize, TY_INT)
	    }
	}

	# Compute the reference point and convert to standard coordinates.
	if (npts > 0) {
	    if (IS_INDEFD(lngref))
		tlngref = asumd (Memd[lng], npts) / npts
	    else
		tlngref = lngref
	    if (IS_INDEFD(latref))
		tlatref = asumd (Memd[lat], npts) / npts
	    else
		tlatref = latref
	    call salloc (tx, npts, TY_DOUBLE)
	    call salloc (ty, npts, TY_DOUBLE)
	    call rg_celtostd (projection, Memd[lng], Memd[lat], Memd[tx],
		Memd[ty], npts, tlngref, tlatref, lngunits, latunits)
	    call amulkd (Memd[tx], 3600.0d0, Memd[tx], npts)
	    call amulkd (Memd[ty], 3600.0d0, Memd[ty], npts)
	    call achtdr (Memd[tx], Memr[x], npts)
	    call achtdr (Memd[ty], Memr[y], npts)
	} else {
	    tlngref = lngref
	    tlatref = latref
	}

	call sfree (sp)

	return (npts)
end


# RG_PLLCOLUMNS -- Print the column descriptions in the output file.

procedure rg_pllcolumns (ofd)

int	ofd			#I the output file descriptor

begin
	call fprintf (ofd, "# Column definitions\n")
	call fprintf (ofd,
	    "#    Column 1: Reference Ra / Longitude coordinate\n")
	call fprintf (ofd,
	    "#    Column 2: Reference Dec / Latitude coordinate\n")
	call fprintf (ofd, "#    Column 3: Input X coordinate\n")
	call fprintf (ofd, "#    Column 4: Input Y coordinate\n")
	call fprintf (ofd, "#    Column 5: Reference line number\n")
	call fprintf (ofd, "#    Column 6: Input line number\n")
	call fprintf (ofd, "\n")
end
