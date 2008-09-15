include <fset.h>
include "../../../lib/xyxymatch.h"

# T_XYXYMATCH -- This task computes the intersection of a set of
# of coordinate lists with a reference coordinate list. The output is
# the set of objects common to both lists. In its simplest form LINXYMATCH
# uses a matching tolerance to generate the common list. Alternatively
# XYXYMATCH can use coordinate transformation information derived from the
# positions of one to three stars common to both lists, a sorting algorithm,
# and a matching tolerance to generate the common list. A more sophisticated
# pattern matching algorithm is also available which requires no coordinate
# transformation input from the user but is expensive computationally.

procedure t_xyxymatch()

bool	interactive, verbose
int	ilist, rlist, olist, rfd, rpfd, ifd, ofd
int	xcol, ycol, xrefcol, yrefcol, maxntriangles, nreftie, nintie
int	ntie, match, nrefstars, nliststars, ninter, nrmaxtri, nreftri
int	ninmaxtri, nintri, ntrefstars, ntliststars, nreject
pointer	sp, inname, refname, outname, refpoints, str, xreftie, yreftie
pointer	xintie, yintie, coeff, xref, yref, rlineno, rsindex, reftri, reftrirat
pointer	xlist, ylist, listindex, ilineno, xtrans, ytrans, intri, intrirat
pointer	xformat, yformat
real	tolerance, separation, xin, yin, xmag, ymag, xrot, yrot, xout, yout
real	ratio

bool	clgetb()
int	clpopnu(), clplen(), clgeti(), clgfil(), open(), clgwrd()
int	rg_getreftie(), rg_lincoeff(), fstati(), rg_rdxyi(), rg_sort()
int	rg_intersection(), rg_factorial(), rg_triangle(), rg_match()
int	rg_mlincoeff()
real	clgetr()

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
	call salloc (xformat, SZ_FNAME, TY_CHAR)
	call salloc (yformat, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the input, output, and reference lists.
	ilist = clpopnu ("input")
	rlist = clpopnu ("reference")
	olist = clpopnu ("output")
	tolerance = clgetr ("tolerance")
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
	xrefcol = clgeti ("xrcolumn")
	yrefcol = clgeti ("yrcolumn")

	# Get the matching parameters.
	match = clgwrd ("matching", Memc[str], SZ_LINE, RG_MATCHSTR)
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
	xout = clgetr ("xref")
	if (IS_INDEFR(xout))
	    xout = 0.0
	yout = clgetr ("yref")
	if (IS_INDEFR(yout))
	    yout = 0.0

	# Get the algorithm parameters.
	separation = clgetr ("separation")
	maxntriangles = clgeti ("nmatch")
	ratio = clgetr ("ratio")
	nreject = clgeti ("nreject")

	# Get the output formatting parameters.
	call clgstr ("xformat", Memc[xformat], SZ_FNAME)
	call clgstr ("yformat", Memc[yformat], SZ_FNAME)

	interactive = clgetb ("interactive")
	verbose = clgetb ("verbose")

	# Open the reference list file if any.
	rfd = NULL
	if (Memc[refpoints] == EOS)
	    rpfd = NULL
	else
	    rpfd = open (Memc[refpoints], READ_ONLY, TEXT_FILE)

	# Initialize.
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

	        # Fetch the reference tie points.
	        if (interactive || rpfd != NULL)
	            nreftie = rg_getreftie (rpfd, Memr[xreftie],
		        Memr[yreftie], 3, RG_REFFILE, interactive)
	        else
	            nreftie = 0

		# Read the reference data.
		if (xref != NULL)
		    call mfree (xref, TY_REAL)
		if (yref != NULL)
		    call mfree (yref, TY_REAL)
		if (rlineno != NULL)
		    call mfree (rlineno, TY_INT)
		if (rsindex != NULL)
		    call mfree (rsindex, TY_INT)
		ntrefstars = rg_rdxyi (rfd, xref, yref, rlineno, xrefcol,
		    yrefcol)
		call malloc (rsindex, ntrefstars, TY_INT)

		# Prepare the reference list for the merge algorithm. If a tie
		# point matching algorithm is selected, sort the list in the
		# y and then the x coordinate and remove coincident points.
		# If the pattern matching algorithm is used then construct the
		# triangles used for matching and sort them in order of
		# increasing ratio.

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

		break
	    }

	    # Fetch the input tie points and compute the coefficients.
	    if (interactive || rpfd != NULL)
	        nintie = rg_getreftie (rpfd, Memr[xintie],
		    Memr[yintie], nreftie, RG_INFILE, interactive)
	    else
	        nintie = 0
	    ntie = min (nreftie, nintie)
	    if (ntie <= 0)
		call rg_lmkcoeff (xin, yin, xmag, ymag, xrot, yrot,
		    xout, yout, Memr[coeff], MAX_NCOEFF)
	    else if (rg_lincoeff (Memr[xreftie], Memr[yreftie],
	        Memr[xintie], Memr[yintie], ntie, Memr[coeff],
		MAX_NCOEFF) == ERR)
		call rg_lmkcoeff (xin, yin, xmag, ymag, xrot, yrot,
		    xout, yout, Memr[coeff], MAX_NCOEFF)

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
		call rg_plincoeff ("xref", "yref", Memr[xreftie],
		    Memr[yreftie], Memr[xintie], Memr[yintie], ntie,
		    Memr[coeff], MAX_NCOEFF)
	    call rg_wlincoeff (ofd, "xref", "yref", Memr[xreftie],
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
		    call rg_pxycolumns (ofd)
		    ninter = rg_intersection (ofd, Memr[xref], Memr[yref],
		        Memi[rsindex], Memi[rlineno], nrefstars, Memr[xlist],
		        Memr[ylist], Memr[xtrans], Memr[ytrans],
			Memi[listindex], Memi[ilineno], nliststars, tolerance,
			Memc[xformat], Memc[yformat])
	        } else if (nliststars > 2) {
	    	    ninmaxtri = rg_factorial (min (max(nliststars,nrefstars),
		        maxntriangles), 3)
	    	    call calloc (intri, SZ_TRIINDEX * ninmaxtri, TY_INT)
	    	    call calloc (intrirat, SZ_TRIPAR * ninmaxtri, TY_REAL)
	    	    nintri = rg_triangle (Memr[xtrans], Memr[ytrans],
		        Memi[listindex], nliststars, Memi[intri],
			Memr[intrirat], ninmaxtri, maxntriangles,
			tolerance, ratio)
	    	    if (nintri <= 0) {
		        if (verbose)
		            call printf (
		            "    No valid input triangles can be defined\n")
		    } else {
		        ninter = rg_match (Memr[xref], Memr[yref], nrefstars,
			    Memr[xtrans], Memr[ytrans], nliststars,
			    Memi[reftri], Memr[reftrirat], nreftri, nrmaxtri,
			    ntrefstars, Memi[intri], Memr[intrirat], nintri,
			    ninmaxtri, ntliststars, tolerance, tolerance,
			    ratio, nreject)
		    }
		    if (nrefstars <= maxntriangles && nliststars <=
		        maxntriangles) {
		        call rg_pxycolumns (ofd)
			call rg_mwrite (ofd, Memr[xref], Memr[yref],
			    Memi[rlineno], Memr[xlist], Memr[ylist],
			    Memi[ilineno], Memi[reftri], nrmaxtri,
			    Memi[intri], ninmaxtri, ninter, Memc[xformat],
			    Memc[yformat])
		    } else {
			if (rg_mlincoeff (Memr[xref], Memr[yref], Memr[xlist],
			    Memr[ylist], Memi[reftri], nrmaxtri,
			    Memi[intri], ninmaxtri, ninter, Memr[coeff],
			    MAX_NCOEFF) == ERR)
			    call rg_lmkcoeff (xin, yin, xmag, ymag, xrot, yrot,
			        xout, yout, Memr[coeff], MAX_NCOEFF)
	        	call rg_compute (Memr[xlist], Memr[ylist],
			    Memr[xtrans], Memr[ytrans], ntliststars,
			    Memr[coeff], MAX_NCOEFF)
	        	nliststars = rg_sort (Memr[xtrans], Memr[ytrans],
		    	    Memi[listindex], ntliststars, separation,
			    YES, YES)
	    		if (verbose)
			    call rg_pmlincoeff ("xref", "yref", Memr[coeff],
			        MAX_NCOEFF)
	    		call rg_wmlincoeff (ofd, "xref", "yref", Memr[coeff],
			    MAX_NCOEFF)
		        call rg_pxycolumns (ofd)
		        ninter = rg_intersection (ofd, Memr[xref], Memr[yref],
		            Memi[rsindex], Memi[rlineno], nrefstars,
			    Memr[xlist], Memr[ylist], Memr[xtrans],
			    Memr[ytrans], Memi[listindex], Memi[ilineno],
			    nliststars, tolerance, Memc[xformat], Memc[yformat])
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


# RG_PXYCOLUMNS -- Print the column descriptions in the output file.

procedure rg_pxycolumns (ofd)

int	ofd			#I the output file descriptor

begin
	call fprintf (ofd, "# Column definitions\n")
	call fprintf (ofd, "#    Column 1: X reference coordinate\n")
	call fprintf (ofd, "#    Column 2: Y reference coordinate\n")
	call fprintf (ofd, "#    Column 3: X input coordinate\n")
	call fprintf (ofd, "#    Column 4: Y input coordinate\n")
	call fprintf (ofd, "#    Column 5: Reference line number\n")
	call fprintf (ofd, "#    Column 6: Input line number\n")
	call fprintf (ofd, "\n")
end
