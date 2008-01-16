include <math.h>
include <math/gsurfit.h>
include "xyxymatch.h"

# RG_GETREFTIE -- Get the reference pixel coordinate tie points by reading
# the image cursor or a file.

int procedure rg_getreftie (fd, xreftie, yreftie, ntie, file_type, interactive)

int	fd			#I the input file descriptor
real	xreftie[ARB]		#O the output x coordinates of the tie points
real	yreftie[ARB]		#O the output y coordinates of the tie points
int	ntie			#I the number of tie points
int	file_type		#I the input file type
bool	interactive		#I the 

int	nref, wcs, key
pointer	sp, str
int	clgcur(), fscan(), nscan()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print the prompt string.
	if (interactive) {

	    # Issue prompt.
	    if (file_type == RG_REFFILE) {
	        call printf (
	            "\nMark 1-%d reference objects on the display\n")
	    } else {
	        call printf (
	            "\nMark the same %d input objects on the display\n")
	    }
		call pargi (ntie)

	    # Mark the points
	    nref = 0
	    while (clgcur ("icommands", xreftie[nref+1], yreftie[nref+1],
	        wcs, key, Memc[str], SZ_LINE) != EOF) {
	        nref = nref + 1
	        if (file_type == RG_REFFILE) {
		    call printf ("    Reference coordinate %d %0.3f %0.3f\n")
			call pargi (nref)
			call pargr (xreftie[nref])
			call pargr (yreftie[nref])
		} else {
		    call printf ("    Input coordinate %d %0.3f %0.3f\n")
			call pargi (nref)
			call pargr (xreftie[nref])
			call pargr (yreftie[nref])
		}
	        if (nref >= ntie)
		    break
	    }

	} else {

	    # Issue prompt.
	    if (fd == STDIN) {
	        if (file_type == RG_REFFILE) {
	            call printf (
	            "\nEnter coordinates of 1-%d reference objects\n")
	        } else {
	            call printf (
	            "Enter coordinates of %d corresponding input objects\n")
	        }
		    call pargi (ntie)
	    }

	    nref = 0
	    while (fscan (fd) != EOF) {
		call gargr (xreftie[nref+1])
		call gargr (yreftie[nref+1])
		if (nscan() < 2)
		    break
		nref = nref + 1
		if (nref >= ntie)
		    break
		call gargr (xreftie[nref+1])
		call gargr (yreftie[nref+1])
		if (nscan() < 4)
		    break
		nref = nref + 1
		if (nref >= ntie)
		    break
		call gargr (xreftie[nref+1])
		call gargr (yreftie[nref+1])
		if (nscan() < 6)
		    break
		nref = nref + 1
		break
	    }

	}

	call sfree (sp)

	return (nref)
end


# RG_GETREFCEL -- Get the reference pixel coordinate tie points by reading
# the image cursor or a file.

int procedure rg_getrefcel (fd, xreftie, yreftie, ntie, projection,
	reflng, reflat, lngunits, latunits, file_type)

int	fd			#I the input file descriptor
real	xreftie[ARB]		#O the output x coordinates of the tie points
real	yreftie[ARB]		#O the output y coordinates of the tie points
int	ntie			#I the number of tie points
char	projection[ARB]		#I the sky projection geometry
double	reflng			#I the ra / longitude of the reference point
double	reflat			#I the dec / latitude of the reference point
int	lngunits		#I the ra / longitude units
int	latunits		#I the dec / latitude units
int	file_type		#I the input file type

int	nref
pointer	sp, dxref, dyref, str
int	fscan(), nscan()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (dxref, ntie, TY_DOUBLE)
	call salloc (dyref, ntie, TY_DOUBLE)

	# Issue prompt.
	if (fd == STDIN) {
	    if (file_type == RG_REFFILE) {
	        call printf (
	            "\nEnter coordinates of 1-%d reference objects\n")
	    } else {
	        call printf (
	            "Enter coordinates of %d corresponding input objects\n")
	    }
	        call pargi (ntie)
	}

	# Read in the tie point.
	nref = 0
	while (fscan (fd) != EOF) {
	    call gargd (Memd[dxref+nref])
	    call gargd (Memd[dyref+nref])
	    if (nscan() < 2)
	        break
	    nref = nref + 1
	    if (nref >= ntie)
	        break
	    call gargd (Memd[dxref+nref])
	    call gargd (Memd[dyref+nref])
	    if (nscan() < 4)
	        break
	    nref = nref + 1
	    if (nref >= ntie)
	        break
	    call gargd (Memd[dxref+nref])
	    call gargd (Memd[dyref+nref])
	    if (nscan() < 6)
	        break
	    nref = nref + 1
	    break
	}

	# Convert to standard coordinates.
	if (nref > 0) {
	    call rg_celtostd (projection, Memd[dxref], Memd[dyref],
	        Memd[dxref], Memd[dyref], nref, reflng, reflat, lngunits,
		latunits)
	    call amulkd (Memd[dxref], 3600.0d0, Memd[dxref], nref)
	    call amulkd (Memd[dyref], 3600.0d0, Memd[dyref], nref)
	    call achtdr (Memd[dxref], xreftie, nref)
	    call achtdr (Memd[dyref], yreftie, nref)
	}

	call sfree (sp)

	return (nref)
end


# RG_PLINCOEFF -- Print the computed transformation on the standard output.

procedure rg_plincoeff (xlabel, ylabel, xref, yref, xlist, ylist, ntie,
	coeff, ncoeff)

char	xlabel[ARB]		#I the x equation label
char	ylabel[ARB]		#I the x equation label
real	xref[ARB]		#I the input x reference coordinates
real	yref[ARB]		#I the input  y reference coordinates
real	xlist[ARB]		#I the input x input coordinates
real	ylist[ARB]		#I the input y input coordinates
int	ntie			#I number of tie points
real	coeff[ARB]		#I the output coefficient array
int	ncoeff			#I the number of coefficients

int	i
real	xmag, ymag, xrot, yrot

begin
	# List the tie points on the standard output.
	if (ntie > 0) {
	    do i = 1, ntie {
	        call printf (
	        "    tie point: %3d  ref: %9.3f %9.3f  input: %9.3f %9.3f\n")
	            call pargi (i)
		    call pargr (xref[i])
		    call pargr (yref[i])
		    call pargr (xlist[i])
		    call pargr (ylist[i])
	    }
	    call printf ("\n")
	}

	# Print the transformation coefficients to the standard output.
	call printf ("Initial linear transformation\n")
	call printf ("    %4.4s[tie] = %10g + %10g * x[tie] + %10g * y[tie]\n")
	    call pargstr (xlabel)
	    call pargr (coeff[3])
	    call pargr (coeff[1])
	    call pargr (coeff[2])
	call printf ("    %4.4s[tie] = %10g + %10g * x[tie] + %10g * y[tie]\n")
	    call pargstr (ylabel)
	    call pargr (coeff[6])
	    call pargr (coeff[4])
	    call pargr (coeff[5])
	call rg_ctogeo (coeff[1], -coeff[2], -coeff[4], coeff[5], xmag, ymag,
	    xrot, yrot)
	call printf (
	"    dx: %0.2f dy: %0.2f xmag: %0.3f ymag: %0.3f xrot: %0.1f yrot: %0.1f\n")
	    call pargr (coeff[3])
	    call pargr (coeff[6])
	    call pargr (xmag)
	    call pargr (ymag)
	    call pargr (xrot)
	    call pargr (yrot)
	call printf ("\n")
end


# RG_PMLINCOEFF -- Print the computed transformation on the standard output.

procedure rg_pmlincoeff (xlabel, ylabel, coeff, ncoeff)

char	xlabel[ARB]		#I the x equation label
char	ylabel[ARB]		#I the x equation label
real	coeff[ARB]		#I the output coefficient array
int	ncoeff			#I the number of coefficients

real	xmag, ymag, xrot, yrot

begin
	# Write the matched transformation coefficients to the standard output.
	call printf ("Matched triangles transformation\n")
	call printf ("    %4.4s[tie] = %10g + %10g * x[tie] + %10g * y[tie]\n")
	    call pargstr (xlabel)
	    call pargr (coeff[3])
	    call pargr (coeff[1])
	    call pargr (coeff[2])
	call printf ("    %4.4s[tie] = %10g + %10g * x[tie] + %10g * y[tie]\n")
	    call pargstr (ylabel)
	    call pargr (coeff[6])
	    call pargr (coeff[4])
	    call pargr (coeff[5])
	call rg_ctogeo (coeff[1], -coeff[2], -coeff[4], coeff[5], xmag, ymag,
	    xrot, yrot)
	call printf (
	"    dx: %0.2f dy: %0.2f xmag: %0.3f ymag: %0.3f xrot: %0.1f yrot: %0.1f\n")
	    call pargr (coeff[3])
	    call pargr (coeff[6])
	    call pargr (xmag)
	    call pargr (ymag)
	    call pargr (xrot)
	    call pargr (yrot)
	call printf ("\n")
end


# RG_WLINCOEFF -- Write the computed transformation to the output file.

procedure rg_wlincoeff (fd, xlabel, ylabel, xref, yref, xlist, ylist, ntie,
	coeff, ncoeff)

int	fd			#I pointer to the output file
char	xlabel[ARB]		#I the x equation label
char	ylabel[ARB]		#I the x equation label
real	xref[ARB]		#I the input x reference coordinates
real	yref[ARB]		#I the input  y reference coordinates
real	xlist[ARB]		#I the input x input coordinates
real	ylist[ARB]		#I the input y input coordinates
int	ntie			#I number of tie points
real	coeff[ARB]		#I the output coefficient array
int	ncoeff			#I the number of coefficients

int	i
real	xmag, ymag, xrot, yrot

begin
	# List the tie points.
	if (ntie > 0) {
	    do i = 1, ntie {
	        call fprintf (fd,
	        "#     tie point: %3d  ref: %9.3f %9.3f  input: %9.3f %9.3f\n")
	            call pargi (i)
		    call pargr (xref[i])
		    call pargr (yref[i])
		    call pargr (xlist[i])
		    call pargr (ylist[i])
	    }
	    call fprintf (fd, "#\n")
	}

	# Write the transformation coefficients to the output file.
	call fprintf (fd, "# Initial linear transformation\n")
	call fprintf (fd,
	    "#     %4.4s[tie] = %10g + %10g * x[tie] + %10g * y[tie]\n")
	    call pargstr (xlabel)
	    call pargr (coeff[3])
	    call pargr (coeff[1])
	    call pargr (coeff[2])
	call fprintf (fd,
	    "#     %4.4s[tie] = %10g + %10g * x[tie] + %10g * y[tie]\n")
	    call pargstr (ylabel)
	    call pargr (coeff[6])
	    call pargr (coeff[4])
	    call pargr (coeff[5])
	call rg_ctogeo (coeff[1], -coeff[2], -coeff[4], coeff[5], xmag, ymag,
	    xrot, yrot)
	call fprintf (fd,
	"# dx: %0.2f dy: %0.2f xmag: %0.3f ymag: %0.3f xrot: %0.1f yrot: %0.1f\n")
	    call pargr (coeff[3])
	    call pargr (coeff[6])
	    call pargr (xmag)
	    call pargr (ymag)
	    call pargr (xrot)
	    call pargr (yrot)
	call fprintf (fd, "#\n")
end


# RG_WMLINCOEFF -- Print the computed transformation on the standard output.

procedure rg_wmlincoeff (ofd, xlabel, ylabel, coeff, ncoeff)

int	ofd			#I the output file descriptor
char	xlabel[ARB]		#I the x equation label
char	ylabel[ARB]		#I the x equation label
real	coeff[ARB]		#I the output coefficient array
int	ncoeff			#I the number of coefficients

real	xmag, ymag, xrot, yrot

begin
	# Write the matched transformation coefficients to the output file.
	call fprintf (ofd, "# Matched triangles transformation\n")
	call fprintf (ofd,
	    "#     %4.4s[tie] = %10g + %10g * x[tie] + %10g * y[tie]\n")
	    call pargstr (xlabel)
	    call pargr (coeff[3])
	    call pargr (coeff[1])
	    call pargr (coeff[2])
	call fprintf (ofd,
	    "#     %4.4s[tie] = %10g + %10g * x[tie] + %10g * y[tie]\n")
	    call pargstr (ylabel)
	    call pargr (coeff[6])
	    call pargr (coeff[4])
	    call pargr (coeff[5])
	call rg_ctogeo (coeff[1], -coeff[2], -coeff[4], coeff[5], xmag, ymag,
	    xrot, yrot)
	call fprintf (ofd,
	"# dx: %0.2f dy: %0.2f xmag: %0.3f ymag: %0.3f xrot: %0.1f yrot: %0.1f\n")
	    call pargr (coeff[3])
	    call pargr (coeff[6])
	    call pargr (xmag)
	    call pargr (ymag)
	    call pargr (xrot)
	    call pargr (yrot)
	call fprintf (ofd, "#\n")
end


# RG_LINCOEFF -- Compute the transformation given one to three tie points.

int procedure rg_lincoeff (xref, yref, xlist, ylist, ntie, coeff, ncoeff)

real	xref[ARB]		#I the input x reference coordinates
real	yref[ARB]		#I the input  y reference coordinates
real	xlist[ARB]		#I the input x input coordinates
real	ylist[ARB]		#I the input y input coordinates
int	ntie			#I number of tie points
real	coeff[ARB]		#O the output coefficient array
int	ncoeff			#I the number of coefficients

int	ier, xier, yier, nfcoeff
pointer	sp, wts, fcoeff, sx, sy
real	xmin, xmax, ymin, ymax
int	rg_onestar(), rg_twostar(), rg_threestar()

begin
	switch (ntie) {
	case 0:
	    ier = ERR
	case 1:
	    ier = rg_onestar (xref, yref, xlist, ylist, ntie, coeff, ncoeff)
	case 2:
	    ier = rg_twostar (xref, yref, xlist, ylist, ntie, coeff, ncoeff)
	case 3:
	    ier = rg_threestar (xref, yref, xlist, ylist, ntie,
	        coeff, ncoeff)
	default:
	    call smark (sp)
	    call salloc (fcoeff, 3, TY_REAL)
	    call salloc (wts, ntie, TY_REAL)
	    call alimr (xlist, ntie, xmin, xmax)
	    call alimr (ylist, ntie, ymin, ymax)
	    call gsinit (sx, GS_POLYNOMIAL, 2, 2, GS_XNONE, xmin, xmax,
	        ymin, ymax)
	    call gsinit (sy, GS_POLYNOMIAL, 2, 2, GS_XNONE, xmin, xmax,
	        ymin, ymax)
	    call amovkr (1.0, Memr[wts], ntie)
	    call gsfit (sx, xlist, ylist, xref, Memr[wts], ntie, WTS_UNIFORM,
		xier)
	    call gsfit (sy, xlist, ylist, yref, Memr[wts], ntie, WTS_UNIFORM,
		yier)
	    if (xier == OK && xier == OK) {
	        call gscoeff (sx, Memr[fcoeff], nfcoeff)
		coeff[3] = Memr[fcoeff]
		coeff[1] = Memr[fcoeff+1]
		coeff[2] = Memr[fcoeff+2]
	        call gscoeff (sy, Memr[fcoeff], nfcoeff)
		coeff[6] = Memr[fcoeff]
		coeff[4] = Memr[fcoeff+1]
		coeff[5] = Memr[fcoeff+2]
		ier = OK
	    } else
		ier = ERR
	    call gsfree (sx)
	    call gsfree (sy)
	    call sfree (sp)
	}

	return (ier)
end


# RG_COMPUTE -- Transform the input list coordinates. The transformation
# may be done in place.

procedure rg_compute (xlist, ylist, xtrans, ytrans, nstars, coeff, ncoeff)

real	xlist[ARB]		#I the input x coordinates
real	ylist[ARB]		#I the input y coordinates
real	xtrans[ARB]		#O the output x transformed coordinates
real	ytrans[ARB]		#O the output y transformed coordinates
int	nstars			#I the number of points
real	coeff[ARB]		#I the input coefficient array
int	ncoeff			#I the number of coefficients

int	i
real	xval, yval

begin
	do i = 1, nstars {
	    xval = xlist[i]
	    yval = ylist[i]
	    xtrans[i] = coeff[1] * xval + coeff[2] * yval + coeff[3]
	    ytrans[i] = coeff[4] * xval + coeff[5] * yval + coeff[6]
	}
end


# RG_INTERSECT -- Compute the intersection of two sorted lists given a
# matching tolerance.

int procedure rg_intersection (ofd, xref, yref, refindex, rlineno, nrefstars,
	xlist, ylist, xtrans, ytrans, listindex, ilineno, nliststars,
	tolerance, xformat, yformat)

int	ofd			#I the output file descriptor
real	xref[ARB]		#I the input x reference coordinates
real	yref[ARB]		#I the input y reference coordinates
int	refindex[ARB]		#I the input reference coordinates sort index
int	rlineno[ARB]		#I the input reference coordinate line numbers
int	nrefstars		#I the number of reference stars
real	xlist[ARB]		#I the input x list coordinates
real	ylist[ARB]		#I the input y list coordinates
real	xtrans[ARB]		#I the input x transformed list coordinates
real	ytrans[ARB]		#I the input y transformed list coordinates
int	listindex[ARB]		#I the input list sort index
int	ilineno[ARB]		#I the input input line numbers
int	nliststars		#I the number of input stars
real	tolerance		#I the matching tolerance
char	xformat[ARB]		#I the output x coordinate format
char	yformat[ARB]		#I the output y coordinate format

int	blp, rp, rindex, lp, lindex, rmatch, lmatch, ninter
pointer	sp, fmtstr
real	dx, dy, tol2, rmax2, r2

begin
	call smark (sp)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)

	# Construct the fromat string
	call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s  %%5d %%5d\n")
	if (xformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (xformat)
	if (yformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (yformat)
	if (xformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (xformat)
	if (yformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (yformat)

	# Initialize the intersection routine.
	tol2 = tolerance ** 2
	blp = 1
	ninter = 0

	# Loop over the reference list stars.
	for (rp = 1; rp <= nrefstars; rp = rp + 1)  {

	    # Get the index of the reference star in question.
	    rindex = refindex[rp]

	    # Compute the start of the search range.
	    for (; blp <= nliststars; blp = blp + 1) {
		lindex = listindex[blp]
	        dy = yref[rindex] - ytrans[lindex]
		if (dy < tolerance)
		    break
	    }

	    # Break if the end of the input list is reached.
	    if (blp > nliststars)
		break

	    # If one is outside the tolerance limits skip to next reference
	    # object.
	    if (dy < -tolerance)
		next

	    # Find the closest match to the reference object.
	    rmax2  = tol2
	    rmatch = 0
	    lmatch = 0
	    for (lp = blp; lp <= nliststars; lp = lp + 1) {

		# Compute the distance between the two points.
		lindex = listindex[lp]
		dy = yref[rindex] - ytrans[lindex]
	        if (dy < -tolerance)
		    break
	        dx = xref[rindex] - xtrans[lindex]
	        r2 = dx ** 2 + dy ** 2

	        # A match has been found.
	        if (r2 <= rmax2) {
		    rmax2 = r2
		    rmatch = rindex
		    lmatch = lindex
	        }
	    }

	    # A match was found so write the results to the output file.
	    if (rmatch > 0 && lmatch > 0) {
		ninter = ninter + 1
		call fprintf (ofd, Memc[fmtstr])
		    call pargr (xref[rmatch])
		    call pargr (yref[rmatch])
		    call pargr (xlist[lmatch])
		    call pargr (ylist[lmatch])
		    call pargi (rlineno[rmatch])
		    call pargi (ilineno[lmatch])
	    }
	}

	call sfree (sp)

	return (ninter)
end


# RG_LLINTERSECT -- Compute the intersection of two sorted lists given a
# matching tolerance.

int procedure rg_llintersect (ofd, lngref, latref, xref, yref, refindex,
        rlineno, nrefstars, xlist, ylist, xtrans, ytrans, listindex, ilineno,
	nliststars, tolerance, lngformat, latformat, xformat, yformat)

int	ofd			#I the output file descriptor
double	lngref[ARB]		#I the input ra/longitude reference coordinates
double	latref[ARB]		#I the input dec/latitude reference coordinates
real	xref[ARB]		#I the input x reference coordinates
real	yref[ARB]		#I the input y reference coordinates
int	refindex[ARB]		#I the input reference coordinates sort index
int	rlineno[ARB]		#I the input reference coordinate line numbers
int	nrefstars		#I the number of reference stars
real	xlist[ARB]		#I the input x list coordinates
real	ylist[ARB]		#I the input y list coordinates
real	xtrans[ARB]		#I the input x transformed list coordinates
real	ytrans[ARB]		#I the input y transformed list coordinates
int	listindex[ARB]		#I the input list sort index
int	ilineno[ARB]		#I the input input line numbers
int	nliststars		#I the number of input stars
real	tolerance		#I the matching tolerance
char	lngformat[ARB]		#I the output ra / longitude coordinate format
char	latformat[ARB]		#I the output dec / latitude coordinate format
char	xformat[ARB]		#I the output x coordinate format
char	yformat[ARB]		#I the output y coordinate format

int	blp, rp, rindex, lp, lindex, rmatch, lmatch, ninter
pointer	sp, fmtstr
real	dx, dy, tol2, rmax2, r2

begin
	call smark (sp)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)

	# Construct the fromat string
	call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s  %%5d %%5d\n")
	if (lngformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (lngformat)
	if (latformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (latformat)
	if (xformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (xformat)
	if (yformat[1] == EOS)
	    call pargstr ("%13.7g")
	else
	    call pargstr (yformat)

	# Initialize the intersection routine.
	tol2 = tolerance ** 2
	blp = 1
	ninter = 0

	# Loop over the reference list stars.
	for (rp = 1; rp <= nrefstars; rp = rp + 1)  {

	    # Get the index of the reference star in question.
	    rindex = refindex[rp]

	    # Compute the start of the search range.
	    for (; blp <= nliststars; blp = blp + 1) {
		lindex = listindex[blp]
	        dy = yref[rindex] - ytrans[lindex]
		if (dy < tolerance)
		    break
	    }

	    # Break if the end of the input list is reached.
	    if (blp > nliststars)
		break

	    # If one is outside the tolerance limits skip to next reference
	    # object.
	    if (dy < -tolerance)
		next

	    # Find the closest match to the reference object.
	    rmax2  = tol2
	    rmatch = 0
	    lmatch = 0
	    for (lp = blp; lp <= nliststars; lp = lp + 1) {

		# Compute the distance between the two points.
		lindex = listindex[lp]
		dy = yref[rindex] - ytrans[lindex]
	        if (dy < -tolerance)
		    break
	        dx = xref[rindex] - xtrans[lindex]
	        r2 = dx ** 2 + dy ** 2

	        # A match has been found.
	        if (r2 <= rmax2) {
		    rmax2 = r2
		    rmatch = rindex
		    lmatch = lindex
	        }
	    }

	    # A match was found so write the results to the output file.
	    if (rmatch > 0 && lmatch > 0) {
		ninter = ninter + 1
		call fprintf (ofd, Memc[fmtstr])
		    call pargd (lngref[rmatch])
		    call pargd (latref[rmatch])
		    call pargr (xlist[lmatch])
		    call pargr (ylist[lmatch])
		    call pargi (rlineno[rmatch])
		    call pargi (ilineno[lmatch])
	    }
	}

	call sfree (sp)

	return (ninter)
end


# RG_LMKCOEFF -- Given the geometry of a linear transformation compute
# the coefficients required to tranform from the input to the reference
# system.

procedure rg_lmkcoeff (xin, yin, xmag, ymag, xrot, yrot, xout, yout,
	coeff, ncoeff)

real	xin, yin		#I the origin of the input coordinates
real	xmag, ymag		#I the input x and y scale factors
real	xrot, yrot		#I the iput x and y rotation factors
real	xout, yout		#I the origin of the reference coordinates
real	coeff[ARB]		#O the output coefficient array
int	ncoeff			#I the  number of coefficients

begin
	# Compute the x fit coefficients.
	coeff[1] = xmag * cos (DEGTORAD(xrot))
	coeff[2] = -ymag * sin (DEGTORAD(yrot))
	coeff[3] = xout - coeff[1] * xin - coeff[2] * yin

	# Compute the y fit coefficients.
	coeff[4] = xmag * sin (DEGTORAD(xrot))
	coeff[5] = ymag * cos (DEGTORAD(yrot))
	coeff[6] = yout - coeff[4] * xin - coeff[5] * yin
end


# RG_ONESTAR -- Compute the transformation coefficients for a simple
# shift operation.

int procedure rg_onestar (xref, yref, xlist, ylist, ntie, coeff, ncoeff)

real	xref[ARB]		#I the input x reference coordinates
real	yref[ARB]		#I the input y reference coordinates
real	xlist[ARB]		#I the input x list coordinates
real	ylist[ARB]		#I the input y list coordinates
int	ntie			#I the number of tie points
real	coeff[ARB]		#O the output coefficient array
int	ncoeff			#I the  number of coefficients

begin
	# Compute the x transformation.
	coeff[1] = 1.0
	coeff[2] = 0.0
	coeff[3] = xref[1] - xlist[1]

	# Compute the y transformation.
	coeff[4] = 0.0
	coeff[5] = 1.0
	coeff[6] = yref[1] - ylist[1]

	return (OK)
end


# RG_TWOSTAR -- Compute the transformation coefficients of a simple shift,
# magnification, and rotation.

int procedure rg_twostar (xref, yref, xlist, ylist, ntie, coeff, ncoeff)

real	xref[ARB]		#I the input x reference coordinates
real	yref[ARB]		#I the input y reference coordinates
real	xlist[ARB]		#I the input x list coordinates
real	ylist[ARB]		#I the input y list coordinates
int	ntie			#I the number of tie points
real	coeff[ARB]		#O the output coefficient array
int	ncoeff			#I the number of coefficients

real	rot, mag, dxlis, dylis, dxref, dyref, cosrot, sinrot
real	rg_posangle()

begin
	# Compute the deltas.
	dxlis = xlist[2] - xlist[1]
	dylis = ylist[2] - ylist[1]
	dxref = xref[2] - xref[1]
	dyref = yref[2] - yref[1]

	# Compute the rotation angle.
	rot = rg_posangle (dxref, dyref) - rg_posangle (dxlis, dylis)
	cosrot = cos (rot)
	sinrot = sin (rot)

	# Compute the magnification factor.
	mag = dxlis ** 2 + dylis ** 2
	if (mag <= 0.0)
	    mag = 0.0
	else
	    mag = sqrt ((dxref ** 2 + dyref ** 2) / mag)

	# Compute the transformation coefficicents.
	coeff[1] = mag * cosrot
	coeff[2] = - mag * sinrot
	coeff[3] = xref[1] - mag * cosrot * xlist[1] + mag * sinrot * ylist[1]
	coeff[4] = mag * sinrot
	coeff[5] = mag * cosrot
	coeff[6] = yref[1] - mag * sinrot * xlist[1] - mag * cosrot * ylist[1]

	return (OK)
end


# RG_THREESTAR -- Compute the transformation coefficients using a simple
# shift, magnification in x and y, rotation, and skew.

int procedure rg_threestar (xref, yref, xlist, ylist, ntie, coeff, ncoeff)

real	xref[ARB]		#I the input x reference coordinates
real	yref[ARB]		#I the input y reference coordinates
real	xlist[ARB]		#I the input x list coordinates
real	ylist[ARB]		#I the input y list coordinates
int	ntie			#I the number of tie points
real	coeff[ARB]		#O the output coefficient array
int	ncoeff			#I the number of coefficients

real	dx23, dx13, dx12, dy23, dy13, dy12, det
bool	fp_equalr()
int	rg_twostar()

begin
	# Compute the deltas.
	dx23 = xlist[2] - xlist[3]
	dx13 = xlist[1] - xlist[3]
	dx12 = xlist[1] - xlist[2]
	dy23 = ylist[2] - ylist[3]
	dy13 = ylist[1] - ylist[3]
	dy12 = ylist[1] - ylist[2]

	# Compute the determinant.
	det = xlist[1] * dy23 - xlist[2] * dy13 + xlist[3] * dy12
	if (fp_equalr (det, 0.0))
	    return (rg_twostar (xref, yref, xlist, ylist, ntie,
	        coeff, ncoeff))

	# Compute the x transformation.
	coeff[1] = (xref[1] * dy23 - xref[2] * dy13 + xref[3] * dy12) / det
	coeff[2] = (-xref[1] * dx23 + xref[2] * dx13 - xref[3] * dx12) / det
	coeff[3] = (xref[1] * (xlist[2] * ylist[3] - xlist[3] * ylist[2]) +
	    xref[2] * (ylist[1] * xlist[3] - xlist[1] * ylist[3]) +
	    xref[3] * (xlist[1] * ylist[2] - ylist[1] * xlist[2])) / det

	# Compute the y transformation.
	coeff[4] = (yref[1] * dy23 - yref[2] * dy13 + yref[3] * dy12) / det
	coeff[5] = (-yref[1] * dx23 + yref[2] * dx13 - yref[3] * dx12) / det
	coeff[6] = (yref[1] * (xlist[2] * ylist[3] - xlist[3] * ylist[2]) +
	    yref[2] * (ylist[1] * xlist[3] - xlist[1] * ylist[3]) +
	    yref[3] * (xlist[1] * ylist[2] - ylist[1] * xlist[2])) / det

	return (OK)
end


# RG_POSANGLE -- Compute the position angle of a 2D vector. The angle is
# measured counter-clockwise from the positive x axis.

real procedure rg_posangle (x, y)

real	x		#I the x vector component
real	y		#I the y vector component

real	theta
bool	fp_equalr()

begin
	if (fp_equalr (y, 0.0)) {		# 0-valued y component
	    if (x > 0.0)
		theta = 0.0
	    else if (x < 0.0)
		theta = PI
	    else
		theta = 0.0
	} else if (fp_equalr (x, 0.0)) {	# 0-valued x component
	    if (y > 0.0)
		theta = PI / 2.0
	    else if (y < 0.0)
		theta = 3.0 * PI / 2.0
	    else
		theta = 0.0
	} else if (x > 0.0 && y > 0.0) {	# 1st quadrant
	    theta = atan (y / x)
	} else if (x > 0.0 && y < 0.0) {	# 4th quadrant
	    theta = 2.0 * PI + atan (y / x)
	} else if (x < 0.0 && y > 0.0) {	# 2nd quadrant
	    theta = PI + atan (y / x)
	} else if (x < 0.0 && y < 0.0) {	# 3rd quadrant
	    theta = PI + atan (y / x)
	}

	return (theta)
end


# RG_CTOGEO -- Transform the linear transformation coefficients to useful
# geometric parameters.

procedure rg_ctogeo (a, b, c, d, xscale, yscale, xrot, yrot)

real	a		#I the x coefficient of the x coordinate fit
real	b		#I the y coefficient of the x coordinate fit
real	c		#I the x coefficient of the y coordinate fit
real	d		#I the y coefficient of the y coordinate fit
real	xscale		#I output x scale
real	yscale		#I output y scale
real	xrot		#I rotation of point on x axis
real	yrot		#I rotation of point on y axis

bool	fp_equalr()

begin
	xscale = sqrt (a * a + c * c)
	yscale = sqrt (b * b + d * d)

	# Get the x and y axes rotation factors.
        if (fp_equalr (a, 0.0) && fp_equalr (c, 0.0))
            xrot = 0.0
        else
            xrot = RADTODEG (atan2 (-c, a))
        if (xrot < 0.0)
            xrot = xrot + 360.0

        if (fp_equalr (b, 0.0) && fp_equalr (d, 0.0))
            yrot = 0.0
        else
            yrot = RADTODEG (atan2 (b, d))
        if (yrot < 0.0)
            yrot = yrot + 360.0
end
