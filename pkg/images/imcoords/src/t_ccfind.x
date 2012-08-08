# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <ctype.h>
include <imhdr.h>
include <pkg/skywcs.h>

# T_CCFIND -- Locate objects with known celestial coordinates in an image
# using the image WCS or a user supplied WCS. Write the matched celestial and
# coordinates list to the output file.

procedure t_ccfind ()

bool	usewcs, center, verbose
double	xref, yref, xmag, ymag, xrot, yrot, tlngref, tlatref, txref, tyref
double	txmag, tymag, txrot, tyrot
int	ip, nchars, sbox, cbox, min_sigdigits, ncenter, maxiter, tol
int	inlist, ninfiles, outlist, noutfiles, imlist, nimages, in, out
int	lngcolumn, latcolumn, lngunits, latunits, coostat, refstat
int	lngrefunits, latrefunits, proj, pfd
pointer	sp, insystem, refsystem, infile, outfile, image, projstr, str
pointer	slngref, slatref, xformat, yformat, coo, refcoo, im, mw
real	datamin, datamax, back

bool	clgetb()
double	clgetd(), imgetd()
int	clpopnu(), clplen(), imtopenp(), imtlen(), clgeti(), clgwrd(), strlen()
int	sk_decwcs(), sk_decim(), open(), clgfil(), imtgetim(), strncmp(), ctod()
int	cc_listran(), strdic(), cc_rdproj()
real	clgetr()
pointer	immap(), cc_mkwcs()
errchk	imgstr(), imgetd(), open()

begin
	# Get some working space.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (insystem, SZ_FNAME, TY_CHAR)
	call salloc (refsystem, SZ_FNAME, TY_CHAR)
	call salloc (slngref, SZ_FNAME, TY_CHAR)
	call salloc (slatref, SZ_FNAME, TY_CHAR)
	call salloc (xformat, SZ_FNAME, TY_CHAR)
	call salloc (yformat, SZ_FNAME, TY_CHAR)
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

	# Get the output results lists.
	outlist = clpopnu ("output")
	noutfiles = clplen (outlist)
	if (noutfiles != ninfiles) {
	    call eprintf (
	    "Error: The number of input and output files must be the same\n")
	    call clpcls (inlist)
	    call clpcls (outlist)
	    call sfree (sp)
	    return
	}


	# Get the input image list.
	imlist = imtopenp ("images")
	nimages = imtlen (imlist)
	if (nimages != ninfiles) {
	    call eprintf (
	    "Error: The number of input files and images must be the same\n")
	    call imtclose (imlist)
	    call clpcls (inlist)
	    call clpcls (outlist)
	    call sfree (sp)
	    return
	}

	# Get the coordinates file format.
	lngcolumn = clgeti ("lngcolumn")
	latcolumn = clgeti ("latcolumn")
	call clgstr ("insystem", Memc[insystem], SZ_FNAME)
	iferr (lngunits = clgwrd ("lngunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    lngunits = 0
	iferr (latunits = clgwrd ("latunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    latunits = 0

	# Get the user wcs if there is one.
	usewcs = clgetb ("usewcs")
	if (! usewcs) {
	    xref = clgetd ("xref")
	    yref = clgetd ("yref")
	    xmag = clgetd ("xmag")
	    ymag = clgetd ("ymag")
	    xrot = clgetd ("xrot")
	    yrot = clgetd ("yrot")
	    call clgstr ("lngref", Memc[slngref], SZ_FNAME)
	    call clgstr ("latref", Memc[slatref], SZ_FNAME)
	    call clgstr ("refsystem", Memc[refsystem], SZ_FNAME)
	    if (strncmp (Memc[refsystem], "INDEF", 5) == 0)
	        Memc[refsystem] = EOS
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
	}
	iferr (lngrefunits = clgwrd ("lngrefunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    lngrefunits = 0
	iferr (latrefunits = clgwrd ("latrefunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    latrefunits = 0

	# Get the centering parameters.
	center = clgetb ("center")
	sbox = clgeti ("sbox")
	cbox = clgeti ("cbox")
	datamin = clgetr ("datamin")
	datamax = clgetr ("datamax")
	back = clgetr ("background")
	maxiter = clgeti ("maxiter")
	tol = clgeti ("tolerance")
	if (mod (sbox,2) == 0)
	    sbox = sbox + 1
	if (mod (cbox,2) == 0)
	    cbox = cbox + 1

	# Get the output formatting parameters.
        call clgstr ("xformat", Memc[xformat], SZ_FNAME)
        call clgstr ("yformat", Memc[yformat], SZ_FNAME)
        #min_sigdigits = clgeti ("min_sigdigits")
        min_sigdigits = 7
	verbose = clgetb ("verbose")

	# Open the input coordinate system and determine its units.
	coostat = sk_decwcs (Memc[insystem], mw, coo, NULL) 
	if (coostat == ERR || mw != NULL) {
	    call eprintf ("Error: Cannot decode the input coordinate system\n")
	    if (mw != NULL)
		call mw_close (mw)
	    call imtclose (imlist)
	    call clpcls (inlist)
	    call clpcls (outlist)
	    call sfree (sp)
	    return
	}
	if (lngunits > 0)
	    call sk_seti (coo, S_NLNGUNITS, lngunits)
	if (latunits > 0)
	    call sk_seti (coo, S_NLATUNITS, latunits)

	# Flush standard output on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Loop over the files.
	while (clgfil (inlist, Memc[infile], SZ_FNAME) != EOF &&
	    clgfil (outlist, Memc[outfile], SZ_FNAME) != EOF &&
	    imtgetim(imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input file of celestial coordinates.
	    in = open (Memc[infile], READ_ONLY, TEXT_FILE)

	    # Open the output file of matched coordinates.
	    out = open (Memc[outfile], NEW_FILE, TEXT_FILE)

	    # Open the input image.
	     im = immap (Memc[image], READ_ONLY, 0)
	     if (IM_NDIM(im) != 2) {
		call printf ("Skipping file: %s Image: %s is not 2D\n")
		    call pargstr (Memc[infile])
		    call pargstr (Memc[image])
		call imunmap (im)
		call close (in)
		call close (out)
		next
	    }

	    # Print the input and out file information.
	    if (verbose && out != STDOUT) {
		call printf ("\nInput File: %s  Output File: %s\n")
		    call pargstr (Memc[infile])
		    call pargstr (Memc[outfile])
		call printf ("    Image: %s  Wcs: %s\n")
		    call pargstr (Memc[image])
		    call pargstr ("")
	    }
	    call fprintf (out, "\n# Input File: %s  Output File: %s\n")
		call pargstr (Memc[infile])
		call pargstr (Memc[outfile])
	    call fprintf (out, "#     Image: %s  Wcs: %s\n")
		    call pargstr (Memc[image])
		    call pargstr ("")

	    # Open the wcs and compile the transformation.
	    if (usewcs) {

		# Read the image wcs, skipping to the next image if the wcs
		# is unreadable.
	        refstat = sk_decim (im, Memc[image], mw, refcoo)
	        if (refstat == ERR || mw == NULL) {
		    if (verbose && out != STDOUT)
	                call printf (
		        "Error: Cannot decode the image coordinate system\n")
	            call fprintf (out,
		        "Error: Cannot decode the image coordinate system\n")
	            if (mw != NULL)
		        call mw_close (mw)
		    call sk_close (refcoo)
		    call imunmap (im)
		    call close (out)
		    call close (in)
		    next
		}

	    } else {

		# Get the image pixel reference coordinates
                if (IS_INDEFD(xref))
                    txref = (1.0d0 + IM_LEN(im,1)) / 2.0
                else
                    txref = xref
                if (IS_INDEFD(yref))
                    tyref = (1.0d0 + IM_LEN(im,2)) / 2.0
                else
                    tyref = yref

		# Get the image scale in arcsec / pixel.
                if (IS_INDEFD(xmag))
                    txmag = 1.0d0
                else
                    txmag = xmag
                if (IS_INDEFD(ymag))
                    tymag = 1.0d0
                else
                    tymag = ymag

		# Get the coordinate axes rotation angles in degrees.
                if (IS_INDEFD(xrot))
                    txrot = 0.0d0
                else
                    txrot = xrot
                if (IS_INDEFD(yrot))
                    tyrot = 0.0d0
                else
                    tyrot = yrot

		# Get the celestial coordinates of the tangent point from
		# the image header or from the user.
		iferr (tlngref = imgetd (im, Memc[slngref])) {
		    ip = 1
		    nchars = ctod (Memc[slngref], ip, tlngref)
		    if (nchars <= 0 || nchars != strlen (Memc[slngref]))
			tlngref = 0.0d0
		    else if (IS_INDEFD(tlngref) || tlngref < 0.0d0 ||
		        tlngref > 360.0d0)
			tlngref = 0.0d0
		}
		iferr (tlatref = imgetd (im, Memc[slatref])) {
		    ip = 1
		    nchars = ctod (Memc[slatref], ip, tlatref)
		    if (nchars <= 0 || nchars != strlen (Memc[slatref]))
			tlatref = 0.0d0
		    else if (IS_INDEFD(tlatref) || tlatref < -90.0d0 ||
		        tlatref > 90.0d0)
			tlatref = 0.0d0
		}

		# Get the image reference system from the image header
		# or from the user.
		if (Memc[refsystem] == EOS)
		    call strcpy (Memc[refsystem], Memc[str], SZ_FNAME)
		else {
		    iferr (call imgstr (im, Memc[refsystem], Memc[str],
		        SZ_FNAME))
			call strcpy (Memc[refsystem], Memc[str], SZ_FNAME)
		}
		refstat = sk_decwcs (Memc[str], mw, refcoo, NULL)
		if (refstat == ERR || mw != NULL) {
	    	    if (mw != NULL)
	                call mw_close (mw)
		    call sk_close (refcoo)
		    refstat = sk_decwcs (Memc[insystem], mw, refcoo, NULL)
		}

		# Force the units of the tangent point.
		if (lngrefunits > 0)
	    	    call sk_seti (refcoo, S_NLNGUNITS, lngrefunits)
		if (latrefunits > 0)
	    	    call sk_seti (refcoo, S_NLATUNITS, latrefunits)

		# Build the wcs.
		mw = cc_mkwcs (refcoo, Memc[projstr], tlngref, tlatref,
		    txref, tyref, txmag, tymag, txrot, tyrot, false)

		# Force the wcs to look like an image wcs.
		call sk_seti (refcoo, S_PIXTYPE, PIXTYPE_LOGICAL)

	    }

	    # Print out a description of the input coordinate and image
	    # systems.
	    if (verbose && out != STDOUT)
		call sk_iiprint ("Insystem", Memc[insystem], NULL, coo)
	    call sk_iiwrite (out, "Insystem", Memc[insystem], NULL, coo)
	    call sk_stats (refcoo, S_COOSYSTEM, Memc[str], SZ_FNAME)
	    if (usewcs) {
	        if (verbose && out != STDOUT) {
		    call sk_iiprint ("Refsystem", Memc[str], mw, refcoo)
		}
	        call sk_iiwrite (out, "Refsystem", Memc[str], mw, refcoo)
	        call fprintf (out, "\n")
	    } else {
	        if (verbose && out != STDOUT) {
		    call sk_iiprint ("Refsystem", Memc[str], NULL, refcoo)
		}
	        call sk_iiwrite (out, "Refsystem", Memc[str], NULL, refcoo)
	        call fprintf (out, "\n")
	    }

	    # Transform the coordinate lists.
	    ncenter = cc_listran (in, out, im, NULL, mw, coo, refcoo, lngcolumn,
		latcolumn, lngunits, latunits, lngrefunits, latrefunits,
		center, sbox / 2, cbox / 2, datamin, datamax, back,
		maxiter, tol, Memc[xformat], Memc[yformat], min_sigdigits)

	    if (verbose && out != STDOUT) {
		call printf ("Located %d objects in image %s\n")
		    call pargi (ncenter)
		    call pargstr (Memc[image])
		call printf ("\n")
	    }
	    call sk_close (refcoo)
	    call mw_close (mw)
    	    call imunmap (im)
	    call close (out)
	    call close (in)
	}


	call sk_close (coo)
	call imtclose (imlist)
	call clpcls (inlist)
	call clpcls (outlist)
	call sfree (sp)
end


define	MAX_FIELDS	100		# Maximum number of fields in list
define	TABSIZE		8		# Spacing of tab stops

# CC_LISTRAN -- Transform the coordinate list.

int procedure cc_listran (infd, outfd, im, mwin, mwout, cooin, cooout,
	lngcolumn, latcolumn, ilngunits, ilatunits, olngunits, olatunits,
	center, sbox, cbox, datamin, datamax, back, maxiter, tol, oxformat,
	oyformat, min_sigdigits)

int	infd			#I the input file descriptor
int	outfd			#I the output file descriptor
pointer	im			#I the input image descriptor
pointer	mwin			#I the input image wcs
pointer	mwout			#I the output image wcs
pointer	cooin			#I the input coordinate descriptor
pointer	cooout			#I the output coordinate descriptor
int	lngcolumn		#I the input ra/longitude column
int	latcolumn		#I the input dec/latitude column
int	ilngunits		#I the input ra/longitude units
int	ilatunits		#I the input dec/latitude units
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
bool	center			#I center the pixel coordinates
int	sbox			#I the search box half-width in pixels
int	cbox			#I the centering box half-width in pixels
real	datamin			#I the minimum good data value
real	datamax			#I the maximum good data value
real	back			#I the background reference value
int	maxiter			#I the maximum number of iterations
int	tol			#I the fitting tolerance in pixels
char	oxformat[ARB]		#I the output x format
char	oyformat[ARB]		#I the output y format
int	min_sigdigits		#I the minimum number of significant digits

double	ilng, ilat, tlng, tlat, olng, olat
int	nline, ip, max_fields, nfields, offset, nchars, nsdig_lng, nsdig_lat
int	tilngunits, tilatunits, tolngunits, tolatunits, cier, ncenter
pointer	sp, inbuf, linebuf, field_pos, outbuf, ctin, ctout
pointer	toxformat, toyformat
int	sk_stati(), li_get_numd(), getline(), cc_center()
pointer	sk_ictran(), sk_octran()
errchk	sk_ictran(), sk_octran()

begin
	# Compile the input and output transformations.
	iferr {
	    ctin = sk_ictran (cooin, mwin) 
	    ctout = sk_octran (cooout, mwout)
	} then
	    return

	# Allocate some memory.
	call smark (sp)
        call salloc (inbuf, SZ_LINE, TY_CHAR)
        call salloc (linebuf, SZ_LINE, TY_CHAR)
        call salloc (field_pos, MAX_FIELDS, TY_INT)
        call salloc (outbuf, SZ_LINE, TY_CHAR)
	call salloc (toxformat, SZ_FNAME, TY_CHAR)
	call salloc (toyformat, SZ_FNAME, TY_CHAR)

	# Set the default input and output units.
	if (ilngunits <= 0)
            tilngunits = sk_stati (cooin, S_NLNGUNITS)
	else
	    tilngunits = ilngunits
        if (ilatunits <= 0)
            tilatunits = sk_stati (cooin, S_NLATUNITS)
	else
	    tilatunits = ilatunits
        if (olngunits <= 0)
            tolngunits = sk_stati (cooout, S_NLNGUNITS)
	else
	    tolngunits = olngunits
        if (olatunits <= 0)
            tolatunits = sk_stati (cooout, S_NLATUNITS)
	else
	    tolatunits = olatunits

	# Set the output format.
        call sk_oformats (cooin, cooout, oxformat, oyformat,
            tolngunits, tolatunits, Memc[toxformat], Memc[toyformat],
            SZ_FNAME)

	# Check the input and output units.
	call sk_iunits (cooin, mwin, tilngunits, tilatunits, tilngunits,
	    tilatunits)
	call sk_ounits (cooout, mwout, tolngunits, tolatunits, tolngunits,
	    tolatunits)

	# Loop over the input coordinates.
	max_fields = MAX_FIELDS
	ncenter = 0
	for (nline = 1; getline (infd, Memc[inbuf]) != EOF; nline = nline + 1) {

	    # Check for blank lines and comment lines.
	    for (ip = inbuf;  IS_WHITE(Memc[ip]);  ip = ip + 1)
                ;
	    if (Memc[ip] == '#') {
                # Pass comment lines on to the output unchanged.
                call putline (outfd, Memc[inbuf])
                next
            } else if (Memc[ip] == '\n' || Memc[ip] == EOS) {
                # Blank lines too.
                call putline (outfd, Memc[inbuf])
                next
            }

	    # Expand tabs into blanks, determine field offsets.
            call strdetab (Memc[inbuf], Memc[linebuf], SZ_LINE, TABSIZE)
            call li_find_fields (Memc[linebuf], Memi[field_pos], max_fields,
                nfields)

	    if (lngcolumn > nfields || latcolumn > nfields) {
                call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                call eprintf ("Skipping object %d in file %s: too few fields\n")
                    call pargi (nline)
                    call pargstr (Memc[outbuf])
                #call putline (outfd, Memc[linebuf])
                next
            }

	    offset = Memi[field_pos+lngcolumn-1]
            nchars = li_get_numd (Memc[linebuf+offset-1], ilng, nsdig_lng)
            if (nchars == 0) {
                call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                call eprintf ("Skipping object %d in file %s: bad ra value\n")
                    call pargi (nline)
                    call pargstr (Memc[outbuf])
                #call putline (outfd, Memc[linebuf])
                next
            }

	    offset = Memi[field_pos+latcolumn-1]
            nchars = li_get_numd (Memc[linebuf+offset-1], ilat, nsdig_lat)
            if (nchars == 0) {
                call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                call eprintf ("Skipping object %d in file %s: bad dec value\n")
                    call pargi (nline)
                    call pargstr (Memc[outbuf])
                #call putline (outfd, Memc[linebuf])
                next
            }

	    # Convert the input coordinates to world coordinates in radians.
	    call sk_incc (cooin, mwin, ctin, tilngunits, tilatunits, ilng,
	        ilat, olng, olat)

	    # Perform the transformation.
	    call sk_lltran (cooin, cooout, olng, olat, INDEFD, INDEFD,
	        0.0d0, 0.0d0, tlng, tlat)

	    # Convert the output celestial coordinates from radians to output
	    # coordinates.
	    call sk_outcc (cooout, mwout, ctout, tolngunits, tolatunits,
	        tlng, tlat, olng, olat)

	    # Is the object on the image ?
	    if (olng <  0.5d0 || olng > (IM_LEN(im,1) + 0.5d0) ||
	        olat < 0.5d0 || olat > (IM_LEN(im,2) + 0.5d0)) {
                call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                call eprintf ("Skipping object %d in file %s: off image %s\n")
                    call pargi (nline)
                    call pargstr (Memc[outbuf])
		    call pargstr (IM_HDRFILE(im))
                #call putline (outfd, Memc[linebuf])
		next
	    }

	    # Center the coordinates.
	    if (center) {
		cier = cc_center (im, sbox, cbox, datamin, datamax, back,
		    maxiter, tol, olng, olat, olng, olat)
		if (cier == ERR) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf (
		"Skipping object %d in file %s: cannot center in image %s\n")
                        call pargi (nline)
                        call pargstr (Memc[outbuf])
		        call pargstr (IM_HDRFILE(im))
                    #call putline (outfd, Memc[linebuf])
		    next
		}
	    }

	    # Output the results.
	    call li_append_lined (Memc[linebuf], Memc[outbuf], SZ_LINE,
	        olng, olat, Memc[toxformat], Memc[toyformat], nsdig_lng,
		nsdig_lat, min_sigdigits)
	    call putline (outfd, Memc[outbuf])
	    ncenter = ncenter + 1
	}

	call sfree (sp)

	return (ncenter)
end


# CC_CENTER -- Given an initial x and y coordinate compute a more accurate
# center using a centroiding technique.

int procedure cc_center (im, sbox, cbox, datamin, datamax, back, maxiter,
	tolerance, xinit, yinit, xcenter, ycenter)

pointer	im			#I pointer to the input image
int	sbox			#I the search box half-width in pixels
int	cbox			#I the centering box half-width in pixels
real	datamin			#I the minimum good data value
real	datamax			#I the maximum good data value
real	back			#I the background reference value
int	maxiter			#I the maximum number of iterations.
int	tolerance		#I the tolerance for convergence in pixels
double	xinit, yinit		#I the initial x and y positions
double	xcenter, ycenter	#I the final x and y positions

bool	converged
double	xold, yold, xnew, ynew
int	i, fbox, x1, x2, y1, y2, nx, ny
real	lo, hi, sky
pointer	buf, sp, xbuf, ybuf
pointer	imgs2r()
real	cc_ctr1d()
errchk	imgs2r(), cc_threshold(), cc_rowsum(), cc_colsum(), cc_ctr1d()


begin
	xold = xinit
	yold = yinit
	converged = false

	do i = 1, maxiter {

	    if (i == 1)
		fbox = sbox
	    else
		fbox = cbox
	    x1 = max (nint (xold) - fbox, 1)
	    x2 = min (nint (xold) + fbox, IM_LEN(im,1))
	    y1 = max (nint (yold) - fbox, 1)
	    y2 = min (nint (yold) + fbox, IM_LEN(im,2))
	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1

	    call smark (sp)
	    call salloc (xbuf, nx, TY_REAL)
	    call salloc (ybuf, ny, TY_REAL)

	    iferr {
		buf = imgs2r (im, x1, x2, y1, y2)
		call cc_threshold (Memr[buf], nx * ny, datamin, datamax,
		    back, lo, hi, sky)
		call cc_rowsum (Memr[buf], Memr[xbuf], nx, ny, lo, hi, sky)
		call cc_colsum (Memr[buf], Memr[ybuf], nx, ny, lo, hi, sky)
		xnew = x1 + cc_ctr1d (Memr[xbuf], nx)
		ynew = y1 + cc_ctr1d (Memr[ybuf], ny)
	    } then {
		call sfree (sp)
		return (ERR)
	    }

	    call sfree (sp)

	    # Force at least one iteration.
	    if (i > 1) {
	        if (abs(nint(xnew) - nint(xold)) <= tolerance &&
	            abs(nint(ynew) - nint(yold)) <= tolerance) {
		    converged = true
		    break
		}
	    }

	    xold = xnew
	    yold = ynew
	}

	if (converged) {
	    xcenter = xnew
	    ycenter = ynew
	    return (OK)
	} else {
	    xcenter = xinit
	    ycenter = yinit
	    return (ERR)
	}
end


# CC_THRESHOLD -- Find the low and high thresholds for the subraster.

procedure cc_threshold (raster, npix, datamin, datamax, back, ldatamin,
	ldatamax, lback)

real    raster[ARB]             #I input data
int     npix                    #I length of input data
real	datamin			#I minimum good data value
real	datamax			#I maximum good data value
real	back			#I background value
real	ldatamin		#I local minimum good data value
real	ldatamax		#I local maximum good data value
real	lback			#I local background value

real	junk
int     awvgr()
errchk  alimr, awvgr

begin
        # use the local data min or max for thresholds that are INDEF.
        if (IS_INDEFR(datamin) || IS_INDEFR(datamax))
            call alimr (raster, npix, ldatamin, ldatamax)
        if (! IS_INDEFR(datamin))
            ldatamin = datamin
        if (! IS_INDEFR(datamax))
            ldatamax = datamax

        if (IS_INDEFR(back)) {
            if (awvgr (raster, npix, lback, junk, ldatamin,
		ldatamax) <= 0)
                call error (1, "No data in good data range")
        } else
            lback = back

        ldatamin = max (ldatamin, lback)
        ldatamax = ldatamax
end


# CC_ROWSUM -- Sum all rows in a raster, subject to the thresholds, the
# background, and other parameters.

procedure cc_rowsum (raster, row, nx, ny, lo, hi, back)

real    raster[nx,ny]           #I the input 2-D subraster
real    row[ARB]                #O the output averaged row vector
int     nx, ny                  #I dimensions of the subraster
real	lo, hi			#I minimum and maximum good data values
real	back			#I the background value

int     i, j
real    pix, minpix, maxpix

begin
	# Compute the x marginal.
        call aclrr (row, nx)
        do j = 1, ny
            do i = 1, nx {
                pix = raster[i,j]
                if (lo <= pix && pix <= hi)
                    row[i] = row[i] + pix - back
            }
        call adivkr (row, real(ny), row, nx)

        # Check for low values.
        call alimr (row, nx, minpix, maxpix)
        if (minpix < 0.0)
            call error (1, "Negative value in marginal row")
end


# CC_COLSUM -- Sum all columns in a raster, subject to the thresholds, the
# background, and other parameters.

procedure cc_colsum (raster, col, nx, ny, lo, hi, back)

real    raster[nx,ny]           #I 2-D subraster
real    col[ARB]                #O 1-D squashed col vector
int     nx, ny                  #I dimensions of the subraster
real	lo, hi			#I minimum and maximum good data values
real	back			#I the background value


int     i, j
real    pix, minpix, maxpix

begin
	# Compute the y marginal.
        call aclrr (col, ny)
        do j = 1, ny
            do i = 1, nx {
                pix = raster[i,j]
                if (lo <= pix && pix <= hi)
                    col[j] = col[j] + pix - back
            }
        call adivkr (col, real(nx), col, ny)

        # Check for low values.
        call alimr (col, ny, minpix, maxpix)
        if (minpix < 0.)
            call error (1, "Negative value in marginal column")
end


# CC_CNTR1D -- Compute the the first moment.

real procedure cc_ctr1d (a, npix)

real    a[ARB]                  #I marginal vector
int     npix                    #I size of the vector

real    centroid, pix, sumi, sumix
int     i

begin
        sumi = 0.
        sumix = 0.
        do i = 1, npix {
            pix = a[i]
            sumi = sumi + pix
            sumix = sumix + pix * (i-1)
        }

        if (sumi == 0.0)
	    call error (1, "The center is undefined\n")
        else
            centroid = sumix / sumi

        return (centroid)
end

