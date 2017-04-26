include <fset.h>
include <ctype.h>
include <math.h>
include <pkg/skywcs.h>

define	HELPFILE1	"imcoords$src/skycur.key"
define	HELPFILE2	"imcoords$src/ttycur.key"

define	CURCMDS		"|show|isystem|osystem||ounits|oformats|"
define	TYPECMDS	"|show|isystem|osystem|iunits|ounits|oformats|"

define	CCMD_SHOW	1
define	CCMD_ISYSTEM	2
define	CCMD_OSYSTEM	3
define	CCMD_IUNITS	4
define	CCMD_OUNITS	5
define	CCMD_OFORMATS	6


# SK_TTYTRAN -- Transform the typed coordinate list.

procedure sk_ttytran (infd, outfd, mwin, mwout, cooin, cooout, ilngunits,
	ilatunits, olngunits, olatunits, olngformat, olatformat)

int	infd			#I the input file descriptor
int	outfd			#I the input file descriptor
pointer	mwin			#I the input image wcs
pointer	mwout			#I the output image wcs
pointer	cooin			#I the input coordinate descriptor
pointer	cooout			#I the output coordinate descriptor
int	ilngunits		#I the input ra/longitude units
int	ilatunits		#I the input dec/latitude units
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
char	olngformat[ARB]		#I the output ra/longitude format
char	olatformat[ARB]		#I the output dec/latitude format

double	ilng, ilat, pilng, pilat, px, rv, tlng, tlat, olng, olat
int	newsystem, newformat, newobject, tilngunits, tilatunits, tolngunits
int	tolatunits, ip, key
pointer	ctin, ctout, sp, cmd, fmtstr, tolngformat, tolatformat, str1, str2
double	sl_da1p()
int	scan(), nscan(), sk_stati(), ctod()
pointer	sk_ictran(), sk_octran()
errchk	sk_ictran(), sk_octran()

begin
	# Initialize.
	newsystem = YES
	newformat = YES
	newobject = NO
	ctin = NULL
	ctout = NULL

	# Get some working space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)
	call salloc (tolngformat, SZ_FNAME, TY_CHAR)
	call salloc (tolatformat, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	# Loop over the input.
	while (scan() != EOF) {
	    call gargstr (Memc[cmd], SZ_LINE)
	    key = Memc[cmd]
	    switch (key) {

	    case '?':
		call pagefile (HELPFILE2, "[space=cmhelp,q=quit,?=help]")

	    case 'q':
		break

	    case ':':
		call sk_ccolon (infd, outfd, cooin, cooout, mwin, mwout,
		    ilngunits, ilatunits, olngunits, olatunits, olngformat,
		    olatformat, Memc[cmd+1], TYPECMDS, newsystem, newformat)

	    default:
		newobject = YES
	    }

	    if (newobject == NO)
		next

	    # Decode the input coordinates.
	    call sscan (Memc[cmd])
	    call gargwrd (Memc[str1], SZ_FNAME)
	    call gargwrd (Memc[str2], SZ_FNAME)
	    if (nscan() < 2)
		next
	    ip = 1
	    if (ctod (Memc[str1], ip, ilng) <= 0)
		next
	    ip = 1
	    if (ctod (Memc[str2], ip, ilat) <= 0)
		next
	    call gargwrd (Memc[str1], SZ_FNAME)
	    call gargwrd (Memc[str2], SZ_FNAME)


	    # Decode the proper motions.
	    if (nscan() < 4) {
		pilng = INDEFD
		pilat = INDEFD
	    } else {
	        ip = 1
	        if (ctod (Memc[str1], ip, pilng) <= 0)
		    next
	        ip = 1
	        if (ctod (Memc[str2], ip, pilat) <= 0)
		    next
		if (IS_INDEFD(pilng) || IS_INDEFD(pilat)) {
		    pilng = INDEFD
		    pilat = INDEFD
		}
	    }

	    # Decode the parallax and radial velocity
	    call gargwrd (Memc[str1], SZ_FNAME)
	    call gargwrd (Memc[str2], SZ_FNAME)
	    if (nscan() < 6) {
		px = 0.0d0
		rv = 0.0d0
	    } else {
	        ip = 1
	        if (ctod (Memc[str1], ip, px) <= 0)
		    next
	        ip = 1
	        if (ctod (Memc[str2], ip, rv) <= 0)
		    next 
		if (IS_INDEFD(px))
		    px = 0.0d0
		if (IS_INDEFD(rv))
		    rv = 0.0d0
	    }

	    # Compile the mwcs transformation.
	    if (newsystem == YES) {
		if (ctin != NULL)
		    call mw_ctfree (cooin)
		if (ctout != NULL)
		    call mw_ctfree (cooout)
		iferr {
		    ctin = sk_ictran (cooin, mwin)
		    ctout = sk_octran (cooout, mwout)
		} then {
		    ctin = NULL
		    ctout = NULL
		}
		newsystem = NO
	    }

	    # Set the input and output coordinate units and the output format.
	    if (newformat == YES) {
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
        	call sk_oformats (cooin, cooout, olngformat, olatformat,
            	    tolngunits, tolatunits, Memc[tolngformat],
		    Memc[tolatformat], SZ_FNAME)
        	call sk_iunits (cooin, mwin, tilngunits, tilatunits,
		    tilngunits, tilatunits)
        	call sk_ounits (cooout, mwout, tolngunits, tolatunits,
		    tolngunits, tolatunits)
		call sprintf (Memc[fmtstr], SZ_LINE, "%%s %s %s\n")
		    call pargstr (Memc[tolngformat])
		    call pargstr (Memc[tolatformat])
		newformat = NO
	    }

	    # Perform the coordinate transformation.
	    if (sk_stati(cooin, S_STATUS) == ERR || sk_stati (cooout,
		S_STATUS) == ERR) {

		olng = ilng
		olat = ilat

	    } else {

	        # Compute the input coordinate to world coordinates in radians.
	        call sk_incc (cooin, mwin, ctin, tilngunits, tilatunits, ilng,
		    ilat, olng, olat)

	        # Convert the proper motions to the correct units.
	        if (!IS_INDEFD(pilng) && !IS_INDEFD(pilat)) {
		    pilng = DEGTORAD(pilng * 15.0d0 / 3600.0d0)
		    pilat = DEGTORAD(pilat / 3600.0d0)
		    call sl_dtps (pilng / 15.0d0, pilat, 0.0d0, olat, pilng,
		        pilat)
		    pilng = sl_da1p (pilng)
		    pilat = pilat - olat 
	        } else {
		    pilng = INDEFD
		    pilat = INDEFD
		}

	        # Perform the transformation.
	        call sk_lltran (cooin, cooout, olng, olat, pilng, pilat, px,
		    rv, tlng, tlat)

	        # Convert the celestial coordinates in radians to the output
	        # coordinates.
	        call sk_outcc (cooout, mwout, ctout, tolngunits, tolatunits,
		    tlng, tlat, olng, olat)
	    }

	    # Write the results.
	    call fprintf (outfd, Memc[fmtstr])
		call pargstr (Memc[cmd])
		call pargd (olng)
		call pargd (olat)
	    if (outfd != STDOUT) {
	        call printf (Memc[fmtstr])
		    call pargstr (Memc[cmd])
		    call pargd (olng)
		    call pargd (olat)
	    }

	    newobject = NO
	}

	call sfree (sp)
end


define	MAX_FIELDS	100		# Maximum number of fields in list
define	TABSIZE		8		# Spacing of tab stops

# SK_LISTRAN -- Transform the coordinate list.

procedure sk_listran (infd, outfd, mwin, mwout, cooin, cooout, lngcolumn,
	latcolumn, plngcolumn, platcolumn, pxcolumn, rvcolumn, ilngunits,
	ilatunits, olngunits, olatunits, olngformat, olatformat,
	min_sigdigits, transform)

int	infd			#I the input file descriptor
int	outfd			#I the input file descriptor
pointer	mwin			#I the input image wcs
pointer	mwout			#I the output image wcs
pointer	cooin			#I the input coordinate descriptor
pointer	cooout			#I the output coordinate descriptor
int	lngcolumn		#I the input ra/longitude column
int	latcolumn		#I the input dec/latitude column
int	plngcolumn		#I the input ra/longitude pm column
int	platcolumn		#I the input dec/latitude pm column
int	pxcolumn		#I the input parallax column
int	rvcolumn		#I the input radial column
int	ilngunits		#I the input ra/longitude units
int	ilatunits		#I the input dec/latitude units
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
char	olngformat[ARB]		#I the output ra/longitude format
char	olatformat[ARB]		#I the output dec/latitude format
int	min_sigdigits		#I the minimum number of significant digits
bool	transform		#I transform the input file

double	ilng, ilat, tlng, tlat, olng, olat, pilng, pilat, px, rv
int	nline, ip, max_fields, nfields, offset, nchars, nsdig_lng, nsdig_lat
int	tilngunits, tilatunits, tolngunits, tolatunits
pointer	sp, inbuf, linebuf, field_pos, outbuf, ctin, ctout
pointer	tolngformat, tolatformat
double	sl_da1p()
int	sk_stati(), li_get_numd(), getline()
pointer	sk_ictran(), sk_octran()
errchk	sk_ictran(), sk_octran()

begin
	# Compile the input abd output transformations.
	# coordinate units.
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
	call salloc (tolngformat, SZ_FNAME, TY_CHAR)
	call salloc (tolatformat, SZ_FNAME, TY_CHAR)

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
        call sk_oformats (cooin, cooout, olngformat, olatformat,
            tolngunits, tolatunits, Memc[tolngformat], Memc[tolatformat],
            SZ_FNAME)

	# Check the input and output units.
	call sk_iunits (cooin, mwin, tilngunits, tilatunits, tilngunits,
	    tilatunits)
	call sk_ounits (cooout, mwout, tolngunits, tolatunits, tolngunits,
	    tolatunits)

	# Loop over the input coordinates.
	max_fields = MAX_FIELDS
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
                call eprintf ("Not enough fields in file %s line %d\n")
                    call pargstr (Memc[outbuf])
                    call pargi (nline)
                call putline (outfd, Memc[linebuf])
                next
            }

	    offset = Memi[field_pos+lngcolumn-1]
            nchars = li_get_numd (Memc[linebuf+offset-1], ilng, nsdig_lng)
            if (nchars == 0) {
                call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                call eprintf ("Bad x value in file '%s' at line %d:\n")
                    call pargstr (Memc[outbuf])
                    call pargi (nline)
                call putline (outfd, Memc[linebuf])
                next
            }

	    offset = Memi[field_pos+latcolumn-1]
            nchars = li_get_numd (Memc[linebuf+offset-1], ilat, nsdig_lat)
            if (nchars == 0) {
                call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                call eprintf ("Bad y value in file '%s' at line %d:\n")
                    call pargstr (Memc[outbuf])
                    call pargi (nline)
                call putline (outfd, Memc[linebuf])
                next
            }

	    # Get the proper motions.
	    if (! IS_INDEFI(plngcolumn) && ! IS_INDEFI(platcolumn)) {
	        if (plngcolumn > nfields || platcolumn > nfields) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf ("Not enough fields in file %s line %d\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
	        offset = Memi[field_pos+plngcolumn-1]
                nchars = li_get_numd (Memc[linebuf+offset-1], pilng, nsdig_lng)
                if (nchars == 0) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf ("Bad pm value in file '%s' at line %d:\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
	        offset = Memi[field_pos+platcolumn-1]
                nchars = li_get_numd (Memc[linebuf+offset-1], pilat, nsdig_lat)
                if (nchars == 0) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf ("Bad pm value in file '%s' at line %d:\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
		if (IS_INDEFD(pilng) || IS_INDEFD(pilat)) {
		    pilng = INDEFD
		    pilat = INDEFD
		}
	    } else {
		pilng = INDEFD
		pilat = INDEFD
	    }

	    # Get the parallax value.
	    if (! IS_INDEFI(pxcolumn)) {
	        if (pxcolumn > nfields) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf ("Not enough fields in file %s line %d\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
	        offset = Memi[field_pos+pxcolumn-1]
                nchars = li_get_numd (Memc[linebuf+offset-1], px, nsdig_lat)
                if (nchars == 0) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf (
		        "Bad parallax value in file '%s' at line %d:\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
		if (IS_INDEFD(px))
		    px = 0.0d0
	    } else
		px = 0.0d0

	    # Get the parallax value.
	    if (! IS_INDEFI(rvcolumn)) {
	        if (rvcolumn > nfields) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf ("Not enough fields in file %s line %d\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
	        offset = Memi[field_pos+rvcolumn-1]
                nchars = li_get_numd (Memc[linebuf+offset-1], rv, nsdig_lat)
                if (nchars == 0) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf (
		        "Bad radial velocity value in file '%s' at line %d:\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
		if (IS_INDEFD(rv))
		    rv = 0.0d0
	    } else
		rv = 0.0d0

	    # Convert the input coordinates to world coordinates in radians.
	    call sk_incc (cooin, mwin, ctin, tilngunits, tilatunits, ilng,
	        ilat, olng, olat)

	    # Convert the proper motions to the correct units.
	    if (IS_INDEFD(pilng) || IS_INDEFD(pilat)) {
		pilng = INDEFD
		pilat = INDEFD
	    } else {
	        pilng = DEGTORAD(pilng * 15.0d0 / 3600.0d0)
	        pilat = DEGTORAD(pilat / 3600.0d0)
	        call sl_dtps (pilng / 15.0d0, pilat, 0.0d0, olat, pilng, pilat)
	        pilng = sl_da1p (pilng)
	        pilat = pilat - olat
	    }

	    # Perform the transformation.
	    call sk_lltran (cooin, cooout, olng, olat, pilng, pilat,
	        px, rv, tlng, tlat)

	    # Convert the output celestial coordinates from radians to output
	    # coordinates.
	    call sk_outcc (cooout, mwout, ctout, tolngunits, tolatunits,
	        tlng, tlat, olng, olat)

	    # Output the results.
	    if (transform) {
	        call li_pack_lined (Memc[linebuf], Memc[outbuf], SZ_LINE,
		    Memi[field_pos], nfields, lngcolumn, latcolumn, olng,
		    olat, Memc[tolngformat], Memc[tolatformat], nsdig_lng,
		    nsdig_lat, min_sigdigits)
	    } else {
		call li_append_lined (Memc[linebuf], Memc[outbuf], SZ_LINE,
		    olng, olat, Memc[tolngformat], Memc[tolatformat],
		    nsdig_lng, nsdig_lat, min_sigdigits)
	    }
	    call putline (outfd, Memc[outbuf])
	}

	call sfree (sp)
end


# SK_COPYTRAN -- Copy the input coordinate file to the output coordinate file.

procedure sk_copytran (infd, outfd, lngcolumn, latcolumn, transform)

int	infd			#I the input file descriptor
int	outfd			#I the output file descriptor
int	lngcolumn		#I the input ra/longitude column
int	latcolumn		#I the input dec/latitude column
bool	transform		#I tranform the input file

double	ilng, ilat
int	ip, nline, max_fields, nfields, xoffset, yoffset, nchars
int	nsdig_lng, nsdig_lat, xwidth, ywidth
pointer	sp, inbuf, linebuf, outbuf, field_pos
int	getline(), li_get_numd()

begin
	call smark (sp)
	call salloc (inbuf, SZ_LINE, TY_CHAR)
	call salloc (linebuf, SZ_LINE, TY_CHAR)
	call salloc (outbuf, SZ_LINE, TY_CHAR)
        call salloc (field_pos, MAX_FIELDS, TY_INT)

	if (transform) {
	    while (getline (infd, Memc[inbuf]) != EOF)
	        call putline (outfd, Memc[inbuf])
	} else {
	    max_fields = MAX_FIELDS
	    for (nline = 1; getline (infd, Memc[inbuf]) != EOF;
	        nline = nline + 1) {

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
                call li_find_fields (Memc[linebuf], Memi[field_pos],
		    max_fields, nfields)

	        if (lngcolumn > nfields || latcolumn > nfields) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf ("Not enough fields in file %s line %d\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }

	        xoffset = Memi[field_pos+lngcolumn-1]
                nchars = li_get_numd (Memc[linebuf+xoffset-1], ilng, nsdig_lng)
                if (nchars == 0) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf ("Bad x value in file '%s' at line %d:\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
	        xwidth = Memi[field_pos+lngcolumn] - Memi[field_pos+lngcolumn-1]

	        yoffset = Memi[field_pos+latcolumn-1]
                nchars = li_get_numd (Memc[linebuf+yoffset-1], ilat, nsdig_lat)
                if (nchars == 0) {
                    call fstats (infd, F_FILENAME, Memc[outbuf], SZ_LINE)
                    call eprintf ("Bad y value in file '%s' at line %d:\n")
                        call pargstr (Memc[outbuf])
                        call pargi (nline)
                    call putline (outfd, Memc[linebuf])
                    next
                }
	        ywidth = Memi[field_pos+latcolumn] - Memi[field_pos+latcolumn-1]

		call li_cappend_line (Memc[linebuf], Memc[outbuf], SZ_LINE,
		    xoffset, yoffset, xwidth, ywidth)
		call putline (outfd, Memc[outbuf])
	    }
	}

	call sfree (sp)
end


# SK_CURTRAN -- Transform the cursor coordinate list.

procedure sk_curtran (outfd, mwin, mwout, cooin, cooout, olngunits, olatunits,
	olngformat, olatformat, transform)

int	outfd			#I the input file descriptor
pointer	mwin			#I the input image wcs
pointer	mwout			#I the output image wcs
pointer	cooin			#I the input coordinate descriptor
pointer	cooout			#I the output coordinate descriptor
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
char	olngformat[ARB]		#I the output ra/longitude format
char	olatformat[ARB]		#I the output dec/latitude format
bool	transform		#I transform the input file

double	ilng, ilat, tlng, tlat, olng, olat
int	wcs, key, tolngunits, tolatunits, newsystem, newformat, newobject
int	ijunk
pointer	sp, cmd, fmtstr, ctin, ctout, tolngformat, tolatformat
real	wx, wy
int	clgcur(), sk_stati()
pointer	sk_ictran(), sk_octran()
errchk	sk_ictran(), sk_octran()

begin
	# Initialize.
	newsystem = YES
	newformat = YES
	newobject = NO
	ctin = NULL
	ctout = NULL

	# Get some working space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)
        call salloc (tolngformat, SZ_FNAME, TY_CHAR)
        call salloc (tolatformat, SZ_FNAME, TY_CHAR)

	while (clgcur ("icommands", wx, wy, wcs, key, Memc[cmd],
	    SZ_LINE) != EOF) {

	    newobject = NO
	    ilng = wx
	    ilat = wy

	    switch (key) {

	    case '?':
		call pagefile (HELPFILE1, "[space=cmhelp,q=quit,?=help]")

	    case 'q':
		break

	    case ':':
		call sk_ccolon (NULL, outfd, cooin, cooout, mwin, mwout,
		    ijunk, ijunk, olngunits, olatunits, olngformat,
		    olatformat, Memc[cmd], CURCMDS, newsystem, newformat)

	    case ' ':
		newobject = YES

	    default:
		;
	    }

	    if (newobject == NO)
		next

	    # Compile the mwcs transformation.
	    if (newsystem == YES) {
	        if (ctin != NULL)
		    call mw_ctfree (ctin)
	        if (ctout != NULL)
		    call mw_ctfree (ctout)
	        iferr {
	    	    ctin = sk_ictran (cooin, mwin)
	    	    ctout = sk_octran (cooout, mwout)
		} then {
	    	    ctin = NULL
	    	    ctout = NULL
		}
		newsystem = NO
	    }

	    # Set the output coordinates units and format.
	    if (newformat == YES) {
        	if (olngunits <= 0)
            	    tolngunits = sk_stati (cooout, S_NLNGUNITS)
        	else
            	    tolngunits = olngunits
        	if (olatunits <= 0)
            	    tolatunits = sk_stati (cooout, S_NLATUNITS)
        	else
            	    tolatunits = olatunits
        	call sk_oformats (cooin, cooout, olngformat, olatformat,
            	    tolngunits, tolatunits, Memc[tolngformat],
		    Memc[tolatformat], SZ_FNAME)
        	call sk_ounits (cooout, mwout, tolngunits, tolatunits,
		   tolngunits, tolatunits)
		if (sk_stati(cooin, S_STATUS) == ERR || sk_stati(cooout,
	    	    S_STATUS) == ERR) {
	    	    if (transform)
		        call strcpy ("%10.3f %10.3f\n", Memc[fmtstr], SZ_LINE)
	     	    else
		        call strcpy ("%10.3f %10.3f  %10.3f %10.3f\n",
			    Memc[fmtstr], SZ_LINE)
		} else {
	    	    if (transform) {
	                call sprintf (Memc[fmtstr], SZ_LINE, "%s %s\n")
	                    call pargstr (Memc[tolngformat])
	                    call pargstr (Memc[tolatformat])
	            } else {
	                call sprintf (Memc[fmtstr], SZ_LINE,
			    "%%10.3f %%10.3f  %s %s\n")
	                call pargstr (Memc[tolngformat])
	                call pargstr (Memc[tolatformat])
	            }
	        }
		newformat = NO
	    }

	    # Compute the transformation.
	    if (sk_stati(cooin, S_STATUS) == ERR || sk_stati(cooout,
	    	    S_STATUS) == ERR) {
		olng = ilng
		olat = ilat
	    } else {
	    	call sk_incc (cooin, mwin, ctin, SKY_DEGREES, SKY_DEGREES,
	            ilng, ilat, olng, olat)
	    	call sk_lltran (cooin, cooout, olng, olat, INDEFD, INDEFD,
		    0.0d0, 0.0d0, tlng, tlat)
	    	call sk_outcc (cooout, mwout, ctout, tolngunits,
		   tolatunits, tlng, tlat, olng, olat)
	    }

	    # Write out the results.
	    if (transform) {
	        call fprintf (outfd, Memc[fmtstr])
	            call pargd (olng)
	            call pargd (olat)
	    } else {
	        call fprintf (outfd, Memc[fmtstr])
		    call pargr (wx)
		    call pargr (wy)
	            call pargd (olng)
	            call pargd (olat)
	    }

	    newobject = NO

	}

	call sfree (sp)
end

# SKY_CCOLON -- Process image cursor colon commands.

procedure sk_ccolon (infd, outfd, cooin, cooout, mwin, mwout, ilngunits,
	ilatunits, olngunits, olatunits, olngformat, olatformat, cmdstr,
	cmdlist, newsystem, newformat)

int	infd			#I the input file descriptor
int	outfd			#I the output file descriptor
pointer	cooin			#U the input coordinate descriptor
pointer	cooout			#U the output coordinate descriptor
pointer	mwin			#U the input image wcs
pointer	mwout			#U the output image wcs
int	ilngunits		#U the input ra/longitude units
int	ilatunits		#U the input dec/latitude units
int	olngunits		#U the output ra/longitude units
int	olatunits		#U the output dec/latitude units
char	olngformat[ARB]		#U the output ra/longitude format
char	olatformat[ARB]		#U the output dec/latitude format
char	cmdstr[ARB]		#I the input command string
char	cmdlist[ARB]		#I the input command list
int	newsystem		#U new coordinate system ?
int	newformat		#U new coordinate format ?

int	ncmd, stat
pointer	sp, cmd, str1, str2, str3, str4, tmw, tcoo
int	sk_stati(), strdic(), sk_decwcs()

begin
        call smark (sp)
        call salloc (cmd, SZ_LINE, TY_CHAR)
        call salloc (str1, SZ_FNAME, TY_CHAR)
        call salloc (str2, SZ_FNAME, TY_CHAR)
        call salloc (str3, SZ_FNAME, TY_CHAR)
        call salloc (str4, SZ_FNAME, TY_CHAR)

        # Get the command.
        call sscan (cmdstr)
        call gargwrd (Memc[cmd], SZ_LINE)
        if (Memc[cmd] == EOS) {
            call sfree (sp)
            return
        }

	# Process the command.
	ncmd = strdic (Memc[cmd], Memc[cmd], SZ_LINE, cmdlist)
        call gargstr (Memc[cmd], SZ_LINE)
	switch (ncmd) {

	case CCMD_SHOW:
            call fprintf (outfd, "\n")
            if (sk_stati (cooin, S_STATUS) == ERR)
                call fprintf (outfd,
                "# Error decoding the input coordinate system\n")
	    call sk_stats (cooin, S_COOSYSTEM, Memc[str1], SZ_FNAME)
            call sk_iiwrite (outfd, "Insystem", Memc[str1], mwin,
                cooin)
	    if (infd == NULL)
	        call sk_wiformats (cooin, ilngunits, ilatunits, "%10.3f",
		    "%10.3f", Memc[str1], Memc[str2], Memc[str3], Memc[str4],
		    SZ_FNAME)
	    else
	        call sk_wiformats (cooin, ilngunits, ilatunits, "INDEF",
		    "INDEF", Memc[str1], Memc[str2], Memc[str3], Memc[str4],
		    SZ_FNAME)
	    call fprintf (outfd, "#     Units: %s %s  Format: %s %s\n")
		call pargstr (Memc[str1])
		call pargstr (Memc[str2])
		call pargstr (Memc[str3])
		call pargstr (Memc[str4])
            if (sk_stati(cooout, S_STATUS) == ERR)
                call fprintf (outfd,
                "# Error decoding the output coordinate system\n")
	    call sk_stats (cooout, S_COOSYSTEM, Memc[str1], SZ_FNAME)
            call sk_iiwrite (outfd, "Outsystem", Memc[str1], mwout,
                cooout)
	    call sk_woformats (cooin, cooout, olngunits, olatunits,
		olngformat, olatformat, Memc[str1], Memc[str2], Memc[str3],
		Memc[str4], SZ_FNAME)
	    call fprintf (outfd, "#     Units: %s %s  Format: %s %s\n")
		call pargstr (Memc[str1])
		call pargstr (Memc[str2])
		call pargstr (Memc[str3])
		call pargstr (Memc[str4])
	    call fprintf (outfd, "\n")

	    if (outfd != STDOUT) {
                call printf ("\n")
                if (sk_stati (cooin, S_STATUS) == ERR)
                    call printf (
                    "Error decoding the input coordinate system\n")
	        call sk_stats (cooin, S_COOSYSTEM, Memc[str1], SZ_FNAME)
                call sk_iiprint ("Insystem", Memc[str1], mwin, cooin)
	        if (infd == NULL)
	            call sk_wiformats (cooin, ilngunits, ilatunits, "%10.3f",
		        "%10.3f", Memc[str1], Memc[str2], Memc[str3],
			Memc[str4], SZ_FNAME)
	        else
	            call sk_wiformats (cooin, ilngunits, ilatunits, "INDEF",
		        "INDEF", Memc[str1], Memc[str2], Memc[str3], Memc[str4],
		        SZ_FNAME)
	        call printf ("#     Units: %s %s  Format: %s %s\n")
		    call pargstr (Memc[str1])
		    call pargstr (Memc[str2])
		    call pargstr (Memc[str3])
		    call pargstr (Memc[str4])
                if (sk_stati(cooout, S_STATUS) == ERR)
                    call printf (
                    "Error decoding the output coordinate system\n")
	        call sk_stats (cooout, S_COOSYSTEM, Memc[str1], SZ_FNAME)
                call sk_iiprint ("Outsystem", Memc[str1], mwout, cooout)
	        call sk_woformats (cooin, cooout, olngunits, olatunits,
		    olngformat, olatformat, Memc[str1], Memc[str2], Memc[str3],
		    Memc[str4], SZ_FNAME)
	        call printf ("    Units: %s %s  Format: %s %s\n")
		    call pargstr (Memc[str1])
		    call pargstr (Memc[str2])
		    call pargstr (Memc[str3])
		    call pargstr (Memc[str4])
	        call printf ("\n")
	    }

	case CCMD_ISYSTEM:
	    stat = sk_decwcs (Memc[cmd], tmw, tcoo, NULL)
	    if (Memc[cmd] == EOS || stat == ERR || (infd == NULL &&
	        tmw == NULL)) {
		if (tmw != NULL)
		    call mw_close (tmw)
		call sk_close (tcoo)
		call fprintf (outfd, "\n")
                if (sk_stati(cooin, S_STATUS) == ERR)
                    call fprintf (outfd,
                    "# Error decoding the input coordinate system\n")
	        call sk_stats (cooin, S_COOSYSTEM, Memc[str1], SZ_FNAME)
                call sk_iiwrite (outfd, "Insystem", Memc[str1], mwin, cooin)
		call fprintf (outfd, "\n")
		if (outfd != STDOUT) {
		    call printf ("\n")
                    if (sk_stati(cooin, S_STATUS) == ERR)
                        call printf (
                        "# Error decoding the input coordinate system\n")
	            call sk_stats (cooin, S_COOSYSTEM, Memc[str1], SZ_FNAME)
                    call sk_iiprint ("Insystem", Memc[str1], mwin, cooin)
		    call printf ("\n")
		}
	    } else {
		if (mwin != NULL)
		    call mw_close (mwin)
		call sk_close (cooin)
		mwin = tmw
		cooin = tcoo
		if (infd == NULL)
		    call sk_seti (cooin, S_PIXTYPE, PIXTYPE_TV)
		newsystem = YES
		newformat = YES
	    }

	case CCMD_OSYSTEM:
	    stat = sk_decwcs (Memc[cmd], tmw, tcoo, NULL)
	    if (Memc[cmd] == EOS || stat == ERR) {
		if (tmw != NULL)
		    call mw_close (tmw)
		call sk_close (tcoo)
		call fprintf (outfd, "\n")
                if (sk_stati(cooout, S_STATUS) == ERR)
                    call fprintf (outfd,
                    "# Error decoding the output coordinate system\n")
	        call sk_stats (cooout, S_COOSYSTEM, Memc[str1], SZ_FNAME)
                call sk_iiwrite (outfd, "Outsystem", Memc[str1], mwout, cooout)
		call fprintf (outfd, "\n")
		if (outfd != STDOUT) {
		    call printf ("\n")
                    if (sk_stati(cooout, S_STATUS) == ERR)
                        call printf (
                        "# Error decoding the output coordinate system\n")
	            call sk_stats (cooout, S_COOSYSTEM, Memc[str1], SZ_FNAME)
                    call sk_iiprint ("Outsystem", Memc[str1], mwout, cooout)
		    call printf ("\n")
		}
	    } else {
		if (mwout != NULL)
		    call mw_close (mwout)
		call sk_close (cooout)
		mwout = tmw
		cooout = tcoo
		newsystem = YES
		newformat = YES
	    }

	case CCMD_IUNITS:
	    call sscan (Memc[cmd])
	    call gargwrd (Memc[str1], SZ_FNAME)
	    call gargwrd (Memc[str2], SZ_FNAME)
	    if (Memc[cmd] == EOS) {
	        call sk_wiformats (cooin, ilngunits, ilatunits, "", "",
		    Memc[str1], Memc[str2], Memc[str3], Memc[str4], SZ_FNAME)
		call fprintf (outfd, "\n")
	        call fprintf (outfd, "# Units: %s %s\n")
		    call pargstr (Memc[str1])
		    call pargstr (Memc[str2])
		call fprintf (outfd, "\n")
		if (outfd != STDOUT) {
		    call printf ("\n")
	            call printf ("Units: %s %s\n")
		        call pargstr (Memc[str1])
		        call pargstr (Memc[str2])
		    call printf ("\n")
		}
	    } else {
		ilngunits = strdic (Memc[str1], Memc[str1], SZ_FNAME,
		    SKY_LNG_UNITLIST)
		ilatunits = strdic (Memc[str2], Memc[str2], SZ_FNAME,
		    SKY_LAT_UNITLIST)
		newformat = YES
	    }

	case CCMD_OUNITS:
	    call sscan (Memc[cmd])
	    call gargwrd (Memc[str1], SZ_FNAME)
	    call gargwrd (Memc[str2], SZ_FNAME)
	    if (Memc[cmd] == EOS) {
	        call sk_woformats (cooin, cooout, olngunits, olatunits,
		    olngformat, olatformat, Memc[str1], Memc[str2], Memc[str3],
		    Memc[str4], SZ_FNAME)
		call fprintf (outfd, "\n")
	        call fprintf (outfd, "# Units: %s %s\n")
		    call pargstr (Memc[str1])
		    call pargstr (Memc[str2])
		call fprintf (outfd, "\n")
		if (outfd != STDOUT) {
		    call printf ("\n")
	            call printf ("Units: %s %s\n")
		        call pargstr (Memc[str1])
		        call pargstr (Memc[str2])
		    call printf ("\n")
		}
	    } else {
		olngunits = strdic (Memc[str1], Memc[str1], SZ_FNAME,
		    SKY_LNG_UNITLIST)
		olatunits = strdic (Memc[str2], Memc[str2], SZ_FNAME,
		    SKY_LAT_UNITLIST)
		newformat = YES
	    }

	case CCMD_OFORMATS:
	    call sscan (Memc[cmd])
	    call gargwrd (Memc[str1], SZ_FNAME)
	    call gargwrd (Memc[str2], SZ_FNAME)
	    if (Memc[cmd] == EOS) {
	        call sk_woformats (cooin, cooout, olngunits, olatunits,
		    olngformat, olatformat, Memc[str1], Memc[str2], Memc[str3],
		    Memc[str4], SZ_FNAME)
		call fprintf (outfd, "\n")
	        call fprintf (outfd, "# Formats: %s %s\n")
		    call pargstr (Memc[str3])
		    call pargstr (Memc[str4])
		call fprintf (outfd, "\n")
		if (outfd != STDOUT) {
		    call printf ("\n")
	            call printf ("Formats: %s %s\n")
		        call pargstr (Memc[str3])
		        call pargstr (Memc[str4])
		    call printf ("\n")
		}
	    } else {
		call strcpy (Memc[str1], olngformat, SZ_FNAME)
		call strcpy (Memc[str2], olatformat, SZ_FNAME)
		newformat = YES
	    }

	default:
	    ;
	}

	call sfree (sp)
end


# SK_GRTRAN -- Transform the grid coordinate list.

procedure sk_grtran (outfd, mwin, mwout, cooin, cooout, ilngmin, ilngmax,
	nilng, ilatmin, ilatmax, nilat, ilngunits, ilatunits, olngunits,
	olatunits, ilngformat, ilatformat, olngformat, olatformat, transform)

int	outfd			#I the input file descriptor
pointer	mwin			#I the input image wcs
pointer	mwout			#I the output image wcs
pointer	cooin			#I the input coordinate descriptor
pointer	cooout			#I the output coordinate descriptor
double	ilngmin			#I the x/ra/longitude minimum
double	ilngmax			#I the x/ra/longitude maximum
int	nilng			#I the number of x/ra/longitude grid points
double	ilatmin			#I the y/dec/longitude minimum
double	ilatmax			#I the y/dec/longitude maximum
int	nilat			#I the number of y/dec/latitude grid points
int	ilngunits		#I the input ra/longitude units
int	ilatunits		#I the input dec/latitude units
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
char	ilngformat[ARB]		#I the input ra/longitude format
char	ilatformat[ARB]		#I the input dec/latitude format
char	olngformat[ARB]		#I the output ra/longitude format
char	olatformat[ARB]		#I the output dec/latitude format
bool	transform		#I transform the input file

double	ilng1, ilng2, ilat1, ilat2, ilngstep, ilatstep, ilng, ilat, olng, olat
double	tlng, tlat
int	i, j, tilngunits, tilatunits, tolngunits, tolatunits
pointer	sp, fmtstr, ctin, ctout, tilngformat, tilatformat
pointer	tolngformat, tolatformat
int	sk_stati()
pointer	sk_ictran(), sk_octran()
errchk	sk_ictran(), sk_octran()

begin
	# Compile the input and output transformations.
	iferr {
	    ctin = sk_ictran (cooin, mwin)
	    ctout = sk_octran (cooout, mwout)
	} then
	    return

	# Get some working space.
	call smark (sp)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)
	call salloc (tilngformat, SZ_FNAME, TY_CHAR)
	call salloc (tilatformat, SZ_FNAME, TY_CHAR)
	call salloc (tolngformat, SZ_FNAME, TY_CHAR)
	call salloc (tolatformat, SZ_FNAME, TY_CHAR)

        # Set the input and output units.
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

	# Set the input and output formats.
        call sk_iformats (cooin,  ilngformat, ilatformat,
            tilngunits, tilatunits, Memc[tilngformat], Memc[tilatformat],
            SZ_FNAME)
        call sk_oformats (cooin, cooout, olngformat, olatformat,
            tolngunits, tolatunits, Memc[tolngformat], Memc[tolatformat],
            SZ_FNAME)

	# Check the input and output units.
        call sk_iunits (cooin, mwin, tilngunits, tilatunits, tilngunits,
            tilatunits)
        call sk_ounits (cooout, mwout, tolngunits, tolatunits, tolngunits,
            tolatunits)

	# Create the format string.
	if (transform) {
	    call sprintf (Memc[fmtstr], SZ_LINE, "%s %s\n")
	        call pargstr (Memc[tolngformat])
	        call pargstr (Memc[tolatformat])
	} else {
	    call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s\n")
	       	call pargstr (Memc[tilngformat])
	       	call pargstr (Memc[tilatformat])
	       	call pargstr (Memc[tolngformat])
	       	call pargstr (Memc[tolatformat])
	}

	# Compute the grid parameters in x/ra/longitude.
	if (IS_INDEFD(ilngmin)) {
	    switch (sk_stati(cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
	        ilng1 = 1.0d0
	    default:
		switch (sk_stati(cooin, S_CTYPE)) {
		case 0:
	            ilng1 = 1.0d0
		default:
		    switch (tilngunits) {
		    case SKY_HOURS:
	                ilng1 = 0.0d0
		    case SKY_DEGREES:
	                ilng1 = 0.0d0
		    case SKY_RADIANS:
	                ilng1 = 0.0d0
		    }
		}
	    }
	} else
	    ilng1 = ilngmin

	if (IS_INDEFD(ilngmax)) {
	    switch (sk_stati(cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
	        ilng2 = sk_stati (cooin, S_NLNGAX)
	    default:
		switch (sk_stati (cooin, S_CTYPE)) {
		case 0:
	            ilng2 = sk_stati(cooin, S_NLNGAX)
		default:
		    switch (tilngunits) {
		    case SKY_HOURS:
	                ilng2 = 24.0d0
		    case SKY_DEGREES:
	                ilng2 = 360.0d0
		    case SKY_RADIANS:
	                ilng2 = TWOPI
		    }
		}
	    }
	} else
	    ilng2 = ilngmax
	if (nilng == 1)
	    ilngstep = 0.0d0
	else
	    ilngstep = (ilng2 - ilng1) / (nilng - 1)

	# Compute the grid parameters in y/dec/latitude.
	if (IS_INDEFD(ilatmin)) {
	    switch (sk_stati (cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
	        ilat1 = 1.0d0
	    default:
		switch (sk_stati (cooin, S_CTYPE)) {
		case 0:
		    ilat1 = 1.0d0
		default:
		    switch (tilatunits) {
		    case SKY_HOURS:
	                ilat1 = 0.0d0
		    case SKY_DEGREES:
	                ilat1 = -90.0d0
		    case SKY_RADIANS:
	                ilat1 = -HALFPI
		    }
		}
	    }
	} else
	    ilat1 = ilatmin

	if (IS_INDEFD(ilatmax)) {
	    switch (sk_stati (cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
	        ilat2 = sk_stati (cooin, S_NLATAX)
	    default:
		switch (sk_stati (cooin, S_CTYPE)) {
		case 0:
	            ilat2 = sk_stati(cooin, S_NLATAX)
		default:
		    switch (tilatunits) {
		    case SKY_HOURS:
	                ilat2 = 24.0d0
		    case SKY_DEGREES:
	                ilat2 = 90.0d0
		    case SKY_RADIANS:
	                ilat2 = HALFPI
		    }
		}
	    }
	} else
	    ilat2 = ilatmax
	if (nilat == 1)
	    ilatstep = 0.0d0
	else
	    ilatstep = (ilat2 - ilat1) / (nilat - 1)

	# Compute the grid of points.
	do j = 1, nilat {

	    ilat = ilat1 + (j - 1) * ilatstep

	    do i = 1, nilng {

	        ilng = ilng1 + (i - 1) * ilngstep

		# Convert the input coordinates to world coordinates in
		# radians.
		call sk_incc (cooin, mwin, ctin, tilngunits, tilatunits,
		    ilng, ilat, olng, olat)

	        # Perform the transformation.
	        call sk_lltran (cooin, cooout, olng, olat, INDEFD,
		    INDEFD, 0.0d0, 0.0d0, tlng, tlat)

	        # Convert the celestial coordinates to output coordinates.
		call sk_outcc (cooout, mwout, ctout, tolngunits, tolatunits,
		    tlng, tlat, olng, olat)

	        # Write out the results
	        if (transform) {
	            call fprintf (outfd, Memc[fmtstr])
	                call pargd (olng)
	                call pargd (olat)
	        } else {
	            call fprintf (outfd, Memc[fmtstr])
		        call pargd (ilng)
		        call pargd (ilat)
	                call pargd (olng)
	                call pargd (olat)
	        }
	    }
	}

	call sfree (sp)
end


# SK_GRCOPY -- Copy the input logical pixel grid to the output logical
# pixel grid.

procedure sk_grcopy (outfd, cooin, cooout, ilngmin, ilngmax, nilng, ilatmin,
	ilatmax, nilat, ilngunits, ilatunits, olngunits, olatunits, ilngformat,
	ilatformat, olngformat, olatformat, transform) 

int	outfd			#I the output file descriptor
pointer	cooin			#I the pointer to input coordinate structure
pointer	cooout			#I the pointer to output coordinate structure
double	ilngmin			#I the x/ra/longitude minimum
double	ilngmax			#I the x/ra/longitude maximum
int	nilng			#I the number of x/ra/longitude grid points
double	ilatmin			#I the y/dec/longitude minimum
double	ilatmax			#I the y/dec/longitude maximum
int	nilat			#I the number of y/dec/latitude grid points
int	ilngunits		#I the input x/ra/longitude units
int	ilatunits		#I the input y/dec/latitude/units
int	olngunits		#I the output x/ra/longitude units
int	olatunits		#I the output y/dec/latitude/units
char	ilngformat[ARB]		#I the input x format string
char	ilatformat[ARB]		#I the intput y format string
char	olngformat[ARB]		#I the output x format string
char	olatformat[ARB]		#I the output y format string
bool	transform		#I transform the input file

double	x1, x2, x, y1, y2, y, xstep, ystep
int	i, j, tilngunits, tilatunits, tolngunits, tolatunits
pointer	sp, tilngformat, tilatformat, tolngformat, tolatformat, fmtstr
int	sk_stati()

begin
	call smark (sp)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)
	call salloc (tilngformat, SZ_FNAME, TY_CHAR)
	call salloc (tilatformat, SZ_FNAME, TY_CHAR)
	call salloc (tolngformat, SZ_FNAME, TY_CHAR)
	call salloc (tolatformat, SZ_FNAME, TY_CHAR)

	# Set the input units.
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

	# Set the input and output formats.
        call sk_iformats (cooin,  ilngformat, ilatformat,
            tilngunits, tilatunits, Memc[tilngformat], Memc[tilatformat],
            SZ_FNAME)
        call sk_oformats (cooin, cooout, olngformat, olatformat,
            tolngunits, tolatunits, Memc[tolngformat], Memc[tolatformat],
            SZ_FNAME)

	# Create the format string.
	if (transform) {
	    call sprintf (Memc[fmtstr], SZ_LINE, "%s %s\n")
	        call pargstr (Memc[tolngformat])
	        call pargstr (Memc[tolatformat])
	} else {
	    call sprintf (Memc[fmtstr], SZ_LINE, "%s %s  %s %s\n")
	        call pargstr (Memc[tilngformat])
	        call pargstr (Memc[tilatformat])
	        call pargstr (Memc[tolngformat])
	        call pargstr (Memc[tolatformat])
	}

	# Compute the grid parameters in x/ra/longitude.
	if (IS_INDEFD(ilngmin)) {
	    switch (sk_stati (cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
	        x1 = 1.0d0
	    default:
		switch (sk_stati (cooin, S_CTYPE)) {
		case 0:
	            x1 = 1.0d0
		default:
		    switch (tilngunits) {
		    case SKY_HOURS:
	                x1 = 0.0d0
		    case SKY_DEGREES:
	                x1 = 0.0d0
		    case SKY_RADIANS:
	                x1 = 0.0d0
		    }
		}
	    }
	} else
	    x1 = ilngmin
	if (IS_INDEFD(ilngmax)) {
	    switch (sk_stati(cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
	        x2 = sk_stati(cooin, S_NLNGAX)
	    default:
		switch (sk_stati (cooin, S_CTYPE)) {
		case 0:
	            x2 = sk_stati (cooin, S_NLNGAX)
		default:
		    switch (tilngunits) {
		    case SKY_HOURS:
	                x2 = 24.0d0
		    case SKY_DEGREES:
	                x2 = 360.0d0
		    case SKY_RADIANS:
	                x2 = TWOPI
		    }
		}
	    }
	} else
	    x2 = ilngmax
	if (nilng == 1)
	    xstep = 0.0d0
	else
	    xstep = (x2 - x1) / (nilng - 1)

	# Compute the grid parameters in y/dec/latitude.
	if (IS_INDEFD(ilatmin)) {
	    switch (sk_stati (cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
	        y1 = 1.0d0
	    default:
		switch (sk_stati(cooin, S_CTYPE)) {
		case 0:
		    y1 = 1.0d0
		default:
		    switch (tilatunits) {
		    case SKY_HOURS:
	                y1 = 0.0d0
		    case SKY_DEGREES:
	                y1 = -90.0d0
		    case SKY_RADIANS:
	                y1 = -HALFPI
		    }
		}
	    }
	} else
	    y1 = ilatmin

	if (IS_INDEFD(ilatmax)) {
	    switch (sk_stati (cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
	        y2 = sk_stati (cooin, S_NLATAX)
	    default:
		switch (sk_stati(cooin, S_CTYPE)) {
		case 0:
	            y2 = sk_stati (cooin, S_NLATAX)
		default:
		    switch (tilatunits) {
		    case SKY_HOURS:
	                y2 = 24.0d0
		    case SKY_DEGREES:
	                y2 = 90.0d0
		    case SKY_RADIANS:
	                y2 = HALFPI
		    }
		}
	    }
	} else
	    y2 = ilatmax
	if (nilat == 1)
	    ystep = 0.0d0
	else
	    ystep = (y2 - y1) / (nilat - 1)

	# Compute the points.
	y = y1
	do j = 1, nilat {
	    x = x1
	    do i = 1, nilng {
		if (transform) {
	            call fprintf (outfd, Memc[fmtstr])
		        call pargd (x)
		        call pargd (y)
		} else {
	            call fprintf (outfd, Memc[fmtstr])
		        call pargd (x)
		        call pargd (y)
		        call pargd (x)
		        call pargd (y)
		}
		x = x + xstep
	    }
	    y = y + ystep
	}

	call sfree (sp)
end


# SK_WIFORMATS -- Format the input units and format strings.

procedure sk_wiformats (cooin, ilngunits, ilatunits, ilngformat,
	ilatformat, ilngunitstr, ilatunitstr, oilngformat, oilatformat, maxch)

pointer	cooin			#I the input coordinate structure
int	ilngunits		#I the output ra/longitude units
int	ilatunits		#I the output dec/latitude units
char	ilngformat[ARB]		#I the output ra/longitude format string
char	ilatformat[ARB]		#I the output dec/latitude format string
char	ilngunitstr[ARB]	#O the output output ra/longitude format string
char	ilatunitstr[ARB]	#O the output output dec/latitude format string
char	oilngformat[ARB]	#O the output output ra/longitude format string
char	oilatformat[ARB]	#O the output output dec/latitude format string
int	maxch			#I the maximum length of the format strings

int	tilngunits, tilatunits
int	sk_stati()

begin
	# Determine the correct units.
        if (ilngunits <= 0)
            tilngunits = sk_stati (cooin, S_NLNGUNITS)
        else
            tilngunits = ilngunits
        if (ilatunits <= 0)
            tilatunits = sk_stati (cooin, S_NLATUNITS)
        else
            tilatunits = ilatunits

	# Format the units strings.
	if (sk_stati(cooin, S_PIXTYPE) != PIXTYPE_WORLD) {
	    call strcpy ("pixels", ilngunitstr, maxch)
	    call strcpy ("pixels", ilatunitstr, maxch)
	} else {
	    switch (tilngunits) {
	    case SKY_HOURS:
	        call strcpy ("hours", ilngunitstr, maxch)
	    case SKY_DEGREES:
	        call strcpy ("degrees", ilngunitstr, maxch)
	    case SKY_RADIANS:
	        call strcpy ("radians", ilngunitstr, maxch)
	    }
	    switch (tilatunits) {
	    case SKY_HOURS:
	        call strcpy ("hours", ilatunitstr, maxch)
	    case SKY_DEGREES:
	        call strcpy ("degrees", ilatunitstr, maxch)
	    case SKY_RADIANS:
	        call strcpy ("radians", ilatunitstr, maxch)
	    }
	}

	# Format the format strings.
	call sk_iformats (cooin, ilngformat, ilatformat,
	    tilngunits, tilatunits, oilngformat, oilatformat,
	    SZ_FNAME)
end


# SK_IFORMATS -- Set the input format strings.

procedure sk_iformats (cooin, ilngformat, ilatformat, ilngunits, ilatunits,
	oilngformat, oilatformat, maxch)

pointer	cooin			#I the input coordinate structure
char	ilngformat[ARB]		#I the input ra/longitude format string
char	ilatformat[ARB]		#I the input dec/latitude format string
int	ilngunits		#I the input ra/longitude units
int	ilatunits		#I the input dec/latitude units
char	oilngformat[ARB]	#O the input ra/longitude format string
char	oilatformat[ARB]	#O the input dec/latitude format string
int	maxch			#I the maximum length of the format strings

int	sk_stati()

begin
	if (ilngformat[1] == EOS) {
	    if (sk_stati(cooin, S_STATUS) == ERR)
		call strcpy ("%10.3f", oilngformat, maxch)
	    else {
		switch (sk_stati(cooin, S_PIXTYPE)) {
		case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL: 
		    call strcpy ("%10.3f", oilngformat, maxch)
		default:
		    switch (ilngunits) {
		    case SKY_HOURS:
			call strcpy ("%12.3h", oilngformat, maxch)
		    case SKY_DEGREES:
			call strcpy ("%12.2h", oilngformat, maxch)
		    case SKY_RADIANS:
			call strcpy ("%13.7g", oilngformat, maxch)
		    }
		}
	    }
	} else
	    call strcpy (ilngformat, oilngformat, maxch)

	if (ilatformat[1] == EOS) {
	    if (sk_stati (cooin, S_STATUS) == ERR)
	        call strcpy ("%10.3f", oilatformat, maxch)
	    else {
	        switch (sk_stati(cooin, S_PIXTYPE)) {
	        case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL: 
		    call strcpy ("%10.3f", oilatformat, maxch)
	        default:
		    switch (ilatunits) {
		    case SKY_HOURS:
			call strcpy ("%12.3h", oilatformat, maxch)
		    case SKY_DEGREES:
			call strcpy ("%12.2h", oilatformat, maxch)
		    case SKY_RADIANS:
			call strcpy ("%13.7g", oilatformat, maxch)
		    }
	        }
	    }
	} else
	    call strcpy (ilatformat, oilatformat, maxch)
end


# SK_WOFORMATS -- Format the units and format strings.

procedure sk_woformats (cooin, cooout, olngunits, olatunits, olngformat,
	olatformat, olngunitstr, olatunitstr, oolngformat, oolatformat, maxch)

pointer	cooin			#I the input coordinate structure
pointer	cooout			#I the output coordinate structure
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
char	olngformat[ARB]		#I the output ra/longitude format string
char	olatformat[ARB]		#I the output dec/latitude format string
char	olngunitstr[ARB]	#O the output output ra/longitude format string
char	olatunitstr[ARB]	#O the output output dec/latitude format string
char	oolngformat[ARB]	#O the output output ra/longitude format string
char	oolatformat[ARB]	#O the output output dec/latitude format string
int	maxch			#I the maximum length of the format strings

int	tolngunits, tolatunits
int	sk_stati()

begin
	# Determine the correct units.
        if (olngunits <= 0)
            tolngunits = sk_stati (cooout, S_NLNGUNITS)
        else
            tolngunits = olngunits
        if (olatunits <= 0)
            tolatunits = sk_stati (cooout, S_NLATUNITS)
        else
            tolatunits = olatunits

	# Format the units strings.
	if (sk_stati(cooout, S_PIXTYPE) != PIXTYPE_WORLD) {
	    call strcpy ("pixels", olngunitstr, maxch)
	    call strcpy ("pixels", olatunitstr, maxch)
	} else {
	    switch (tolngunits) {
	    case SKY_HOURS:
	        call strcpy ("hours", olngunitstr, maxch)
	    case SKY_DEGREES:
	        call strcpy ("degrees", olngunitstr, maxch)
	    case SKY_RADIANS:
	        call strcpy ("radians", olngunitstr, maxch)
	    }
	    switch (tolatunits) {
	    case SKY_HOURS:
	        call strcpy ("hours", olatunitstr, maxch)
	    case SKY_DEGREES:
	        call strcpy ("degrees", olatunitstr, maxch)
	    case SKY_RADIANS:
	        call strcpy ("radians", olatunitstr, maxch)
	    }
	}

	# Format the format strings.
	call sk_oformats (cooin, cooout, olngformat, olatformat,
	    tolngunits, tolatunits, oolngformat, oolatformat,
	    SZ_FNAME)
end


# SK_OFORMATS -- Set the output format strings.

procedure sk_oformats (cooin, cooout, olngformat, olatformat, olngunits,
	olatunits, oolngformat, oolatformat, maxch)

pointer	cooin			#I the input coordinate structure
pointer	cooout			#I the output coordinate structure
char	olngformat[ARB]		#I the output ra/longitude format string
char	olatformat[ARB]		#I the output dec/latitude format string
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
char	oolngformat[ARB]	#O the output output ra/longitude format string
char	oolatformat[ARB]	#O the output output dec/latitude format string
int	maxch			#I the maximum length of the format strings

int	sptype
int	sk_stati()

begin
	if (olngformat[1] == EOS) {
	    if (sk_stati(cooin, S_STATUS) == ERR)
		call strcpy ("%10.3f", oolngformat, maxch)
	    else {
		if (sk_stati(cooout, S_STATUS) == ERR)
		    sptype = sk_stati (cooin, S_PIXTYPE)
		else
		    sptype = sk_stati (cooout, S_PIXTYPE)
		switch (sptype) {
		case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL: 
		    call strcpy ("%10.3f", oolngformat, maxch)
		default:
		    switch (olngunits) {
		    case SKY_HOURS:
			call strcpy ("%12.3h", oolngformat, maxch)
		    case SKY_DEGREES:
			call strcpy ("%12.2h", oolngformat, maxch)
		    case SKY_RADIANS:
			call strcpy ("%13.7g", oolngformat, maxch)
		    }
		}
	    }
	} else
	    call strcpy (olngformat, oolngformat, maxch)

	if (olatformat[1] == EOS) {
	    if (sk_stati (cooin, S_STATUS) == ERR)
		call strcpy ("%10.3f", oolatformat, maxch)
	    else {
		if (sk_stati(cooout, S_STATUS) == ERR)
		    sptype = sk_stati (cooin, S_PIXTYPE)
		else
		    sptype = sk_stati (cooout, S_PIXTYPE)
		switch (sptype) {
		case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL: 
		    call strcpy ("%10.3f", oolatformat, maxch)
		default:
		    switch (olatunits) {
		    case SKY_HOURS:
			call strcpy ("%12.3h", oolatformat, maxch)
		    case SKY_DEGREES:
			call strcpy ("%12.2h", oolatformat, maxch)
		    case SKY_RADIANS:
			call strcpy ("%13.7g", oolatformat, maxch)
		    }
		}
	    }
	} else
	    call strcpy (olatformat, oolatformat, maxch)
end


# SK_ICTRAN -- Compile the input mwcs transformation.

pointer procedure sk_ictran (cooin, mwin) 

pointer	cooin			#I the input coordinate descriptor
pointer	mwin			#I the input mwcs descriptor

int	axbits
pointer	ctin
int	sk_stati()
pointer	mw_sctran()
errchk	mw_sctran()

begin
	if (mwin != NULL) {
	    switch (sk_stati(cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
		axbits = 2 ** (sk_stati(cooin, S_XLAX) - 1) +
		    2 ** (sk_stati(cooin, S_YLAX) - 1)
		iferr {
		    if (sk_stati(cooin, S_PIXTYPE) == PIXTYPE_PHYSICAL)
		        ctin = mw_sctran (mwin, "physical", "world", axbits)
		    else
		        ctin = mw_sctran (mwin, "logical", "world", axbits)
		} then
		    call error (0, "Error compiling input mwcs transform")
	    default:
		ctin = NULL
	    }
	} else {
	    ctin = NULL
	}

	return (ctin)
end


# SK_IUNITS -- Set the input celestial coordinate units.

procedure sk_iunits (cooin, mwin, ilngunits, ilatunits, oilngunits, oilatunits)

pointer	cooin			#I the input coordinate descriptor
pointer	mwin			#I the input mwcs descriptor
int	ilngunits		#I the input ra/longitude units
int	ilatunits		#I the input dec/latitude units
int	oilngunits		#O the output input ra/longitude units
int	oilatunits		#O the output input dec/latitude units

int	sk_stati()

begin
	if (mwin != NULL) {
	    switch (sk_stati(cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
		oilngunits = SKY_DEGREES
		oilatunits = SKY_DEGREES
	    default:
	        oilngunits = ilngunits
	        oilatunits = ilatunits
	    }
	} else {
	    oilngunits = ilngunits
	    oilatunits = ilatunits
	}
end


# SK_OCTRAN -- Compile the output mwcs transformation.

pointer procedure sk_octran (cooout, mwout)

pointer	cooout			#I the output coordinate descriptor
pointer	mwout			#I the output mwcs descriptor

int	axbits
pointer	ctout
int	sk_stati()
pointer	mw_sctran()
errchk	mw_sctran()

begin
	if (mwout != NULL) {
	    switch (sk_stati(cooout, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
		axbits = 2 ** (sk_stati (cooout, S_XLAX) - 1) +
		    2 ** (sk_stati (cooout, S_YLAX) - 1)
		iferr {
		    if (sk_stati (cooout, S_PIXTYPE) == PIXTYPE_PHYSICAL)
		        ctout = mw_sctran (mwout, "world", "physical", axbits)
		    else
		        ctout = mw_sctran (mwout, "world", "logical", axbits)
		} then
		    call error (0, "Error compiling output mwcs transform")
	    default:
		ctout = NULL
	    }
	} else {
	    ctout = NULL
	}

	return (ctout)
end


# SK_OUNITS -- Compile the output mwcs transformation and set the output
# celestial coordinate units.

procedure sk_ounits (cooout, mwout, olngunits, olatunits, oolngunits,
	oolatunits)

pointer	cooout			#I the output coordinate descriptor
pointer	mwout			#I the output mwcs descriptor
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
int	oolngunits		#O the output output ra/longitude units
int	oolatunits		#O the output output dec/latitude units

int	sk_stati()

begin
	if (mwout != NULL) {
	    switch (sk_stati(cooout, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_TV, PIXTYPE_PHYSICAL:
		oolngunits = SKY_RADIANS
		oolatunits = SKY_RADIANS
	    default:
	        oolngunits = olngunits
	        oolatunits = olatunits
	    }
	} else {
	    oolngunits = olngunits
	    oolatunits = olatunits
	}
end


# SK_INCC -- Transform the input coordinates to the correct celestial
# coordinates in radians.

procedure sk_incc (cooin, mwin, ctin, ilngunits, ilatunits, ilng, ilat,
	olng, olat)

pointer	cooin			#I the input coordinate descriptor
pointer	mwin			#I the input mwcs descriptor
pointer	ctin			#I the mwcs transformation descriptor
int	ilngunits		#I the input ra/longitude units
int	ilatunits		#I the input dec/latitude units
double	ilng			#I the input ra/longitude coordinates 
double	ilat			#I the input dec/latitude coordinates 
double	olng			#O the output ra/longitude coordinates 
double	olat			#O the output dec/latitude coordinates 

double	tlng, tlat
double	sk_statd()
int	sk_stati()

begin
	# Convert the input image coordinates to world coordinates.
	if (mwin != NULL) {
	    switch (sk_stati (cooin, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_PHYSICAL:
		if (ctin == NULL) {
		    olng = ilng
		    olat = ilat
		} else if (sk_stati (cooin, S_PLNGAX) < sk_stati (cooin,
	    	    S_PLATAX)) {
		    call mw_c2trand (ctin, ilng, ilat, olng, olat)
		} else {
		    call mw_c2trand (ctin, ilng, ilat, olat, olng)
		}
	    case PIXTYPE_TV:
		tlng = (ilng - sk_statd(cooin, S_VXOFF)) /
		    sk_statd (cooin, S_VXSTEP)
		tlat = (ilat - sk_statd (cooin, S_VYOFF)) /
		    sk_statd (cooin, S_VYSTEP)
		if (ctin == NULL) {
		    olng = tlng
		    olat = tlat
		} else if (sk_stati (cooin, S_PLNGAX) < sk_stati (cooin,
		        S_PLATAX)) {
		    call mw_c2trand (ctin, tlng, tlat, olng, olat)
		} else {
		    call mw_c2trand (ctin, tlng, tlat, olat, olng)
		}
	    case PIXTYPE_WORLD:
	        olng = ilng
	        olat = ilat
	    }
	} else {
	    olng = ilng
	    olat = ilat
	}

	# Convert the input values to radians.
	switch (ilngunits) {
	case SKY_HOURS:
	    olng = DEGTORAD(15.0d0 * olng)
	case SKY_DEGREES:
	    olng = DEGTORAD(olng)
	case SKY_RADIANS:
	    ;
	}
	switch (ilatunits) {
	case SKY_HOURS:
	    olat = DEGTORAD(15.0d0 * olat)
	case SKY_DEGREES:
	    olat = DEGTORAD(olat)
	case SKY_RADIANS:
	    ;
	}
end


# SK_OUTCC -- Transform the output celestial coordinates to the correct
# output coordinate system.

procedure sk_outcc (cooout, mwout, ctout, olngunits, olatunits, ilng, ilat,
	olng, olat)

pointer	cooout			#I the output coordinate descriptor
pointer	mwout			#I the output mwcs descriptor
pointer	ctout			#I the output mwcs transformation descriptor
int	olngunits		#I the output ra/longitude units
int	olatunits		#I the output dec/latitude units
double	ilng			#I the output ra/longitude coordinates 
double	ilat			#I the output dec/latitude coordinates 
double	olng			#O the output coordinates 
double	olat			#O the output coordinates 

double	tlng, tlat
double	sk_statd()
int	sk_stati()

begin
	# Convert the output image coordinates to image coordinates.
	#if (mwout == NULL || (sk_stati(cooin, S_PIXTYPE) == PIXTYPE_WORLD &&
	#    sk_stati (cooout, S_PIXTYPE) == PIXTYPE_WORLD)) {
	if (mwout == NULL || ctout == NULL) { 
	    switch (olngunits) {
	    case SKY_HOURS:
		olng = RADTODEG(ilng / 15.0d0)
	    case SKY_DEGREES:
		olng = RADTODEG(ilng)
	    case SKY_RADIANS:
		    ;
	    }
	    switch (olatunits) {
	    case SKY_HOURS:
		olat = RADTODEG(ilat / 15.0d0)
	    case SKY_DEGREES:
		olat = RADTODEG(ilat)
	    case SKY_RADIANS:
		    ;
	    }
	} else  {
	    switch (sk_stati (cooout, S_PIXTYPE)) {
	    case PIXTYPE_LOGICAL, PIXTYPE_PHYSICAL:
		tlng = RADTODEG(ilng)
		tlat = RADTODEG(ilat)
		if (ctout == NULL) {
		    olng = tlat
		    olat = tlng
		} else if (sk_stati(cooout, S_PLNGAX) < sk_stati(cooout,
		    S_PLATAX)) {
		    call mw_c2trand (ctout, tlng, tlat, olng, olat)
		} else {
		    call mw_c2trand (ctout, tlat, tlng, olng, olat)
		}
	    case PIXTYPE_TV:
		tlng = RADTODEG(ilng)
		tlat = RADTODEG(ilat)
		if (ctout == NULL) {
		    olng = tlat
		    olat = tlng
		} else if (sk_stati(cooout, S_PLNGAX) < sk_stati(cooout,
		        S_PLATAX)) {
		    call mw_c2trand (ctout, tlng, tlat, olng, olat)
		} else {
		    call mw_c2trand (ctout, tlat, tlng, olng, olat)
		}
		olng = olng * sk_statd(cooout, S_VXSTEP) +
		    sk_statd(cooout, S_VXOFF)
		olat = olat * sk_statd (cooout, S_VYSTEP) +
		    sk_statd (cooout, S_VYOFF)
	    case PIXTYPE_WORLD:
		if (sk_stati(cooout, S_PLNGAX) > sk_stati(cooout,
		    S_PLATAX)) {
		    olng = ilat
		    olat = ilng
	            switch (olngunits) {
	            case SKY_HOURS:
		    	olat = RADTODEG(olat / 15.0d0)
	            case SKY_DEGREES:
		    	olat = RADTODEG(olat)
	            case SKY_RADIANS:
		    	;
	            }
	            switch (olatunits) {
	            case SKY_HOURS:
		    	olng = RADTODEG(olng / 15.0d0)
	            case SKY_DEGREES:
		    	olng = RADTODEG(olng)
	            case SKY_RADIANS:
		    	;
	            }
		} else {
	            switch (olngunits) {
	            case SKY_HOURS:
		    	olng = RADTODEG(ilng / 15.0d0)
	            case SKY_DEGREES:
		    	olng = RADTODEG(ilng)
	            case SKY_RADIANS:
		    	;
	            }
	            switch (olatunits) {
	            case SKY_HOURS:
		    	olat = RADTODEG(ilat / 15.0d0)
	            case SKY_DEGREES:
		    	olat = RADTODEG(ilat)
	            case SKY_RADIANS:
		   	;
	            }
	    	}
	    }
	}
end
