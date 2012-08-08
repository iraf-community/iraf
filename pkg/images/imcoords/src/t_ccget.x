# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include <evvexpr.h>
include <math.h>
include <ctotok.h>
include <lexnum.h>
include <ctype.h>
include <pkg/skywcs.h>

# Define the input data structure

define	DC_DLENGTH		10
define	DC_NCOLUMNS		Memi[$1]    # the number of columns in record
define	DC_LNGCOLUMN		Memi[$1+1]  # the ra / longitude column index
define	DC_LATCOLUMN		Memi[$1+2]  # the dec / latitude column index 
define	DC_COLNAMES		Memi[$1+3]  # the column names pointer
define	DC_RECORD		Memi[$1+4]  # the record pointer
define	DC_COFFSETS		Memi[$1+5]  # the column offsets

define	MAX_NCOLUMNS		100         # the maximum number of columns
define	SZ_COLNAME		19          # the column name
define	TABSIZE			8           # the spacing of the tab stops

# Define the output structure

define  EC_ELENGTH		10

define	EC_NEXPR		Memi[$1]    # the number of expressions
define	EC_ELIST		Memi[$1+1]  # the expression list pointer
define	EC_ERANGES		Memi[$1+2]  # the expression column ranges
define	EC_EFORMATS		Memi[$1+3]  # the expression formats
define	EC_ELNGFORMAT		Memi[$1+4]  # the expression formats
define	EC_ELATFORMAT		Memi[$1+5]  # the expression formats

define	MAX_NEXPR		20
define	MAX_NERANGES		100
define	SZ_EXPR			SZ_LINE
define	SZ_EFORMATS		 9

# T_CCGET -- Given a field center, field width, and field epoch extract objects
# within the rectangular field from a catalog.

procedure t_ccget ()

double	dlngcenter, dlatcenter, dlngwidth, dlatwidth, tlngcenter, tlatcenter
double	dlng1, dlng2, dlat1, dlat2
int	ip, inlist, ninfiles, outlist, noutfiles, fclngunits, fclatunits
int	fldstat, catstat, outstat, catlngunits, catlatunits, olngunits
int	olatunits, in, out
pointer	sp, lngcenter, latcenter, fcsystem, catsystem, outsystem, olngformat
pointer	olatformat, lngcolumn, latcolumn, colnames, exprs, formats
pointer	infile, outfile, str
pointer	fldcoo, catcoo, outcoo, mw, dc, ec
bool	verbose
double	clgetd()
pointer	cc_dinit(), cc_einit()
int	clpopnu(), clplen(), ctod(), strncmp(), clgwrd(), sk_decwcs()
int	sk_stati(), clgfil(), open()
bool	clgetb(), streq()
errchk	clgwrd()

begin
	# Open the list of input catalogs. These catalogs must have the
	# same format.
	inlist = clpopnu ("input")
	ninfiles = clplen (inlist)
	if (ninfiles <= 0) {
	    call eprintf ("Error: The input catalog list is empty\n")
	    call clpcls (inlist)
	    return
	}

	# Open the list of output catalogs. The number of output catalogs
	# must be 1 or equal to the number of input catalogs.
	outlist = clpopnu ("output")
	noutfiles = clplen (outlist)
	if (noutfiles <= 0) {
	    call eprintf ("Error: The output catalog list is empty\n")
	    call clpcls (inlist)
	    call clpcls (outlist)
	    return
	} else if (noutfiles > 1 && noutfiles != ninfiles) {
	    call eprintf (
	    "Error: The number of input and output catalogs are not the same\n")
	    call clpcls (inlist)
	    call clpcls (outlist)
	    return
	}

	# Get some working space.
	call smark (sp)
	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)
	call salloc (lngcenter, SZ_FNAME, TY_CHAR)
	call salloc (latcenter, SZ_FNAME, TY_CHAR)
	call salloc (fcsystem, SZ_FNAME, TY_CHAR)
	call salloc (catsystem, SZ_FNAME, TY_CHAR)
	call salloc (lngcolumn, SZ_FNAME, TY_CHAR)
	call salloc (latcolumn, SZ_FNAME, TY_CHAR)
	call salloc (colnames, SZ_LINE, TY_CHAR)
	call salloc (outsystem, SZ_FNAME, TY_CHAR)
	call salloc (olngformat, SZ_FNAME, TY_CHAR)
	call salloc (olatformat, SZ_FNAME, TY_CHAR)
	call salloc (exprs, SZ_LINE, TY_CHAR)
	call salloc (formats, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the field center coordinates and make some preliminary checks.
	call clgstr ("lngcenter", Memc[lngcenter], SZ_FNAME)
	call clgstr ("latcenter", Memc[latcenter], SZ_FNAME)
	ip = 1
	if (ctod (Memc[lngcenter], ip, dlngcenter) <= 0)
	    dlngcenter = INDEFD
	else if (dlngcenter < 0.0 || dlngcenter > 360.0)
	    dlngcenter = INDEFD
	ip = 1
	if (ctod (Memc[latcenter], ip, dlatcenter) <= 0)
	    dlatcenter = INDEFD
	else if (dlatcenter < -90.0 || dlatcenter > 90.0)
	    dlatcenter = INDEFD
	if (IS_INDEFD(dlngcenter) || IS_INDEFD(dlatcenter)) {
	    call eprintf ( "Error: Undefined field center\n")
	    call clpcls (inlist)
	    call clpcls (outlist)
	    call sfree (sp)
	    return
	}

	dlngwidth = clgetd ("lngwidth")
	if (dlngwidth < 0.0 || dlngwidth > 360.0)
	    dlngwidth = INDEFD
	dlatwidth = clgetd ("latwidth")
	if (dlatwidth < 0.0 || dlatwidth > 180.0)
	    dlatwidth = INDEFD
	if (IS_INDEFD(dlngwidth) || IS_INDEFD(dlatwidth)) {
	    call eprintf ( "Error: Undefined field width\n")
	    call clpcls (inlist)
	    call clpcls (outlist)
	    call sfree (sp)
	    return
	}

	# Get the field coordinate system and convert INDEF to EOS
	# to avoid passing the wcs decoding routine a large number.
	call clgstr ("fcsystem", Memc[fcsystem], SZ_FNAME)
	if (strncmp (Memc[fcsystem], "INDEF", 5) == 0)
	    Memc[fcsystem] = EOS

	# Get the field center coordinate units.
	iferr (fclngunits = clgwrd ("fclngunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    fclngunits = 0
	iferr (fclatunits = clgwrd ("fclatunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    fclatunits = 0

	# Get the coordinates file format.
	call clgstr ("lngcolumn", Memc[lngcolumn], SZ_FNAME)
	call clgstr ("latcolumn", Memc[latcolumn], SZ_FNAME)

	# Get the catalog coordinate system and convert INDEF to EOS
	# to avoid passing the wcs decoding routine a large number.
	call clgstr ("catsystem", Memc[catsystem], SZ_FNAME)
	if (strncmp (Memc[catsystem], "INDEF", 5) == 0)
	    Memc[catsystem] = EOS

	# Get the input catalog coordinate units.
	iferr (catlngunits = clgwrd ("catlngunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    catlngunits = 0
	iferr (catlatunits = clgwrd ("catlatunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    catlatunits = 0

	# Get the output catalog coordinates system.
	call clgstr ("outsystem", Memc[outsystem], SZ_FNAME)
	if (strncmp (Memc[outsystem], "INDEF", 5) == 0)
	    Memc[outsystem] = EOS

	# Get the output catalog coordinate units.
	iferr (olngunits = clgwrd ("olngunits", Memc[str], SZ_FNAME,
	    SKY_LNG_UNITLIST))
	    olngunits = 0
	iferr (olatunits = clgwrd ("olatunits", Memc[str], SZ_FNAME,
	    SKY_LAT_UNITLIST))
	    olatunits = 0
	call clgstr ("olngformat", Memc[olngformat], SZ_LINE)
	call clgstr ("olatformat", Memc[olatformat], SZ_LINE)

	# Get the output catalog format.
	call clgstr ("colaliases", Memc[colnames], SZ_LINE)
	call clgstr ("exprs", Memc[exprs], SZ_LINE)
	call clgstr ("formats", Memc[formats], SZ_LINE)

	verbose = clgetb ("verbose")

	# Open the reference coordinate system.
	if (streq (Memc[catsystem], Memc[fcsystem]) &&
	    (fclngunits == catlngunits) &&
	    (fclatunits == catlatunits)) {
	    fldcoo = NULL
	} else {
	    fldstat = sk_decwcs (Memc[fcsystem], mw, fldcoo, NULL)
	    if (fldstat == ERR || mw != NULL) {
	        if (mw != NULL)
	            call mw_close (mw)
	        fldcoo = NULL
	    }
	}

	# Open the catalog coordinate system.
	catstat = sk_decwcs (Memc[catsystem], mw, catcoo, NULL) 
	if (catstat == ERR || mw != NULL) {
	    call eprintf ("Error: Cannot decode the input coordinate system\n")
	    if (mw != NULL)
		call mw_close (mw)
	    if (fldcoo != NULL)
	        call sk_close (fldcoo)
	    call clpcls (inlist)
	    call clpcls (outlist)
	    call sfree (sp)
	    return
	}

	# Determine the units of the input coordinate system.
	if (catlngunits <= 0)
	    catlngunits = sk_stati (catcoo, S_NLNGUNITS)
	if (catlatunits <= 0)
	    catlatunits = sk_stati (catcoo, S_NLATUNITS)
	call sk_seti (catcoo, S_NLNGUNITS, catlngunits)
	call sk_seti (catcoo, S_NLATUNITS, catlatunits)
	if (fldcoo == NULL) {
	    if (fclngunits <= 0)
	        fclngunits = sk_stati (catcoo, S_NLNGUNITS)
	    if (fclatunits <= 0)
	        fclatunits = sk_stati (catcoo, S_NLATUNITS)
	} else {
	    if (fclngunits <= 0)
	        fclngunits = sk_stati (fldcoo, S_NLNGUNITS)
	    if (fclatunits <= 0)
	        fclatunits = sk_stati (fldcoo, S_NLATUNITS)
	    call sk_seti (fldcoo, S_NLNGUNITS, fclngunits)
	    call sk_seti (fldcoo, S_NLATUNITS, fclatunits)
	}

	# Open the output catalog coordinate system.
	if (streq (Memc[outsystem], Memc[catsystem]) &&
	    (olngunits == catlngunits) &&
	    (olatunits == catlatunits)) {
	    outcoo = NULL
	} else {
	    outstat = sk_decwcs (Memc[outsystem], mw, outcoo, NULL) 
	    if (outstat == ERR || mw != NULL) {
	        call eprintf (
		    "Warning: Cannot decode the output coordinate system\n")
	        if (mw != NULL)
		    call mw_close (mw)
	        outcoo = NULL
	    }
	}

	# Set the output catalog units.
	if (outcoo == NULL) {
	    if (olngunits <= 0)
	        olngunits = sk_stati (catcoo, S_NLNGUNITS)
	    if (olatunits <= 0)
	        olatunits = sk_stati (catcoo, S_NLATUNITS)
	} else {
	    if (olngunits <= 0)
	        olngunits = sk_stati (outcoo, S_NLNGUNITS)
	    if (olatunits <= 0)
	        olatunits = sk_stati (outcoo, S_NLATUNITS)
	    call sk_seti (outcoo, S_NLNGUNITS, olngunits)
	    call sk_seti (outcoo, S_NLATUNITS, olatunits)
	}

	# Get default output coordinate formats.
	if (outcoo != NULL) {
	    if (Memc[olngformat] == EOS || Memc[olngformat] == ' ') {
	        switch (sk_stati(outcoo, S_NLNGUNITS)) {
		case SKY_HOURS:
		    call strcpy ("  %010.1h", Memc[olngformat], SZ_EFORMATS)
		case SKY_DEGREES:
		    call strcpy ("  %9.0h", Memc[olngformat], SZ_EFORMATS)
		case SKY_RADIANS:
		    call strcpy ("  %9.7g", Memc[olngformat], SZ_EFORMATS)
	        }
	    }
	    if (Memc[olatformat] == EOS || Memc[olngformat] == ' ') {
	        switch (sk_stati(outcoo, S_NLATUNITS)) {
		case SKY_HOURS:
		    call strcpy ("  %010.1h", Memc[olatformat], SZ_EFORMATS)
		case SKY_DEGREES:
		    call strcpy ("  %9.0h", Memc[olatformat], SZ_EFORMATS)
		case SKY_RADIANS:
		    call strcpy ("  %9.7g", Memc[olatformat], SZ_EFORMATS)
	        }
	    }
	}

	# Convert the field center coordinates to the catalog
	# coordinate system.
	if (fldcoo == NULL) {
	    tlngcenter = dlngcenter
	    tlatcenter = dlatcenter
	} else {
	    call sk_ultran (fldcoo, catcoo, dlngcenter, dlatcenter,
	        tlngcenter, tlatcenter, 1) 
	}

	# Determine the corners of the field in degrees. At present
	# the maximum longitude width is actually 180 not 360 degrees
	# and the maximum latitude width is 180 degrees.
	call cc_limits (catcoo, tlngcenter, tlatcenter, dlngwidth,
	    dlatwidth, dlng1, dlng2, dlat1, dlat2)

	# Flush standard output on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize the data structure.
	dc = cc_dinit (Memc[colnames], Memc[lngcolumn], Memc[latcolumn])

	# Initialize the expressions structure.
	ec = cc_einit (Memc[exprs], Memc[formats], Memc[olngformat],
	    Memc[olatformat])

	# Decode the expressions using info in the data structure.
	call cc_edecode (dc, ec)

	# Loop over the catalog files.
	while (clgfil (inlist, Memc[infile], SZ_FNAME) != EOF) {

	    # Open text file of coordinates.
	    in = open (Memc[infile], READ_ONLY, TEXT_FILE)

	    # Open the output file.
	    if (clgfil (outlist, Memc[outfile], SZ_FNAME) != EOF)
	        out = open (Memc[outfile], NEW_FILE, TEXT_FILE)

	    # Print the input and output file information.
	    if (verbose && out != STDOUT) {
		call printf ("\nCatalog File: %s  Output File: %s\n")
		    call pargstr (Memc[infile])
		    call pargstr (Memc[outfile])
	    }
	    if (out != NULL) {
		call fprintf (out, "\n# Catalog File: %s  Output File: %s\n")
		    call pargstr (Memc[infile])
		    call pargstr (Memc[outfile])
	    }

	    # Print information about the field center coordinate system.
	    if (fldcoo == NULL) {
	        if (verbose && out != STDOUT)
		    call sk_iiprint ("Field System", Memc[catsystem], NULL,
		        catcoo)
		if (out != NULL)
		    call sk_iiwrite (out, "Field System", Memc[catsystem],
		        NULL, catcoo)
	    } else {
		if (verbose && out != STDOUT)
		    call sk_iiprint ("Field System", Memc[fcsystem], NULL,
		        fldcoo)
		if (out != NULL)
		    call sk_iiwrite (out, "Field System", Memc[fcsystem], NULL,
		        fldcoo)
	    }

	    # Print information about the input coordinate system.
	    if (verbose && out != STDOUT)
		call sk_iiprint (
		    "Catalog System", Memc[catsystem], NULL, catcoo)
	    if (out != NULL)
		call sk_iiwrite (out, "Catalog System", Memc[catsystem], NULL,
		    catcoo)

	    # Print information about the output coordinate system.
	    if (outcoo == NULL) {
	        if (verbose && out != STDOUT)
		    call sk_iiprint ("Output System", Memc[catsystem], NULL,
		        catcoo)
		if (out != NULL)
		    call sk_iiwrite (out, "Output System", Memc[catsystem],
		        NULL, catcoo)
	    } else {
		if (verbose && out != STDOUT)
		    call sk_iiprint ("Output System", Memc[outsystem], NULL,
		        outcoo)
		if (out != NULL)
		    call sk_iiwrite (out, "Output System", Memc[outsystem],
		        NULL, outcoo)
	    }
		    
	    # Print the corners field parameters.
	    if (verbose && out != STDOUT) {
		if (sk_stati (catcoo, S_NLNGUNITS) == SKY_HOURS)
		    call printf (
		        "#\n# Field Center: %10h %9h  Width: %0.4f %0.4f\n")
		else
		    call printf (
		        "#\n# Field Center: %11h %9h  Width: %0.4f %0.4f\n")
		    call pargd (tlngcenter)
		    call pargd (tlatcenter)
		    call pargd (dlngwidth)
		    call pargd (dlatwidth)
		if (sk_stati (catcoo, S_NLNGUNITS) == SKY_HOURS)
		    call printf ("# Field Limits: %9H %9H  %9h %9h\n#\n")
		else
		    call printf ("# Field Limits: %9h %9h  %9h %9h\n#\n")
		    call pargd (dlng1)
		    call pargd (dlng2)
		    call pargd (dlat1)
		    call pargd (dlat2)
	    }

	    if (out != NULL) {
		if (sk_stati (catcoo, S_NLNGUNITS) == SKY_HOURS)
		    call fprintf (out,
		        "#\n# Field Center: %10h %9h  Width: %0.4f %0.4f\n")
		else
		    call fprintf (out,
		        "#\n# Field Center: %11h %9h  Width: %0.4f %0.4f\n")
		    call pargd (tlngcenter)
		    call pargd (tlatcenter)
		    call pargd (dlngwidth)
		    call pargd (dlatwidth)
		if (sk_stati (catcoo, S_NLNGUNITS) == SKY_HOURS)
		    call fprintf (out, "# Field Limits: %9H %9H  %9h %9h\n#\n")
		else
		    call fprintf (out, "# Field Limits: %9h %9h  %9h %9h\n#\n")
		    call pargd (dlng1)
		    call pargd (dlng2)
		    call pargd (dlat1)
		    call pargd (dlat2)
	    }

	    # Read in the data line by line, selecting the records of
	    # interest.
	    call cc_select (in, out, dc, ec, catcoo, outcoo, tlngcenter,
	        tlatcenter, dlngwidth, dlatwidth, dlng1, dlng2, dlat1,
		dlat2, verbose)

	    call close (in)
	    if (noutfiles == ninfiles)
		call close (out)
	}

	call cc_dfree (dc)
	call cc_efree (ec)

	if (noutfiles != ninfiles)
	    call close (out)
	if (fldcoo != NULL)
	    call sk_close (fldcoo)
	call sk_close (catcoo)
	if (outcoo != NULL)
	    call sk_close (outcoo)
	call clpcls (inlist)
	call clpcls (outlist)

	call sfree (sp)
end


# CC_LIMITS - Given the field center and field width compute the ra /
# longitude and dec / latitude limits of the region of interest.

procedure cc_limits (catcoo, dlngcenter, dlatcenter, dlngwidth, dlatwidth,
        dlng1, dlng2, dlat1, dlat2)

pointer	catcoo			#I the pointer to the catalog wcs
double	dlngcenter		#I the field center ra / longtitude
double	dlatcenter		#I the field center dec / latitude
double	dlngwidth		#I the field ra / longitude width (degrees)
double	dlatwidth		#I the field dec / latitude width (degrees)
double 	dlng1			#O the lower field ra / longitude limit
double 	dlng2			#O the upper field ra / longitude limit
double 	dlat1			#O the lower field dec / latitude limit
double 	dlat2			#O the upper field dec / longitude limit

double	tlngcenter, tlatcenter, cosdec, dhlngwidth
int	sk_stati()

begin
	# Convert the field center coordinates to degrees.
        switch (sk_stati(catcoo, S_NLNGUNITS)) {
        case SKY_HOURS:
            tlngcenter = 15.0d0 * dlngcenter
        case SKY_DEGREES:
            tlngcenter = dlngcenter
        case SKY_RADIANS:
            tlngcenter = RADTODEG(dlngcenter)
        default:
            tlngcenter = dlngcenter
        }
        switch (sk_stati (catcoo, S_NLATUNITS)) {
        case SKY_HOURS:
            tlatcenter = 15.0d0 * dlatcenter
        case SKY_DEGREES:
            tlatcenter = dlatcenter
        case SKY_RADIANS:
            tlatcenter = RADTODEG(dlatcenter)
        default:
            tlatcenter = dlatcenter
        }

	# Find the field corners.
	dlat1 = tlatcenter - 0.5d0 * dlatwidth
	if (dlat1 <= -90.0d0) {
	    dlat1 = -90.0d0
	    dlat2 = min  (tlatcenter + 0.5d0 * dlatwidth, 90.0d0)
	    dlng1 = 0.0d0
	    dlng2 = 360.0d0
	    return
	}

	dlat2 = tlatcenter + 0.5d0 * dlatwidth
	if (dlat2 >= 90.0d0) {
	    dlat2 = 90.0d0
	    dlat1 = max (tlatcenter - 0.5d0 * dlatwidth, -90.0d0)
	    dlng1 = 0.0d0
	    dlng2 = 360.0d0
	    return
	}

	if (tlatcenter > 0.0d0)
	    cosdec = cos (DEGTORAD(dlat2))
	else
	    cosdec = cos (DEGTORAD(dlat1))
	dhlngwidth = 0.5d0 * dlngwidth / cosdec
	if (dhlngwidth >= 180.0d0) {
	    dlng1 = 0.0d0
	    dlng2 = 360.0d0
	} else {
	    dlng1 = tlngcenter - dhlngwidth
	    if (dlng1 < 0.0d0)
	        dlng1 = dlng1 + 360.0d0
	    dlng2 = tlngcenter + dhlngwidth 
	    if (dlng2 > 360.0d0)
	        dlng2 = dlng2 - 360.0d0
	}
end


# CC_SELECT -- Select and print the records matching the field position
# and size criteria.

procedure cc_select (in, out, dc, ec, catcoo, outcoo, lngcenter, latcenter,
	lngwidth, latwidth, dlng1, dlng2, dlat1, dlat2, verbose)

int	in			#I the input file file descriptor
int	out			#I the output file descriptor
pointer	dc			#I the file data structure
pointer	ec			#I the expression structure
pointer	catcoo			#I the input catalog coordinate structure
pointer	outcoo			#I the output catalog coordinate structure
double	lngcenter, latcenter	#I the field center coordinates 
double	lngwidth, latwidth	#I the field widths in degrees
double	dlng1, dlng2		#I the ra / longitude limits in degrees
double	dlat1, dlat2		#I the dec / latitude limits in degrees
bool	verbose			#I verbose mode

double	dlngcenter, dlatcenter, tlng, tlat, dlng, dlat, dist
double  tmplng, tlngcenter
int	ip, op, i, j, nline, lngoffset, latoffset, offset1, offset2, nsig
pointer	sp, inbuf, outbuf, newval, eptr, rptr, fptr, pexpr
pointer	evvexpr(), locpr()
int	getline(), li_get_numd(), sk_stati(), gstrcpy(), strlen()
bool	streq()
extern	cc_getop()

begin
	call smark (sp)
	call salloc (inbuf, SZ_LINE, TY_CHAR)
	call salloc (outbuf, SZ_LINE, TY_CHAR)
	call salloc (newval, SZ_LINE, TY_CHAR)

	# Convert the field center coordinates to degrees.
        switch (sk_stati(catcoo, S_NLNGUNITS)) {
        case SKY_HOURS:
            dlngcenter = 15.0d0 * lngcenter
        case SKY_RADIANS:
            dlngcenter = RADTODEG(lngcenter)
        default:
            dlngcenter = lngcenter
        }
        switch (sk_stati (catcoo, S_NLATUNITS)) {
        case SKY_HOURS:
            dlatcenter = 15.0d0 * latcenter
        case SKY_RADIANS:
            dlatcenter = RADTODEG(latcenter)
        default:
            dlatcenter = latcenter
        }

	for (nline = 1; getline (in, Memc[inbuf]) != EOF; nline = nline + 1) {

	    # Skip over leading white space.
            for (ip = inbuf; IS_WHITE(Memc[ip]); ip = ip + 1)
                ;

            # Skip comment and blank lines.
            if (Memc[ip] == '#')
                next
            else if (Memc[ip] == '\n' || Memc[ip] == EOS)
                next

            # Expand tabs into blanks, determine field offsets.
            call strdetab (Memc[inbuf], Memc[DC_RECORD(dc)], SZ_LINE, TABSIZE)
            call li_find_fields (Memc[DC_RECORD(dc)], Memi[DC_COFFSETS(dc)],
	        MAX_NCOLUMNS, DC_NCOLUMNS(dc))

	    # Decode the longitude coordinate.
	    if (DC_LNGCOLUMN(dc) > DC_NCOLUMNS(dc))
		next
	    lngoffset = Memi[DC_COFFSETS(dc)+DC_LNGCOLUMN(dc)-1]
	    if (li_get_numd (Memc[DC_RECORD(dc)+lngoffset-1], tlng, nsig) == 0)
		next

	    # Decode the latitude coordinate.
	    if (DC_LATCOLUMN(dc) > DC_NCOLUMNS(dc))
		next
	    latoffset = Memi[DC_COFFSETS(dc)+DC_LATCOLUMN(dc)-1]
	    if (li_get_numd (Memc[DC_RECORD(dc)+latoffset-1], tlat, nsig) == 0)
		next

	    # Convert the catalog coordinates to degrees.
            switch (sk_stati(catcoo, S_NLNGUNITS)) {
            case SKY_HOURS:
                dlng = 15.0d0 * tlng
            case SKY_RADIANS:
                dlng = RADTODEG(tlng)
            default:
		dlng = tlng
            }
            switch (sk_stati (catcoo, S_NLATUNITS)) {
            case SKY_HOURS:
                dlat = 15.0d0 * tlat
            case SKY_RADIANS:
                dlat = RADTODEG(tlat)
            default:
		dlat = tlat
            }

	    # Test the converted ra /dec or longitude / latitude value
	    # versus the user defined ra / longitude and dec / latitude
	    # limits.
	    if (dlat < dlat1 || dlat > dlat2)
		next
	    if (dlng1 < dlng2) {
		if (dlng >= dlng1 && dlng <= dlng2)
		    ;
		else
		    next
	    } else {
		if (dlng > dlng2 && dlng < dlng1)
		    next
		else
		    ;
	    }

	    # Check the longitude coordinate distance to remove pathologies
	    # in longitude or latitude strips involving the pole. This is
	    # an extra test of my own.
	    if (dlng1 < dlng2) {
	        dist = abs (dlng - dlngcenter)
	    } else {
		if (dlng > dlng1)
		    tmplng = dlng - 360.0d0 
		else
		    tmplng = dlng
		if (dlngcenter > dlng1)
		    tlngcenter = dlngcenter - 360.0d0
		else
		    tlngcenter = dlngcenter
		dist = abs (tmplng - tlngcenter)
	    }
	    if (abs (2.0d0*dist*cos(DEGTORAD(dlat))) > lngwidth) 
	        next

	    # If all the columns are selected and no column expressions have
	    # been defined dump the input records to the output.

	    if (outcoo == NULL && (streq (Memc[EC_ELIST(ec)], "*") ||
	        streq (Memc[EC_ELIST(ec)], "c[*]"))) {
	        if (verbose && out != STDOUT)
		    call putline (STDOUT, Memc[DC_RECORD(dc)])
	        if (out != NULL)
		    call putline (out, Memc[DC_RECORD(dc)])
		next
	    }

	    # Otherwise loop through the user specified output fields
	    # and expressions.

	    # Initialize the expression list pointers.
	    rptr = EC_ERANGES(ec)
	    eptr = EC_ELIST(ec)
	    fptr = EC_EFORMATS(ec)

	    # Initiliaze the output buffer.
	    op = outbuf
	    Memc[op] = EOS

	    do i = 1, EC_NEXPR(ec) {

		# The next user output field is an expression.
		if (IS_INDEFI(Memi[rptr])) {

		    pexpr = evvexpr (Memc[eptr], locpr (cc_getop), dc, 0, dc, 0)
		    switch (O_TYPE(pexpr)) {
		    case TY_BOOL:
			if (Memc[fptr] == '%')
			    call sprintf (Memc[newval], SZ_LINE, Memc[fptr])
			else
			    call sprintf (Memc[newval], SZ_LINE, "%5b")
			    call pargi (O_VALI(pexpr))
		    case TY_CHAR:
			if (Memc[fptr] == '%')
			    call sprintf (Memc[newval], SZ_LINE, Memc[fptr])
			else
			    call sprintf (Memc[newval], SZ_LINE, "  %s")
			   call pargstr (O_VALC(pexpr))
		    case TY_INT:
			if (Memc[fptr] == '%')
			    call sprintf (Memc[newval], SZ_LINE, Memc[fptr])
			else
			    call sprintf (Memc[newval], SZ_LINE, "  %10d")
			    call pargi (O_VALI(pexpr))
		    case TY_REAL:
			if (Memc[fptr] == '%')
			    call sprintf (Memc[newval], SZ_LINE, Memc[fptr])
			else
			    call sprintf (Memc[newval], SZ_LINE, "  %10g")
			    call pargr (O_VALR(pexpr))
		    case TY_DOUBLE:
			if (Memc[fptr] == '%')
			    call sprintf (Memc[newval], SZ_LINE, Memc[fptr])
			else
			    call sprintf (Memc[newval], SZ_LINE, "  %10g")
			    call pargd (O_VALD(pexpr))
		    }
		    op = op + gstrcpy (Memc[newval], Memc[op],
		        min (SZ_LINE - op + outbuf, strlen (Memc[newval])))

		# The next user fields are columns.
		} else if (Memi[rptr] >= 1 && Memi[rptr+1] <= MAX_NCOLUMNS) {

		    # Transform the coordinates if necessary.
		    if (outcoo != NULL)
	    		call sk_ultran (catcoo, outcoo, tlng, tlat, tlng,
			    tlat, 1) 

		    pexpr = NULL
		    do j = max (1, Memi[rptr]), min (Memi[rptr+1],
		        DC_NCOLUMNS(dc)), Memi[rptr+2] {
	    		offset1 = Memi[DC_COFFSETS(dc)+j-1]
	    		offset2 = Memi[DC_COFFSETS(dc)+j]
			if (outcoo != NULL && offset1 == lngoffset) {
			    call sprintf (Memc[newval], SZ_LINE,
			        Memc[EC_ELNGFORMAT(ec)])
				call pargd (tlng)
		            op = op + gstrcpy (Memc[newval], Memc[op],
		                min (SZ_LINE - op + outbuf,
				strlen (Memc[newval])))
			} else if (outcoo != NULL && offset1 == latoffset) {
			    call sprintf (Memc[newval], SZ_LINE,
			        Memc[EC_ELATFORMAT(ec)])
				call pargd (tlat)
		            op = op + gstrcpy (Memc[newval], Memc[op],
		                min (SZ_LINE - op + outbuf,
				strlen (Memc[newval])))
			} else
			    op = op + gstrcpy (Memc[DC_RECORD(dc)+offset1-1],
			        Memc[op], min (SZ_LINE - op + outbuf,
			        offset2 - offset1))
		    }
		}

		# Update the expression list pointers.
		eptr = eptr + SZ_EXPR + 1
		rptr = rptr + 3
		fptr = fptr + SZ_EFORMATS + 1
		if (pexpr != NULL)
		    call mfree (pexpr, TY_STRUCT)
	    }

	    # Attach a newline and EOS to the newly formatted line and output
	    # it.
	    if (Memc[outbuf] != EOS) {
		Memc[op] = '\n'
		Memc[op+1] = EOS
	        if (verbose && out != STDOUT)
		    call putline (STDOUT, Memc[outbuf])
	        if (out != NULL)
		    call putline (out, Memc[outbuf])
	    }

	}

	call sfree (sp)
end


# CC_DINIT -- Initialize the ccget data structure.

pointer procedure cc_dinit (cnames, lngname, latname)

char	cnames[ARB]		#I optional list of columm names
char	lngname[ARB]		#I the ra / longitude column name or number
char	latname[ARB]		#I the dec / latitude column name or number

int	i, ip, op
pointer	dc, cptr
int	cc_cnames(), ctotok(), ctoi()
bool	streq()

begin
	call calloc (dc, DC_DLENGTH, TY_STRUCT)

	# Define the column names.
	call calloc (DC_COLNAMES(dc), MAX_NCOLUMNS * (SZ_COLNAME + 1), TY_CHAR)
	Memc[DC_COLNAMES(dc)] = EOS

	ip = 1
	cptr = DC_COLNAMES(dc)
	do i = 1, MAX_NCOLUMNS {
	    op = 1
	    if (cc_cnames (cnames, ip, Memc[cptr], SZ_COLNAME) == EOF) {
	        call sprintf (Memc[cptr], SZ_COLNAME, "c%d")
		    call pargi (i)
	    } else if (ctotok (Memc[cptr], op, Memc[cptr], SZ_COLNAME) !=
	        TOK_IDENTIFIER) {
	        call sprintf (Memc[cptr], SZ_COLNAME, "c%d")
		    call pargi (i)
	    }
	    call strlwr (Memc[cptr])
	    cptr = cptr + SZ_COLNAME + 1
	}

	# Find the longitude and latitude columns.
	ip = 1
	DC_LNGCOLUMN(dc) = 0
	if (ctoi (lngname, ip, DC_LNGCOLUMN(dc)) <= 0) {
	    cptr = DC_COLNAMES(dc)
	    do i = 1, MAX_NCOLUMNS {
		if (streq (lngname, Memc[cptr])) {
		    DC_LNGCOLUMN(dc) = i
		    break
		}
	        cptr = cptr + SZ_COLNAME + 1
	    }
	}
	if (DC_LNGCOLUMN(dc) <= 0)
	    DC_LNGCOLUMN(dc) = 2

	ip = 1
	DC_LATCOLUMN(dc) = 0
	if (ctoi (latname, ip, DC_LATCOLUMN(dc)) <= 0) {
	    cptr = DC_COLNAMES(dc)
	    do i = 1, MAX_NCOLUMNS {
		if (streq (latname, Memc[cptr])) {
		    DC_LATCOLUMN(dc) = i
		    break
		}
	        cptr = cptr + SZ_COLNAME + 1
	    }
	}
	if (DC_LATCOLUMN(dc) <= 0)
	    DC_LATCOLUMN(dc) = DC_LNGCOLUMN(dc) + 1

	call calloc (DC_RECORD(dc), SZ_LINE, TY_CHAR)
	Memc[DC_RECORD(dc)) = EOS 

	call calloc (DC_COFFSETS(dc), MAX_NCOLUMNS + 1, TY_INT)

	return (dc)
end


# CC_DFREE -- Free the ccget data structure.

procedure cc_dfree (dc)

pointer	dc			#U pointer to the data structure

begin
	call mfree (DC_COLNAMES(dc), TY_CHAR)
	call mfree (DC_RECORD(dc), TY_CHAR)
	call mfree (DC_COFFSETS(dc), TY_INT)
	call mfree (dc, TY_STRUCT)
end


# CC_CNAMES -- Decode the list of column names into individual column names.

int procedure cc_cnames (colnames, ip, name, maxch)

char    colnames[ARB]           #I list of column names
int     ip                      #I pointer into the list of names
char    name[ARB]               #O the output column name
int     maxch                   #I maximum length of a column name

int     op, token
int     ctotok(), strlen()

begin
        # Decode the column labels.
        op = 1
        while (colnames[ip] != EOS) {

            token = ctotok (colnames, ip, name[op], maxch)
            if (name[op] == EOS)
                next

            #if ((token == TOK_UNKNOWN) || (token == TOK_CHARCON))
                #break

            if ((token == TOK_PUNCTUATION) && (name[op] == ',')) {
                if (op == 1)
                    next
                else
                    break
            }

	    if (token != TOK_IDENTIFIER) {
		op = 1
		next
	    }

            op = op + strlen (name[op])
	    if (colnames[ip] == ' ') {
                if (op == 1)
                    next
                else
                    break
	    }
        }

        name[op] = EOS
        if ((colnames[ip] == EOS) && (op == 1))
            return (EOF)
        else
            return (op - 1)
end


# CC_EINIT -- Initialize the ccget expression structure.

pointer procedure cc_einit (exprs, formats, lngformat, latformat)

char	exprs[ARB]		#I the input expression list
char	formats[ARB]		#I the input formats list
char	lngformat[ARB]		#I the input output ra / longitude format
char	latformat[ARB]		#I the input output dec / latitude format

int	i, ip, nexpr
pointer	ec, cptr, fptr
int	cc_enames()

begin
	call calloc (ec, EC_ELENGTH, TY_STRUCT)

	# Define the column names.
	call malloc (EC_ELIST(ec), MAX_NEXPR * (SZ_EXPR + 1), TY_CHAR)
	Memc[EC_ELIST(ec)] = EOS

	# Create list of expressions.
	ip = 1
	cptr = EC_ELIST(ec)
	nexpr = 0
	do i = 1, MAX_NEXPR {
	    if (cc_enames (exprs, ip, Memc[cptr], SZ_EXPR) == EOF)
		break
	    call strlwr (Memc[cptr])
	    cptr = cptr + SZ_EXPR + 1
	    nexpr = nexpr + 1
	}
	EC_NEXPR(ec) = nexpr


	# Decode the list of expressions into column names, column ranges,
	# and column expressions.
	call calloc (EC_ERANGES(ec), 3 * MAX_NERANGES + 1, TY_INT)

	call calloc (EC_EFORMATS(ec), MAX_NEXPR * (SZ_EFORMATS + 1), TY_CHAR)
	Memc[EC_EFORMATS(ec)] = EOS
	ip  = 1
	fptr = EC_EFORMATS(ec)
	cptr = EC_ELIST(ec)
	do i = 1, EC_NEXPR(ec) {
	    if (cc_enames (formats, ip, Memc[fptr], SZ_EFORMATS) == EOF)
		break
	    fptr = fptr + SZ_EFORMATS + 1
	    cptr = cptr + SZ_EXPR + 1
	}

	call calloc (EC_ELNGFORMAT(ec), SZ_EFORMATS, TY_CHAR)
	call strcpy (lngformat, Memc[EC_ELNGFORMAT(ec)], SZ_EFORMATS)
	call calloc (EC_ELATFORMAT(ec), SZ_EFORMATS, TY_CHAR)
	call strcpy (latformat, Memc[EC_ELATFORMAT(ec)], SZ_EFORMATS)

	return (ec)
end


# CC_EFREE -- Free the ccget expression structure.

procedure cc_efree (ec)

pointer	ec			#U pointer to the expression structure

begin
	call mfree (EC_ELIST(ec), TY_CHAR)
	call mfree (EC_ERANGES(ec), TY_INT)
	call mfree (EC_EFORMATS(ec), TY_CHAR)
	call mfree (EC_ELNGFORMAT(ec), TY_CHAR)
	call mfree (EC_ELATFORMAT(ec), TY_CHAR)
	call mfree (ec, TY_STRUCT)
end


# CC_ENAMES -- Decode the list of expressions into individual expressions.

int procedure cc_enames (exprs, ip, name, maxch)

char    exprs[ARB]	        #I list of expressions
int     ip                      #I pointer into the list of names
char    name[ARB]               #O the output column name
int     maxch                   #I maximum length of a column name

int     op, token
int     ctotok(), strlen()

begin
        # Decode the column labels.
        op = 1
        while (exprs[ip] != EOS) {

            token = ctotok (exprs, ip, name[op], maxch)
            if (name[op] == EOS)
                next

            if ((token == TOK_PUNCTUATION) && (name[op] == ',')) {
                if (op == 1)
                    next
                else
                    break
            }


            op = op + strlen (name[op])
        }

        name[op] = EOS
        if ((exprs[ip] == EOS) && (op == 1))
            return (EOF)
        else
            return (op - 1)
end


# CC_EDECODE -- Decode the expression list.

procedure cc_edecode (dc, ec)

pointer	dc			#I the pointer to the data structure
pointer	ec			#I the pointer to the expression structure

int	i, j, ip1, ip2, c1, c2, lindex, rindex, column
pointer	sp, ename, eptr, cptr, rptr
char	lbracket, rbracket
int	ctotok(), strldx(), ctoi()
bool	streq()

begin
	call smark (sp)
	call salloc (ename, SZ_EXPR, TY_CHAR)

	# Initialize.
	lbracket = '['
	rbracket = ']'
	eptr = EC_ELIST(ec)
	rptr = EC_ERANGES(ec)

	do i = 1, EC_NEXPR(ec) {

	    ip1 = 1
	    lindex = strldx (lbracket, Memc[eptr])
	    rindex = strldx (rbracket, Memc[eptr])
	    ip2 = lindex + 1
	    if (Memc[eptr] == 'c' && lindex == 2 && rindex > lindex) {
		if (Memc[eptr+lindex] == '*') {
		    c1 = 1
		    c2 = MAX_NCOLUMNS
		} else {
		    if (ctoi (Memc[eptr], ip2, c1) <= 0)
		        c1 = 0
		    else  if (c1 < 1 || c1 > MAX_NCOLUMNS)
		        c1 = 0
		    if (ctoi (Memc[eptr], ip2, c2) <= 0)
		        c2 = 0
		    else
		        c2 = -c2
		    if (c2 < 1 || c2 > MAX_NCOLUMNS)
		        c2 = 0
		}

		if (c1 > 0 && c2 > c1) {
		    Memi[rptr] = c1
		    Memi[rptr+1] = c2
		    Memi[rptr+2] = 1
		}
	    } else if (ctotok (Memc[eptr], ip1, Memc[ename], SZ_EXPR) ==
	        TOK_IDENTIFIER) {
	        cptr = DC_COLNAMES(dc)
		column = 0
	        do j = 1, MAX_NCOLUMNS {
		    if (streq (Memc[eptr], Memc[cptr])) {
			column = j
		        break
		    }
	            cptr = cptr + SZ_COLNAME + 1
	        }
		if (column > 0) {
		    Memi[rptr] = j
		    Memi[rptr+1] = j
		    Memi[rptr+2] = 1
		} else if (ctotok (Memc[eptr], ip1, Memc[ename], SZ_EXPR) !=
		    EOS) {
		    Memi[rptr] = INDEFI
		    Memi[rptr+1] = INDEFI
		    Memi[rptr+2] = INDEFI
		}
	    } else {
		Memi[rptr] = INDEFI
		Memi[rptr+1] = INDEFI
		Memi[rptr+2] = INDEFI
	    }
	    eptr = eptr + SZ_EXPR + 1
	    rptr = rptr + 3
	}

	call sfree (sp)
end


# CC_GETOP -- Fetch an operand from the data structure.

procedure cc_getop (dc, operand, o)

pointer	dc			#I pointer to the data structure
char	operand[ARB]		#I name of operand to be returned
pointer	o			#I pointer to output operand

int	ip, column, offset, csize, type, nchars
pointer	cptr
bool	streq()
int	lexnum(), ctod(), ctoi()

begin
	# Find the symbol.
	cptr = DC_COLNAMES(dc)
	column = 0
	do ip = 1, MAX_NCOLUMNS {
	    if (streq (operand, Memc[cptr])) {
		column = ip
		break
	    }
	    cptr = cptr + SZ_COLNAME + 1
	}
	if (column <= 0)
	    call xvv_error1 ("Column '%s' not found", operand[1])

	# Find column pointer.
	offset = Memi[DC_COFFSETS(dc)+column-1]
	csize = Memi[DC_COFFSETS(dc)+column] - offset
	cptr = DC_RECORD(dc)+offset-1

	# Determine the type of the symbol.
	ip = 1
	type = lexnum (Memc[cptr], ip, nchars)
	#if (Memc[cptr+nchars+ip-1] != EOS)
	    #type = LEX_NONNUM

	# Decode the symbol.
	switch (type) {
	case LEX_OCTAL, LEX_DECIMAL, LEX_HEX:
	    call xvv_initop (o, 0, TY_INT)
	    ip = 1
	    nchars = ctoi (Memc[cptr], ip, O_VALI(o))
	case LEX_REAL:
	    call xvv_initop (o, 0, TY_DOUBLE)
	    ip = 1
	    nchars = ctod (Memc[cptr], ip, O_VALD(o))
	case LEX_NONNUM:
	    call xvv_initop (o, csize, TY_CHAR)
	    call strcpy (Memc[cptr], O_VALC(o), csize)
	}
end
