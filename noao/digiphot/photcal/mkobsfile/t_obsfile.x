include <fset.h>
include <ctotok.h>
include <ctype.h>
include "../lib/obsfile.h"

# T_OBSFILE -- Make an observations catalog using the data extracted
# from the APPHOT/DAOPHOT catalogs by TXDUMP/TDUMP or supplied in a simple
# text file by the user, the observing parameters file, the shifts and
# aperture corrections file and the image sets file.  OBSFILE expects to
# see the following 8 fields in the input data file: image name, xcenter,
# ycenter, exposure time, filter id, airmass, magnitude and magnitude error
# and uses a simple string containing a list of column numbers to determine
# which data is in which column. The observing parameters file is decoded
# in the same way.

procedure t_obsfile ()

int	photfiles	# pointer to the list of photometry files
pointer	columnstr	# pointer to the list of input columns
pointer	infilters	# pointer to the input list of filter ids
int	normtime	# normalize to an exposure time of 1 time unit
int	allfilters	# only output objects with data in all filters
real	tolerance	# tolerance in pixels for position matching
int	verify		# verify interactive user input
int	verbose		# print status, warning and errors messages

int	incat, sets, sh, ap, obs, outcat, fmt, wrap
int	csp, cp, nincolumns, nfilters, nimages, npts, sortimid
pointer	sp, outfilters, fname, tfname, column, incolumns, obscolumns, imtable
pointer	objid, x, y, mag, merr, imid, id
real	minmagerr

bool	clgetb()
int	clpopnu(), clgfil(), open(), clplen(), ph_filters(), ph_getlabels()
int	ctoi(), ph_setimtable(), ph_esetimtable(), ph_mkimtable(), btoi()
int	access()
pointer	stopen()
real	clgetr()

begin
	# Allocate working memory.
	call smark (sp)

	call salloc (infilters, SZ_LINE, TY_CHAR)
	call salloc (outfilters, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (tfname, SZ_FNAME, TY_CHAR)

	call salloc (columnstr, SZ_LINE, TY_CHAR)
	call salloc (column, SZ_FNAME, TY_CHAR)
	call salloc (incolumns, CAT_NFIELDS, TY_INT)
	call salloc (obscolumns, OBS_NFIELDS, TY_INT)

	# Get the input file list.
	photfiles = clpopnu ("photfiles")
	if (clplen (photfiles) <= 0)
	    call error (0, "The input file list is empty")

	# Decode the input columns list. There must be at least CAT_NFIELDS
	# column numbers in the incolumns list. The number 0 can be used as
	# a place holder for missing columns.

	call clgstr ("incolumns", Memc[columnstr], SZ_LINE)
	csp = 1
	nincolumns = 0
	while (ph_getlabels (Memc[columnstr], csp, Memc[column], SZ_FNAME) !=
	    EOF) {
	    cp = 1
	    if (ctoi (Memc[column], cp, Memi[incolumns+nincolumns]) <= 0)
		Memi[incolumns+nincolumns] = 0
	    nincolumns = nincolumns + 1
	}
	if (nincolumns < CAT_NFIELDS)
	    call error (0,
	       "There are too few columns defined in the incolumns string")

	# Open the idfilters string.
	call clgstr ("idfilters", Memc[infilters], SZ_LINE)
	nfilters = ph_filters (Memc[infilters], Memc[outfilters], SZ_LINE)

	# Open the image sets file.
	call clgstr ("imsets", Memc[fname], SZ_FNAME)
	sets = open (Memc[fname], READ_ONLY, TEXT_FILE)

	# Open the output catalog file.
	call clgstr ("observations", Memc[fname], SZ_FNAME)
	outcat = open (Memc[fname], NEW_FILE, TEXT_FILE)
	call sprintf (Memc[tfname], SZ_FNAME, "f%s.dat")
	    call pargstr (Memc[fname])
	if (access (Memc[tfname], 0, 0) == YES)
	    call delete (Memc[tfname])
	fmt = open (Memc[tfname], NEW_FILE, TEXT_FILE)
	wrap = btoi (clgetb ("wrap"))

	# Get the minimum magnitude error.
	minmagerr = clgetr ("minmagerr")

	# Normalize the exposure times?
	normtime = btoi (clgetb ("normtime"))

	# Get the position matching parameters.
	tolerance = clgetr ("tolerance")
	allfilters = btoi (clgetb ("allfilters"))

	# Verify interactive user input.
	verify = btoi (clgetb ("verify"))
	verbose = btoi (clgetb ("verbose"))

	# Open the shifts file.
	call clgstr ("shifts", Memc[fname], SZ_FNAME)
	if (Memc[fname] == EOS)
	    sh = NULL
	else
	    sh = open (Memc[fname], READ_ONLY, TEXT_FILE)

	# Open the aperture corrections file.
	call clgstr ("apercors", Memc[fname], SZ_FNAME)
	if (Memc[fname] == EOS)
	    ap = NULL
	else
	    ap = open (Memc[fname], READ_ONLY, TEXT_FILE)

	# Open the observing parameters file and decode the column list. 

	call clgstr ("obsparams", Memc[fname], SZ_FNAME)
	if (Memc[fname] == EOS)
	    obs = NULL
	else {
	    obs = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    call amovki (0, Memi[obscolumns], OBS_NFIELDS)
	    if (obs != STDIN) {
	        call clgstr ("obscolumns", Memc[columnstr], SZ_LINE)
	        csp = 1
	        nincolumns = 1
		Memi[obscolumns] = 1
	        while (ph_getlabels (Memc[columnstr], csp, Memc[column],
	            SZ_FNAME) != EOF && (nincolumns < OBS_NFIELDS)) {
		    cp = 1
		    if (ctoi (Memc[column], cp,
		        Memi[obscolumns+nincolumns]) <= 0)
		        Memi[obscolumns+nincolumns] = 0
		    else if (Memi[obscolumns+nincolumns] == 1)
		        nincolumns = nincolumns - 1
		    nincolumns = nincolumns + 1
	        }
	    }
	}

	# Open a symbol table to hold a list of image charactersitics.

	imtable = stopen ("imtable", 2 * LEN_IMTABLE, LEN_IMTABLE, 10 *
	    LEN_IMTABLE)

	# Read in the image set file and store each unique image name in
	# the symbol table. Initialize the records fields.

	if (sets == NULL)
	    nimages = 0
	else if (sets == STDIN)
	    nimages = ph_esetimtable (imtable, nfilters, verify)
	else 
	    nimages = ph_setimtable (imtable, sets, nfilters, verbose)

	# For each image referenced in the observing parameters file extract
	# the image name, the filter id if defined, the exposure time if
	# defined and the airmass if defined. Enter the new values of filter
	# id and airmass in the symbol table and compute the effective
	# exposure time required to correct the instrumental magnitudes to
	# the new exposure time.

	if (obs != NULL) {
	    if (obs == STDIN)
	        call ph_eobsimtable (imtable, nimages, verify) 
	    else {
		if (sets == STDIN && verbose == YES) {
		    call fstats (obs, F_FILENAME, Memc[fname], SZ_FNAME)
		    call eprintf ("Warning: Filter ids in %s ")
			call pargstr (Memc[fname])
		    call eprintf ("will replace those listed in idfilters\n")
		}
	        call ph_obsimtable (imtable, obs, Memi[obscolumns], verbose)
	    }
	}

	# Read in the x and y shifts for the image. Images for which no
	# shift is defined are assigned an x-y shift of 0.0.

	if (sh != NULL) {
	    if (sh == STDIN)
	        call ph_eshimtable (imtable, nimages, verify)
	    else
	        call ph_shimtable (imtable, sh, verbose)
	}

	# Read in the aperture corrections. Images for which no aperture
	# correction is defined are assigned an aperture correction of 0.0.

	if (ap != NULL) {
	    if (ap == STDIN)
	        call ph_eapimtable (imtable, nimages, verify)
	    else
	        call ph_apimtable (imtable, ap, verbose)
	}

	# Read in the data.  Extract each unique image name and match it
	# with the corresponding image name in the symbol table. Skip data
	# for images which are not in the symbol table. For each unique image
	# name which does match a symbol table image name, extract the exposure
	# time, filter id, and airmass and enter them in the symbol table.
	# Read the  x and y and magnitude and magnitude error data into
	# separate data arrays.

	objid = NULL
	x = NULL
	y = NULL
	mag = NULL
	merr = NULL
	imid = NULL
	id = NULL
	sortimid = NO

	npts = 0
	while (clgfil (photfiles, Memc[fname], SZ_FNAME) != EOF) {
	    incat = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    npts = ph_mkimtable (imtable, incat, Memi[incolumns], objid, x,
	        y, mag, merr, imid, id, npts, normtime, sortimid, verbose)
	    call close (incat)
	}

	if (objid != NULL)
	    call realloc (objid, npts * (DEF_LENLABEL+1), TY_CHAR)
	call realloc (x, npts, TY_REAL)
	call realloc (y, npts, TY_REAL)
	call realloc (mag, npts, TY_REAL)
	call realloc (merr, npts, TY_REAL)
	call realloc (imid, npts, TY_INT)
	call realloc (id, npts, TY_INT)

	# Sort the data by image id and object id if position matching is
	# turned off or by image id and y coordinate if position matching is
	# turned on.

	if (objid == NULL)
	    call ph_sort (imtable, nimages, "", Memr[x], Memr[y], Memr[mag],
	        Memr[merr], Memi[imid], Memi[id], npts, sortimid, tolerance)
	else
	    call ph_sort (imtable, nimages, Memc[objid], Memr[x], Memr[y],
	        Memr[mag], Memr[merr], Memi[imid], Memi[id], npts, sortimid,
		tolerance)
	# Free the image id sorting index arrays.

	if (imid != NULL)
	    call mfree (imid, TY_INT)

	# Apply the exposure time corrections, x-y shifts, and the aperture
	# corrections.

	call ph_correct (imtable, Memr[x], Memr[y], Memr[mag], Memr[merr],
	    minmagerr)

	if (verbose == YES) {
	    call fstats (outcat, F_FILENAME, Memc[fname], SZ_FNAME)
	    call printf ("\nObservations file: %s\n")
	        call pargstr (Memc[fname])
	}

	# Match by order in the catalog if tolerance is less than or equal to
	# zero or by x-y position if tolerance is greater than zero and output
	# the data.

	if (objid == NULL)
	    call ph_match (imtable, nimages, Memc[outfilters], nfilters, outcat,
	        "", Memr[x], Memr[y], Memr[mag], Memr[merr], Memi[id], npts,
		tolerance, allfilters, wrap, verbose)
	else
	    call ph_match (imtable, nimages, Memc[outfilters], nfilters, outcat,
	        Memc[objid], Memr[x], Memr[y], Memr[mag], Memr[merr],
		Memi[id], npts, tolerance, allfilters, wrap, verbose)

	if (verbose == YES)
	    call printf ("\n")

	# Write the format file for the output catalog.

	call ph_format (fmt, Memc[outfilters], nfilters)

	# Free the data arrays.

	if (objid != NULL)
	    call mfree (objid, TY_CHAR)
	if (x != NULL)
	    call mfree (x, TY_REAL)
	if (y != NULL)
	    call mfree (y, TY_REAL)
	if (mag != NULL)
	    call mfree (mag, TY_REAL)
	if (merr != NULL)
	    call mfree (merr, TY_REAL)
	if (id != NULL)
	    call mfree (id, TY_INT)

	# Close the symbol table.
	call stclose (imtable)

	# Close the files and file lists.
	if (sets != NULL)
	    call close (sets)
	if (sh != NULL)
	    call close (sh)
	if (ap != NULL)
	    call close (ap)
	if (obs != NULL)
	    call close (obs)
	call close (outcat)
	call close (fmt)
	call clpcls (photfiles)

	call sfree (sp)
end


# PH_SORT -- Sort the data.

procedure ph_sort (imtable, nimages, objid, x, y, mag, merr, imid, id, npts,
	sortimid, tolerance)

pointer	imtable			# pointer to the symbol table
int	nimages			# the number of images in the symbol table
char	objid[ARB]		# the array of object ids objid[1] = EOS if none
real	x[ARB]			# array of x coordinates
real	y[ARB]			# array of y coordinates
real	mag[ARB]		# array of magnitudes
real	merr[ARB]		# array of magnitude errors
int	imid[ARB]		# array of image ids
int	id[ARB]			# array of object ids
int	npts			# the number of points
int	sortimid		# sort by image and id
real	tolerance		# the tolerance in pixels

int	i, nptr
pointer	sp, sym, symbol
pointer	sthead(), stnext()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (sym, nimages, TY_INT)

	# Read in the symbol list in the last in first out sense.

	symbol = sthead (imtable)
	do i = 1, nimages {
	    Memi[sym+i-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	# Do the image/object id sort if necessary. This is a real sort
	# where the actual contents of the arrays are rearranged.
	# After the image/object id sort recompute the offsets specifying the
	# location of the image data in the symbol table.  The first
	# pass through the symbol table to picks up the information for the
	# images that have only one entry. The second pass picks up
	# information for images that have more than one entry.

	if (sortimid == YES) {
	    if (objid[1] == EOS)
	        call ph_4r2isort (x, y, mag, merr, imid, id, npts)
	    else
	        call ph_1c4r2isort (objid, x, y, mag, merr, imid, id, npts)
	    nptr = npts + 1
	    do i = 1, nimages {
	        symbol = Memi[sym+i-1]
	        if (IMT_NENTRIES(symbol) <= 0)
		    next
		nptr = nptr - IMT_NENTRIES(symbol)
	        IMT_OFFSET(symbol) = nptr
	    }
	}

	# Sort on y if necessary. This is an index sort and does not alter
	# the position of the data.

	if (tolerance > 0) {
	    do i = 1, nimages {
	        symbol = Memi[sym+i-1]
	        if (IMT_NENTRIES(symbol) <= 0)
		    next
		nptr = IMT_OFFSET(symbol)
		call ph_qsort (y[nptr], id[nptr], IMT_NENTRIES(symbol), nptr)
	    }
	}

	call sfree (sp)
end


# PH_CORRECT -- Correct the x, y and mag values using the  x-y shifts, aperture
# corrections, and exposure times.

procedure ph_correct (imtable, x, y, mag, merr, minmagerr)

pointer	imtable		# pointer to the symbol table
real	x[ARB]		# array of x coordinates
real	y[ARB]		# array of y coordinates
real	mag[ARB]	# array of magnitudes
real	merr[ARB]	# array of magnitude errors
real	minmagerr	# the minimum magnitude error

int	i, nptr, ndata
pointer	sp, imname, symbol, osymbol
real	magshift
pointer	sthead(), stnext(), stfind()

begin
	# Allocate working space.

	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	# Loop through the images adding the x, y and magnitude shifts to
	# the data.

	symbol = sthead (imtable)
	while (symbol != NULL) {

	    # Get the correct symbol.
	    ndata = IMT_NENTRIES(symbol)
	    osymbol = stfind (imtable, IMT_IMNAME(symbol))

	    # If data is present make the corrections once for each image.

	    if ((ndata > 0) && (osymbol == symbol)) {
	        nptr = IMT_OFFSET(symbol)

		# Correct the positions.
	        call aaddkr (x[nptr], IMT_XSHIFT(symbol), x[nptr], ndata)
	        call aaddkr (y[nptr], IMT_YSHIFT(symbol), y[nptr], ndata)

		# Correct the magnitudes.
	        magshift = 2.5 * log10 (IMT_ITIME(symbol)) + IMT_APERCOR(symbol)
		do i = nptr, nptr + ndata - 1 {
		    if (IS_INDEFR(mag[i]))
			next
		    mag[i] = mag[i] + magshift
		    if (IS_INDEFR(merr[i]))
			next
		    if (merr[i] < minmagerr)
			merr[i] = minmagerr
		}
	    }

	    # Get the next symbol.
	    symbol = stnext (imtable, symbol)
	}

	call sfree (sp)
end


# PH_MATCH -- Match up the photometry lists by order in the input catalog
# or x-y position.

procedure ph_match (imtable, nimages, filters, nfilters, out, objid, x, y,
	mag, merr, ysort, npts, tolerance, allfilters, wrap, verbose)

pointer	imtable			# pointer to the image name symbol table
int	nimages			# the number of images in the symbol table
char	filters[ARB]		# the filters string
int	nfilters		# the number of filters
int	out			# the output file descriptor
char	objid[ARB]		# the object id array
real	x[ARB]			# the x input array
real	y[ARB]			# the y input array
real	mag[ARB]		# the magnitude array
real	merr[ARB]		# the magnitude error array
int	ysort[ARB]		# the y sort index
int	npts			# the number of points
real	tolerance		# the x-y matching tolerance in pixels
int	allfilters		# match in all filters
int	wrap			# format the output file for easy reading
int	verbose			# print status, warning and error messages

int	i, k, nsets, imptr, filterno, ndata
pointer	sp, imsym, filterid, label, sym, match, osymbol, symbol, index
bool	streq()
int	strdic(), ph_nthlabel()
pointer	sthead(), stnext(), stfind()

begin
	# Write out the output file column headers.
	call fprintf (out, "\n")
	if (wrap == YES) {
	    call fprintf (out,
"# FIELD%17tFILTER%34tOTIME%40tAIRMASS%49tXCENTER%59tYCENTER%71tMAG%77tMERR\n")
	} else {
	    do i = 1, nfilters {
		if (i == 1)
		    call fprintf (out,
"# FIELD%17tFILTER%34tOTIME%40tAIRMASS%49tXCENTER%59tYCENTER%71tMAG%77tMERR ")
		else
		    call fprintf (out,
"%2tFILTER%19tOTIME%25tAIRMASS%34tXCENTER%44tYCENTER%56tMAG%62tMERR ")
	    }
	    call fprintf (out,"\n")
	}
	call fprintf (out, "\n")

	# Allocate and or initialize working space.
	call smark (sp)
	call salloc (imsym, nimages, TY_INT) 
	call salloc (filterid, SZ_FNAME, TY_CHAR)
	call salloc (label, SZ_FNAME, TY_CHAR)
	call salloc (sym, nfilters, TY_INT)
	if (tolerance > 0.0)
	    call malloc (match, npts, TY_INT)
	else
	    match = NULL
	index = NULL


	# Read in the symbol table pointers and rearrange them so that the
	# image name symbols are ordered in the same way as the image set
	# numbers. Compute the total number of image sets at the same time.

	nsets = 0
	symbol = sthead (imtable)
	do i = nimages, 1, -1 {
	    nsets = max (nsets, IMT_IMSETNO(symbol))
	    Memi[imsym+i-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	# Loop over the image sets.
	imptr = 1
	do i = 1, nsets {

	    # Clear the array of symbols and set the label.
	    ndata = 0
	    call amovki (NULL, Memi[sym], nfilters)
	    call strcpy (IMT_LABEL(Memi[imsym+imptr-1]), Memc[label], SZ_FNAME)

	    # Loop over the images in the set.
	    repeat {

		# Quit if the image pointer exceeds the number of images.
		if (imptr > nimages)
		    break

		# Get the next symbol for a given image set.
	        symbol = Memi[imsym+imptr-1]
	        if (IMT_IMSETNO(symbol) != i)
		    break

		# Check for duplicate image names.
		osymbol = stfind (imtable, IMT_IMNAME(symbol))
		if (osymbol != symbol) {
		    IMT_OFFSET(symbol) = IMT_OFFSET(osymbol)
		    IMT_NENTRIES(symbol) = IMT_NENTRIES(osymbol)
		    IMT_XAIRMASS(symbol) = IMT_XAIRMASS(osymbol)
		    IMT_OTIME(symbol) = IMT_OTIME(osymbol)
		    call strcpy (IMT_IFILTER(osymbol), IMT_IFILTER(symbol),
			SZ_FNAME)
		}

		# Search for the filter id, if one is not found then
		# use the first unoccupied space.

		filterno = strdic (IMT_IFILTER(symbol), Memc[filterid],
		    SZ_FNAME, filters)
		if (streq (IMT_IFILTER(symbol), "INDEF")) {
		    do k = 1, nfilters {
			if (Memi[sym+k-1] != NULL)
			    next
			if (ph_nthlabel (filters, k, Memc[filterid],
			    SZ_FNAME) != k)
			    Memc[filterid] = EOS
			filterno = k
			break
		    }
		}

		# Load the data pointers.
		if (filterno <= 0) {

		    if (verbose == YES) {
		        call eprintf (
		            "Warning: Image set: %d  label: %s  image: %s\n")
			    call pargi (i)
			    call pargstr (IMT_LABEL(symbol))
			    call pargstr (IMT_IMNAME(symbol))
		        call eprintf (
		    	    "\tWarning: Filter %s ")
			    call pargstr (IMT_IFILTER(symbol))
			call eprintf ("does not belong to filter set %s\n")
			    call pargstr (filters)
		    }

		} else if (Memi[sym+filterno-1] != NULL) {

		    if (verbose == YES) {
		        call eprintf (
		            "Warning: Image set: %d  label: %s  image: %s\n")
			    call pargi (i)
			    call pargstr (IMT_LABEL(symbol))
			    call pargstr (IMT_IMNAME(symbol))
		        call eprintf ("\tData for filter %s is redundant\n")
			    call pargstr (Memc[filterid])
		    }

		} else {

		    Memi[sym+filterno-1] = symbol
		    ndata = max (ndata, IMT_NENTRIES(symbol))
		}

		imptr = imptr + 1

	    }

	    # Skip matching if there are no points.
	    if (ndata <= 0) {
		if (verbose == YES) {
		    call eprintf ("Image set: %d  Label: %s has no data\n")
		        call pargi (i)
		        call pargstr (Memc[label])
		}
		next
	    }

	    # Do the matching.
	    if (tolerance <= 0.0) {
		if (objid[1] == EOS)
		    call ph_join (out, Memc[label], Memi[sym], filters,
		        nfilters, "", x, y, mag, merr, allfilters, wrap,
			verbose)
		else
		    call ph_join (out, Memc[label], Memi[sym], filters,
		        nfilters, objid, x, y, mag, merr, allfilters, wrap,
			verbose)
	    } else {
		if (index == NULL)
		    call malloc (index, nfilters * ndata, TY_INT)
		else
		    call realloc (index, nfilters * ndata, TY_INT)
		if (objid[1] == EOS)
		    call ph_merge (out, Memc[label], Memi[sym], filters,
		        nfilters, "", x, y, mag, merr, ysort, Memi[match],
			Memi[index], ndata, tolerance, allfilters, wrap,
			verbose)
		else
		    call ph_merge (out, Memc[label], Memi[sym], filters,
		        nfilters, objid, x, y, mag, merr, ysort, Memi[match],
			Memi[index], ndata, tolerance, allfilters, wrap,
			verbose)
	    }
	}

	# Free working space.
	if (match != NULL)
	    call mfree (match, TY_INT)
	if (index != NULL)
	    call mfree (index, TY_INT)
	call sfree (sp)
end


# PH_FORMAT -- Write the format file for the output catalog.

procedure ph_format (fd, filters, nfilters)

int	fd 		# file descripter for the format file
char	filters[ARB]	# list of filter ids
int	nfilters	# number of filters

int	i, col
pointer	sp, filterid
int	ph_nthlabel()

begin
	call smark (sp)
	call salloc (filterid, SZ_FNAME, TY_CHAR)

	call fprintf (fd, "# Declare the observations file variables\n\n")
	call fprintf (fd, "observations\n\n")

	col = FIRST_COLUMN
	do i = 1, nfilters {
	    if (ph_nthlabel (filters, i, Memc[filterid], SZ_FNAME) != i)
		Memc[filterid] = EOS
	    call fprintf (fd,
	        "T%s%15t%d%30t# time of observation in filter %s\n")
		call pargstr (Memc[filterid])
		call pargi (col)
		call pargstr (Memc[filterid])
	    col = col + 1
	    call fprintf (fd, "X%s%15t%d%30t# airmass in filter %s\n")
		call pargstr (Memc[filterid])
		call pargi (col)
		call pargstr (Memc[filterid])
	    col = col + 1
	    call fprintf (fd, "x%s%15t%d%30t# x coordinate in filter %s\n")
		call pargstr (Memc[filterid])
		call pargi (col)
		call pargstr (Memc[filterid])
	    col = col + 1
	    call fprintf (fd, "y%s%15t%d%30t# y coordinate in filter %s\n")
		call pargstr (Memc[filterid])
		call pargi (col)
		call pargstr (Memc[filterid])
	    col = col + 1
	    call fprintf (fd,
	        "m%s%15t%d%30t# instrumental magnitude in filter %s\n")
		call pargstr (Memc[filterid])
		call pargi (col)
		call pargstr (Memc[filterid])
	    col = col + 1
	    call fprintf (fd,
	        "error(m%s)%15t%d%30t# magnitude error in filter %s\n")
		call pargstr (Memc[filterid])
		call pargi (col)
		call pargstr (Memc[filterid])
	    call fprintf (fd, "\n")
	    col = col + DELTA_COLUMN 
	}

	call sfree (sp)
end


# PH_NXTIMAGE -- Find the first line in the input catalog containing the next
# image name, given the current image name. Return the new image name and line.

int procedure ph_nxtimage (fd, columns, image, line, lbufsize)

int	fd		# file descriptor of the input text file
int	columns[ARB]	# the list of input columns
char	image[ARB]	# the name of the current image
pointer	line		# pointer to the output line
int	lbufsize	# the line buffer size

int	i, op, colno
long	stat
pointer	sp, str
bool	streq()
int	getline(), nscan()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (line == NULL || lbufsize <= 0) {
	    lbufsize = lbufsize + SZ_LINE
	    call malloc (line, lbufsize, TY_CHAR)
	}

	colno = columns[CAT_IMAGE]
	Memc[str] = EOS
	repeat {

	    op = 1
	    repeat {
	        stat = getline (fd, Memc[line+op-1])
	        if (stat == EOF)
		    break
	        op = op + stat
		if (op > lbufsize) {
		    lbufsize = lbufsize + SZ_LINE
		    call realloc (line, lbufsize, TY_CHAR)
		}
	    } until (Memc[line+op-2] == '\n')
	    if (stat == EOF)
		break
	    else
		stat = op - 1

	    call sscan (Memc[line])
	    do i = 1, colno
		call gargwrd (Memc[str], SZ_FNAME)
	    if (nscan() != colno)
		next

	} until (! streq (image, Memc[str]))
	call strcpy (Memc[str], image, SZ_FNAME)

	call sfree (sp)

	return (stat)
end


# PH_GETIMAGE -- Read the next line in the input catalog and return the image
# name and the line.

int procedure ph_getimage (fd, columns, image, line, lbufsize)

int	fd		# file descriptor of the input text file
int	columns[ARB]	# the list of input columns
char	image[ARB]	# the name of the current image
pointer	line		# pointer to the output line
int	lbufsize	# the size of the output buffer

int	op, i, colno
int	stat
int	getline(), nscan()

begin
	if (line == NULL || lbufsize <= 0) {
	    lbufsize = lbufsize + SZ_LINE
	    call malloc (line, lbufsize, TY_CHAR)
	}

	colno = columns[CAT_IMAGE]
	repeat {

	    op = 1
	    repeat {
	        stat = getline (fd, Memc[line+op-1])
	        if (stat == EOF)
		    break
		op = op + stat
		if (op > lbufsize) {
		    lbufsize = lbufsize + SZ_LINE
		    call realloc (line, lbufsize, TY_CHAR)
		}
	    } until (Memc[line+op-2] == '\n')
	    if (stat == EOF)
		break
	    else
		stat = op - 1

	    call sscan (Memc[line])
	    do i = 1, colno
	        call gargwrd (image, SZ_FNAME)
	    if (nscan() != colno)
	        next

	    break
	}

	return (stat)
end


# PH_IMDATA -- Decode the filter id, integration time and airmass from
# a line of the input catalog.

procedure ph_imdata (line, columns, filterid, sz_filterid, itime, airmass,
	otime)

char	line[ARB]	# input line to be scanned
int	columns[ARB]	# the list of input columns
char	filterid[ARB]	# the name of the filter
int	sz_filterid	# maximum length of the filter id
real	itime		# the exposure time of the observation
real	airmass		# the airmass of the observation
real	otime		# the time of observations

int	i, fcol, icol, acol, ocol, maxcol
pointer	sp, str
int	nscan()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	fcol = columns[CAT_IFILTER]
	icol = columns[CAT_ITIME]
	acol = columns[CAT_XAIRMASS]
	ocol = columns[CAT_OTIME]
	maxcol = max (fcol, icol, acol, ocol)

	call strcpy ("INDEF", filterid, sz_filterid)
	itime = INDEFR
	airmass = INDEFR
	otime = INDEFR

	call sscan (line)
	do i = 1, maxcol {
	    if ( i == fcol) {
		call gargwrd (filterid, sz_filterid)
		if (nscan() != fcol)
		    call strcpy ("INDEF", filterid, sz_filterid)
	    } else if (i == icol) {
		call gargr (itime)
		if (nscan() != icol)
		    itime = INDEFR
	    } else if (i == acol) {
		call gargr (airmass)
		if (nscan() != acol)
		    airmass = INDEFR
	    } else if (i == ocol) {
		call gargr (otime)
		if (nscan() != ocol)
		    otime = INDEFR
	    } else {
		call gargwrd (Memc[str], SZ_LINE)
	    }
	}

	call sfree (sp)
end


# PH_OBSDATA -- Decode the filter id, integration time and airmass from
# a line of the observing parameters file.

procedure ph_obsdata (line, columns, filterid, sz_filterid, itime, airmass,
	otime)

char	line[ARB]	# input line to be scanned
int	columns[ARB]	# the list of input columns
char	filterid[ARB]	# the name of the filter
int	sz_filterid	# maximum length of the filter id
real	itime		# the exposure time of the observation
real	airmass		# the airmass of the observation
real	otime		# the time of observation

int	i, fcol, icol, acol, ocol, maxcol
pointer	sp, str
int	nscan()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	fcol = columns[OBS_IFILTER]
	icol = columns[OBS_ITIME]
	acol = columns[OBS_XAIRMASS]
	ocol = columns[OBS_OTIME]
	maxcol = max (fcol, icol, acol, ocol)

	call strcpy ("INDEF", filterid, sz_filterid)
	itime = INDEFR
	airmass = INDEFR
	otime = INDEFR

	call sscan (line)
	do i = 1, maxcol {
	    if ( i == fcol) {
		call gargwrd (filterid, sz_filterid)
		if (nscan() != fcol)
		    call strcpy ("INDEF", filterid, sz_filterid)
	    } else if (i == icol) {
		call gargr (itime)
		if (nscan() != icol)
		    itime = INDEFR
	    } else if (i == acol) {
		call gargr (airmass)
		if (nscan() != acol)
		    airmass = INDEFR
	    } else if (i == ocol) {
		call gargr (otime)
		if (nscan() != ocol)
		    otime = INDEFR
	    } else {
		call gargwrd (Memc[str], SZ_LINE)
	    }
	}

	call sfree (sp)
end


# PH_STARDATA -- Decode the image name, x and y coordinates, magnitude
# and magnitude error from the input catalog.

int procedure ph_stardata (line, columns, objid, x, y, mag, merr)

char	line[ARB]	# input line to be scanned
int	columns[ARB]	# the list of input columns
char	objid[ARB]	# the object id
real	x, y		# the output x and y coordinates
real	mag, merr	# the output magnitude and magnitude error

int	i, icol, xcol, ycol, mcol, ecol, maxcol, stat
pointer	sp, str
int	nscan()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	icol = columns[CAT_ID]
	xcol= columns[CAT_XCENTER]
	ycol= columns[CAT_YCENTER]
	mcol = columns[CAT_MAG]
	ecol = columns[CAT_MERR]
	maxcol = max (icol, xcol, ycol, mcol, ecol)

	objid[1] = EOS
	x = INDEFR
	y = INDEFR
	mag = INDEFR
	merr = INDEFR
	stat = OK

	call sscan (line)
	do i = 1, maxcol {

	    if (i == icol) {
		call gargwrd (objid, DEF_LENLABEL)
		if (nscan() != icol)
		    stat = ERR
	    } else if (i == xcol) {
		call gargr (x)
		if (nscan() != xcol)
		    stat = ERR
	    } else if (i == ycol) {
		call gargr (y)
		if (nscan() != ycol)
		    stat = ERR
	    } else if (i == mcol) {
		call gargr (mag)
		if (nscan() != mcol)
		    stat = ERR
	    } else if (i == ecol) {
		call gargr (merr)
		if (nscan() != ecol)
		    stat = ERR
	    } else {
		call gargwrd (Memc[str], SZ_LINE)
		if (nscan() != i)
		    stat = ERR
	    }
	}

	call sfree (sp)

	return (stat)
end


# PH_FILTERS -- Reformat the filter string so that it is a suitable string
# dictionary for the STRDIC routine.

int procedure ph_filters (infilters, outfilters, max_lenfilters)

char	infilters[ARB]		# input list of filters
char	outfilters[ARB]		# output list of filters
int	max_lenfilters		# maximum length of the filter string

int	ip, nfilters
pointer	sp, filter
int	ph_getlabels()

begin
	call smark (sp)
	call salloc (filter, max_lenfilters, TY_CHAR)

	ip = 1
	nfilters = 0
	outfilters[1] = EOS
	while (ph_getlabels (infilters, ip, Memc[filter], max_lenfilters) !=
	    EOF) {
	    call strcat (",", outfilters, max_lenfilters)
	    call strcat (Memc[filter], outfilters, max_lenfilters)
	    nfilters = nfilters + 1
	}

	call sfree (sp)

	return (nfilters)
end


# PH_NTHLABEL -- Get the nth label out of a list of labels.

int procedure ph_nthlabel (list, item, label, maxch)

char	list[ARB]		# input list
int	item			# item to be extracted
char	label[ARB]		# extracted label
int	maxch			# maximum length of the extracted label

int	ip, nitems
int	ph_getlabels()

begin
	nitems = 0

	ip = 1
	while (ph_getlabels (list, ip, label, maxch) != EOF) {
	    nitems = nitems + 1
	    if (nitems == item)
		break
	}

	return (nitems)
end


# PH_GETLABELS -- Get the next label from a list of labels.

int procedure ph_getlabels (list, ip, label, maxch)

char	list[ARB]		# list of labels
int	ip			# pointer in to the list of labels
char	label[ARB]		# the output label
int	maxch			# maximum length of a column name

int	op, token
int	ctotok(), strlen()

begin
	# Decode the column labels.
	op = 1
	while (list[ip] != EOS) {

	    token = ctotok (list, ip, label[op], maxch)
	    if (label[op] == EOS)
		next
	    if ((token == TOK_UNKNOWN) || (token == TOK_CHARCON))
		break
	    if ((token == TOK_PUNCTUATION) && (label[op] == ',')) {
		if (op == 1)
		    next
		else
		    break
	    }

	    op = op + strlen (label[op])
	    if (IS_WHITE(list[ip]))
	        break
	}

	label[op] = EOS
	if ((list[ip] == EOS) && (op == 1))
	    return (EOF)
	else
	    return (op - 1)
end
