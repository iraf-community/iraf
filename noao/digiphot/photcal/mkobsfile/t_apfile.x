include <fset.h>
include <ctotok.h>
include <ctype.h>
include "../lib/apfile.h"

# T_APFILE -- Compute apperture corrections using the data extracted
# from a simple text file written by the user and an optional corrections
# file. APFILE expects to see the following nine quantities in the input data
# file(s): image name, xcenter, ycenter, filterid, exposure time, xairmass,
# list of apertures, list of magnitudes and list of magnitude errors, and
# uses a simple list of columns to determine which data is in which column.
# The obsparams file is decoded in the same way.

procedure t_apfile ()

int	photfiles	# pointer to the list of photometry files
pointer	columnstr	# pointer to the list of columns
int	naperts		# number of apertures to extract
int	mode		# the file mode for the log and plot files
int	smallap		# the index of the smallest aperture to use  
int	largeap		# the index of the largest aperture to use  
real	maglim		# the maximum magnitude error
int	mterms		# the maximum number of terms in the cog model to fit
int	interactive	# interactive mode
pointer	graphics	# the default graphics device
int	verify		# verify interactive user input

int	incat, apcat, magfd, logfd, plotfd, obs, csp, cp, nincolumns, npts
pointer	sp, column, incolumns, obscolumns, fname, params
pointer	imtable, imid, id, x, y, nap, rap, mag, merr, sortimid, gd, mgd
bool	clgetb()
int	clpopnu(), clplen(), ph_agrange(), open(), btoi(), clgeti()
int	ctoi(), clgfil(), ph_amkimtable(), fstati()
pointer	stopen(), gopen()
real	clgetr()

begin
	# Setup to flush on a newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working memory.
	call smark (sp)
	call salloc (columnstr, SZ_LINE, TY_CHAR)
	call salloc (column, SZ_FNAME, TY_CHAR)
	call salloc (incolumns, CAT_NFIELDS, TY_INT)
	call salloc (obscolumns, OBS_NFIELDS, TY_INT)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (params, MAX_MTERMS, TY_DOUBLE)

	# Get the input file list.
	photfiles = clpopnu ("photfiles")
	if (clplen (photfiles) <= 0)
	    call error (0, "The input file list is empty")

	# Decode the input columns list. There must be at least CAT_RAPERT
	# range specifiers in the incolumns list. The number 0 can be used as
	# a place holder for missing columns.

	call clgstr ("incolumns", Memc[columnstr], SZ_LINE)
	call amovki (0, Memi[incolumns], CAT_NFIELDS)
	nincolumns = 0
	csp = 1
	while (ph_agrange (Memc[columnstr], csp, Memc[column], SZ_FNAME) !=
	    EOF) {

	    # Decode the individual ranges.
	    if (nincolumns >= CAT_NFIELDS)
		call error (0,
	            "Too many fields specified in the incolumns parameter")
	    cp = 1
	    if (ctoi (Memc[column], cp, Memi[incolumns+nincolumns]) > 0)
	        nincolumns = nincolumns + 1
	    else
		break
	}

	# Check that sufficient fields have been defined.
	if (nincolumns < CAT_MERR)
	    call error (0, "Too few fields defined in the incolumns parameter")

	naperts = clgeti ("naperts")
	smallap = max (1, min (naperts, clgeti ("smallap")))
	largeap = clgeti ("largeap")
	if (IS_INDEFI(largeap) || largeap <= 0)
	    largeap = naperts
	else
	    largeap = max (1, min (naperts, largeap))
	if (largeap <= smallap)
	    call error (0,
	    "The large aperture is less than or equal to the large aperture\n")

	# Open the output catalog file.
	call clgstr ("apercors", Memc[fname], SZ_FNAME)
	apcat = open (Memc[fname], NEW_FILE, TEXT_FILE)

	# Determine the access mode for the plot and log files.
	if (clgetb ("append"))
	    mode = APPEND
	else
	    mode = NEW_FILE

	# Open the best magnitudes file if any.
	call clgstr ("magfile", Memc[fname], SZ_FNAME)
	if (Memc[fname] == EOS)
	    magfd = NULL
	else
	    magfd = open (Memc[fname], NEW_FILE, TEXT_FILE)

	# Open the logfile if any.
	call clgstr ("logfile", Memc[fname], SZ_FNAME)
	if (Memc[fname] == EOS)
	    logfd = NULL
	else
	    logfd = open (Memc[fname], mode, TEXT_FILE)

	# Open the plot file if any.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("plotfile", Memc[fname], SZ_FNAME)
	if (Memc[fname] == EOS)
	    plotfd = NULL
	else
	    plotfd = open (Memc[fname], mode, BINARY_FILE)
	if (plotfd != NULL)
	    mgd = gopen (Memc[graphics], NEW_FILE, plotfd)
	else
	    mgd = NULL

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
	        while (ph_agrange (Memc[columnstr], csp, Memc[column],
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

	# Verify interactive user input.
	maglim = clgetr ("maglim")
	mterms = max (1, min (MAX_MTERMS, clgeti ("nparams")))
	call ph_agetp (Memd[params])
	interactive = btoi (clgetb ("interactive"))
	verify = btoi (clgetb ("verify"))

	# Open a symbol table to hold a list of image charactersitics.

	imtable = stopen ("imtable", 2 * LEN_IMTABLE, LEN_IMTABLE, 10 *
	    LEN_IMTABLE)

	# Read in the data.  Extract each unique image name and accompanying
	# airmass and add it to the symbol table. For each star in each image
	# extract the x and y centers, and the list of aperture radii,
	# magnitudes, and magnitude errors.

	imid = NULL
	id = NULL
	x = NULL
	y = NULL
	nap = NULL
	rap = NULL
	mag = NULL
	merr = NULL
	sortimid = NO

	npts = 0
	while (clgfil (photfiles, Memc[fname], SZ_FNAME) != EOF) {
	    incat = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    npts = ph_amkimtable (imtable, incat, Memi[incolumns], naperts,
	        imid, id, x, y, nap, rap, mag, merr, npts, sortimid, maglim)
	    call close (incat)
	}

	call realloc (imid, npts, TY_INT)
	call realloc (id, npts, TY_INT)
	call realloc (x, npts, TY_REAL)
	call realloc (y, npts, TY_REAL)
	call realloc (nap, npts, TY_INT)
	call realloc (rap, naperts * npts, TY_REAL)
	call realloc (mag, naperts * npts, TY_REAL)
	call realloc (merr, naperts * npts, TY_REAL)

	# For each image referenced in the airmass file extract
	# the image name, and the airmass if defined. Enter the new value of
	# airmass in the symbol table and compute the effective
	# exposure time required to correct the instrumental magnitudes to
	# the new exposure time.

	if (obs != NULL) {
	    if (obs == STDIN)
	        call ph_eaobsimtable (imtable, verify) 
	    else
	        call ph_aobsimtable (imtable, obs, Memi[obscolumns])
	}

	# Sort the data by image id and object id if required.

	if (sortimid == YES)
	    call ph_asort (imtable, Memi[imid], Memi[id], Memr[x], Memr[y],
	        Memi[nap], Memr[rap], Memr[mag], Memr[merr], naperts, npts)

	# Free the image id sorting index arrays.

	if (imid != NULL)
	    call mfree (imid, TY_INT)

	# Compute the curves of growth.

	if (interactive == YES) {
	    gd = gopen (Memc[graphics], NEW_FILE, STDGRAPH)
	    call ph_aigrow (gd, apcat, magfd, logfd, mgd, imtable, Memi[id],
	        Memr[x], Memr[y], Memi[nap], Memr[rap], Memr[mag], Memr[merr],
		naperts, Memd[params], mterms, smallap, largeap)
	    call gclose (gd)
	} else
	    call ph_agrow (apcat, magfd, logfd, mgd, imtable, Memi[id], Memr[x],
	        Memr[y], Memi[nap], Memr[rap], Memr[mag], Memr[merr], naperts,
		Memd[params], mterms, smallap, largeap)
	
	# Free the data arrays.

	if (id != NULL)
	    call mfree (id, TY_INT)
	if (x != NULL)
	    call mfree (x, TY_REAL)
	if (y != NULL)
	    call mfree (y, TY_REAL)
	if (nap != NULL)
	    call mfree (nap, TY_INT)
	if (rap != NULL)
	    call mfree (rap, TY_REAL)
	if (mag != NULL)
	    call mfree (mag, TY_REAL)
	if (merr != NULL)
	    call mfree (merr, TY_REAL)

	# Close the symbol table.
	call stclose (imtable)

	# Close the files and file lists.
	call close (apcat)
	if (magfd != NULL)
	    call close (magfd)
	if (logfd != NULL)
	    call close (logfd)
	if (mgd != NULL)
	    call gclose (mgd)
	if (plotfd != NULL)
	    call close (plotfd)
	if (obs != NULL)
	    call close (obs)
	call clpcls (photfiles)

	call sfree (sp)
end


# PH_ASORT -- Sort the data.

procedure ph_asort (imtable, imid, id, x, y, nap, rap, mag, merr, naperts, npts)

pointer	imtable			# pointer to the symbol table
int	imid[ARB]		# array of image ids
int	id[ARB]			# array of object ids
real	x[ARB]			# array of x coordinates
real	y[ARB]			# array of y coordinates
int	nap[ARB]		# array of aperture numbers
real	rap[naperts,ARB]	# the aperture radii list
real	mag[naperts,ARB]	# array of magnitudes
real	merr[naperts,ARB]	# array of magnitude errors
int	naperts			# the number of apertures
int	npts			# the number of points

int	nimages, i, nptr
pointer	sp, sym, symbol
int	stnsymbols()
pointer	sthead(), stnext()

begin
	nimages = stnsymbols (imtable, 0)
	if (nimages <= 0)
	    return

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
	# where the actual contents of the arrays are rearcolumnd.
	# After the image/object id sort recompute the offsets specifying the
	# location of the image data in the symbol table.  The first
	# pass through the symbol table to picks up the information for the
	# images that have only one entry. The second pass picks up
	# information for images that have more than one entry.

	call ph_5r3isort (imid, id, nap, x, y, rap, mag, merr, naperts, npts)
	nptr = npts + 1
	do i = 1, nimages {
	    symbol = Memi[sym+i-1]
	    if (IMT_NENTRIES(symbol) <= 0)
	        next
	    nptr = nptr - IMT_NENTRIES(symbol)
	    IMT_OFFSET(symbol) = nptr
	}

	call sfree (sp)
end


# PH_ANXTIMAGE -- Find the first line in the input catalog containing the next
# image name, given the current image name. Return the new image nam and line.

int procedure ph_anxtimage (fd, columns, image, line, lbufsize)

int	fd		# file descriptor of the input text file
int	columns[ARB]	# the list of input columns
char	image[ARB]	# the name of the current image
pointer	line		# pointer to the output line
int	lbufsize	# current maximum line buffer size

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


# PH_AGETIMAGE -- Read the next line in the input catalog and return the image
# name and the line.

int procedure ph_agetimage (fd, columns, image, line, lbufsize)

int	fd		# file descriptor of the input text file
int	columns[ARB]	# the list of input columns
char	image[ARB]	# the name of the current image
pointer	line		# pointer to the output line
int	lbufsize	# size of the output line

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


# PH_AIMDATA -- Decode the airmass from a a line of the input catalog.

procedure ph_aimdata (line, columns, filterid, sz_filterid, itime, airmass,
	otime)

char	line[ARB]	# input line to be scanned
int	columns[ARB]	# the list of input columns
char	filterid[ARB]	# the filter id
int	sz_filterid	# the maximum size of the filter id
real	itime		# the integration time
real	airmass		# the airmass of the observation
real	otime		# the time of observation

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

	airmass = INDEFR

	call sscan (line)
	do i = 1, maxcol {
	    if (i == fcol) {
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
	    } else if (i ==  ocol) {
		call gargr (otime)
		if (nscan() != ocol)
		    otime = INDEFR
	    } else {
		call gargwrd (Memc[str], SZ_LINE)
	    }
	}

	call sfree (sp)
end


# PH_AOBSDATA -- Decode the filterid, exposure time, and airmass from a
# line of the airmass file.

procedure ph_aobsdata (line, columns, filterid, sz_filterid, itime, airmass,
	otime)

char	line[ARB]	# input line to be scanned
int	columns[ARB]	# the list of input columns
char	filterid[ARB]	# the filter id
int	sz_filterid	# maximum size if filter id string
real	itime		# the exposure time
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

	call sscan (line)
	do i = 1, maxcol {
	    if (i == fcol) {
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


# PH_ASTARDATA -- Decode the image name, x and y coordinates, magnitude
# and magnitude error from the input catalog.

int procedure ph_astardata (line, columns, x, y, nap, rap, mag, merr, naperts,
	magerr)

char	line[ARB]	# input line to be scanned
int	columns[ARB]	# the list of input columns
real	x, y		# the output x and y coordinates
int	nap		# the number of apertures
real	rap[ARB]	# the output aperture radii
real	mag[ARB]	# the output magnitude array
real	merr[ARB]	# the output magnitude error array
int	naperts		# the number of apertures
real	magerr		# the maximum magnitude error

int	i, xcol, ycol, rcol1, rcol2, mcol1, mcol2, ecol1, ecol2, maxcol, stat
pointer	sp, str
int	nscan()
real	rval

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	xcol= columns[CAT_XCENTER]
	ycol= columns[CAT_YCENTER]
	rcol1 = columns[CAT_RAPERT]
	rcol2 = columns[CAT_RAPERT] + naperts - 1
	mcol1 = columns[CAT_MAG]
	mcol2 = columns[CAT_MAG] + naperts - 1
	ecol1 = columns[CAT_MERR]
	ecol2 = columns[CAT_MERR] + naperts - 1
	maxcol = max (xcol, ycol, rcol1, rcol2, mcol1, mcol2, ecol1, ecol2)

	x = INDEFR
	y = INDEFR
	nap = 0
	call amovkr (INDEFR, rap, naperts)
	call amovkr (INDEFR, mag, naperts)
	call amovkr (INDEFR, merr, naperts)
	stat = OK

	call sscan (line)
	do i = 1, maxcol {

	    if (i == xcol) {
		call gargr (rval)
		if (nscan() != xcol)
		    stat = ERR
		else
		    x = rval
	    } else if (i == ycol) {
		call gargr (rval)
		if (nscan() != ycol)
		    stat = ERR
		else
		    y = rval
	    } else if (i >= rcol1 && i <= rcol2) {
		call gargr (rval)
		if (nscan() != i)
		    stat = ERR
		else
		    rap[i-rcol1+1] = rval
	    } else if (i >= mcol1 && i <= mcol2) {
		call gargr (rval)
		if (nscan() != i)
		    stat = ERR
		#else if (IS_INDEFR(rval))
		    #stat = ERR
		else {
		    mag[i-mcol1+1] = rval
		    nap = nap + 1
		}
	    } else if (i >= ecol1 && i <= ecol2) {
		call gargr (rval)
		if (nscan() != i)
		    stat = ERR
		#else if (IS_INDEFR(rval))
		    #stat = ERR
		#else if (rval > magerr)
		    #stat = ERR
		else
		    #merr[i-ecol1+1] = sqrt (1.0e-6 + rval ** 2)
		    merr[i-ecol1+1] = rval
    	    } else {
		call gargwrd (Memc[str], SZ_LINE)
		if (nscan() != i)
		    stat = ERR
	    }

	    if (stat == ERR)
		break
	    nap = nap + 1
	}

	call sfree (sp)

	# Compute the magnitude differences.
	if (stat == ERR) {
	    return (ERR)
	} else if (nap > 1) {
	    nap = 0
	    do i = 1, naperts {
		if (IS_INDEFR(mag[i]) || IS_INDEFR(merr[i]) || merr[i] >
		    magerr)
		    break
		nap = nap + 1
	    }
	    #if (nap <= 1)
	    if (nap <= 0)
		return (ERR)
	    if (nap > 1) {
	        do i = nap, 2, -1 {
		    mag[i] = mag[i] - mag[i-1]
		    merr[i] = sqrt (1.0e-6 + merr[i] ** 2)
	        }
	    }
	    return (OK)
	} else
	    return (ERR)
end


# PH_AGRANGE -- Get the next column from a list of columns.

int procedure ph_agrange (list, ip, label, maxch)

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
