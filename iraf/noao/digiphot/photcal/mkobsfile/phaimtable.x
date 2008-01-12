include <fset.h>
include "../lib/apfile.h"

# PH_EAOBSIMTABLE -- Enter the correct values of the filterid, exposure time,
# and airmass into the image symbol table from the standard input.

procedure ph_eaobsimtable (imtable, verify)

pointer	imtable			# pointer to the symbol table
int	verify			# verify the user input

int	nimages, i
pointer	sp, sym, filterid, symbol
real	itime, xairmass, otime
int	stnsymbols(), scan(), nscan(), strncmp()
pointer	sthead(), stnext()

begin
	nimages = stnsymbols (imtable, 0)
	if (nimages <= 0)
	    return

	call smark (sp)
	call salloc (sym, nimages, TY_POINTER)
	call salloc (filterid, SZ_FNAME, TY_CHAR)

	# Reverse the order of the symbols in the symbol table.

	symbol = sthead (imtable)
	do i = nimages, 1, -1 {
	    Memi[sym+i-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	call printf ("\n")
	call printf ("Enter filterid, exposure time and airmass\n")

	# Loop over the images.
	do i = 1, nimages {

	    symbol = Memi[sym+i-1]

	    # Issue the prompt for each image.
	    call printf (
	    "    Image %s (f t X T, <CR>=4 X INDEF, <EOF>=quit entry): " )
		    call pargstr (IMT_IMNAME(symbol))
	    call flush (STDOUT)

	    # Scan the standard input.
	    if (scan() == EOF) {
		call printf ("\n")
		break
	    }

	    # Read in the airmass.
	    call gargwrd (Memc[filterid], SZ_FNAME)
	    call gargr (itime)
	    call gargr (xairmass)
	    call gargr (otime)
	    if (nscan() < 1) {
		call strcpy ("INDEF", Memc[filterid], SZ_FNAME)
		itime = INDEFR
		xairmass = INDEFR
		otime = INDEFR
	    } else if (nscan() < 2) {
		itime = INDEFR
		xairmass = INDEFR
		otime = INDEFR
	    } else if (nscan() < 3) {
		xairmass = INDEFR
		otime = INDEFR
	    } else if (nscan() < 4) {
		otime = INDEFR
	    }

	    # Update the symbol table.
	    if (strncmp (Memc[filterid], "INDEF", 5) != 0)
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
	    if (! IS_INDEFR(itime))
		IMT_ITIME(symbol) = itime
	    if (! IS_INDEFR(xairmass)) {
	        IMT_XAIRMASS(symbol) = xairmass
	        IMT_NXAIRMASS(symbol) = xairmass - DEF_AIROFFSET
	    } else {
	        IMT_XAIRMASS(symbol) = DEF_AIROFFSET
	        IMT_NXAIRMASS(symbol) = 0.0
		call printf (
		    "    Warning: Setting airmass for image %s to %g\n")
		    call pargstr (IMT_IMNAME(symbol))
		    call pargr (DEF_AIROFFSET)
	    }
	    if (! IS_INDEFR(otime))
		IMT_OTIME(symbol) = otime

	    # Verify the input.
	    if (verify == NO)
		next

	    # Issue the verify prompt.
	    call printf ("        Verify (f t X T, <CR>=%s %g %g %0.1h): ")
		call pargstr (IMT_IFILTER(symbol))
		call pargr (IMT_ITIME(symbol))
		call pargr (IMT_XAIRMASS(symbol))
		call pargr (IMT_OTIME(symbol))
	    call flush (STDOUT)

	    # Scan the standard input.
	    if (scan() == EOF) {
		call printf ("\n")
		next
	    }

	    # Read in the airmass.
	    call gargwrd (Memc[filterid], SZ_FNAME)
	    call gargr (itime)
	    call gargr (xairmass)
	    call pargr (otime)
	    if (nscan() == 4) {
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
		IMT_ITIME(symbol) = itime
	        if (! IS_INDEFR(xairmass)) {
	            IMT_XAIRMASS(symbol) = xairmass
	            IMT_NXAIRMASS(symbol) = xairmass - DEF_AIROFFSET
		} else {
	            IMT_XAIRMASS(symbol) = DEF_AIROFFSET
	            IMT_NXAIRMASS(symbol) = 0.0
		    call printf (
		        "    Warning: Setting airmass for image %s to %g\n")
		        call pargstr (IMT_IMNAME(symbol))
		        call pargr (DEF_AIROFFSET)
		}
		IMT_OTIME(symbol) = otime
	    } else if (nscan() == 3) {
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
		IMT_ITIME(symbol) = itime
	        if (! IS_INDEFR(xairmass)) {
	            IMT_XAIRMASS(symbol) = xairmass
	            IMT_NXAIRMASS(symbol) = xairmass - DEF_AIROFFSET
		} else {
	            IMT_XAIRMASS(symbol) = DEF_AIROFFSET
	            IMT_NXAIRMASS(symbol) = 0.0
		    call printf (
		        "    Warning: Setting airmass for image %s to %g\n")
		        call pargstr (IMT_IMNAME(symbol))
		        call pargr (DEF_AIROFFSET)
		}
	    } else if (nscan() == 2) {
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
		IMT_ITIME(symbol) = itime
	    } else if (nscan() == 1)
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
	}

	call printf ("\n")

	call sfree (sp)
end


# PH_AOBSIMTABLE -- Enter the correct values of the filterid, exposure time,
# and airmass into the image table. 

procedure ph_aobsimtable (imtable, fd, columns)

pointer	imtable			# pointer to the symbol table
int	fd			# file descriptor of the input text file
int	columns[ARB]		# the list of input columns

int	stat, lbufsize
pointer	sp, image, fname, filterid, line, sym
real	itime, airmass, otime
bool	streq()
int	ph_anxtimage()
pointer	stfind()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (filterid, SZ_FNAME, TY_CHAR)

	Memc[image] = EOS
	call fstats (fd, F_FILENAME, Memc[fname], SZ_FNAME)

	line = NULL
	lbufsize = 0

	repeat {

	    # Get the image name.
	    stat = ph_anxtimage (fd, columns, Memc[image], line, lbufsize)
	    if (stat == EOF)
		break

	    # Locate the image name in the symbol table.
	    sym = stfind (imtable, Memc[image])
	    if (sym == NULL) {
		call printf (
      		    "Warning: File: %s image: %s ")
		    call pargstr (Memc[fname])
		    call pargstr (Memc[image])
		call printf ("is not in the input image list\n")
		next
	    }

	    # Decode the data.
	    call ph_aobsdata (Memc[line], columns, Memc[filterid], SZ_FNAME,
	        itime, airmass, otime)

	    # Enter the data in the symbol table.
	    if (! streq (Memc[filterid], "INDEF"))
		call strcpy (Memc[filterid], IMT_IFILTER(sym), SZ_FNAME)
	    if (! IS_INDEFR(itime))
		IMT_ITIME(sym) = itime
	    if (! IS_INDEFR(airmass)) {
	        IMT_XAIRMASS(sym) = airmass
	        IMT_NXAIRMASS(sym) = airmass - DEF_AIROFFSET
	    } else {
	        IMT_XAIRMASS(sym) = DEF_AIROFFSET
	        IMT_NXAIRMASS(sym) = 0.0
		call printf (
		    "    Warning: Setting airmass for image %s to %g\n")
		    call pargstr (IMT_IMNAME(sym))
		    call pargr (DEF_AIROFFSET)
	    }
	    if (! IS_INDEFR(otime))
		IMT_OTIME(sym) = otime
	}

	if (line != NULL) {
	    call mfree (line, TY_CHAR)
	    line = NULL
	    lbufsize = 0
	}

	call sfree (sp)
end


# PH_AMKIMTABLE -- Create the image table using the input text file(s)
# and the list of column numbers defining the fields required by the
# program.

int procedure ph_amkimtable (imtable, fd, columns, naperts, imid, id, x, y,
	nap, rap, mag, merr, nptr, sortimid, magerr)

pointer	imtable			# pointer to the symbol table
int	fd			# file descriptor of the input text file
int	columns[ARB]		# the list of input columns
int	naperts			# the number of apertures
pointer	imid			# pointer to the image id
pointer	id			# pointer to the image ids
pointer	x			# pointer to the x coordinate array
pointer	y			# pointer to the y coordinate array
pointer	nap			# pointer to the number of apertures array
pointer	rap			# pointer to the aperture radii array
pointer	mag			# pointer to the magnitude array
pointer	merr			# pointer to the magnitude error array
int	nptr			# pointer to the current data point
int	sortimid		# does data need to be sorted on imid or id
real	magerr			# the maximum magnitude error

int	stat, dbufsize, lbufsize
pointer	sp, fname, image, imname, filterid, line, sym
real	itime, airmass, otime

bool	streq()
int	ph_anxtimage(), ph_astardata(), ph_agetimage(), stnsymbols()
pointer	stfind(), stenter()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (filterid, SZ_FNAME, TY_CHAR)

	# Allocate some initial buffer space.
	if (nptr == 0) {
	    dbufsize = DEF_BUFSIZE
	    call malloc (imid, dbufsize, TY_INT)
	    call malloc (id, dbufsize, TY_INT)
	    call malloc (x, dbufsize, TY_REAL)
	    call malloc (y, dbufsize, TY_REAL)
	    call malloc (nap, dbufsize, TY_INT)
	    call malloc (rap, naperts * dbufsize, TY_REAL)
	    call malloc (mag, naperts * dbufsize, TY_REAL)
	    call malloc (merr, naperts * dbufsize, TY_REAL)
	}

	# Initialize.
	Memc[image] = EOS
	call fstats (fd, F_FILENAME, Memc[fname], SZ_FNAME)

	# Find the first image.
	lbufsize = 0
	line = NULL
	stat = ph_anxtimage (fd, columns, Memc[image], line, lbufsize)

	# Read the data.
	while (stat != EOF) {

	    # Check to see if the image name is already in the symbol table
	    # and if not enter it.
	    sym = stfind (imtable, Memc[image])
	    if (sym == NULL) {
		sym = stenter (imtable, Memc[image], LEN_IMT_STRUCT)
		IMT_IMNO(sym) = stnsymbols (imtable, 0)
		IMT_NENTRIES(sym) = 0
		IMT_RO(sym) = INDEFR
		IMT_OFFSET(sym) = 0
		call strcpy ("INDEF", IMT_IFILTER(sym), SZ_FNAME)
		IMT_ITIME(sym) = INDEFR
		IMT_XAIRMASS(sym) = INDEFR
		IMT_NXAIRMASS(sym) = INDEFR
		IMT_OTIME(sym) = INDEFR
		call strcpy (Memc[image], IMT_IMNAME(sym), SZ_FNAME)
	    }

	    # Decode the airmass information.
	    call ph_aimdata (Memc[line], columns, Memc[filterid], SZ_FNAME,
	        itime, airmass, otime)

	    # Enter the new filterid, itime, and airmass only if there are no
	    # previous entries for that image.

	    if (streq (IMT_IFILTER(sym), "INDEF"))
		call strcpy (Memc[filterid], IMT_IFILTER(sym), SZ_FNAME)
	    if (IS_INDEFR(IMT_ITIME(sym))) {
		if (IS_INDEFR(itime))
		    IMT_ITIME(sym) = 1.0
		else
		    IMT_ITIME(sym) = itime
	    }
	    if (IS_INDEFR(IMT_XAIRMASS(sym))) {
		if (! IS_INDEFR(airmass)) {
	            IMT_XAIRMASS(sym) = airmass
	            IMT_NXAIRMASS(sym) = airmass - DEF_AIROFFSET
		} else {
	            IMT_XAIRMASS(sym) = DEF_AIROFFSET
	            IMT_NXAIRMASS(sym) = 0.0
		    call printf (
		        "    Warning: Setting airmass for image %s to %g\n")
		    call pargstr (IMT_IMNAME(sym))
		    call pargr (DEF_AIROFFSET)
		}
	    }
	    if (IS_INDEFR(IMT_OTIME(sym)))
		IMT_OTIME(sym) = otime

	    # Does the data need to be sorted on image or id. This should
	    # usually not be necessary for most DAOPHOT or APPHOT data
	    # but will be necessary if data from the same images is contained
	    # in more than 1 file.

	    if (IMT_NENTRIES(sym) > 0)
		sortimid = YES
	    else
		IMT_OFFSET(sym) = nptr + 1

	    # Get the data.
	    repeat {

		# Decode x, y, magnitude and error.
		if (ph_astardata (Memc[line], columns, Memr[x+nptr],
		    Memr[y+nptr], Memi[nap+nptr], Memr[rap+naperts*nptr],
		    Memr[mag+naperts*nptr], Memr[merr+naperts*nptr],
		    naperts, magerr) == OK) {
		    Memi[imid+nptr] = IMT_IMNO(sym)
		    IMT_NENTRIES(sym) = IMT_NENTRIES(sym) + 1
		    if (IMT_NENTRIES(sym) == 1)
			IMT_RO(sym) = 0.5 * Memr[rap]
		    Memi[id+nptr] = IMT_NENTRIES(sym)
		    nptr = nptr + 1
		}

		# Allocate more buffer space if necessary.
		if (nptr >= dbufsize) {
		    dbufsize = dbufsize + DEF_BUFSIZE
	            call realloc (imid, dbufsize, TY_INT)
	            call realloc (id, dbufsize, TY_INT)
	    	    call realloc (x, dbufsize, TY_REAL)
	            call realloc (y, dbufsize, TY_REAL)
	            call realloc (nap, dbufsize, TY_INT)
		    call realloc (rap, naperts * dbufsize, TY_REAL)
	            call realloc (mag, naperts * dbufsize, TY_REAL)
	            call realloc (merr, naperts * dbufsize, TY_REAL)
		}

		# Decode the next data.
		stat = ph_agetimage (fd, columns, Memc[imname], line, lbufsize)

	    } until ((stat == EOF) || (! streq (Memc[imname], Memc[image])))

	    call strcpy (Memc[imname], Memc[image], SZ_FNAME)

	}

	if (line != NULL) {
	    call mfree (line, TY_CHAR)
	    line = NULL
	    lbufsize = 0
	}

	call sfree (sp)

	return (nptr)
end
