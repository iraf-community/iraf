include <fset.h>
include "../lib/obsfile.h"

# PH_ESETIMTABLE -- Read in the image set name and image names from the
# standard input and initialize the table fields.

int procedure ph_esetimtable (imtable, nfilters, verify)

pointer	imtable			# pointer to the image symbol table
int	nfilters		# the number of filters in the filter set
int	verify			# verify the user input

int	setno, nsymbols, field, stat
pointer	sp, name, image, str, sym
bool	streq()
int	scan(), nscan(), strlen()
pointer	stenter()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Initialize the set and image counters.
	setno = 0
	nsymbols = 0
	field = 1
	call printf ("\n")

	# Loop over the user defined image sets.
	repeat {

	    if (field == 1) {

		# Print the prompt for the image set name.
		call printf (
		    "Enter name of image set %d (name, <EOF>=quit entry): ")
		    call pargi (setno + 1)
		call flush (STDOUT)

		# Read the image set name.
		stat = scan()
		if (stat == EOF) {
		    call printf ("\n")
		    break
		} else {
		    call gargwrd (Memc[name], SZ_FNAME)
		    if (nscan() != 1)
		        next
		}

		# Verify the image set name.
		if (verify == YES) {
		    call printf ("    Verify name (name, <CR>=%s): ")
			call pargstr (Memc[name])
		    call flush (STDOUT)
		    stat = scan()
		    if (stat == EOF)
		        call printf ("\n")
		    else {
		        call gargwrd (Memc[str], SZ_FNAME)
		        if (nscan() == 1)
			    call strcpy (Memc[str], Memc[name], SZ_FNAME)
		    }
		}

		# Prepare for the next image set field.
		field = field + 1
		setno = setno + 1
		next

	    } else if (field <= (nfilters + 1)) {

		# Prompt for the image name.
		call printf (
		    "    Enter image name %d (name, <CR>=INDEF): ")
		    call pargi (field - 1)
		call flush (STDOUT)

		# Get the next image name.
		stat = scan ()
		if (stat == EOF) {
		    call printf ("\n")
		    next
		} else {
		    call gargwrd (Memc[image], SZ_FNAME)
		    if (nscan() != 1)
		        call strcpy ("INDEF", Memc[image], SZ_FNAME)
		}

		# Verify
		if (verify == YES) {
		    call printf (
		    "        Verify name (name,  <CR>=%s): ")
		        call pargstr (Memc[image])
		    call flush (STDOUT)
		    stat = scan ()
		    if (stat == EOF)
		        call printf ("\n")
		    else {
		        call gargwrd (Memc[str], SZ_FNAME)
		        if (nscan() == 1)
			    call strcpy (Memc[str], Memc[image], SZ_FNAME)
		    }
		}

		nsymbols = nsymbols + 1
		field = field + 1

		# Enter the new symbol name.
		if (streq (Memc[image], "INDEF")) {
		    call sprintf (Memc[image+strlen(Memc[image])], SZ_FNAME,
			"%d")
			call pargi (nsymbols)
		}
		sym = stenter (imtable, Memc[image], LEN_IMT_STRUCT)

		IMT_IMSETNO(sym) = setno
		IMT_IMNO(sym) = nsymbols
		IMT_OFFSET(sym) = 0
		IMT_NENTRIES(sym) = 0

		IMT_XSHIFT(sym) = 0.0
		IMT_YSHIFT(sym) = 0.0
		IMT_APERCOR(sym) = 0.0
		IMT_ITIME(sym) = INDEFR
		IMT_OTIME(sym) = INDEFR
		IMT_XAIRMASS(sym) = INDEFR
		call strcpy ("INDEF",  IMT_IFILTER(sym), SZ_FNAME)

		call strcpy (Memc[name], IMT_LABEL(sym), SZ_FNAME)
		call strcpy (Memc[image], IMT_IMNAME(sym), SZ_FNAME)

		next

	    } else {
		field = 1
	    }
	}

	# Print a newline.
	call printf ("\n")

	call sfree (sp)

	return (nsymbols)
end


# PH_SETIMTABLE -- Read in the image names from the image sets file
# and initialize the fields.

int procedure ph_setimtable (imtable, fd, nfilters, verbose)

pointer	imtable			# pointer to the symbol table
int	fd			# file descriptor of the input text file
int	nfilters		# the number of filters in the set
int	verbose			# verbose mode

char	colon
int	field, setno, nsymbols, nimages
pointer	sp, name, image, sym
bool	streq()
int	fscan(), nscan(), strlen(), itoc()
pointer	stenter()

begin
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	setno = 0
	nsymbols = 0

	while (fscan (fd) != EOF) {

	    # Initialize.
	    nimages = 0
	    field = 1
	    colon = ' '

	    # Decode the image set name and the image names for each set.
	    # Skip blank lines and lines beginning with a pound sign.

	    repeat {

		if (field == 1) {
	            call gargwrd (Memc[name], SZ_FNAME)
	            if (Memc[name] == EOS || Memc[name] == '#')
		        break
		    field = field + 1
		    if (Memc[name] == ':') {
			Memc[name] = EOS
			colon = ':'
		    } else
			call gargc (colon)
		    if ((nscan() != field) || (colon != ':')) 
			break
		    field = field  + 1
		    setno = setno + 1
		    if (Memc[name] == EOS) {
			if (itoc (setno, Memc[name], SZ_FNAME) <= 0)
			    Memc[name] = EOS
		    }
		    next
		} else {
		    call gargwrd (Memc[image], SZ_FNAME)
		    if (nscan() != field)
			break
		    field = field + 1
		}

		nimages = nimages + 1
		nsymbols = nsymbols + 1

		if (streq (Memc[image], "INDEF")) {
		    call sprintf (Memc[image+strlen(Memc[image])], SZ_FNAME,
			"%d")
			call pargi (nsymbols)
		}

		# Enter the new symbol name.
		sym = stenter (imtable, Memc[image], LEN_IMT_STRUCT)

		IMT_IMSETNO(sym) = setno
		IMT_IMNO(sym) = nsymbols
		IMT_OFFSET(sym) = 0
		IMT_NENTRIES(sym) = 0

		IMT_XSHIFT(sym) = 0.0
		IMT_YSHIFT(sym) = 0.0
		IMT_APERCOR(sym) = 0.0
		IMT_ITIME(sym) = INDEFR
		IMT_XAIRMASS(sym) = INDEFR
		IMT_OTIME(sym) = INDEFR
		call strcpy ("INDEF",  IMT_IFILTER(sym), SZ_FNAME)

		call strcpy (Memc[name], IMT_LABEL(sym), SZ_FNAME)
		call strcpy (Memc[image], IMT_IMNAME(sym), SZ_FNAME)
	    }

	    # Test for error conditions.
	    if (verbose == NO)
		next
	    if (Memc[name] == EOS || Memc[name] == '#') {
		# blank lines and comment lines are skipped
	    } else if (colon != ':') {
		call eprintf ("Warning: Error decoding image set %s\n")
		    call pargstr (Memc[name])
	    } else if (nimages == 0) {
		call eprintf ("Warning: Image set %d name %s is empty\n")
		    call pargi (setno)
		    call pargstr (Memc[name])
	    } else if (nimages < nfilters) {
		call eprintf ("Warning: Image set %d name  %s is incomplete\n")
		    call pargi (setno)
		    call pargstr (Memc[name])
	    } else if (nimages > nfilters) {
		call eprintf ("Warning: Image set %d name  %s ")
		    call pargi (setno)
		    call pargstr (Memc[name])
		call eprintf ("has more than %d images\n")
		    call pargi (nimages)
	    }
	}

	call sfree (sp)

	return (nsymbols)
end


# PH_ESHIMTABLE -- Enter the correct values of the x-y coordinate shifts
# the image symbol table from the standard input.

procedure ph_eshimtable (imtable, nimages, verify)

pointer	imtable			# pointer to the symbol table
int	nimages			# number of images
int	verify			# verify the user input

char	comma
int	i, osetno, setno
pointer	sp, sym, symbol
real	xshift, yshift
int	scan(), nscan()
pointer	sthead(), stnext()

begin
	if (nimages <= 0)
	    return

	call smark (sp)
	call salloc (sym, nimages, TY_POINTER)

	# Reverse the order of the symbols in the symbol table.

	symbol = sthead (imtable)
	do i = nimages, 1, -1 {
	    Memi[sym+i-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	call printf ("\n")

	# Loop over the images.
	osetno = 0
	do i = 1, nimages {

	    symbol = Memi[sym+i-1]
	    setno = IMT_IMSETNO(symbol)

	    # Issue the prompt for each set.
	    if (setno != osetno) {
	        call printf ("Image set %d (%s):  ")
	            call pargi (setno)
		    call pargstr (IMT_LABEL(symbol))
		call printf ("Enter the shift in x and y\n")
	    }

	    # Issue the prompt for each image.
	    call printf (
	    "    Image %s (xshift  yshift, <CR>=0.0 0.0, <EOF>=quit entry): " )
		call pargstr (IMT_IMNAME(symbol))
	    call flush (STDOUT)

	    # Initialize
	    osetno = setno

	    # Read a line from STDIN.
	    if (scan() == EOF) {
		call printf ("\n")
		break
	    }

	    # Decode the x and y shifts and update the symbol table.
	    call gargr (xshift)
	    call gargr (yshift)
	    if (nscan() < 1) {
		xshift = 0.0
		yshift = 0.0
	    } else if (nscan() < 2) {
		call reset_scan()
	        call gargr (xshift)
		call gargc (comma)
	        call gargr (yshift)
		if (nscan() < 3 || comma != ',')
		    yshift = 0.0
	    }
	    IMT_XSHIFT(symbol) = xshift
	    IMT_YSHIFT(symbol) = yshift

	    # Verify the input.
	    if (verify == NO)
		next

	    # Issue the verify prompt for each image.
	    call printf (
	        "        Verify (xshift yshift, <CR>=%g %g): " )
		call pargr (IMT_XSHIFT(symbol))
		call pargr (IMT_YSHIFT(symbol))
	    call flush (STDOUT)

	    # Read a line from STDIN.
	    if (scan() == EOF) {
		call printf ("\n")
		next
	    }

	    # Decode the x and y shifts.
	    call gargr (xshift)
	    call gargr (yshift)
	    if (nscan() == 2) {
	        IMT_XSHIFT(symbol) = xshift
	        IMT_YSHIFT(symbol) = yshift
	    } else if (nscan() == 1) {
		call reset_scan()
	        call gargr (xshift)
		call gargc (comma)
	        call gargr (yshift)
	        IMT_XSHIFT(symbol) = xshift
		if (nscan() == 3 && comma == ',')
	            IMT_YSHIFT(symbol) = yshift
	    }
	}

	call printf ("\n")

	call sfree (sp)
end


# PH_SHIMTABLE -- Read in the x and y shifts file and enter the correct
# x and y shifts in the symbol table.

procedure ph_shimtable (imtable, fd, verbose)

pointer	imtable			# pointer to the symbol table
int	fd			# file descriptor of the input text file
int	verbose			# verbose mode

pointer	sp, image, fname, sym
int	fscan(), nscan()
pointer	stfind()
real	xoffset, yoffset

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call fstats (fd, F_FILENAME, Memc[fname], SZ_FNAME)

	while (fscan (fd) != EOF) {

	    # Get the image name and the x and y shifts data.
	    call gargwrd (Memc[image], SZ_FNAME)
	    call gargr (xoffset)
	    call gargr (yoffset)
	    if (nscan () != 3)
		next

	    # Locate the image name in the symbol table and enter the shifts.
	    sym = stfind (imtable, Memc[image])
	    if (sym == NULL) {
		if (verbose == YES) {
		    call eprintf (
                	"Warning: File: %s image: %s is ")
		        call pargstr (Memc[fname])
		        call pargstr (Memc[image])
		    call eprintf ("not in the image sets file\n")
		}
		next
	    }

	    IMT_XSHIFT(sym) = xoffset
	    IMT_YSHIFT(sym) = yoffset
	}

	call sfree (sp)
end


# PH_EAPIMTABLE -- Enter the correct values of the aperture correction in
# the image symbol table from the standard input.

procedure ph_eapimtable (imtable, nimages, verify)

pointer	imtable			# pointer to the symbol table
int	nimages			# number of images
int	verify			# verify the user input

int	i, osetno, setno
pointer	sp, sym, symbol
real	apercor
int	scan(), nscan()
pointer	sthead(), stnext()

begin
	if (nimages <= 0)
	    return

	call smark (sp)
	call salloc (sym, nimages, TY_POINTER)

	# Reverse the order of the symbols in the symbol table.

	symbol = sthead (imtable)
	do i = nimages, 1, -1 {
	    Memi[sym+i-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	call printf ("\n")

	# Loop over the images.
	osetno = 0
	do i = 1, nimages {

	    symbol = Memi[sym+i-1]
	    setno = IMT_IMSETNO(symbol)

	    # Issue the prompt for each set.
	    if (setno != osetno) {
	        call printf ("Image set %d (%s):  ")
	            call pargi (setno)
		    call pargstr (IMT_LABEL(symbol))
		call printf ("Enter the aperture correction\n")
	    }

	    # Issue the prompt for each image.
	    call printf (
	     "    Image %s (magnitude, <CR>=0.0, <EOF>=quit entry): " )
		call pargstr (IMT_IMNAME(symbol))
	    call flush (STDOUT)

	    # Initialize
	    osetno = setno

	    # Scan the standard input.
	    if (scan() == EOF) {
		call printf ("\n")
		break
	    }

	    # Decode the aperture correction and update the symbl table.
	    call gargr (apercor)
	    if (nscan() < 1)
		apercor = 0.0
	    IMT_APERCOR(symbol) = apercor

	    # Optionally verify the input.
	    if (verify == NO)
		next

	    # Issue the verify prompt.
	    call printf (
	     "        Verify (magnitude, <CR>=%g): " )
		call pargr (IMT_APERCOR(symbol))
	    call flush (STDOUT)

	    # Scan the standard input.
	    if (scan() == EOF) {
		call printf ("\n")
		next
	    }

	    # Decode the aperture correction.
	    call gargr (apercor)
	    if (nscan() == 1)
	        IMT_APERCOR(symbol) = apercor
	}

	call printf ("\n")

	call sfree (sp)
end


# PH_APIMTABLE -- Read in the aperture corrections file and enter the correct
# aperture correction in the symbol table.

procedure ph_apimtable (imtable, fd, verbose)

pointer	imtable			# pointer to the symbol table
int	fd			# file descriptor of the input text file
int	verbose			# print status, warning and error messages

pointer	sp, image, fname, sym
int	fscan(), nscan()
pointer	stfind()
real	apercor

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call fstats (fd, F_FILENAME, Memc[fname], SZ_FNAME)

	while (fscan (fd) != EOF) {

	    # Get the image name and the x and y shifts data.
	    call gargwrd (Memc[image], SZ_FNAME)
	    call gargr (apercor)
	    if (nscan () != 2)
		next

	    # Locate the image name in the symbol table and enter the shifts.
	    sym = stfind (imtable, Memc[image])
	    if (sym == NULL) {
		if (verbose == YES) {
		    call eprintf (
                	"Warning: File: %s image: %s ")
		        call pargstr (Memc[fname])
		        call pargstr (Memc[image])
		    call eprintf ("is not in the image sets file\n")
		}
		next
	    } 

	    IMT_APERCOR(sym) = apercor
	}

	call sfree (sp)
end


# PH_EOBSIMTABLE -- Enter the correct values of the filterid, exposure time and
# and airmass into the image symbol table from the standard input.

procedure ph_eobsimtable (imtable, nimages, verify)

pointer	imtable			# pointer to the symbol table
int	nimages			# number of images
int	verify			# verify the user input

int	i, osetno, setno
pointer	sp, sym, filterid, symbol
real	itime, xairmass, otime
int	scan(), nscan(), strncmp()
pointer	sthead(), stnext()

begin
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

	# Loop over the images.
	osetno = 0
	do i = 1, nimages {

	    symbol = Memi[sym+i-1]
	    setno = IMT_IMSETNO(symbol)

	    # Issue the prompt for each set.
	    if (setno != osetno) {
	        call printf ("Image set %d (%s): ")
	            call pargi (setno)
		    call pargstr (IMT_LABEL(symbol))
		call printf (
	        "Enter filter id, exposure time, airmass, observation time\n")
	    }

	    # Issue the prompt for each image.
	    call printf (
            "    Image %s (f t X T, <CR>=4 X (INDEF), <EOF>=quit entry): " )
		    call pargstr (IMT_IMNAME(symbol))
	    call flush (STDOUT)

	    # Initialize.
	    osetno = setno

	    # Scan the standard input.
	    if (scan() == EOF) {
		call printf ("\n")
		break
	    }

	    # Read in the filterid,  exposure time and airmass.
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
	    if (strncmp (IMT_IFILTER(symbol), "INDEF", 5) == 0)
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
	    if (! IS_INDEFR(itime))
	        IMT_ITIME(symbol) = itime
	    if (! IS_INDEFR(xairmass))
	        IMT_XAIRMASS(symbol) = xairmass
	    if (! IS_INDEFR(otime))
		IMT_OTIME(symbol) = otime

	    # Verify the input.
	    if (verify == NO)
		next

	    # Issue the verify prompt.
	    call printf (
	        "        Verify (f t X T, <CR>=%s %g %g %0.1h): ")
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

	    # Read in the filterid,  exposure time and airmass.
	    call gargwrd (Memc[filterid], SZ_FNAME)
	    call gargr (itime)
	    call gargr (xairmass)
	    call gargr (otime)
	    if (nscan() == 4) {
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
		IMT_ITIME(symbol) = itime
		IMT_XAIRMASS(symbol) = xairmass
		IMT_OTIME(symbol) = otime
	    } else if (nscan() == 3) {
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
		IMT_ITIME(symbol) = itime
		IMT_XAIRMASS(symbol) = xairmass
	    } else if (nscan() == 2) {
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
		IMT_ITIME(symbol) = itime
	    } else if (nscan() == 1)
		call strcpy (Memc[filterid], IMT_IFILTER(symbol), SZ_FNAME)
	}

	call printf ("\n")

	call sfree (sp)
end


# PH_OBSIMTABLE -- Enter the correct values of the exposure time, airmass and
# and filter id into the image table. The exposure time entered into the
# image table is the effective exposure time required to convert the
# computed magnitudes to the new exposure time.

procedure ph_obsimtable (imtable, fd, columns, verbose)

pointer	imtable			# pointer to the symbol table
int	fd			# file descriptor of the input text file
int	columns[ARB]		# the list of input columns
int	verbose			# print status, warning and error messages

int	stat, lbufsize
pointer	sp, image, fname, filterid, line, sym
real	itime, airmass, otime
bool	streq()
int	ph_nxtimage()
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
	    stat = ph_nxtimage (fd, columns, Memc[image], line, lbufsize)
	    if (stat == EOF)
		break

	    # Locate the image name in the symbol table.
	    sym = stfind (imtable, Memc[image])
	    if (sym == NULL) {
		if (verbose == YES) {
		    call eprintf (
      			"Warning: File: %s image: %s ")
		        call pargstr (Memc[fname])
		        call pargstr (Memc[image])
		    call eprintf ("is not in the image sets file\n")
		}
		next
	    }

	    # Decode the data.
	    call ph_obsdata (Memc[line], columns, Memc[filterid], SZ_FNAME,
	        itime, airmass, otime)

	    # Enter the data in the symbol table.
	    if (! IS_INDEFR(itime))
	        IMT_ITIME(sym) = itime
	    if (! IS_INDEFR(airmass))
	        IMT_XAIRMASS(sym) = airmass
	    if (! IS_INDEFR(otime))
		IMT_OTIME(sym) = otime
	    if (! streq (Memc[filterid], "INDEF"))
	        call strcpy (Memc[filterid], IMT_IFILTER(sym), SZ_FNAME)
	}

	if (line != NULL) {
	    call mfree (line, TY_CHAR)
	    line = NULL
	    lbufsize = 0
	}

	call sfree (sp)
end


# PH_MKIMTABLE -- Create the image table using the input text file(s)
# and the list of column numbers defining the fields required by the
# program.

int procedure ph_mkimtable (imtable, fd, columns, objid, x, y, mag, merr,
        imid, id, nptr, normtime, sortimid, verbose)

pointer	imtable			# pointer to the symbol table
int	fd			# file descriptor of the input text file
int	columns[ARB]		# the list of input columns
pointer	objid			# pointer to the id array
pointer	x			# pointer to the x coordinate array
pointer	y			# pointer to the y coordinate array
pointer	mag			# pointer to the magnitude array
pointer	merr			# pointer to the magnitude error array
pointer	imid			# pointer to the image id
pointer	id			# pointer to the image ids
int	nptr			# pointer to the current data point
int	normtime		# normalize exposure times
int	sortimid		# does data need to be sorted on imid or id
int	verbose			# print status, warning and error messages

int	idcol, stat, dbufsize, lbufsize
pointer	sp, fname, image, imname, filterid, objname, line, sym
real	itime, airmass, otime

bool	streq()
int	ph_nxtimage(), ph_stardata(), ph_getimage()
pointer	stfind()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (filterid, SZ_FNAME, TY_CHAR)
	call salloc (objname, DEF_LENLABEL, TY_CHAR)

	# Is there id information.
	idcol = columns[CAT_ID]

	# Allocate some initial buffer space.
	if (nptr == 0) {
	    dbufsize = DEF_BUFSIZE
	    if (idcol > 0)
		call malloc (objid, dbufsize * (DEF_LENLABEL+ 1), TY_CHAR)
	    call malloc (x, dbufsize, TY_REAL)
	    call malloc (y, dbufsize, TY_REAL)
	    call malloc (mag, dbufsize, TY_REAL)
	    call malloc (merr, dbufsize, TY_REAL)
	    call malloc (imid, dbufsize, TY_INT)
	    call malloc (id, dbufsize, TY_INT)
	}

	# Initialize.
	Memc[image] = EOS
	call fstats (fd, F_FILENAME, Memc[fname], SZ_FNAME)

	# Find the first image.
	lbufsize = 0
	line = NULL
	stat = ph_nxtimage (fd, columns, Memc[image], line, lbufsize)

	# Read the data.
	while (stat != EOF) {

	    # Locate the image name in the symbol table.
	    sym = stfind (imtable, Memc[image])
	    if (sym == NULL) {
		if (Memc[image] != EOS && verbose == YES) {
		    call eprintf (
			"Warning: File: %s Image: %s ")
		        call pargstr (Memc[fname])
		        call pargstr (Memc[image])
		    call eprintf ("is not in the image sets file\n")
		}
		stat = ph_nxtimage (fd, columns, Memc[image], line, lbufsize)
		next
	    }

	    # Decode the filter, exposure time and airmass information.
	    call ph_imdata (Memc[line], columns, Memc[filterid], SZ_FNAME,
	        itime, airmass, otime)

	    # Enter the new values in the symbol table.
	    if (IS_INDEFR(IMT_ITIME(sym))) {
		if (IS_INDEFR(itime))
		    IMT_ITIME(sym) = 1.0
		else if (normtime == NO)
		    IMT_ITIME(sym) = 1.0
		else
		    IMT_ITIME(sym) = itime
	    } else if (IMT_NENTRIES(sym) > 0) { 
		# do nothing
	    } else if (IS_INDEFR(itime)) {
		if (normtime == NO)
		    IMT_ITIME(sym) = 1.0
	    } else {
	        IMT_ITIME(sym) = IMT_ITIME(sym) / itime
	    }
	    if (IS_INDEFR(IMT_XAIRMASS(sym)))
	        IMT_XAIRMASS(sym) = airmass
	    if (IS_INDEFR(IMT_OTIME(sym)))
	        IMT_OTIME(sym) = otime
	    if (streq (IMT_IFILTER(sym), "INDEF"))
	        call strcpy (Memc[filterid], IMT_IFILTER(sym), SZ_FNAME)

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
		if (ph_stardata (Memc[line], columns, Memc[objname],
		    Memr[x+nptr], Memr[y+nptr], Memr[mag+nptr],
		    Memr[merr+nptr]) == OK) {
		    if (idcol > 0)
			call strcpy (Memc[objname],
			    Memc[objid+nptr*(DEF_LENLABEL+1)],
			    DEF_LENLABEL)
		    Memi[imid+nptr] = IMT_IMNO(sym)
		    IMT_NENTRIES(sym) = IMT_NENTRIES(sym) + 1
		    Memi[id+nptr] = IMT_NENTRIES(sym)
		    nptr = nptr + 1
		}

		# Allocate more buffer space if necessary.
		if (nptr >= dbufsize) {
		    dbufsize = dbufsize + DEF_BUFSIZE
		    if (idcol > 0)
			call realloc (objid, dbufsize * (DEF_LENLABEL + 1),
			    TY_CHAR)
	    	    call realloc (x, dbufsize, TY_REAL)
	            call realloc (y, dbufsize, TY_REAL)
	            call realloc (mag, dbufsize, TY_REAL)
	            call realloc (merr, dbufsize, TY_REAL)
	            call realloc (imid, dbufsize, TY_INT)
	            call realloc (id, dbufsize, TY_INT)
		}

		# Decode the next data.
		stat = ph_getimage (fd, columns, Memc[imname], line, lbufsize)

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
