include "../lib/obsfile.h"

# T_MKPHOTCORS - Query the user for the image sets, observing parameters, 
# positional shifts and aperture corrections required by the preprocessor
# task MKOBSFILE. Input to OBSQUERY is the names of the output image set,
# observing parameters, shifts and aperture corrections files, and a list
# of filter ids.

procedure t_mkphotcors ()

pointer	infilters	# pointer to the input list of filter ids
pointer	imsets		# name of the output image set file
pointer	obsparams	# pointer to the output obsparams file
pointer	shifts		# pointer to the output shifts file
pointer	apercors	# pointer to the output aperture corrections file
pointer	obscolumnstr	# pointer to format of obsparams
int	verify		# verify interactive user input
int	verbose		# print status, warning and error messages

int	nfilters, csp, cp, ncolumns, nimages
int	insets, outsets, inobs, outobs, insh, outsh, inap, outap
pointer	sp, outfilters, imtable, obscolname, obscolumns
bool	clgetb()
int	btoi(), open(), ph_filters(), ph_esetimtable(), ph_setimtable()
int	ctoi(), access(), ph_getlabels()
pointer	stopen()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (imsets, SZ_FNAME, TY_CHAR)
	call salloc (infilters, SZ_LINE, TY_CHAR)
	call salloc (outfilters, SZ_LINE, TY_CHAR)
	call salloc (obsparams, SZ_FNAME, TY_CHAR)
	call salloc (shifts, SZ_FNAME, TY_CHAR)
	call salloc (apercors, SZ_FNAME, TY_CHAR)
	call salloc (obscolumnstr, SZ_LINE, TY_CHAR)
	call salloc (obscolname, SZ_FNAME, TY_CHAR)
	call salloc (obscolumns, OBS_NFIELDS, TY_INT)

	# Get the parameters.
	call clgstr ("imsets", Memc[imsets], SZ_FNAME)
	call clgstr ("idfilters", Memc[infilters], SZ_LINE)
	call clgstr ("obsparams", Memc[obsparams], SZ_FNAME)
	call clgstr ("shifts", Memc[shifts], SZ_FNAME)
	call clgstr ("apercors", Memc[apercors], SZ_FNAME)
	call clgstr ("obscolumns", Memc[obscolumnstr], SZ_LINE)
	verify = btoi (clgetb ("verify"))
	verbose = btoi (clgetb ("verbose"))

	# Open the idfilters string.
	nfilters = ph_filters (Memc[infilters], Memc[outfilters], SZ_LINE)

	# Open the input and output image sets file.
	if (Memc[imsets] == EOS) {
	    outsets = NULL
	    insets = open ("STDIN", READ_ONLY, TEXT_FILE)
	} else if (access (Memc[imsets], READ_ONLY, TEXT_FILE) == YES) {
	    outsets = NULL
	    insets = open (Memc[imsets], READ_ONLY, TEXT_FILE)
	} else {
	    outsets = open (Memc[imsets], NEW_FILE, TEXT_FILE)
	    insets = open ("STDIN", READ_ONLY, TEXT_FILE)
	}

	# Open the input and output observing parameters file.
	if (Memc[obsparams] == EOS) {
	    outobs = NULL
	    inobs = NULL
	} else if (access (Memc[obsparams], READ_ONLY, TEXT_FILE) == YES) {
	    outobs = NULL
	    inobs = open (Memc[obsparams], READ_ONLY, TEXT_FILE)
	    call amovki (0, Memi[obscolumns], OBS_NFIELDS)
	    csp = 1
	    ncolumns = 0
	    while (ph_getlabels (Memc[obscolumnstr], csp, Memc[obscolname],
	        SZ_FNAME) != EOF) {
		cp = 1
		if (ctoi (Memc[obscolname], cp, Memi[obscolumns+ncolumns]) <= 0)
		    Memi[obscolumns+ncolumns] = 0
		ncolumns = ncolumns + 1
	    }
	} else {
	    outobs = open (Memc[obsparams], NEW_FILE, TEXT_FILE)
	    inobs = open ("STDIN", READ_ONLY, TEXT_FILE)
	}

	# Open the input and output shifts file.
	if (Memc[shifts] == EOS) {
	    outsh = NULL
	    insh = NULL
	} else if (access (Memc[shifts], READ_ONLY, TEXT_FILE) == YES) {
	    outsh = NULL
	    insh = open (Memc[shifts], READ_ONLY, TEXT_FILE)
	} else {
	    outsh = open (Memc[shifts], NEW_FILE, TEXT_FILE)
	    insh = open ("STDIN", READ_ONLY, TEXT_FILE)
	}

	# Open the aperture corrections file.
	if (Memc[apercors] == EOS) {
	    outap = NULL
	    inap = NULL
	} else if (access (Memc[apercors], READ_ONLY, TEXT_FILE) == YES) {
	    outap = NULL
	    inap = open (Memc[apercors], READ_ONLY, TEXT_FILE)
	} else {
	    outap = open (Memc[apercors], NEW_FILE, TEXT_FILE)
	    inap = open ("STDIN", READ_ONLY, TEXT_FILE)
	}

	# Open a symbol table to hold a list of image charactersitics.

	imtable = stopen ("imtable", 2 * LEN_IMTABLE, LEN_IMTABLE, 10 *
	    LEN_IMTABLE)

	# Read in the image set file and store each unique image name in
	# the symbol table. Initialize the records fields.

	if (insets == STDIN)
	    nimages = ph_esetimtable (imtable, nfilters, verify)
	else
	    nimages = ph_setimtable (imtable, insets, nfilters, verbose)

	# For each image referenced in the observing parameters file extract
	# the image name, the filter id if defined, the exposure time if
	# defined and the airmass if defined. Enter the new values of filter
	# id and airmass in the symbol table and compute the effective
	# exposure time required to correct the instrumental magnitudes to
	# the new exposure time.

	if (inobs == STDIN)
	    call ph_eobsimtable (imtable, nimages, verify) 
	else if (inobs != NULL)
	    call ph_obsimtable (imtable, inobs, Memi[obscolumns], verbose)

	# Read in the x and y shifts for the image. Images for which no
	# shift is defined are assigned an x-y shift of 0.0.

	if (insh == STDIN)
	    call ph_eshimtable (imtable, nimages, verify)
	else if (insh != NULL)
	    call ph_shimtable (imtable, insh, verbose)

	# Read in the aperture corrections. Images for which no aperture
	# correction is defined are assigned an aperture correction of 0.0.

	if (inap == STDIN)
	    call ph_eapimtable (imtable, nimages, verify)
	else if (inap != NULL)
	    call ph_apimtable (imtable, inap, verbose)

	# Write the results to the various output files.
	if (nimages > 0)
	    call ph_wimtable (imtable, outsets, outobs, outsh, outap, nimages)

	# Close the symbol table.
	call stclose (imtable)

	# Close the input files.
	if (insets != NULL)
	    call close (insets)
	if (inobs != NULL)
	    call close (inobs)
	if (insh != NULL)
	    call close (insh)
	if (inap != NULL)
	    call close (inap)

	# Close the output files.
	if (outsets != NULL) {
	    call close (outsets)
	    if (nimages <= 0)
		call delete (Memc[imsets])
	}
	if (outobs != NULL) {
	    call close (outobs)
	    if (nimages <= 0)
		call delete (Memc[obsparams])
	}
	if (outsh != NULL) {
	    call close (outsh)
	    if (nimages <= 0)
		call delete (Memc[shifts])
	}
	if (outap != NULL) {
	    call close (outap)
	    if (nimages <= 0)
		call delete (Memc[apercors])
	}

	call sfree (sp)
end


# PH_WIMTABLE -- Write the contents of the symbol table to the output image
# set file, the output observing parameters file, the output shifts file,
# and the output aperture corrections file. The only assumption here is
# that all images in a given image set are adjacent to each other in the
# symbol table, an assumption that is always true.

procedure ph_wimtable (imtable, outsets, outobs, outsh, outap, nimages)

pointer	imtable		# pointer to the the input symbol table
int	outsets		# the output image set file descriptor
int	outobs		# the output observing parameters file descriptor
int	outsh		# the output positional shifts file descriptor
int	outap		# the output aperture corrections file descriptor
int	nimages		# number of images in the symbol table

int	i, osetno, setno
pointer	sp, sym, symbol
pointer	sthead(), stnext()

begin
	call smark (sp)
	call salloc (sym, nimages, TY_POINTER)

	# Reorder the list of symbols.
	symbol = sthead (imtable)
	do i = nimages, 1, -1 {
	    Memi[sym+i-1] = symbol
	    symbol = stnext (imtable, symbol)
	}

	# Write the output files.
	osetno = 0
	do i = 1, nimages {

	    symbol = Memi[sym+i-1]
	    setno = IMT_IMSETNO(symbol)

	    if (outsets != NULL) {
		if (setno != osetno) {
		    if (setno == 1)
		        call fprintf (outsets, "%s :")
		    else
		        call fprintf (outsets, "\n%s :")
			call pargstr (IMT_LABEL(symbol))
		}
		call fprintf (outsets, "  %s")
		    call pargstr (IMT_IMNAME(symbol))
		if (i == nimages)
		    call fprintf (outsets, "\n")
	    }

	    if (outobs != NULL) {
		call fprintf (outobs, "%s  %s  %g  %g %0.1h\n")
		    call pargstr (IMT_IMNAME(symbol))
		    call pargstr (IMT_IFILTER(symbol))
		    call pargr (IMT_ITIME(symbol))
		    call pargr (IMT_XAIRMASS(symbol))
		    call pargr (IMT_OTIME(symbol))
	    }

	    if (outsh != NULL) {
		call fprintf (outsh, "%s  %g  %g\n")
		    call pargstr (IMT_IMNAME(symbol))
		    call pargr (IMT_XSHIFT(symbol))
		    call pargr (IMT_YSHIFT(symbol))
	    }

	    if (outap != NULL) {
		call fprintf (outap, "%s  %g\n")
		    call pargstr (IMT_IMNAME(symbol))
		    call pargr (IMT_APERCOR(symbol))
	    }

	    osetno = setno
	}

	call sfree (sp)
end
