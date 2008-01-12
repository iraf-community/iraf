include	<error.h>

# T_DEMATCH -- matches densities listed in the input database to
# log exposure values retrieved from a system maintained or user
# provided file.  The output matches are added to the input database.

procedure t_dematch ()

pointer	filter, emulsion, wedge_db, density_db, db, exp, den, sp
pointer wedge, expo
int	nskip, rec, nvalues

pointer	ddb_map()
bool	clgetb()
int	ddb_locate(), ddb_geti(), clgeti()

begin
	call smark (sp)
	call salloc (filter, SZ_FNAME, TY_CHAR)
	call salloc (emulsion,  SZ_FNAME, TY_CHAR)
	call salloc (wedge_db, SZ_FNAME, TY_CHAR)
	call salloc (density_db, SZ_FNAME, TY_CHAR)
	call salloc (wedge, SZ_FNAME, TY_CHAR)

	# Get parameters
	call clgstr ("database", Memc[density_db], SZ_FNAME)
	call clgstr ("wedge", Memc[wedge], SZ_FNAME)
	call clgstr ("filter", Memc[filter], SZ_FNAME)
	call clgstr ("emulsion", Memc[emulsion], SZ_FNAME)
	call clgstr ("wedgefile", Memc[wedge_db], SZ_FNAME)
	nskip = clgeti ("nskip")

	# Retrieve exposure information; one wedge per run
	call hd_rwedge (Memc[wedge_db], exp, Memc[wedge], Memc[filter], 
	    Memc[emulsion], clgetb("verbose"))

	db = ddb_map (Memc[density_db], READ_ONLY)
	iferr {
	    rec = ddb_locate (db, "density")
	    nvalues = ddb_geti (db, rec, "den_val")
	} then
	    call error (0, "Error locating density record in database")

	call salloc (den, nvalues, TY_REAL)
	iferr (call ddb_gar (db, rec, "den_val", Memr[den], nvalues, nvalues))
	    call error (0, "Error reading density information")

	# Close db file before reopening for append.
	call ddb_unmap (db)

	# Add fields for wedge, filter and plate as "calibrate" record
	db = ddb_map (Memc[density_db], APPEND)
	call ddb_prec (db, "calibrate")
	call ddb_pstr (db, "wedge", Memc[wedge])
	call ddb_pstr (db, "filter", Memc[filter])
	call ddb_pstr (db, "emulsion", Memc[emulsion])

	# Exposures are returned in increasing order.  Make sure the
	# exposures are output in the same order as density values.

	call salloc (expo, nvalues, TY_REAL)
	call amovr (Memr[exp+nskip], Memr[expo], nvalues)

	if (Memr[den] > Memr[den+nvalues-1])
	    call hd_reorderr (Memr[expo], nvalues)

	# Now add exposure values to database
	call ddb_ptime (db)
	call ddb_prec (db, "exposure")
	call ddb_par (db, "log_exp", Memr[expo], nvalues)

	call ddb_unmap (db)

	call mfree (exp, TY_REAL)
	call sfree (sp)
end


# HD_RWEDGE -- Read wedge information from database file for a given
# wedge, filter, emulsion combination.  A pointer to the extracted
# exposure values is returned as an argument.

procedure hd_rwedge (db_file, exp, wedge, filter, emulsion, verbose)

char	db_file[SZ_FNAME]	# Name of database with exposure information
pointer	exp			# Pointer to array of exposure values - output
char	wedge[SZ_FNAME]		# Wedge number
char	filter[SZ_FNAME]	# Filter used
char	emulsion[SZ_FNAME]	# Emulsion used
bool	verbose			# Print record of exposure information?

pointer	db
char	wfe[SZ_FNAME]
int	rec, nvalues, stat, i

pointer ddb_map()
int	ddb_locate(), ddb_geti(), ddb_scan()
errchk	ddb_map, ddb_scan

begin
	# Convert strings to upper case for matching in database
	call strupr (wedge)
	call strupr (filter)
	call strupr (emulsion)

	db = ddb_map (db_file, READ_ONLY)
	iferr (rec = ddb_locate (db, wedge))
	    call erract (EA_FATAL)

	# Construct field name of filter/emulsion combination in question
	call sprintf (wfe, SZ_FNAME, "%s/%s")
	    call pargstr (emulsion)
	    call pargstr (filter)

	# Retrieve exposure values from database file
	iferr (nvalues = ddb_geti (db, rec, wfe))
	    call erract (EA_FATAL)

	call malloc (exp, nvalues, TY_REAL)
	stat = ddb_scan (db)

	do i = 1, nvalues {
	    call gargr (Memr[exp+i-1])

	    # Calibration values are stored 8 values per line
	    if (mod (i, 8) == 0) {
		if (nvalues > i)
		    stat = ddb_scan (db)
	    }
	}
	 
	if (verbose) {
	    call printf ("\nCalibration log exposure values for %s/%s: \n")
	        call pargstr (wedge)
	        call pargstr (wfe)
	    
	    do i = 1, nvalues {
	        call printf ("%8g ")
		    call pargr (Memr[exp+i-1])
	        if (mod (i, 8) == 0)
		    call printf ("\n")
	    }

	    # Need a newline if the last line didn't have 8 entries.
	    if (mod (nvalues, 8) != 0)
		call printf ("\n")

	    call flush (STDOUT)
	}

	# Exposures are returned sorted in increasing order.  That is,
	# the exposure for the lightest spot is element one, and the
	# darkest spot's exposure value is the last array element.

	if (Memr[exp] > Memr[exp+nvalues-1])
	    # Out of order - flip elements
	    call hd_reorderr (Memr[exp], nvalues)

	call ddb_unmap (db)
end
