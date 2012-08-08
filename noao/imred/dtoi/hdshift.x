include	<math/curfit.h>

# T_HDSHIFT -- Task hdshift in the dtoi package.  This task is provided
# to support Kormendy's method of combining related characteristic curves.
# A zero point shift in log exp unique to each set of spots is calculated and
# subtracted.  A single curve is fit to the combined, shifted data in
# a separate task (hdfit).

procedure t_hdshift ()

pointer	sp, de, fun, save, db, cv, ps_coeff, den, exp, cvf
int	fd, rec, nsave, ncoeff, nvalues, i, nfile, junk
real	a0, ref_a0

pointer	ddb_map()
int	clpopni(), ddb_locate(), ddb_geti(), cvstati(), strncmp(), clgfil()

begin
	# Allocate space on stack for string buffers
	call smark (sp)
	call salloc (de, SZ_FNAME, TY_CHAR)
	call salloc (cvf, SZ_FNAME, TY_CHAR)
	call salloc (fun, SZ_FNAME, TY_CHAR)

	# Get list of the database names.  The curfit information is retrieved
	# from the first file in the list, the list is then rewound.

	fd = clpopni ("database")
	junk = clgfil (fd, Memc[cvf], SZ_FNAME)
	call clprew (fd)

	# Get coefficients of common fit from cv_file
	db = ddb_map (Memc[cvf], READ_ONLY)
	rec = ddb_locate (db, "cv")
	nsave = ddb_geti (db, rec, "save")
	call salloc (save, nsave, TY_REAL)
	call ddb_gar (db, rec, "save", Memr[save], nsave, nsave)
	call ddb_gstr (db, rec, "function", Memc[fun], SZ_LINE)

	call cvrestore (cv, Memr[save])
	ncoeff = cvstati (cv, CVNCOEFF)
	call salloc (ps_coeff, ncoeff, TY_REAL)

	if (strncmp (Memc[fun], "power", 5) == 0)
	    call cvcoeff (cv, Memr[ps_coeff], ncoeff)
	else
	    call cvpower (cv, Memr[ps_coeff], ncoeff)

	do i = 1, ncoeff {
	    call eprintf ("%d   %.7g\n")
	        call pargi (i)
	        call pargr (Memr[ps_coeff+i-1])
	}

	call ddb_unmap (db)

	nfile = 0
	while (clgfil (fd, Memc[de], SZ_FNAME) != EOF) {
	    db = ddb_map (Memc[de], READ_ONLY)
	    call hds_read (db, den, exp, nvalues)
	    call hds_calc (den, exp, nvalues, Memr[ps_coeff], ncoeff, a0)
	    nfile = nfile + 1
	    if (nfile == 1)
		ref_a0 = a0
	    a0 = a0 - ref_a0

	    call printf ("file %s: subtracting zero point a0 = %.7g\n")
		call pargstr (Memc[de])
		call pargr (a0)

	    # Write new log exposure information to database
	    db = ddb_map (Memc[de], APPEND)
	    call hds_wdb  (db, exp, nvalues, a0)
	    call mfree (den, TY_REAL)
	    call mfree (exp, TY_REAL)
	    call ddb_unmap (db)
	}

	call clpcls (fd)
	call sfree (sp)
end


# HDS_READ -- Read the density and exposure values from the database file.
# The density above fog and log exposure values are returned, as well as
# the number of data pairs read.

procedure hds_read (db, den, exp, nvalues)

pointer	db			# Pointer to input database file
pointer	den			# Pointer to density array - returned
pointer	exp			# Pointer to exposure array - returned
int	nvalues			# Number of data pairs read - returned

real	fog
int	nden, nexp, rec
int	ddb_locate(), ddb_geti()
real	ddb_getr()

begin
	# Get fog value to be subtracted from density
	rec = ddb_locate (db, "fog")
	fog = ddb_getr (db, rec, "density")

	# Get density array
	rec = ddb_locate (db, "density")
	nden = ddb_geti (db, rec, "den_val")
	call malloc (den, nden, TY_REAL)
	call ddb_gar (db, rec, "den_val", Memr[den], nden, nden)
	call asubkr (Memr[den], fog, Memr[den], nden)

	# Get exposure array
	rec = ddb_locate (db, "exposure")
	nexp = ddb_geti (db, rec, "log_exp")
	call malloc (exp, nexp, TY_REAL)
	call ddb_gar (db, rec, "log_exp", Memr[exp], nexp, nexp)

	nvalues = min (nden, nexp)
end


# HDS_CALC -- Calculate the individual shift, a0.

procedure hds_calc (den, exp, nvalues, ps_coeff, ncoeff, a0)

pointer	den
pointer	exp
int	nvalues
real	ps_coeff[ARB]
int	ncoeff
real	a0

int	i
real	yavg, ycalc, xavg

begin
	# Calculate average density and log exposure values
	xavg = 0.0
	yavg = 0.0

	do i = 1, nvalues {
	    xavg = xavg + Memr[den+i-1]
	    yavg = yavg + Memr[exp+i-1]
	}

	xavg = xavg / real (nvalues)
	yavg = yavg / real (nvalues)

	ycalc = 0.0
	do i = 2, ncoeff
	    ycalc = ycalc + ps_coeff[i] * (xavg ** real (i-1))

	# Subtraction yields the zero point shift in question
	a0 = yavg - ycalc
end


# HDS_WDB -- Write shifted log exposure values to database.

procedure hds_wdb  (db, exp, nvalues, a0)

pointer	db		# Pointer to database
pointer	exp		# Pointer to array of exposure values
int	nvalues		# Number of exposure values in sample
real	a0		# Shift to be subtracted

pointer	sp, expsub

begin
	call smark (sp)
	call salloc (expsub, nvalues, TY_REAL)

	call ddb_ptime (db)
	call ddb_prec (db, "exposure")

	call eprintf ("a0 = %g\n")
	    call pargr (a0)

	call asubkr (Memr[exp], a0, Memr[expsub], nvalues)
	call ddb_par (db, "log_exp", Memr[expsub], nvalues)

	call ddb_putr (db, "A0 shift", a0)
	call sfree (sp)
end
