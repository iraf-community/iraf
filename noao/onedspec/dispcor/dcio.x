include	<error.h>
include	<pkg/dttext.h>

# Symbol table structure for the dispersion solutions.
define	LEN_DC		2		# Length of dispersion solution struct.
define	DC_SHIFT	Memr[$1]	# Shift
define	DC_CV		Memi[$1+1]	# Dispersion function (CURFIT)


# DC_OPEN  -- Initialize the dispersion data structure.
# DC_CLOSE -- Close the echelle dispersion data structures.
# DC_GDB   -- Get an echelle dispersion database entry.
# DC_GSPEC -- Get an echelle spectrum.
# DC_EVAL  -- Evaluate the dispersion function.


# DC_OPEN -- Initialize the dispersion routines.  This consists
# of opening a symbol table for the dispersion solution functions.

procedure dc_open ()

pointer	stopen()

pointer	stp			# Dispersion solution symbol table
common	/dciocom/ stp

begin
	# Open symbol tables.
	stp = stopen ("disp", 10, 10, 10*SZ_FNAME)
end


# DC_CLOSE -- Close the dispersion data structures.

procedure dc_close ()

pointer sym, sthead, stnext

pointer	stp			# Dispersion solution symbol table
common	/dciocom/ stp

begin
	for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym))
	    call dcvfree (DC_CV(sym))
	call stclose (stp)
end


# DC_GDB  -- Get a dispersion database entry.
# The database entry is read only once from the database and stored in a
# symbol table keyed by the spectrum name.  Subsequent requests for the
# reference spectrum returns the data from the symbol table.

procedure dc_gdb (spec, db, shift, cv)

char	spec[ARB]	# Spectrum image name
char	db[ARB]		# Database
double	shift		# Dispersion shift
pointer	cv		# Dispersion solution curve (CURFIT)

int	rec, n, dtgeti()
real	dtgetr()
bool	streq()
pointer	sp, str, coeffs
pointer	sym, stfind(), stenter()
pointer	dt, dtmap1()
errchk	dtmap1, dtgad, dcvrestore

pointer	stp			# Dispersion solution symbol table
common	/dciocom/ stp

begin
	# Check if dispersion solution is in the symbol table from a previous
	# call.  If not in the symbol table get it from the database and
	# store it in the symbol table.

	sym = stfind (stp, spec)
	if (sym == NULL) {
	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call strcpy ("id", Memc[str], SZ_LINE)
	    call imgcluster (spec, Memc[str+2], SZ_LINE)
	    dt = dtmap1 (db, Memc[str], READ_ONLY)

	    call sprintf (Memc[str], SZ_LINE, "identify %s")
	        call pargstr (spec)
	    for (rec=1; rec<DT_NRECS(dt); rec=rec+1)
		if (streq (Memc[str], DT_NAME(dt,rec)))
		    break

	    iferr (shift = dtgetr (dt, rec, "shift"))
	        shift = 0.
	    n = dtgeti (dt, rec, "coefficients")
	    call salloc (coeffs, n, TY_DOUBLE)
	    call dtgad (dt, rec, "coefficients", Memd[coeffs], n, n)
	    call dcvrestore (cv, Memd[coeffs])

	    sym = stenter (stp, spec, LEN_DC)
	    DC_SHIFT(sym) = shift
	    DC_CV(sym) = cv

	    call dtunmap (dt)
	    call sfree (sp)
	} else {
	    shift = DC_SHIFT(sym)
	    cv = DC_CV(sym)
	}
end


# DC_GSPEC -- Get a spectrum.  This consists of mapping the image
# and determining the dispersion function.  This function may be either from
# a reference spectrum stored in the database or the linear coordinates
# if previously determined.

pointer procedure dc_gspec (spec, db, aps, apflag, ap, flag)

char	spec[ARB]	# Spectrum image name
char	db[ARB]		# Dispersion database
pointer	aps		# Aperture list
bool	apflag		# Ignore aperture in global defaults?
int	ap		# Spectrum aperture number
int	flag		# Dispersion correction flag

pointer	sp, ref, im, immap()
int	imgeti(), nscan()
real	imgetr()
bool	is_in_range()
errchk	dc_gdb, imgstr

include	"dispcor.com"

begin
	# Map the input spectrum.  If error warn and return null pointer.
	iferr (im = immap (spec, READ_ONLY, 0)) {
	    call erract (EA_WARN)
	    return (NULL)
	}

	# Check if the spectrum has the requested aperture number.
	iferr (ap = imgeti (im, "BEAM-NUM"))
	    ap = INDEFI
	if (apflag && !is_in_range (Memi[aps], ap)) {
	    call imunmap (im)
	    return (NULL)
	}

	# Determine if spectrum has been dispersion corrected.
	iferr (dcflag = imgeti (im, "DC-FLAG"))
	    dcflag = -1
	flag = dcflag

	# If dispersion corrected get linear wavelength parameters.
	if (dcflag > -1) {
	    iferr (crval = imgetr (im, "W0"))
		crval = imgetr (im, "CRVAL1")
	    iferr (cdelt = imgetr (im, "WPC"))
		iferr (cdelt = imgetr (im, "CDELT1"))
		    cdelt = imgetr (im, "CD1_1")
	    return (im)
	}

	# Get the reference spectra dispersion functions.
	# Return a null pointer if none is specified.

	call smark (sp)
	call salloc (ref, SZ_FNAME, TY_CHAR)

	iferr {
	    call imgstr (im, "refspec1", Memc[ref], SZ_FNAME)
	    call sscan (Memc[ref])
	    call gargwrd (Memc[ref], SZ_FNAME)
	    call gargd (wt1)
	    if (nscan() == 1)
		wt1 = 1.
	    call dc_gdb (Memc[ref], db, shift1, cv1)
	} then {
	    call imunmap (im)
	    call sfree (sp)
	    return (NULL)
	}

	iferr {
	    call imgstr (im, "refspec2", Memc[ref], SZ_FNAME)
	    call sscan (Memc[ref])
	    call gargwrd (Memc[ref], SZ_FNAME)
	    call gargd (wt2)
	    if (nscan() == 1)
		wt2 = 1.
	    call dc_gdb (Memc[ref], db, shift2, cv2)
	} then
	    wt2 = 0.

	call sfree (sp)
	return (im)
end


# DC_EVAL -- Evaluate the dispersion function.

double procedure dc_eval (x)

double	x			# Pixel coordinate
double	w			# Wavelength

double	dcveval()

include	"dispcor.com"

begin
	if (dcflag == -1) {
	    w = shift1 + dcveval (cv1, x)
	    if (wt2 != 0.)
	        w = wt1 * w + wt2 * (shift2 + dcveval (cv2, x))
	} else if (dcflag == 0)
	    w = crval + cdelt * (x - 1)
	else
	    w = 10. ** (crval + cdelt * (x - 1))

	return (w)
end
