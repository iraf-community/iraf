include	<error.h>
include	<imhdr.h>

# Symbol table structure for echelle dispersion functions.
define	LEN_EC		4		# Length of dispersion solution struct.
define	EC_OFFSET	Memi[$1]	# Aperture to order offset
define	EC_SLOPE	Memi[$1+1]	# Aperture to order slope
define	EC_ECF		Memi[$1+2]	# Dispersion function (GSURFIT)
define	EC_SHIFT	Memr[$1+3]	# Dispersion functin shift


# EC_OPEN  -- Initialize the echelle dispersion data structures.
# EC_CLOSE -- Close the echelle dispersion data structures.
# EC_GDB   -- Get an echelle dispersion database entry.
# EC_GSPEC -- Get an echelle spectrum.
# EC_EVAL  -- Evaluate the dispersion function.


# EC_OPEN -- Initialize the echelle dispersion routines.  This consists
# of opening a symbol table for the dispersion solution functions and
# linear wavelength parameters.

procedure ec_open ()

pointer	stopen()

include	"ecdispcor.com"

begin
	stp = stopen ("disp", 10, 10, 10*SZ_FNAME)
	crval = NULL
	cdelt = NULL
end


# EC_CLOSE -- Close the echelle dispersion data structures.

procedure ec_close ()

pointer sym, sthead, stnext

include	"ecdispcor.com"

begin
	# Close each echelle dispersion function and then the symbol table.
	for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym))
	    call dgsfree (EC_ECF(sym))
	call stclose (stp)

	# Free memory used for linear wavelength parameters.
	call mfree (crval, TY_REAL)
	call mfree (cdelt, TY_REAL)
end


# EC_GDB  -- Get a dispersion database entry.
# The database entry is read only once from the database and stored in a
# symbol table keyed by the spectrum name.  Subsequent requests for the
# reference spectrum returns the data from the symbol table.

procedure ec_gdb (spec, db, offst, slpe, ecf, shift)

char	spec[ARB]	# Spectrum image name
char	db[ARB]		# Database
int	offst		# Aperture to order offset
int	slpe		# Aperture to order slope
pointer	ecf		# Dispersion solution curve (GSURFIT)
double	shift		# Dispersion shift

int	rec, n, dtlocate(), dtgeti()
real	dtgetr()
pointer	sp, str, coeffs
pointer	sym, stfind(), stenter()
pointer	dt, dtmap()
errchk	dtmap, dtlocate, dtgad, dgsrestore

include	"ecdispcor.com"

begin
	# Check if dispersion solution is in the symbol table from a previous
	# call.  If not in the symbol table get it from the database and
	# store it in the symbol table.

	sym = stfind (stp, spec)
	if (sym == NULL) {
	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[str], SZ_LINE, "%s/ec%s")
		call pargstr (db)
		call pargstr (spec)
	    dt = dtmap (Memc[str], READ_ONLY)

	    call sprintf (Memc[str], SZ_LINE, "ecidentify %s")
	        call pargstr (spec)
	    rec = dtlocate (dt, Memc[str])
	    if (rec == EOF)
	        call error (0, "Ecdispcor: Database entry not found")

	    iferr (offst = dtgeti (dt, rec, "offset"))
	        offst = 0
	    iferr (slpe = dtgeti (dt, rec, "slope"))
	        slpe = 1
	    iferr (shift = dtgetr (dt, rec, "shift"))
	        shift = 0.
	    n = dtgeti (dt, rec, "coefficients")
	    call salloc (coeffs, n, TY_DOUBLE)
	    call dtgad (dt, rec, "coefficients", Memd[coeffs], n, n)
	    call dgsrestore (ecf, Memd[coeffs])

	    sym = stenter (stp, spec, LEN_EC)
	    EC_OFFSET(sym) = offst
	    EC_SLOPE(sym) = slpe
	    EC_SHIFT(sym) = shift
	    EC_ECF(sym) = ecf

	    call dtunmap (dt)
	    call sfree (sp)
	} else {
	    offst = EC_OFFSET(sym)
	    slpe = EC_SLOPE(sym)
	    ecf = EC_ECF(sym)
	    shift = EC_SHIFT(sym)
	}
end


# EC_GSPEC -- Get an echelle spectrum.  This consists of mapping the image
# and determining the dispersion function.  This function may be either from
# a reference spectrum stored in the database or the linear coordinates
# if previously determined.

pointer procedure ec_gspec (spec, db, ap, flag)

char	spec[ARB]	# Spectrum image name
char	db[ARB]		# Dispersion database
int	ap		# Aperture
int	flag		# Dispersion correction flag

pointer	sp, str, str1, im, immap()
int	i, j, offset2, slope2, nscan(), imgeti()
errchk	ec_gdb, imgstr

include	"ecdispcor.com"

begin
	# Map the spectrum.  If an error warn and return a null pointer.
	iferr (im = immap (spec, READ_ONLY, 0)) {
	    call erract (EA_WARN)
	    return (NULL)
	}

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)

	# Always return same aperture number.
	ap = 1

	# Determine if spectrum has been dispersion corrected.
	iferr (dcflag = imgeti (im, "DC-FLAG"))
	    dcflag = -1
	flag = dcflag

	# If previously dispersion corrected get the linear wavelength params.
	if (dcflag > -1) {
	    offset = 0.
	    slope = 1.
	    call mfree (crval, TY_REAL)
	    call mfree (cdelt, TY_REAL)
	    call malloc (crval, IM_LEN(im,2), TY_REAL)
	    call malloc (cdelt, IM_LEN(im,2), TY_REAL)
	    do i = 1, IM_LEN(im,2) {
		call sprintf (Memc[str1], SZ_LINE, "apnum%d")
		    call pargi (i)
	        call imgstr (im, Memc[str1], Memc[str], SZ_LINE)
	        call sscan (Memc[str])
	        call gargi (j)
	        call gargi (j)
		call gargr (Memr[crval+i-1])
		call gargr (Memr[cdelt+i-1])
	    }
		
	    call sfree (sp)
	    return (im)
	}

	# Get the reference spectra and read the dispersion function from
	# the database.  Return a null pointer if no reference spectrum
	# specified.

	iferr {
	    call imgstr (im, "refspec1", Memc[str], SZ_FNAME)
	    call sscan (Memc[str])
	    call gargwrd (Memc[str], SZ_FNAME)
	    call gargd (wt1)
	    if (nscan() == 1)
		wt1 = 1.
	    call ec_gdb (Memc[str], db, offset, slope, ecf1, shift1)
	} then {
	    call imunmap (im)
	    call sfree (sp)
	    return (NULL)
	}

	iferr {
	    call imgstr (im, "refspec2", Memc[str], SZ_FNAME)
	    call sscan (Memc[str])
	    call gargwrd (Memc[str], SZ_FNAME)
	    call gargd (wt2)
	    if (nscan() == 1)
		wt2 = 1.
	    call ec_gdb (Memc[str], db, offset2, slope2, ecf2, shift2)

	    # Check both reference spectra have the same order mapping.
	    if (offset2 != offset || slope2 != slope) {
	        call imunmap (im)
	        call eprintf ("Reference spectra are incompatible for %s\n")
		    call pargstr (spec)
	        return (NULL)
	    }
	} then
	    wt2 = 0.

	call sfree (sp)
	return (im)
end


# EC_EVAL -- Evaluate dispersion function.  The order is specified through
# a common parameter as are the dispersion data.

double procedure ec_eval (x)

double	x		# Pixel coordinate
double	w		# Wavelength

int	i
double	dgseval()

include	"ecdispcor.com"

begin
	if (dcflag == -1) {
            w = (shift1 + dgseval (ecf1, x, order)) / order
	    if (wt2 != 0.)
                w = wt1 * w +
		    wt2 * ((shift2 + dgseval (ecf2, x, order)) / order)
	} else if (dcflag == 0) {
	    i = order
	    w = Memr[crval+i-1] + Memr[cdelt+i-1] * (x - 1)
	} else {
	    i = order
	    w = 10. * (Memr[crval+i-1] + Memr[cdelt+i-1] * (x - 1))
	}
	return (w)
end
