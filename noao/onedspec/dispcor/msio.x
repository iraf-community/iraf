include	<error.h>
include	<imhdr.h>
 
# Symbol table structure for the dispersion solutions.
define	LEN_MS		3		# Length of dispersion solution struct.
define	MS_SHIFT	Memi[$1]	# Pointer to shifts
define	MS_CV		Memi[$1+1]	# Pointer to disp. functions (CURFIT)
define	MS_NAPS		Memi[$1+2]	# Number of apertures
 
 
# MS_OPEN  -- Initialize the multispec dispersion data structures.
# MS_CLOSE -- Close the multispec dispersion data structures.
# MS_GDB   -- Get a multispec dispersion database entry.
# MS_GSPEC -- Get a multispec spectrum.
# MS_EVAL  -- Evaluate the dispersion function.
 
 
# MS_OPEN -- Initialize the multispec dispersion routines.  This consists
# of opening a symbol table for the dispersion solution functions and
# linear wavelength parameters.
 
procedure ms_open ()
 
pointer	stopen()
 
include	"msdispcor.com"
 
begin
	stp = stopen ("disp", 10, 10, 10*SZ_FNAME)
	shift1 = NULL
	shift2 = NULL
	cv1 = NULL
	cv2 = NULL
	crval = NULL
	cdelt = NULL
end
 
 
# MS_CLOSE -- Close the multispec dispersion data structures.
 
procedure ms_close ()
 
int	i
pointer sym, sthead, stnext
 
include	"msdispcor.com"
 
begin
	# Close each multispec dispersion function and then the symbol table.
	for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {
	    do i = 1, MS_NAPS(sym)
	        call dcvfree (Memi[MS_CV(sym)+i-1])
	    call mfree (MS_SHIFT(sym), TY_DOUBLE)
	    call mfree (MS_CV(sym), TY_POINTER)
	}
	call stclose (stp)
 
	# Free memory used for linear wavelength parameters.
	call mfree (crval, TY_REAL)
	call mfree (cdelt, TY_REAL)
end
 
 
# MS_GDB  -- Get a dispersion database entry.
# The database entry is read only once from the database and stored in a
# symbol table keyed by the spectrum name.  Subsequent requests for the
# reference spectrum returns the data from the symbol table.
 
procedure ms_gdb (spec, db, shift, cv, naps)
 
char	spec[ARB]	# Spectrum image name
char	db[ARB]		# Database
pointer	shift		# Shifts
pointer	cv		# Function pointers
int	naps		# Number of apertures
 
int	i, rec, n, dtlocate(), dtgeti()
real	dtgetr()
pointer	sp, str, coeffs
pointer	sym, stfind(), stenter()
pointer	dt, dtmap1()
errchk	dtmap1, dtlocate, dtgad, dcvrestore
 
include	"msdispcor.com"
 
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
 
	    call calloc (shift, naps, TY_DOUBLE)
	    call calloc (cv, naps, TY_POINTER)
	    do i = 1, naps {
		if (naps > 1) {
	            call sprintf (Memc[str], SZ_LINE, "identify %s[*,%d]")
	                call pargstr (spec)
		        call pargi (i)
		} else {
	            call sprintf (Memc[str], SZ_LINE, "identify %s")
	                call pargstr (spec)
		}
	        rec = dtlocate (dt, Memc[str])
	        if (rec == EOF) {
		    call eprintf ("MSDISPCOR: Database entry %s not found.\n")
			call pargstr (Memc[str])
		    next
		}
 
	        iferr (Memd[shift+i-1] = dtgetr (dt, rec, "shift"))
	            Memd[shift+i-1] = 0.
	        n = dtgeti (dt, rec, "coefficients")
	        call salloc (coeffs, n, TY_DOUBLE)
	        call dtgad (dt, rec, "coefficients", Memd[coeffs], n, n)
	        call dcvrestore (Memi[cv+i-1], Memd[coeffs])
	    }
 
	    call dtunmap (dt)
	    call sfree (sp)
 
	    sym = stenter (stp, spec, LEN_MS)
	    MS_NAPS(sym) = naps
	    MS_SHIFT(sym) = shift
	    MS_CV(sym) = cv
	} else {
	    shift = MS_SHIFT(sym)
	    cv = MS_CV(sym)
	}
end
 
 
# MS_GSPEC -- Get an multispec spectrum.  This consists of mapping the image
# and determining the dispersion function.  This function may be either from
# a reference spectrum stored in the database or the linear coordinates
# if previously determined.
 
pointer procedure ms_gspec (spec, db, ap, flag)
 
char	spec[ARB]	# Spectrum image name
char	db[ARB]		# Dispersion database
int	ap		# Aperture
int	flag		# Dispersion correction flag
 
pointer	sp, str, str1, im, immap()
int	i, j, naps, nscan(), imgeti()
errchk	ms_gdb, imgstr
 
include	"msdispcor.com"
 
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
	naps = IM_LEN(im,2)
 
	# Determine if spectrum has been dispersion corrected.
	iferr (dcflag = imgeti (im, "DC-FLAG"))
	    dcflag = -1
	flag = dcflag
 
	# If previously dispersion corrected get the linear wavelength params.
	if (dcflag > -1) {
	    call mfree (crval, TY_REAL)
	    call mfree (cdelt, TY_REAL)
	    call malloc (crval, naps, TY_REAL)
	    call malloc (cdelt, naps, TY_REAL)
	    do i = 1, naps {
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
	    call ms_gdb (Memc[str], db, shift1, cv1, naps)
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
	    call ms_gdb (Memc[str], shift2, cv2, naps)
	} then
	    wt2 = 0.
 
	call sfree (sp)
	return (im)
end
 
 
# MS_EVAL -- Evaluate dispersion function.  The line is specified through
# a common parameter as are the dispersion data.
 
double procedure ms_eval (x)
 
double	x		# Pixel coordinate
double	w		# Wavelength
 
double	dcveval()
 
include	"msdispcor.com"
 
begin
	if (dcflag == -1) {
	    if (Memi[cv1+line-1] == NULL)
		w = x
	    else
	        w = Memd[shift1+line-1] + dcveval (Memi[cv1+line-1], x)
	    if (wt2 != 0.) {
		if (Memi[cv2+line-1] == NULL)
		    w = wt1 * w + wt2 * x
		else
	            w = wt1 * w + wt2 * (Memd[shift2+line-1] +
		        dcveval (Memi[cv2+line-1], x))
	    }
	} else if (dcflag == 0)
	    w = Memr[crval+line-1] + Memr[cdelt+line-1] * (x - 1)
	else
	    w = 10. ** (Memr[crval+line-1] + Memr[cdelt+line-1] * (x - 1))
 
	return (w)
end

