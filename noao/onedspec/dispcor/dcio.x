include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pkg/dttext.h>
include	"dispcor.h"

# Symbol table structure for the dispersion solutions.
define	LEN_DC		10		# Length of dispersion solution struct.
define	DC_FORMAT	Memi[$1]	# Type of dispersion
define	DC_PAPS		Memi[$1+1]	# Pointer to aperture numbers
define	DC_PAPCEN	Memi[$1+2]	# Pointer to aperture centers
define	DC_PSHIFT	Memi[$1+3]	# Pointer to shifts
define	DC_PCOEFF	Memi[$1+4]	# Pointer to coefficients
define	DC_NAPS		Memi[$1+5]	# Number of apertures
define  DC_OFFSET       Memi[$1+6]      # Aperture to order offset
define  DC_SLOPE        Memi[$1+7]      # Aperture to order slope
define  DC_COEFFS       Memi[$1+8]      # Dispersion coefficients
define  DC_SHIFT        Memr[$1+9]      # Dispersion function shift


# DC_OPEN    -- Initialize the dispersion data structures
# DC_CLOSE   -- Close the dispersion data structures
# DC_GMS     -- Get a multispec spectrum
# DC_GMSDB   -- Get a multispec dispersion database entry
# DC_REFSHFT -- Get a reference shift
# DC_GEC     -- Get an echelle spectrum
# DC_GECDB   -- Get an echelle dispersion database entry
# DC_ECMS    -- Convert echelle dispersion coeffs to multispec coeffs


# DC_OPEN -- Initialize the dispersion routines.  This consists
# of opening a symbol table for the dispersion solution functions.  A
# symbol table is used since the same dispersion reference (arc image)
# may be be used multiple times and the database access is slow.

procedure dc_open (stp, db)

pointer	stp			# Symbol table pointer
char	db[SZ_FNAME]		# Database name

pointer	sym, stopen(), stenter(), stpstr()

begin
	stp = stopen ("disp", 10, 10, 10*SZ_FNAME)
	sym = stenter (stp, "database", 1)
	Memi[sym] = stpstr (stp, db, 0)
end


# DC_CLOSE -- Close the dispersion data structures.

procedure dc_close (stp)

int	i
pointer stp, sym, sthead, stnext

begin
	# Close each dispersion function and then the symbol table.
	for (sym = sthead (stp); sym != NULL; sym = stnext (stp, sym)) {
	    if (DC_FORMAT(sym) == 1) {
		do i = 1, DC_NAPS(sym)
		    call mfree (Memi[DC_PCOEFF(sym)+i-1], TY_DOUBLE)
		call mfree (DC_PAPS(sym), TY_INT)
		call mfree (DC_PAPCEN(sym), TY_REAL)
		call mfree (DC_PSHIFT(sym), TY_DOUBLE)
		call mfree (DC_PCOEFF(sym), TY_POINTER)
	    } else if (DC_FORMAT(sym) == 2) {
		call mfree (DC_COEFFS(sym), TY_DOUBLE)
	    }
	}
	call stclose (stp)
end


# DC_GMS -- Get a multispec spectrum.  This consists of mapping the image
# and setting a MWCS coordinate transformation.  If not dispersion corrected
# the dispersion function is found in the database for the reference
# spectra and set in the MWCS.

procedure dc_gms (im, mw, stp, ap, fd)

pointer	im		#I IMIO pointer
pointer	mw		#I MWCS pointer
pointer	stp		#I Dispersion symbol table
pointer	ap		#O Aperture data structure
int	fd		#I Logfile descriptor

double	wt1, wt2
int	i, j, k, l, dc, sfd, axis[2], naps, naps1, naps2, ncoeffs
pointer	sp, spec, str1, str2, coeff, papcen, pshift, coeffs, ct
pointer	paps1, paps2, pshift1, pshift2, pcoeff1, pcoeff2

bool	clgetb()
double	mw_c1trand()
int	nscan(), stropen()
pointer	mw_sctran()
errchk	dc_gmsdb, dc_refshft, imgstr

data	axis/1,2/
define	done_	90

begin
	call smark (sp)
	call salloc (spec, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	coeff = NULL

	call imstats (im, IM_IMAGENAME, Memc[spec], SZ_FNAME)

	# Set WCS attributes
	naps = IM_LEN(im,2)
	call malloc (ap, LEN_AP(naps), TY_STRUCT)
	ct = mw_sctran (mw, "logical", "physical", 2)
	do i = 1, naps {
	    PL(ap,i) = nint (mw_c1trand (ct, double(i)))
	    call shdr_gwattrs (mw, PL(ap,i), AP(ap,i), BM(ap,i), DT(ap,i),
		W1(ap,i), DW(ap,i), NW(ap,i), Z(ap,i), LW(ap,i), UP(ap,i),
		coeff)
	    dc = DT(ap,i)
	}
	call mw_ctfree (ct)

	# Determine if spectrum has been dispersion corrected and return
	# This assumes all spectra have the same dispersion type
	if (dc > -1)
	    goto done_

	# Get the reference spectra dispersion function from the database
	# and determine a reference shift.  Return a null pointer if no
	# reference spectrum specified.

	iferr {
	    call imgstr (im, "REFSPEC1", Memc[str1], SZ_LINE)
	    call sscan (Memc[str1])
	    call gargwrd (Memc[str1], SZ_LINE)
	    call gargd (wt1)
	    if (nscan() == 1)
		wt1 = 1.
	} then {
	    call strcpy (Memc[spec], Memc[str1], SZ_LINE)
	    wt1 = 1.
	}
	iferr (call dc_gmsdb (Memc[str1], stp, paps1, papcen, pshift,
	    pcoeff1, naps1)) {
	    call sfree (sp)
	    call erract (EA_ERROR)
	}
	call salloc (pshift1, naps1, TY_DOUBLE)
	call amovd (Memd[pshift], Memd[pshift1], naps1)
	if (fd != NULL) {
	    call fprintf (fd, "%s: REFSPEC1 = '%s %.3g'\n")
		call pargstr (Memc[spec])
		call pargstr (Memc[str1])
		call pargd (wt1)
	}

	iferr (call  dc_refshft (Memc[spec], stp, Memc[str1], "REFSHFT1", im,
	    Memi[paps1], Memr[papcen], Memd[pshift1], naps1, fd))
	    ;

	iferr {
            call imgstr (im, "REFSPEC2", Memc[str1], SZ_LINE)
            call sscan (Memc[str1])
            call gargwrd (Memc[str1], SZ_LINE)
            call gargd (wt2)
            if (nscan() == 1)
                wt2 = 1.
            call dc_gmsdb (Memc[str1], stp, paps2, papcen, pshift, pcoeff2,
		naps2)
            call salloc (pshift2, naps2, TY_DOUBLE)
            call amovd (Memd[pshift], Memd[pshift2], naps2)
	    if (fd != NULL) {
		call fprintf (fd, "%s: REFSPEC2 = '%s %.3g'\n")
		    call pargstr (Memc[spec])
		    call pargstr (Memc[str1])
		    call pargd (wt2)
	    }
	    iferr (call  dc_refshft (Memc[spec], stp, Memc[str1],
		"REFSHFT2", im, Memi[paps2], Memr[papcen], Memd[pshift2],
		naps2, fd))
	        ;
	} then
	    wt2 = 0.

	# Enter dispersion function in the MWCS.
	do i = 1, naps {
	    j = AP(ap,i)
	    for (k=0; k<naps1 && Memi[paps1+k]!=j; k=k+1)
		;
	    if (k == naps1)
		for (k=0; k<naps1 && !IS_INDEFI(Memi[paps1+k]); k=k+1)
		    ;
	    if (k == naps1) {
		if (clgetb ("ignoreaps"))
		    k = 0
		else {
		    call sprintf (Memc[str1], SZ_LINE,
			"%s - Missing reference for aperture %d")
			call pargstr (Memc[spec])
			call pargi (j)
		    call fatal (1, Memc[str1])
		}
	    }
	    coeffs = Memi[pcoeff1+k]
	    ncoeffs = nint (Memd[coeffs])
	    l = 20 * (ncoeffs + 2)
	    if (wt2 > 0.)
		l = 2 * l
	    call realloc (coeff, l, TY_CHAR)
	    call aclrc (Memc[coeff], l)
	    sfd = stropen (Memc[coeff], l, NEW_FILE)
	    call fprintf (sfd, "%.3g %g")
		call pargd (wt1)
		call pargd (Memd[pshift1+k])

	    # The following assumes some knowledge of the data structure in
	    # order to shortten the the attribute string.

	    call fprintf (sfd, " %d %d")
		call pargi (nint (Memd[coeffs+1]))
		call pargi (nint (Memd[coeffs+2]))
	    do k = 3, ncoeffs {
		call fprintf (sfd, " %g")
		    call pargd (Memd[coeffs+k])
	    }

	    if (wt2 > 0.) {
		for (k=0; k<naps2 && Memi[paps2+k]!=j; k=k+1)
		    ;
		if (k == naps2)
		    for (k=0; k<naps2 && !IS_INDEFI(Memi[paps2+k]); k=k+1)
			;
		if (k == naps2) {
		    if (clgetb ("ignoreaps"))
			k = 0
		    else {
			call sprintf (Memc[str1], SZ_LINE,
			    "%s - Missing reference for aperture %d")
			    call pargstr (Memc[spec])
			    call pargi (j)
			call strclose (sfd)
			call mfree (coeff, TY_CHAR)
			call sfree (sp)
			call fatal (1, Memc[str1])
		    }
		}
		coeffs = Memi[pcoeff2+k]
		ncoeffs = nint (Memd[coeffs])
		call fprintf (sfd, " %5.3g %g")
		    call pargd (wt2)
		    call pargd (Memd[pshift2+k])
		call fprintf (sfd, " %d %d")
		    call pargi (nint (Memd[coeffs+1]))
		    call pargi (nint (Memd[coeffs+2]))
		do k = 3, ncoeffs {
		    call fprintf (sfd, " %g")
			call pargd (Memd[coeffs+k])
		}
	    }

	    DT(ap,i) = 2
	    call mw_swattrs (mw, 1, "label", "Wavelength")
	    call mw_swattrs (mw, 1, "units", "Angstroms")
	    call shdr_swattrs (mw, PL(ap,i), AP(ap,i), BM(ap,i), DT(ap,i),
		W1(ap,i), DW(ap,i), NW(ap,i), Z(ap,i), LW(ap,i), UP(ap,i),
		Memc[coeff])
	    call strclose (sfd)
	}

done_	# Set aperture parameters in terms of logical image.
	ct = mw_sctran (mw, "physical", "logical", 2)
	do i = 1, naps {
	    j = nint (mw_c1trand (ct, double(1)))
	    k = nint (mw_c1trand (ct, double(NW(ap,i))))
	    NW(ap,i) = min (IM_LEN(im,1), max (j, k))
	}
	call mw_ctfree (ct)

	ct = mw_sctran (mw, "logical", "multispec", 3)
	do i = 1, naps {
	    call mw_c2trand (ct, double(1), double(i), W1(ap,i), wt1)
	    call mw_c2trand (ct, double(NW(ap,i)), double(i), W2(ap,i), wt1)
	    DW(ap,i) = (W2(ap,i) - W1(ap,i)) / (NW(ap,i) - 1)
	}
	call mw_ctfree (ct)

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# DC_GMSDB -- Get a dispersion database entry.
# The database entry is read only once from the database and stored in a
# symbol table keyed by the spectrum name.  Subsequent requests for the
# reference spectrum returns the data from the symbol table.

procedure dc_gmsdb (spec, stp, paps, papcen, pshift, pcoeff, naps)

char	spec[ARB]	# Spectrum image name
pointer	stp		# Symbol table pointer
pointer	paps		# Pointer to aperture numbers
pointer	papcen		# Pointer to aperture centers
pointer	pshift		# Pointer to shifts
pointer	pcoeff		# Pointer to coefficients
int	naps		# Number of apertures

double	dval
int	i, n, dtgeti(), getline(), ctod()
real	low, high, dtgetr()
pointer	sp, str, coeffs, sym, db, dt, stfind(), stenter(), strefsbuf(), dtmap1()
errchk	dtmap1, dtgeti, dtgad

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
	    db = strefsbuf (stp, Memi[stfind (stp, "database")])
	    dt = dtmap1 (Memc[db], Memc[str], READ_ONLY)

	    naps = max (1, DT_NRECS(dt))
	    call calloc (paps, naps, TY_INT)
	    call calloc (papcen, naps, TY_REAL)
	    call calloc (pshift, naps, TY_DOUBLE)
	    call calloc (pcoeff, naps, TY_POINTER)
	    if (DT_NRECS(dt) > 0) {
		for (i = 1; i <= naps; i = i + 1) {
		    iferr (Memi[paps+i-1] = dtgeti (dt, i, "aperture"))
			Memi[paps+i-1] = INDEFI
		    iferr (low = dtgetr (dt, i, "aplow"))
			low = 0.
		    iferr (high = dtgetr (dt, i, "aphigh"))
			high = 0.
		    Memr[papcen+i-1] = (low + high) / 2.
		    iferr (Memd[pshift+i-1] = dtgetr (dt, i, "shift"))
			Memd[pshift+i-1] = 0.
		    n = dtgeti (dt, i, "coefficients")
		    call malloc (coeffs, 1+n, TY_DOUBLE)
		    Memd[coeffs] = n
		    call dtgad (dt, i, "coefficients", Memd[coeffs+1], n, n)
		    Memi[pcoeff+i-1] = coeffs
		}
	    } else {
		Memi[paps] = INDEFI
		Memr[papcen] = INDEFR
		Memd[pshift] = 0.
		call malloc (coeffs, 100, TY_DOUBLE)
		n = 3
		call seek (Memi[dt], BOF)
		while (getline (Memi[dt], Memc[str]) != EOF) {
		    i = 1
		    if (ctod (Memc[str], i, dval) == 0)
			next
		    if (mod (n, 100) == 0)
			call realloc (coeffs, n+100, TY_DOUBLE)
		    Memd[coeffs+n] = dval
		    n = n + 1
		}
		Memd[coeffs] = n - 1
		Memd[coeffs+1] = 5
		Memd[coeffs+2] = n - 2
		Memi[pcoeff] = coeffs
	    }

	    call dtunmap (dt)
	    call sfree (sp)

	    sym = stenter (stp, spec, LEN_DC)
	    DC_FORMAT(sym) = 1
	    DC_PAPS(sym) = paps
	    DC_PAPCEN(sym) = papcen
	    DC_PSHIFT(sym) = pshift
	    DC_PCOEFF(sym) = pcoeff
	    DC_NAPS(sym) = naps
	} else {
	    if (DC_FORMAT(sym) != 1)
		call error (1, "Not a multispec dispersion function")
	    paps = DC_PAPS(sym)
	    papcen = DC_PAPCEN(sym)
	    pshift = DC_PSHIFT(sym)
	    pcoeff = DC_PCOEFF(sym)
	    naps = DC_NAPS(sym)
	}
end


# DC_REFSHFT -- Compute dispersion shift.

procedure dc_refshft (spec, stp, refspec, keywrd, im, aps, apcens, shifts,
	naps, fd)

char	spec[ARB]		# Spectrum to be corrected
pointer	stp			# Symbol table pointer
char	refspec[ARB]		# Reference spectrum
char	keywrd[ARB]		# Header keyword (for log only)
pointer	im			# IMIO pointer to spectrum to be corrected
int	aps[naps]		# Reference apertures
real	apcens[naps]		# Reference aperture centers
double	shifts[naps]		# Reference aperture shifts (to be modified)
int	naps			# Number of refernce apertures
int	fd			# Logfile descriptor

int	i, j, k, pnaps
double	apcen, shift, sumx, sumy, sumxx, sumyy, sumxy, a, b
pointer	sp, refshft, option, paps, papcen, pshift, pcoeff
bool	streq()
errchk	imgstr, dc_gmsdb

begin
	call smark (sp)
	call salloc (refshft, SZ_FNAME, TY_CHAR)
	call salloc (option, SZ_FNAME, TY_CHAR)

	# Parse header parameter.
	call imgstr (im, keywrd, Memc[refshft], SZ_FNAME)
	call sscan (Memc[refshft])
	call gargwrd (Memc[refshft], SZ_FNAME)
	if (streq (Memc[refshft], refspec)) {
	    call sfree (sp)
	    return
	}
	call gargwrd (Memc[option], SZ_FNAME)

	# Get reference shift apertures.
	call dc_gmsdb (Memc[refshft], stp, paps, papcen, pshift, pcoeff, pnaps)
	if (pnaps == 0) {
	    call sfree (sp)
	    return
	}
	
	# Compute mean shift and RMS.
	sumy = 0.
	sumyy = 0.
	do i = 1, pnaps {
	    sumy = sumy + Memd[pshift+i-1]
	    sumyy = sumyy + Memd[pshift+i-1] ** 2
	}
	sumy = sumy / pnaps
	sumyy = sqrt (max (0.D0, sumyy / pnaps - sumy ** 2))

	# Print.
	call printf ("%s: %s = '%s %s', shift = %.6g, rms = %.6g\n")
	    call pargstr (spec)
	    call pargstr (keywrd)
	    call pargstr (Memc[refshft])
	    call pargstr (Memc[option])
	    call pargd (sumy)
	    call pargd (sumyy)
	if (fd != NULL) {
	    call fprintf (fd,
	    "%s: %s = '%s %s', shift = %.6g, rms = %.6g\n")
	        call pargstr (spec)
		call pargstr (keywrd)
	        call pargstr (Memc[refshft])
	        call pargstr (Memc[option])
	        call pargd (sumy)
	        call pargd (sumyy)
	}

	if (streq (Memc[option], "interp")) {
	    if (pnaps > 1) {
	        sumx = 0.
	        sumy = 0.
	        sumxx = 0.
	        sumyy = 0.
	        sumxy = 0.
	        do i = 0, pnaps-1 {
		    apcen = Memr[papcen+i]
		    shift = Memd[pshift+i]
		    sumx = sumx + apcen
		    sumy = sumy + shift
		    sumxx = sumxx + apcen * apcen
		    sumyy = sumyy + shift * shift
		    sumxy = sumxy + apcen * shift
	        }
	        b = pnaps * sumxx - sumx * sumx
	        a = (sumy * sumxx - sumx * sumxy) / b
	        b = (pnaps * sumxy - sumx * sumy) / b
	    } else {
	        a = sumy
	        b = 0.
	    }
	    do i = 1, naps
	        shifts[i] = shifts[i] + a + b * apcens[i]
	    call printf ("\tintercept = %.6g, slope = %.6g\n")
		call pargd (a)
		call pargd (b)
	    if (fd != NULL) {
	        call fprintf (fd,
		    "\tintercept = %.6g, slope = %.6g\n")
		    call pargd (a)
		    call pargd (b)
	    }
	} else if (streq (Memc[option], "nearest")) {
	    do i = 1, naps {
	        k = 0
	        sumy = abs (apcens[i] - Memr[papcen])
	        for (j = 1; j < pnaps; j = j + 1)
		    if (abs (apcens[i] - Memr[papcen+j]) < sumy) {
		        k = j
		        sumy = abs (apcens[i] - Memr[papcen+k])
		    }
	        shifts[i] = shifts[i] + Memd[pshift+k]
		call printf ("\t%4d %7.2f %4d %7.2f %.6g\n")
		    call pargi (aps[i])
		    call pargr (apcens[i])
		    call pargi (Memi[paps+k])
		    call pargr (Memr[papcen+k])
		    call pargd (Memd[pshift+k])
	        if (fd != NULL) {
		    call fprintf (fd,
		        "\t%4d %7.2f %4d %7.2f %.6g\n")
		        call pargi (aps[i])
		        call pargr (apcens[i])
		        call pargi (Memi[paps+k])
		        call pargr (Memr[papcen+k])
		        call pargd (Memd[pshift+k])
	        }
	    }
	} else
	    call aaddkd (shifts, sumy, shifts, naps)

	call sfree (sp)
end


# DC_GEC -- Get an echelle spectrum.  This consists of mapping the image
# and setting a MWCS coordinate transformation.  If not dispersion corrected
# the dispersion function is found in the database for the reference
# spectra and set in the MWCS.

procedure dc_gec (im, mw, stp, ap, fd)

pointer	im		#I IMIO pointer
pointer	mw		#I MWCS pointer
pointer	stp		#I Symbol table
pointer	ap		#O Aperture data structure
int	fd		#I Logfile descriptor

double	wt1, wt2
int	i, j, k, l, dc, sfd, axis[2], naps, ncoeffs, offset, slope
pointer	sp, spec, str1, str2, coeff, coeffs, ct
pointer	pshift1, pshift2, pshift3, pcoeff1, pcoeff2, pcoeff3

double	mw_c1trand()
int	nscan(), stropen()
pointer	mw_sctran()
errchk	dc_gecdb, imgstr

data	axis/1,2/
define	done_	90

begin
	call smark (sp)
	call salloc (spec, SZ_FNAME, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	coeff = NULL

	call imstats (im, IM_IMAGENAME, Memc[spec], SZ_FNAME)

	# Set WCS attributes
	naps = IM_LEN(im,2)
	call malloc (ap, LEN_AP(naps), TY_STRUCT)
	ct = mw_sctran (mw, "logical", "physical", 2)
	do i = 1, naps {
	    PL(ap,i) = nint (mw_c1trand (ct, double(i)))
	    call shdr_gwattrs (mw, PL(ap,i), AP(ap,i), BM(ap,i), DT(ap,i),
		W1(ap,i), DW(ap,i), NW(ap,i), Z(ap,i), LW(ap,i), UP(ap,i),
		coeff)
	    dc = DT(ap,i)
	}
	call mw_ctfree (ct)

	# Determine if spectrum has been dispersion corrected and return
	# This assumes all orders have the same dispersion type
	if (dc > -1)
	    goto done_

	# Get the reference spectra dispersion function from the database
	# and determine a reference shift.  Return a null pointer if no
	# reference spectrum specified.

	iferr {
	    call imgstr (im, "REFSPEC1", Memc[str1], SZ_LINE)
	    call sscan (Memc[str1])
	    call gargwrd (Memc[str1], SZ_LINE)
	    call gargd (wt1)
	    if (nscan() == 1)
		wt1 = 1.
	} then {
	    call strcpy (Memc[spec], Memc[str1], SZ_LINE)
	    wt1 = 1.
	}
	call salloc (pshift1, naps, TY_DOUBLE)
	call salloc (pcoeff1, naps, TY_POINTER)
	slope = 0
	iferr (call dc_gecdb (Memc[str1], stp, ap, Memd[pshift1],
	    Memi[pcoeff1], naps, offset, slope)) {
	    call sfree (sp)
	    call erract (EA_ERROR)
	}
	if (fd != NULL) {
	    call fprintf (fd, "%s: REFSPEC1 = '%s %.3g'\n")
		call pargstr (Memc[spec])
		call pargstr (Memc[str1])
		call pargd (wt1)
	}

        iferr {
            call imgstr (im, "refshft1", Memc[str1], SZ_LINE)
	    call salloc (pshift3, naps, TY_DOUBLE)
	    call salloc (pcoeff3, naps, TY_POINTER)
	    call dc_gecdb (Memc[str1], stp, ap, Memd[pshift3],
		Memi[pcoeff3], naps, offset, slope)
            if (fd != NULL) {
                call fprintf (fd, "%s: REFSHFT1 = '%s', shift = %.6g\n")
                    call pargstr (Memc[spec])
                    call pargstr (Memc[str1])
                    call pargd (Memd[pshift3])
            }
	    call aaddd (Memd[pshift1], Memd[pshift3], Memd[pshift1], naps)
        } then
            ;

	iferr {
            call imgstr (im, "REFSPEC2", Memc[str1], SZ_LINE)
            call sscan (Memc[str1])
            call gargwrd (Memc[str1], SZ_LINE)
            call gargd (wt2)
            if (nscan() == 1)
                wt2 = 1.
	    call salloc (pshift2, naps, TY_DOUBLE)
	    call salloc (pcoeff2, naps, TY_POINTER)
	    call dc_gecdb (Memc[str1], stp, ap, Memd[pshift2],
		Memi[pcoeff2], naps, offset, slope)
	    if (fd != NULL) {
		call fprintf (fd, "%s: REFSPEC2 = '%s %.3g'\n")
		    call pargstr (Memc[spec])
		    call pargstr (Memc[str1])
		    call pargd (wt2)
	    }

	    iferr {
		call imgstr (im, "refshft2", Memc[str1], SZ_LINE)
		call salloc (pshift3, naps, TY_DOUBLE)
		call salloc (pcoeff3, naps, TY_POINTER)
		call dc_gecdb (Memc[str1], stp, ap, Memd[pshift3],
		    Memi[pcoeff3], naps, offset, slope)
		if (fd != NULL) {
		    call fprintf (fd, "%s: REFSHFT2 = '%s', shift = %.6g\n")
			call pargstr (Memc[spec])
                    call pargstr (Memc[str1])
                    call pargd (Memd[pshift3])
		}
		call aaddd (Memd[pshift1], Memd[pshift3], Memd[pshift1], naps)
	    } then
		;
	} then
	    wt2 = 0.

	# Enter dispersion function in the MWCS.
	do i = 1, naps {
	    coeffs = Memi[pcoeff1+i-1]
	    ncoeffs = nint (Memd[coeffs])
	    l = 20 * (ncoeffs + 2)
	    if (wt2 > 0.)
		l = 2 * l
	    call realloc (coeff, l, TY_CHAR)
	    call aclrc (Memc[coeff], l)
	    sfd = stropen (Memc[coeff], l, NEW_FILE)
	    call fprintf (sfd, "%.3g %g")
		call pargd (wt1)
		call pargd (Memd[pshift1+i-1])

	    # The following assumes some knowledge of the data structure in
	    # order to shortten the the attribute string.

	    call fprintf (sfd, " %d %d %.3g %.3g")
		call pargi (nint (Memd[coeffs+1]))
		call pargi (nint (Memd[coeffs+2]))
		call pargd (Memd[coeffs+3])
		call pargd (Memd[coeffs+4])
	    do j = 5, ncoeffs {
		call fprintf (sfd, " %g")
		    call pargd (Memd[coeffs+j])
	    }

	    if (wt2 > 0.) {
		coeffs = Memi[pcoeff2+i-1]
		ncoeffs = nint (Memd[coeffs])
		call fprintf (sfd, "%5.3g %g")
		    call pargd (wt2)
		    call pargd (Memd[pshift2+i-1])
		call fprintf (sfd, " %d %d %.3g %.3g")
		    call pargi (nint (Memd[coeffs+1]))
		    call pargi (nint (Memd[coeffs+2]))
		    call pargd (Memd[coeffs+3])
		    call pargd (Memd[coeffs+4])
		do j = 5, ncoeffs {
		    call fprintf (sfd, " %g")
			call pargd (Memd[coeffs+j])
		}
	    }

	    DT(ap,i) = 2
	    call shdr_swattrs (mw, PL(ap,i), AP(ap,i), BM(ap,i), DT(ap,i),
		W1(ap,i), DW(ap,i), NW(ap,i), Z(ap,i), LW(ap,i), UP(ap,i),
		Memc[coeff])
	    call strclose (sfd)
	}

done_	# Set WCS parameters in terms of logical image.
        ct = mw_sctran (mw, "physical", "logical", 2)
        do i = 1, naps {
            j = nint (mw_c1trand (ct, double(1)))
            k = nint (mw_c1trand (ct, double(NW(ap,i))))
            NW(ap,i) = min (IM_LEN(im,1), max (j, k))
        }
        call mw_ctfree (ct)

	ct = mw_sctran (mw, "logical", "multispec", 3)
	do i = 1, naps {
	    call mw_c2trand (ct, double(1), double(i), W1(ap,i), wt1)
	    call mw_c2trand (ct, double(NW(ap,i)), double(i), W2(ap,i), wt1)
	    DW(ap,i) = (W2(ap,i) - W1(ap,i)) / (NW(ap,i) - 1)
	}
	call mw_ctfree (ct)

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# DC_GECDB -- Get a dispersion database entry.
# The database entry is read only once from the database and stored in a
# symbol table keyed by the spectrum name.  Subsequent requests for the
# reference spectrum returns the data from the symbol table.

procedure dc_gecdb (spec, stp, ap, shifts, pcoeff, naps, offset, slope)

char	spec[ARB]	# Spectrum image name
pointer	stp		# Symbol table pointer
pointer	ap		# Aperture data structure
double	shifts[naps]	# Shifts
pointer	pcoeff[naps]	# Pointer to coefficients
int	naps		# Number of apertures
int     offset          # Aperture to order offset
int     slope           # Aperture to order slope

double  shift
real    dtgetr()
int     i, rec, offst, slpe, n, dtlocate(), dtgeti()
pointer sp, str, coeffs, sym, db, dt, stfind(), stenter(), strefsbuf(), dtmap1()
errchk  dtmap1, dtlocate, dtgeti, dtgad

begin
	# Check if dispersion solution is in the symbol table from a previous
	# call.  If not in the symbol table get it from the database and
	# store it in the symbol table.

	sym = stfind (stp, spec)
	if (sym == NULL) {
	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call strcpy ("ec", Memc[str], SZ_LINE)
	    call imgcluster (spec, Memc[str+2], SZ_LINE)
	    db = strefsbuf (stp, Memi[stfind (stp, "database")])
	    dt = dtmap1 (Memc[db], Memc[str], READ_ONLY)

            call sprintf (Memc[str], SZ_LINE, "ecidentify %s")
                call pargstr (spec)
            rec = dtlocate (dt, Memc[str])
            if (rec == EOF)
                call fatal (0, "Ecdispcor: Database entry not found")

            iferr (offst = dtgeti (dt, rec, "offset"))
                offst = 0
            iferr (slpe = dtgeti (dt, rec, "slope"))
                slpe = 1
            iferr (shift = dtgetr (dt, rec, "shift"))
                shift = 0.
            n = dtgeti (dt, rec, "coefficients")
            call malloc (coeffs, n, TY_DOUBLE)
            call dtgad (dt, rec, "coefficients", Memd[coeffs], n, n)

            sym = stenter (stp, spec, LEN_DC)
	    DC_FORMAT(sym) = 2
            DC_OFFSET(sym) = offst
            DC_SLOPE(sym) = slpe
            DC_SHIFT(sym) = shift
            DC_COEFFS(sym) = coeffs

	    call dtunmap (dt)
	    call sfree (sp)
	} else {
	    if (DC_FORMAT(sym) != 2)
		call error (1, "Not an echelle dispersion function")
            offst = DC_OFFSET(sym)
            slpe = DC_SLOPE(sym)
            coeffs = DC_COEFFS(sym)
            shift = DC_SHIFT(sym)
	}

	# Check aperture to order parameters.
	if (slope == 0) {
	    offset = offst
	    slope = slpe
	} else if (offset != offst || slope != slpe) {
	    call fatal (1,
		"Reference order offset/slope parameters don't agree")
	}

	# Convert to multispec coefficients
	do i = 1, naps {
	    BM(ap,i) = offset + slope * AP(ap,i)
	    call dc_ecms (BM(ap,i), Memd[coeffs], pcoeff[i])
	    shifts[i] = shift / BM(ap,i)
	}
end


# DC_ECMS -- Convert echelle dispersion coefficients to multispec coefficients

procedure dc_ecms (order, eccoeff, mscoeff)

int	order			# Echelle order
double	eccoeff[ARB]		# Echelle dispersion coefficients
pointer	mscoeff			# Pointer to multispec coefficients

int	i, j, k, type, xorder, yorder
double	xmin, xmax, ymin, ymax, ymaxmin, yrange, y, coeff, a, b, c

begin
	type = nint (eccoeff[1])
	xorder = nint (eccoeff[2])
	yorder = nint (eccoeff[3])
	xmin = eccoeff[5]
	xmax = eccoeff[6]
	ymin = eccoeff[7]
	ymax = eccoeff[8]

	yrange = 2. / (ymax - ymin)
	ymaxmin = (ymax + ymin) / 2
	y = (order - ymaxmin) * yrange

	call malloc (mscoeff, 5+xorder, TY_DOUBLE)
	Memd[mscoeff] = 4+xorder
	Memd[mscoeff+1] = type
	Memd[mscoeff+2] = xorder
	Memd[mscoeff+3] = xmin
	Memd[mscoeff+4] = xmax

	switch (type) {
	case 1:
	    do k = 1, xorder {
		j = 9 + k - 1
		coeff = eccoeff[j]
		if (yorder > 1) {
		    j = j + xorder
		    coeff = coeff + eccoeff[j] * y
		}
		if (yorder > 2) {
		    a = 1
		    b = y
		    do i = 3, yorder {
			c = 2 * y * b - a
			j = j + xorder
			coeff = coeff + eccoeff[j] * c
			a = b
			b = c
		    }
		}
		Memd[mscoeff+4+k] = coeff / order
	    }
	case 2:
	    do k = 1, xorder {
		j = 9 + k - 1
		coeff = eccoeff[j]
		if (yorder > 1) {
		    j = j + xorder
		    coeff = coeff + eccoeff[j] * y
		}
		if (yorder > 2) {
		    a = 1
		    b = y
		    do i = 3, yorder {
			c = ((2 * i - 3) * y * b - (i - 2) * a) / (i - 1)
			j = j + xorder
			coeff = coeff + eccoeff[j] * c
			a = b
			b = c
		    }
		}
		Memd[mscoeff+4+k] = coeff / order
	    }
	}
end
