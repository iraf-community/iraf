include <ctype.h>

define	VLIGHT	2.997925e18	# Speed of light in Angstroms/sec

# Options
define	OPTION	"|ext|mags|fnu|flam|bands|stars|"
define	EXT	1	# Extinction
define	MAGS	2	# Standard star magnitudes
define	FNU	3	# Standard star fluxes
define	FLAM	4	# Standard star fluxes
define	BANDS	5	# Standard star band passes
define	STARS	6	# Standard stars

# T_LCALIB -- List information in calibration file:
#		1) Extinction vs wavelength
#		2) Magnitude vs wavelength
#		3) F-nu vs wavelength
#		4) F-lambda vs wavelength
#		5) Bandpass vs wavelength
#		6) Standard stars

procedure t_lcalib ()

int	i, nwaves, fd
real	fnu, flam, fnuzero
pointer	sp, str, file, list, waves, bands, mags, extns

int	fntgfn(), getline(), open(), clgwrd()
real	clgetr()
pointer	fntopn()
errchk	ext_load, getlcalib

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (file, SZ_LINE, TY_CHAR)

	#Switch on the option.
	switch (clgwrd ("option", Memc[str], SZ_LINE, OPTION)) {
	case EXT:
	    call ext_load (waves, extns, nwaves)
	    do i = 1, nwaves {
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (Memr[extns+i-1])
	    }
	    call ext_free (waves, extns)
	case MAGS:
	    call getlcalib (waves, bands, mags, nwaves)
	    do i = 1, nwaves {
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (Memr[mags+i-1])
	    }
	    call freelcalib (waves, bands, mags)
	case FNU:
	    fnuzero = clgetr ("fnuzero")
	    call getlcalib (waves, bands, mags, nwaves)
	    do i = 1, nwaves {
		fnu = fnuzero * 10. ** (-0.4 * Memr[mags+i-1])
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (fnu)
	    }
	    call freelcalib (waves, bands, mags)
	case FLAM:
	    fnuzero = clgetr ("fnuzero")
	    call getlcalib (waves, bands, mags, nwaves)
	    do i = 1, nwaves {
		fnu = fnuzero * 10. ** (-0.4 * Memr[mags+i-1])
		flam = fnu * VLIGHT /  Memr[waves+i-1] ** 2
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (flam)
	    }
	    call freelcalib (waves, bands, mags)
	case BANDS:
	    call getlcalib (waves, bands, mags, nwaves)
	    do i = 1, nwaves {
	        call printf ("%6f %12.5g\n")
		    call pargr (Memr[waves+i-1])
		    call pargr (Memr[bands+i-1])
	    }
	    call freelcalib (waves, bands, mags)
	case STARS:
	    call clgstr ("caldir", Memc[str], SZ_LINE)
	    call strcat ("*.dat", Memc[str], SZ_LINE)
	    list = fntopn (Memc[str])
	    while (fntgfn (list, Memc[file], SZ_LINE) != EOF) {
		iferr (fd = open (Memc[file], READ_ONLY, TEXT_FILE))
		    next
		if (getline (fd, Memc[str]) != EOF) {
		    call printf ("%s:%s")
			call pargstr (Memc[file])
			call pargstr (Memc[str+1])
		}
		call close (fd)
	    }
	    call fntcls (list)
	default:
	    call eprintf ("Unknown option: %s\n")
		call pargstr (Memc[str])
	}

	call sfree (sp)
end


define	NALLOC	128	# Allocation block size

# GETLCALIB -- Read flux calib data for specified star from database directory.

procedure getlcalib (waves, dwaves, mags, nwaves)

pointer	waves, dwaves, mags
int	nwaves

real	wave, mag, dwave
int	i, j, fd, nalloc
pointer	sp, dir, star, file

int	open(), fscan(), nscan()
errchk	open

begin
	call smark (sp)
	call salloc (dir, SZ_FNAME, TY_CHAR)
	call salloc (star, SZ_FNAME, TY_CHAR)
	call salloc (file, SZ_FNAME, TY_CHAR)

	# Convert the star name to a file name and open the file.
	call clgstr ("caldir", Memc[dir], SZ_FNAME)
	call clgstr ("star_name", Memc[star], SZ_FNAME)
	call sprintf (Memc[file], SZ_FNAME, "%s%s.dat")
	    call pargstr (Memc[dir])
	    call pargstr (Memc[star])
	call strlwr (Memc[file])
	j = file
	for (i=file; Memc[i]!=EOS; i=i+1) {
	    if (IS_WHITE(Memc[i]) || Memc[i]=='+' || Memc[i]=='-')
		next
	    Memc[j] = Memc[i]
	    j = j + 1
	}
	Memc[j] = EOS
	fd = open (Memc[file], READ_ONLY, TEXT_FILE)

	# Read the calibration data.
	nalloc = 0
	nwaves = 0
	while (fscan (fd) != EOF) {
	    call gargr (wave)
	    call gargr (mag)
	    call gargr (dwave)
	    if (nscan() != 3)
		next

	    if (nalloc == 0) {
		nalloc = nalloc + NALLOC
		call malloc (waves, nalloc, TY_REAL)
		call malloc (mags, nalloc, TY_REAL)
		call malloc (dwaves, nalloc, TY_REAL)
	    } else if (nwaves == nalloc) {
		nalloc = nalloc + NALLOC
		call realloc (waves, nalloc, TY_REAL)
		call realloc (mags, nalloc, TY_REAL)
		call realloc (dwaves, nalloc, TY_REAL)
	    }

	    Memr[waves+nwaves] = wave
	    Memr[mags+nwaves] = mag
	    Memr[dwaves+nwaves] = dwave
	    nwaves = nwaves + 1
	}
	call close (fd)

	if (nwaves == 0)
	    call error (1, "No calibration data found")

	call realloc (waves, nwaves, TY_REAL)
	call realloc (mags, nwaves, TY_REAL)
	call realloc (dwaves, nwaves, TY_REAL)

	call sfree (sp)
end


# FREELCALIB -- Free calibration data arrays.

procedure freelcalib (waves, dwaves, mags)

pointer	waves, dwaves, mags

begin
	call mfree (waves, TY_REAL)
	call mfree (dwaves, TY_REAL)
	call mfree (mags, TY_REAL)
end
