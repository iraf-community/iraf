include <ctype.h>
include	<error.h>
include	<mach.h>

define	STD_TYPES	"|star|blackbody|"
define	UNKNOWN		0	# Unknown calibration file type
define	STAR		1	# Standard star calibration file
define	BLACKBODY	2	# Blackbody calibration file

define	NALLOC	128	# Allocation block size

# GETCALIB -- Get flux data.
# This is either for a blackbody of specified magnitude and type or
# a specified standard star with calibration data in a database directory.

procedure getcalib (waves, dwaves, mags, nwaves)

pointer	waves			#O Pointer to calibration wavelengths
pointer	dwaves			#O Pointer to calibration bandpasses
pointer	mags			#O Pointer to calibration magnitudes
int	nwaves			#O Number of calibration points

real	weff, wave, mag, dwave, wave1, wave2
int	i, j, fd, nalloc
pointer	sp, dir, star, name, file, type, units, band, str
pointer	un, unang

bool	streq()
int	open(), fscan(), nscan(), getline(), strdic()
pointer	un_open()
errchk	getbbcal, open, un_open, un_ctranr
define	getstd_	10

begin
	call smark (sp)
	call salloc (dir, SZ_FNAME, TY_CHAR)
	call salloc (star, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (file, SZ_LINE, TY_CHAR)
	call salloc (type, SZ_LINE, TY_CHAR)
	call salloc (units, SZ_LINE, TY_CHAR)
	call salloc (band, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	Memc[str] = EOS

	# Convert the star name to a file name and open the file.
	# If an error occurs print a list of files.

getstd_	call clgstr ("caldir", Memc[dir], SZ_FNAME)
	call clgstr ("star_name", Memc[star], SZ_FNAME)

	call strcpy (Memc[star], Memc[name], SZ_LINE)
	call strlwr (Memc[name])
	j = name
	for (i=name; Memc[i]!=EOS; i=i+1) {
	    if (IS_WHITE(Memc[i]) || Memc[i]=='+' || Memc[i]=='-')
		next
	    Memc[j] = Memc[i]
	    j = j + 1
	}
	Memc[j] = EOS

	# Check if this is an alternate name.
	call sprintf (Memc[file], SZ_LINE, "%snames.men")
	    call pargstr (Memc[dir])
	ifnoerr (fd = open (Memc[file], READ_ONLY, TEXT_FILE)) {
	    while (fscan (fd) != EOF) {
		call gargwrd (Memc[file], SZ_LINE)
		if (streq (Memc[file], Memc[name])) {
		    call gargwrd (Memc[name], SZ_LINE)
		    break
		}
	    }
	}

	call sprintf (Memc[file], SZ_LINE, "%s%s.dat")
	    call pargstr (Memc[dir])
	    call pargstr (Memc[name])

	iferr (fd = open (Memc[file], READ_ONLY, TEXT_FILE)) {
	    if (streq (Memc[file], Memc[str]))
		call erract (EA_ERROR)
	    call strcpy (Memc[file], Memc[str], SZ_LINE)
	    call sprintf (Memc[file], SZ_LINE, "%sstandards.men")
		call pargstr (Memc[dir])
	    fd = open (Memc[file], READ_ONLY, TEXT_FILE)
	    while (getline (fd, Memc[file]) != EOF)
		call putline (STDERR, Memc[file]) 
	    call close (fd)
	    Memc[star] = EOS
	    goto getstd_
	}

	# Read the calibration data.
	type = STAR
	call strcpy ("angstroms", Memc[units], SZ_LINE)
	Memc[band] = EOS
	weff = INDEFR

	nalloc = 0
	nwaves = 0
	while (fscan (fd) != EOF) {

	    # Check for comments and parameters.
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan() != 1)
		next
	    if (Memc[str] == '#') {
		call gargwrd (Memc[str], SZ_LINE)
		call strlwr (Memc[str])
		if (streq (Memc[str], "type")) {
		    call gargwrd (Memc[str], SZ_LINE)
		    type = strdic (Memc[str], Memc[str], SZ_LINE, STD_TYPES)
		} else if (streq (Memc[str], "units"))
		    call gargwrd (Memc[units], SZ_LINE)
		else if (streq (Memc[str], "band"))
		    call gargwrd (Memc[band], SZ_LINE)
		else if (streq (Memc[str], "weff"))
		    call gargr (weff)
		next
	    }
	    call reset_scan ()

	    # Read data.
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

	# This routine returns wavelengths in Angstroms.
	un = un_open (Memc[units])
	unang = un_open ("Angstroms")
	call un_ctranr (un, unang, weff, weff, 1)
	do i = 1, nwaves {
	    wave = Memr[waves+i-1]
	    dwave = Memr[dwaves+i-1]
	    wave1 = wave - dwave / 2
	    wave2 = wave + dwave / 2
	    call un_ctranr (un, unang, wave1, wave1, 1)
	    call un_ctranr (un, unang, wave2, wave2, 1)
	    wave = (wave1 + wave2) / 2.
	    dwave = abs (wave1 - wave2)
	    Memr[waves+i-1] = wave
	    Memr[dwaves+i-1] = dwave
	}
	call un_close (un)
	call un_close (unang)

	switch (type) {
	case UNKNOWN:
	    call freecalib (waves, dwaves, mags)
	    call error (1, "Unknown calibration type")
	case BLACKBODY:
	    call getbbcal (Memr[waves], Memr[mags], nwaves, Memc[band],
		weff, Memc[dir])
	}

	call sfree (sp)
end


# GETBBCAL -- Get blackbody calibration data.

procedure getbbcal (waves, mags, nwaves, band, weff, caldir)

real	waves[nwaves]		#I Calibration wavelengths
real	mags[nwaves]		#I Calibration magnitudes
int	nwaves			#I Number of calibration points
char	band[ARB]		#I Bandpass of data
real	weff			#I Effective wavelength
char	caldir[ARB]		#I Calibration directory

int	i, j, col1, col2, fd
real	mag, m1, m2, dm, teff, t, dt
pointer	sp, bands, magband, sptype, default, fname, str

bool	streq(), strne()
int	clgwrd(), nowhite(), ctor(), strdic(), strncmp()
int	open(), fscan(), nscan()
real	clgetr()
errchk	open

begin
	if (band [1] == EOS || IS_INDEFR(weff))
	    call error (1,
	    "Blackbody calibration file has no band or effective wavelength")

	call smark (sp)
	call salloc (bands, SZ_LINE, TY_CHAR)
	call salloc (magband, SZ_LINE, TY_CHAR)
	call salloc (sptype, SZ_LINE, TY_CHAR)
	call salloc (default, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Create list of acceptable magnitudes.
	call sprintf (Memc[bands], SZ_LINE, "|")
	call sprintf (Memc[fname], SZ_FNAME, "%sparams.dat")
	    call pargstr (caldir)
	ifnoerr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
	    while (fscan (fd) != EOF) {
		call gargwrd (Memc[str], SZ_LINE)
		if (Memc[str] != '#')
		    next
		call gargwrd (Memc[str], SZ_LINE)
		if (strne (Memc[str], "Type"))
		    next

		call gargwrd (Memc[str], SZ_LINE)
		j = nscan()
		repeat {
		    i = j
		    call gargwrd (Memc[str], SZ_LINE)
		    j = nscan()
		    if (i == j)
			break
		    call strcat (Memc[str], Memc[bands], SZ_LINE)
		    call strcat ("|", Memc[bands], SZ_LINE)
		}
		break
	    }
	    call close (fd)
	}
	col1 = strdic (band, Memc[str], SZ_LINE, Memc[bands]) + 2
	if (col1 == 2) {
	    call strcat (band, Memc[bands], SZ_LINE)
	    call strcat ("|", Memc[bands], SZ_LINE)
	}
	col1 = strdic (band, Memc[str], SZ_LINE, Memc[bands]) + 2
	call clpstr ("magband.p_min", Memc[bands])

	# Get blackbody parameters.
	mag = clgetr ("mag")
	col2 = clgwrd ("magband", Memc[magband], SZ_LINE, Memc[bands]) + 2
	call clgstr ("teff", Memc[sptype], SZ_LINE)
	i = nowhite (Memc[sptype], Memc[sptype], SZ_LINE)

	# Convert spectral type to effective temperature.
	i = 1
	if (ctor (Memc[sptype], i, teff) == 0) {
	    teff = INDEFR
	    call sprintf (Memc[fname], SZ_FNAME, "%sparams.dat")
		call pargstr (caldir)
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    while (fscan (fd) != EOF) {
		call gargwrd (Memc[str], SZ_FNAME)
		if (strncmp (Memc[str], Memc[sptype], 2) != 0)
		    next
		call gargr (t)
		if (nscan() < 2)
		    next

		call strcpy (Memc[str], Memc[default], SZ_LINE)
		teff = t

		if (streq (Memc[default], Memc[sptype]))
		    break
	    }
	    call close (fd)

	    if (IS_INDEFR(teff))
		call error (1, "Failed to determine effective temperature")
	    if (strne (Memc[default], Memc[sptype])) {
		call eprintf ("WARNING: Effective temperature for %s not found")
		    call pargstr (Memc[sptype])
		call eprintf (" - using %s\n")
		    call pargstr (Memc[default])
		call strcpy (Memc[default], Memc[sptype], SZ_LINE)
	    }
	} else
	    Memc[sptype] = EOS

	# Transform the input magnitude from the input passband to the
	# data passband if necessary.
	if (strne (Memc[magband], band)) {
	    
	    # Get spectral type if necessary.
	    if (Memc[sptype] == EOS) {
		dt = MAX_REAL
		call sprintf (Memc[fname], SZ_FNAME, "%sparams.dat")
		    call pargstr (caldir)
		fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
		while (fscan (fd) != EOF) {
		    call gargwrd (Memc[str], SZ_FNAME)
		    if (Memc[str+2] != 'V')
			next
		    call gargr (t)
		    if (nscan() < 2)
			next
		    if (abs (t - teff) < dt) {
			dt = abs (t - teff)
			call strcpy (Memc[str], Memc[sptype], SZ_LINE)
		    }
		}
		call close (fd)

		if (Memc[sptype] == EOS)
		    call error (1, "Failed to determine spectral type")
		call eprintf ("WARNING: Assuming spectral type of %s\n")
		    call pargstr (Memc[sptype])
	    }

	    # Get magnitude correction.
	    dm = INDEFR
	    call sprintf (Memc[fname], SZ_FNAME, "%sparams.dat")
		call pargstr (caldir)
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    while (fscan (fd) != EOF) {
		call gargwrd (Memc[str], SZ_LINE)
		if (strncmp (Memc[str], Memc[sptype], 2) != 0)
		    next

		call gargr (t)

		m1 = INDEFR
		m2 = INDEFR
		do i = 1, max (col1, col2) {
		    call gargr (t)
		    if (i == col1)
			m1 = t
		    if (i == col2)
			m2 = t
		}

		if (!IS_INDEFR(m1) && !IS_INDEFR(m2)) {
		    call strcpy (Memc[str], Memc[default], SZ_LINE)
		    dm = m1 - m2
		    if (streq (Memc[default], Memc[sptype]))
			break
		}
	    }
	    call close (fd)

	    if (IS_INDEFR(dm)) {
		call sprintf (Memc[str], SZ_LINE,
		"No information in %s to convert %s mag to %s mag for %s star")
		    call pargstr (Memc[fname])
		    call pargstr (Memc[magband])
		    call pargstr (band)
		    call pargstr (Memc[sptype])
		call error (1, Memc[str])
	    }
	    if (strne (Memc[default], Memc[sptype])) {
		call eprintf (
		"WARNING: Converting %s mag to %s mag using spectral type %s\n")
		    call pargstr (Memc[magband])
		    call pargstr (band)
		    call pargstr (Memc[default])
	    }

	    mag = mag + dm

	    call eprintf ("Blackbody: %s = %.2f, %s = %.2f, Teff = %d\n")
		call pargstr (Memc[magband])
		call pargr (mag - dm)
		call pargstr (band)
		call pargr (mag)
		call pargr (teff)

	} else {
	    call eprintf ("Blackbody: %s = %.2f, Teff = %d\n")
		call pargstr (band)
		call pargr (mag)
		call pargr (teff)
	}

	# Convert the calibration magnitudes to the specified magnitude and
	# apply the blackbody function.
	m1 = -2.5 * log10 (weff**3 * (exp(1.4387E8/(weff*teff)) - 1))
	do i = 1, nwaves
	    mags[i] = mags[i] + mag + m1 +
		2.5 * log10 (waves[i]**3 * (exp(1.4387E8/(waves[i]*teff)) - 1))

	call sfree (sp)
end


# FREECALIB -- Free calibration data arrays.

procedure freecalib (waves, dwaves, mags)

pointer	waves, dwaves, mags

begin
	call mfree (waves, TY_REAL)
	call mfree (dwaves, TY_REAL)
	call mfree (mags, TY_REAL)
end
