include <ctype.h>
include	<error.h>

define	NALLOC	128	# Allocation block size

# GETCALIB -- Read flux calib data for specified star from database directory.

procedure getcalib (waves, dwaves, mags, nwaves)

pointer	waves, dwaves, mags
int	nwaves

real	wave, mag, dwave
int	i, j, fd, nalloc
pointer	sp, dir, star, name, file, temp

bool	streq()
int	open(), fscan(), nscan(), getline()
errchk	open
define	getstd_	10

begin
	call smark (sp)
	call salloc (dir, SZ_FNAME, TY_CHAR)
	call salloc (star, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (file, SZ_LINE, TY_CHAR)
	call salloc (temp, SZ_LINE, TY_CHAR)
	Memc[temp] = EOS

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
	    if (streq (Memc[file], Memc[temp]))
		call erract (EA_ERROR)
	    call strcpy (Memc[file], Memc[temp], SZ_LINE)
	    call sprintf (Memc[file], SZ_LINE, "%sstandards.men")
		call pargstr (Memc[dir])
	    fd = open (Memc[file], READ_ONLY, TEXT_FILE)
	    while (getline (fd, Memc[file]) != EOF)
		call putline (STDERR, Memc[file]) 
	    call close (fd)
	    goto getstd_
	}

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


# FREECALIB -- Free calibration data arrays.

procedure freecalib (waves, dwaves, mags)

pointer	waves, dwaves, mags

begin
	call mfree (waves, TY_REAL)
	call mfree (dwaves, TY_REAL)
	call mfree (mags, TY_REAL)
end
