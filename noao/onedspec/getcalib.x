include <ctype.h>

define	NALLOC	128	# Allocation block size

# GETCALIB -- Read flux calib data for specified star from database directory.

procedure getcalib (waves, dwaves, mags, nwaves)

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


# FREECALIB -- Free calibration data arrays.

procedure freecalib (waves, dwaves, mags)

pointer	waves, dwaves, mags

begin
	call mfree (waves, TY_REAL)
	call mfree (dwaves, TY_REAL)
	call mfree (mags, TY_REAL)
end
