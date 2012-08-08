include	<error.h>
include	<math.h>

# Group type definitions.
define	GROUPS	"|position|title|date|ccdtype|subset|"
define	POSITION	1	# Group by position
define	TITLE		2	# Group by title
define	DATE		3	# Group by date
define	CCDTYPE		4	# Group by ccdtype
define	SUBSET		5	# Group by subset

define	NALLOC		10	# Allocate memory in this size block

# T_CCDGROUPS -- Group images into files based on parameters with common values.
# The output consists of files containing the image names of images from the
# input image list which have the same group type such as position, date,
# or title.

procedure t_ccdgroups ()

int	images			# List of images
pointer	root			# Output group root name
int	group			# Group type
real	radius			# Position radius
bool	verbose			# Verbose output (package parameter)

int	ngroup, fd, ntitles, npositions, ndates, ccdtype
pointer	im, sp, image, output, suffix, titles, positions, dates

bool	clgetb()
real	clgetr()
int	position_group(), title_group(), date_group()
int	imtopenp(), imtgetim(), open(), clgwrd()
errchk	set_input, position_group, title_group, date_group, open

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (suffix, SZ_FNAME, TY_CHAR)

	# Get the task parameters.
	images = imtopenp ("images")
	call clgstr ("output", Memc[root], SZ_FNAME)
	group = clgwrd ("group", Memc[image], SZ_FNAME, GROUPS)
	radius = clgetr ("radius")
	call clgstr ("instrument", Memc[image], SZ_FNAME)
	call hdmopen (Memc[image])
	verbose = clgetb ("verbose")

	# Loop through the images and place them into groups.
	positions = NULL
	npositions = 0
	titles = NULL
	ntitles = 0
	dates = NULL
	ndates = 0
	while (imtgetim (images, Memc[image], SZ_FNAME) != EOF) {
	    call set_input (Memc[image], im, ccdtype)
	    if (im == NULL)
		next

	    iferr {
		switch (group) {
	        case POSITION:
		    ngroup = position_group (im, positions, npositions, radius)
	        case TITLE:
	            ngroup = title_group (im, titles, ntitles)
	        case DATE:
	            ngroup = date_group (im, dates, ndates)
	        }

		# Define the output group file.
		switch (group) {
		case POSITION, TITLE, DATE:
	            call sprintf (Memc[output], SZ_FNAME, "%s%d")
		        call pargstr (Memc[root])
		        call pargi (ngroup)
		case CCDTYPE:
		    call ccdtypes (im, Memc[suffix], SZ_FNAME)
		    call sprintf (Memc[output], SZ_FNAME, "%s%d")
			call pargstr (Memc[root])
			call pargstr (Memc[suffix])
		case SUBSET:
		    call ccdsubset (im, Memc[suffix], SZ_FNAME)
		    call sprintf (Memc[output], SZ_FNAME, "%s%d")
			call pargstr (Memc[root])
			call pargstr (Memc[suffix])
		}

		# Print the operation if verbose.
		if (verbose) {
	            call printf ("%s --> %s\n")
		        call pargstr (Memc[image])
		        call pargstr (Memc[output])
		}

		# Enter the image in the appropriate group file.
	        fd = open (Memc[output], APPEND, TEXT_FILE)
	        call fprintf (fd, "%s\n")
		    call pargstr (Memc[image])
	        call close (fd)
	    } then
		 call erract (EA_WARN)

	    call imunmap (im)
	}

	# Finish up.
	call imtclose (images)
	if (positions != NULL)
	    call mfree (positions, TY_REAL)
	if (titles != NULL)
	    call mfree (titles, TY_CHAR)
	if (dates != NULL)
	    call mfree (dates, TY_CHAR)
	call sfree (sp)
end


# TITLE_GROUP -- Group images by title.

int procedure title_group (im, titles, ntitles)

pointer	im			# Image
pointer	titles			# Pointer to title strings
int	ntitles			# Number of titles

int	i, nalloc
pointer	sp, title, ptr
bool	streq()
errchk	hdmgstr

begin
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	call hdmgstr (im, "title", Memc[title], SZ_LINE)

	for (i=1; i<=ntitles; i=i+1) {
	    ptr = titles + (i - 1) * SZ_LINE
	    if (streq (Memc[title], Memc[ptr]))
		break
	}
	if (i > ntitles) {
	    if (i == 1) {
		nalloc = NALLOC
		call malloc (titles, nalloc * SZ_LINE, TY_CHAR)
	    } else if (i > nalloc) {
		nalloc = nalloc + NALLOC
		call realloc (titles, nalloc * SZ_LINE, TY_CHAR)
	    }
	    ptr = titles + (i - 1) * SZ_LINE
	    call strcpy (Memc[title], Memc[ptr], SZ_LINE-1)
	    ntitles = i
	}

	call sfree (sp)
	return (i)
end


# POSITION_GROUP -- Group by RA and DEC position.  The RA is in hours and
# the DEC is in degrees.  The radius is in seconds of arc.

int procedure position_group (im, positions, npositions, radius)

pointer	im			# Image
pointer	positions		# Positions
int	npositions		# Number of positions
real	radius			# Matching radius

real	ra, dec, dra, ddec, r, hdmgetr()
int	i, nalloc
pointer	ptr
errchk	hdmgetr

begin
	ra = hdmgetr (im, "ra")
	dec = hdmgetr (im, "dec")

	for (i=1; i<=npositions; i=i+1) {
	    ptr = positions + 2 * i - 2
	    dra = ra - Memr[ptr]
	    ddec = dec - Memr[ptr+1]
	    if (dra > 12.)
		dra = dra - 24.
	    if (dra < -12.)
		dra = dra + 24.
	    dra = dra * cos (DEGTORAD (dec)) * 15.
	    r = sqrt (dra ** 2 + ddec ** 2) * 3600.
	    if (r < radius)
		break
	}
	if (i > npositions) {
	    if (i == 1) {
		nalloc = NALLOC
		call malloc (positions, nalloc * 2, TY_REAL)
	    } else if (i > nalloc) {
		nalloc = nalloc + NALLOC
		call realloc (positions, nalloc * 2, TY_REAL)
	    }
	    ptr = positions + 2 * i - 2
	    Memr[ptr] = ra
	    Memr[ptr+1] = dec
	    npositions = i
	}

	return (i)
end


# DATE_GROUP -- Group by date.

int procedure date_group (im, dates, ndates)

pointer	im			# Image
pointer	dates			# Pointer to date strings
int	ndates			# Number of dates

int	i, nalloc, stridxs()
pointer	sp, date, ptr
bool	streq()
errchk	hdmgstr

begin
	call smark (sp)
	call salloc (date, SZ_LINE, TY_CHAR)
	call hdmgstr (im, "date-obs", Memc[date], SZ_LINE)

	# Strip time if present.
	i = stridxs ("T", Memc[date])
	if (i > 0)
	    Memc[date+i-1] = EOS

	for (i=1; i<=ndates; i=i+1) {
	    ptr = dates + (i - 1) * SZ_LINE
	    if (streq (Memc[date], Memc[ptr]))
		break
	}
	if (i > ndates) {
	    if (i == 1) {
		nalloc = NALLOC
		call malloc (dates, nalloc * SZ_LINE, TY_CHAR)
	    } else if (i > nalloc) {
		nalloc = nalloc + NALLOC
		call realloc (dates, nalloc * SZ_LINE, TY_CHAR)
	    }
	    ptr = dates + (i - 1) * SZ_LINE
	    call strcpy (Memc[date], Memc[ptr], SZ_LINE-1)
	    ndates = i
	}

	call sfree (sp)
	return (i)
end
