include	<error.h>
include	<imhdr.h>

define	MAP_LENSTR	99		# Length of strings

# Map data structure.
define	MAP_LEN		64		# Length of map structure
define	MAP_NAME	Memc[P2C($1)]	# Name of map
define	MAP_TYPE	Memi[$1+51]	# Type of map
define	MAP_MAP		Memi[$1+52]	# Structure pointer
define	MAP_COPY	Memi[$1+53]	# Data buffer for copy
define	MAP_NC		Memi[$1+54]	# Number of columns
define	MAP_NL		Memi[$1+55]	# Number of columns
define	MAP_SAMPLE	Memi[$1+56]	# Sample size for lines
define	MAP_BUF		Memi[$1+57]	# Data buffer for constant or sampling
define	MAP_BUF1	Memi[$1+58]	# Data buffer for sampling
define	MAP_BUF2	Memi[$1+59]	# Data buffer for sampling
define	MAP_LINE1	Memi[$1+60]	# Sampling line number
define	MAP_LINE2	Memi[$1+61]	# Sampling line number
define	MAP_LASTLINE	Memi[$1+62]	# Last line
define	MAP_LASTBUF	Memi[$1+63]	# Data buffer last returned

# Types of maps allowed.
define	MAP_CONST	1		# Constant
define	MAP_IMAGE	2		# Image
define	MAP_GSURFIT	3		# GSURFIT


# MAP_GLR -- Get a line of map data.

pointer procedure map_glr (map, line, mode)

pointer	map		#I Map pointer
int	line		#I Line
int	mode		#I Access mode (READ_ONLY, READ_WRITE)

int	i, nc, nl, sample, line1, line2
real	a, b
pointer	buf, buf1, buf2, mim_glr(), mgs_glr()
errchk	malloc, mim_glr, mgs_glr

begin
	# Check for repeated request.
	if (line == MAP_LASTLINE(map)) {
	    buf = MAP_LASTBUF(map)
	    if (mode == READ_WRITE) {
		nc = MAP_NC(map)
		if (MAP_COPY(map) == NULL)
		    call malloc (MAP_COPY(map), nc, TY_REAL)
		call amovr (Memr[buf], Memr[MAP_COPY(map)], nc)
		buf = MAP_COPY(map)
	    }
	    return (buf)
	}

	nc = MAP_NC(map)
	nl = MAP_NL(map)
	sample = MAP_SAMPLE(map)

	# Check for subsampling.  A constant map will never be sampled.
	if (sample > 1) {
	    if (MAP_BUF1(map) == NULL) {
		call malloc (MAP_BUF(map), nc, TY_REAL)
		call malloc (MAP_BUF1(map), nc, TY_REAL)
		call malloc (MAP_BUF2(map), nc, TY_REAL)
	    }
	    line1 = (line-1) / sample * sample + 1
	    line2 = min (nl, line1 + sample)
	    buf1 = MAP_BUF1(map)
	    buf2 = MAP_BUF2(map)
	    if (line1 == MAP_LINE2(map)) {
		MAP_BUF2(map) = buf1
		MAP_BUF1(map) = buf2
		MAP_LINE2(map) = MAP_LINE1(map)
		MAP_LINE1(map) = line1
		buf1 = MAP_BUF1(map)
		buf2 = MAP_BUF2(map)
	    } else if (line2 == MAP_LINE1(map)) {
		MAP_BUF1(map) = buf2
		MAP_BUF2(map) = buf1
		MAP_LINE1(map) = MAP_LINE2(map)
		MAP_LINE2(map) = line2
		buf1 = MAP_BUF1(map)
		buf2 = MAP_BUF2(map)
	    }
	    if (line1 != MAP_LINE1(map)) {
		switch (MAP_TYPE(map)) {
		case MAP_IMAGE:
		    buf = mim_glr (MAP_MAP(map), line1)
		case MAP_GSURFIT:
		    buf = mgs_glr (MAP_MAP(map), line1)
		}
		call amovr (Memr[buf], Memr[buf1], nc)
		MAP_LINE1(map) = line1
	    }
	    if (line2 != MAP_LINE2(map)) {
		switch (MAP_TYPE(map)) {
		case MAP_IMAGE:
		    buf = mim_glr (MAP_MAP(map), line2)
		case MAP_GSURFIT:
		    buf = mgs_glr (MAP_MAP(map), line2)
		}
		call amovr (Memr[buf], Memr[buf2], nc)
		MAP_LINE2(map) = line2
	    }
	    if (line == line1)
		buf = buf1
	    else if (line == line2)
		buf = buf2
	    else {
		buf = MAP_BUF(map)
		b = real (line - line1) / sample
		a = 1 - b
		do i = 0, nc-1
		    Memr[buf+i] = a * Memr[buf1+i] + b * Memr[buf2+i]
	    }
	} else {
	    switch (MAP_TYPE(map)) {
	    case MAP_IMAGE:
		buf = mim_glr (MAP_MAP(map), line)
	    case MAP_GSURFIT:
		buf = mgs_glr (MAP_MAP(map), line)
	    case MAP_CONST:
		buf = MAP_BUF(map)
	    }
	}
	MAP_LASTLINE(map) = line
	MAP_LASTBUF(map) = buf

	# Make a copy which might be modified by the caller.
	if (mode == READ_WRITE) {
	    nc = MAP_NC(map)
	    if (MAP_COPY(map) == NULL)
		call malloc (MAP_COPY(map), nc, TY_REAL)
	    call amovr (Memr[buf], Memr[MAP_COPY(map)], nc)
	    buf = MAP_COPY(map)
	}

	return (buf)
end


# MAP_OPEN -- Open map.  Return NULL if no map is found.

pointer procedure map_open (name, refim)

char	name[ARB]	#I Name
pointer	refim		#I Reference image
pointer	map		#O Map pointer returned

int	i, nc, nl, nowhite(), ctor()
real	const
pointer	sp, mapstr, im, gs, immap(), mim_open(), mgs_open()
errchk	calloc, malloc, imgstr, mim_open, mgs_open

begin
	call smark (sp)
	call salloc (mapstr, SZ_FNAME, TY_CHAR)

	i = 1
	nc = IM_LEN(refim,1)
	nl = IM_LEN(refim,2)

	call calloc (map, MAP_LEN, TY_STRUCT)
	MAP_NC(map) = nc
	MAP_NL(map) = nl

	iferr {
	    # Check for missing map name, and keyword redirection.
	    if (nowhite (name, Memc[mapstr], SZ_FNAME) == 0)
		call error (1, "No map specified")
	    if (Memc[mapstr] == '!')
		call imgstr (refim, Memc[mapstr+1], Memc[mapstr], SZ_FNAME)
	    call strcpy (Memc[mapstr], MAP_NAME(map), MAP_LENSTR)

	    ifnoerr (im = immap (MAP_NAME(map), READ_ONLY, 0)) {
		call imunmap (im)
		MAP_TYPE(map) = MAP_IMAGE
		MAP_MAP(map) = mim_open (MAP_NAME(map), refim)
	    } else ifnoerr (call mgs_ggs (refim, MAP_NAME(map), gs)) {
		MAP_TYPE(map) = MAP_GSURFIT
		MAP_MAP(map) = mgs_open (MAP_NAME(map), refim, gs)
	    } else if (ctor (MAP_NAME(map), i, const) > 0) {
		MAP_TYPE(map) = MAP_CONST
		call malloc (MAP_BUF(map), nc, TY_REAL)
		call amovkr (const, Memr[MAP_BUF(map)], nc)
	    } else {
		call mfree (map, TY_STRUCT)
		call sprintf (Memc[mapstr], SZ_FNAME, "Can't open map (%s)")
		    call pargstr (name)
		call error (2, Memc[mapstr])
	    }
	} then {
	    call map_close (map)
	    call erract (EA_ERROR)
	}

	call sfree (sp)
	return (map)
end


# MAP_OPENGS -- Open GSURFIT map given the GSURFIT pointer.

pointer procedure map_opengs (gs, refim)

pointer	gs		#I GSURFIT pointer
pointer	refim		#I Reference image
pointer	map		#O Map pointer returned

pointer	mgs_open()
errchk	calloc, mgs_open

begin
	iferr {
	    call calloc (map, MAP_LEN, TY_STRUCT)
	    MAP_NC(map) = IM_LEN(refim,1)
	    MAP_NL(map) = IM_LEN(refim,2)
	    MAP_TYPE(map) = MAP_GSURFIT
	    MAP_MAP(map) = mgs_open (MAP_NAME(map), refim, gs)
	} then {
	    call map_close (map)
	    call erract (EA_ERROR)
	}

	return (map)
end


# MAP_CLOSE -- Unmap map structure.

procedure map_close (map)

pointer	map			#I Map pointer

begin
	if (map == NULL)
	    return

	switch (MAP_TYPE(map)) {
	case MAP_IMAGE:
	    call mim_close (MAP_MAP(map))
	case MAP_GSURFIT:
	    call mgs_close (MAP_MAP(map))
	}

	call mfree (MAP_COPY(map), TY_REAL)
	call mfree (MAP_BUF(map), TY_REAL)
	call mfree (MAP_BUF1(map), TY_REAL)
	call mfree (MAP_BUF2(map), TY_REAL)
	call mfree (map, TY_STRUCT)
end


# MAP_GETS -- Get string parameter.

procedure map_gets (map, param, val, maxchar)

pointer	map		#I Map pointer
char	param[ARB]	#I Parameter
char	val[ARB]	#O Parameter string value 
int	maxchar		#I Maximum number of characters to return

bool	streq()
errchk	mim_gets(), mgs_gets()

begin
	if (streq (param, "mapname"))
	    call strcpy (MAP_NAME(map), val, maxchar)
	else {
	    switch (MAP_TYPE(map)) {
	    case MAP_IMAGE:
		call mim_gets (MAP_MAP(map), param, val, maxchar)
	    case MAP_GSURFIT:
		call mgs_gets (MAP_MAP(map), param, val, maxchar)
	    default:
		call error (1, "map_gets: unknown parameter")
	    }
	}
end


# MAP_GETI -- Get integer parameter.

procedure map_geti (map, param, val)

pointer	map		#I Map pointer
char	param[ARB]	#I Parameter
int	val		#O Value

errchk	mim_geti(), mgs_geti()

begin
	switch (MAP_TYPE(map)) {
	case MAP_IMAGE:
	    call mim_geti (MAP_MAP(map), param, val)
	case MAP_GSURFIT:
	    call mgs_geti (MAP_MAP(map), param, val)
	default:
	    call error (1, "map_geti: unknown parameter")
	}
end


# MAP_GETR -- Get real parameter.

procedure map_getr (map, param, val)

pointer	map		#I Map pointer
char	param[ARB]	#I Parameter
real	val		#O Value

bool	streq()
errchk	mim_getr(), mgs_getr()

begin
	if (streq (param, "constant")) {
	    if (MAP_TYPE(map) == MAP_CONST) {
		val = Memr[MAP_BUF(map)]
	        return
	    } else
	        call error (1, "map_getr: map is not constant")
	}

	switch (MAP_TYPE(map)) {
	case MAP_IMAGE:
	    call mim_getr (MAP_MAP(map), param, val)
	case MAP_GSURFIT:
	    call mgs_getr (MAP_MAP(map), param, val)
	default:
	    call error (1, "map_getr: unknown parameter")
	}
end


# MAP_SETI -- Set integer parameter.

procedure map_seti (map, param, val)

pointer	map		#I Map pointer
char	param[ARB]	#I Parameter
int	val		#I Value

bool	streq()
errchk	mim_seti(), mgs_seti

begin
	switch (MAP_TYPE(map)) {
	case MAP_CONST:
	    ;
	case MAP_IMAGE:
	    if (streq (param, "sample"))
		MAP_SAMPLE(map) = max (1, val)
	    else
		call mim_seti (MAP_MAP(map), param, val)
	case MAP_GSURFIT:
	    if (streq (param, "sample"))
		MAP_SAMPLE(map) = max (1, val)
	    else
		call mgs_seti (MAP_MAP(map), param, val)
	}
end


# MAP_SETR -- Set real parameter.

procedure map_setr (map, param, val)

pointer	map		#I Map pointer
char	param[ARB]	#I Parameter
real	val		#I Value

errchk	mim_setr(), mgs_setr

begin
	switch (MAP_TYPE(map)) {
	case MAP_IMAGE:
	    call mim_setr (MAP_MAP(map), param, val)
	case MAP_GSURFIT:
	    call mgs_setr (MAP_MAP(map), param, val)
	default:
	    call error (1, "map_setr: unknown parameter")
	}
end


# MAP_SETS -- Set string parameter.

procedure map_sets (map, param, val)

pointer	map		#I Map pointer
char	param[ARB]	#I Parameter
char	val[ARB]	#I Value

errchk	mim_sets(), mgs_sets

begin
	switch (MAP_TYPE(map)) {
	case MAP_IMAGE:
	    call mim_sets (MAP_MAP(map), param, val)
	case MAP_GSURFIT:
	    call mgs_sets (MAP_MAP(map), param, val)
	default:
	    call error (1, "map_sets: unknown parameter")
	}
end
