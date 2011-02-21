# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
include	<ctype.h>
include	<mach.h>

# RNGRANGES -- Yet another ranges package.
# This ranges package allows real number ranges (including negative values)
# and @ lists.  It is an object oriented package using a pointer.
#
# RNG_OPEN     -- Open a range string.  Return a pointer to the ranges.
# RNG_CLOSE    -- Close range structure.
# RNG_INDEX    -- Get ith range element.  Return EOF if index is out of range.
# RNG_NEAREST  -- Get nearest range index and value to input value.
# 		 Return the difference.
# RNG_INRANGER  -- Check if real value is within a range.
# RNG_INRANGEI -- Check if integer value is within a range.
# RNG_ELEMENTR -- Check if real value is an element.
# RNG_ELEMENTI -- Check if integer value is an element.
# RNG_ADD      -- Add a range.
# RNG_ERROR    -- Set error flag and free memory.


# Definitions for the RANGES structure.

define	LEN_RNG		2			# Length of main structure
define	RNG_ALLOC	10			# Allocation size
define	RNG_MAXINT	(MAX_INT/2)		# Maximum range integer

define	RNG_NPTS	Memi[$1]		# Number of points in ranges
define	RNG_NRNGS	Memi[$1+1]		# Number of range intervals
define	RNG_X1		Memr[P2R($1+4*($2)-2)]	# Start of range
define	RNG_X2		Memr[P2R($1+4*($2)-1)]	# End of range
define	RNG_DX		Memr[P2R($1+4*($2))]	# Interval step
define	RNG_NX		Memi[$1+4*($2)+1]	# Number of intervals step


# RNG_OPEN -- Open a range string.  Return a pointer to the ranges.

pointer procedure rng_open (rstr, r1, r2, dr)

char	rstr[ARB]		# Range string
real	r1, r2, dr		# Default range and range limits
pointer	rg			# Range pointer

int	i, fd, strlen(), open(), getline()
real	a, b, c
pointer	sp, str, ptr
errchk	open, rng_add

begin
	call smark (sp)
	call salloc (str, max (strlen (rstr), SZ_LINE), TY_CHAR)
	call calloc (rg, LEN_RNG, TY_STRUCT)

	a = r1
	b = r2
	c = dr
	if (IS_INDEF(a))
	    a = 0
	if (IS_INDEF(b))
	    b = RNG_MAXINT
	if (IS_INDEF(c))
	    c = 1

	i = 1
	while (rstr[i] != EOS) {

	    # Find beginning and end of a range and copy it to the work string
	    while (IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n')
	        i = i + 1
	    if (rstr[i] == EOS)
		break

	    ptr = str
	    while (!(IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n' ||
		rstr[i]==EOS)) {
		Memc[ptr] = rstr[i]
	        i = i + 1
		ptr = ptr + 1
	    }
	    Memc[ptr] = EOS

	    # Add range(s)
	    if (Memc[str] == '@') {
		fd = open (Memc[str+1], READ_ONLY, TEXT_FILE)
		while (getline (fd, Memc[str]) != EOF)
		    call rng_add (rg, Memc[str], a, b, c)
		call close (fd)
	    } else
		call rng_add (rg, Memc[str], a, b, c)
	}

	if (RNG_NRNGS(rg) == 0)
	    call rng_add (rg, "*", a, b, c)

	call sfree (sp)
	return (rg)
end


# RNG_CLOSE -- Close range structure

procedure rng_close (rg)

pointer	rg			#I Range descriptor

begin
	call mfree (rg, TY_STRUCT)
end


# RNG_INDEX -- Get ith range element.  Return EOF if index is out of range.

int procedure rng_index (rg, ival, rval)

pointer	rg			#I Range descriptor
int	ival			#I Range index
real	rval			#O Range value

int	i, j

begin
	if (ival < 1 || ival > RNG_NPTS(rg))
	    return (EOF)

	j = 1 + RNG_NPTS(rg)
	do i = RNG_NRNGS(rg), 1, -1 {
	    j = j - RNG_NX(rg,i)
	    if (ival >= j) {
		rval = RNG_X1(rg,i) + (ival - j) * RNG_DX(rg,i)
		return (ival)
	    }
	}
end


# RNG_NEAREST -- Get nearest range index and value to input value.
# Return the difference.

real procedure rng_nearest (rg, x, ival, rval)

pointer	rg			#I Range descriptor
real	x			#I Value to be matched
int	ival			#O Index to range values
real	rval			#O Range value

int	i, j, k
real	drmin, dx

begin
	ival = 1
	rval = RNG_X1(rg,1)
	drmin = abs (x - rval)

	k = 1
	do i = 1, RNG_NRNGS(rg) {
	    dx = x - RNG_X1(rg,i)
	    j = max (0, min (RNG_NX(rg,i)-1, nint (dx / RNG_DX(rg,i))))
	    dx = abs (dx - j * RNG_DX(rg,i))
	    if (dx < drmin) {
		drmin = dx
		ival = j + k
		rval = RNG_X1(rg,i) + j * RNG_DX(rg,i)
	    }
	    k = k + RNG_NX(rg,i)
	}
	return (x - rval)
end


# RNG_INRANGER -- Check if real value is within a range.

bool procedure rng_inranger (rg, x)

pointer	rg			#I Range descriptor
real	x			#I Value to check

int	i
real	x1, x2

begin
	do i = 1, RNG_NRNGS(rg) {
	    x1 = RNG_X1(rg,i)
	    x2 = RNG_X2(rg,i)
	    if (x >= min (x1, x2) && x <= max (x1, x2))
		return (true)
	}
	return (false)
end


# RNG_INRANGEI -- Check if integer value is within an integer range.

bool procedure rng_inrangei (rg, x)

pointer	rg			#I Range descriptor
int	x			#I Value to check

bool	rng_inranger()

begin
	return (rng_inranger (rg, real(x)))
end


# RNG_ELEMENTR -- Check if real value is an element.

bool procedure rng_elementr (rg, x, delta)

pointer	rg			#I Range descriptor
real	x			#I Value to check
real	delta			#I Maximum distance from element

int	ival
real	rval, rng_nearest()

begin
	return (abs (rng_nearest (rg, x, ival, rval)) < delta)
end


# RNG_ELEMENTI -- Check if integer value is an element.

bool procedure rng_elementi (rg, x)

pointer	rg			#I Range descriptor
int	x			#I Value to check

int	ival
real	rval, rng_nearest()

begin
	return (abs (rng_nearest (rg, real(x), ival, rval)) < 0.49)
end


# RNG_ADD -- Add a range

procedure rng_add (rg, rstr, r1, r2, dr)

pointer	rg			# Range descriptor
char	rstr[ARB]		# Range string
real	r1, r2, dr		# Default range and range limits

int	i, j, nrgs, strlen(), ctor()
real	x1, x2, dx, nx
pointer	sp, str, ptr
errchk	rng_error

begin
	call smark (sp)
	call salloc (str, strlen (rstr), TY_CHAR)

	i = 1
	while (rstr[i] != EOS) {

	    # Find beginning and end of a range and copy it to the work string
	    while (IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n')
	        i = i + 1
	    if (rstr[i] == EOS)
		break

	    # Convert colon syntax to hyphen/x syntax.
	    j=0
	    ptr = str
	    while (!(IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n' ||
		rstr[i]==EOS)) {
		if (rstr[i] == ':') {
		    if (j == 0)
			Memc[ptr] = '-'
		    else if (j == 1)
			Memc[ptr] = 'x'
		    else
			call rng_error (1, rstr, r1, r2, dr, rg)
		    j = j + 1
		} else
		    Memc[ptr] = rstr[i]
	        i = i + 1
		ptr = ptr + 1
	    }
	    Memc[ptr] = EOS

	    # Parse range
	    if (Memc[str] == '@')
		call rng_error (2, rstr, r1, r2, dr, rg)
	    else if (Memc[str] == '*') {
		x1 = r1
		x2 = r2
		dx = dr
		if ((x2 - x1) / dx + 1 > RNG_MAXINT)
		    x2 = x1 + (RNG_MAXINT - 1) * dx
	    } else {
		j = 1
		if (ctor (Memc[str], j, x1) == 0)
		    call rng_error (3, rstr, r1, r2, dr, rg)
		if (Memc[str+j-1] == '-') {
		    j = j + 1
		    if (ctor (Memc[str], j, x2) == 0)
			call rng_error (3, rstr, r1, r2, dr, rg)
		    if (Memc[str+j-1] == 'x') {
			j = j + 1
			if (ctor (Memc[str], j, dx) == 0)
			    call rng_error (3, rstr, r1, r2, dr, rg)
		    } else
			dx = dr
		} else if (Memc[str+j-1] == 'x') {
		    j = j + 1
		    if (ctor (Memc[str], j, dx) == 0)
			call rng_error (3, rstr, r1, r2, dr, rg)
		    if (dx < 0)
			x2 = min (r1, r2)
		    else
			x2 = max (r1, r2)
		    if ((x2 - x1) / dx + 1 > RNG_MAXINT)
			x2 = x1 + (RNG_MAXINT - 1) * dx
		} else {
		    x2 = x1
		    dx = dr
		}
	    }

	    if (x1 < min (r1, r2) || x1 > max (r1, r2) ||
	        x2 < min (r1, r2) || x2 > max (r1, r2))
		call rng_error (4, rstr, r1, r2, dr, rg)

	    nrgs = RNG_NRNGS(rg)
	    if (mod (nrgs, RNG_ALLOC) == 0)
		call realloc (rg, LEN_RNG+4*(nrgs+RNG_ALLOC), TY_STRUCT)
	    nrgs = nrgs + 1
	    RNG_NRNGS(rg) = nrgs
	    RNG_X1(rg, nrgs) = x1
	    RNG_X2(rg, nrgs) = x2
	    RNG_DX(rg, nrgs) = dx
	    nx = (x2 - x1) / dx + 1
	    RNG_NX(rg, nrgs) = min (nx, real (RNG_MAXINT))
	    RNG_NPTS(rg) = min (nx + RNG_NPTS(rg), real (RNG_MAXINT))
	}

	call sfree (sp)
end


# RNG_ERROR -- Set error flag and free memory.
# Note that the pointer is freed at this point.

procedure rng_error (errnum, rstr, r1, r2, dr, rg)

int	errnum			# Error number
char	rstr[ARB]		# Range string
real	r1, r2, dr		# Default range and range limits
pointer	rg			# Range pointer to be freed.

pointer	errstr

begin
	call salloc (errstr, SZ_LINE, TY_CHAR)

	switch (errnum) {
	case 1:
	    call sprintf (Memc[errstr], SZ_LINE,
	        "Range syntax error: Too many colons (%s)")
		call pargstr (rstr)
	case 2:
	    call sprintf (Memc[errstr], SZ_LINE,
	        "Range syntax error: Cannot nest @files (%s)")
		call pargstr (rstr)
	case 3:
	    call sprintf (Memc[errstr], SZ_LINE,
	        "Range syntax error: (%s)")
		call pargstr (rstr)
	case 4:
	    call sprintf (Memc[errstr], SZ_LINE,
		"Range syntax error: Range out of bounds %g to %g (%s)")
		call pargr (min (r1, r2))
		call pargr (max (r1, r2))
		call pargstr (rstr)
	case 5:
	    call sprintf (Memc[errstr], SZ_LINE,
		"Range syntax error: Too many range elements (%s)")
		call pargstr (rstr)
	}

	call rng_close (rg)
	call error (errnum, Memc[errstr])
end
