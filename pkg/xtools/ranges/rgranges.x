# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<pkg/rg.h>

define	NRGS	10		# Allocation size

# RG_RANGES -- Parse a range string.  Return a pointer to the ranges.

pointer procedure rg_ranges (rstr, rmin, rmax)

char	rstr[ARB]		# Range string
int	rmin			# Minimum value
int	rmax			# Maximum value
pointer	rg			# Range pointer

int	i, fd, strlen(), open(), getline()
pointer	sp, str, ptr
errchk	open, rg_add

begin
	call smark (sp)
	call salloc (str, max (strlen (rstr), SZ_LINE), TY_CHAR)
	call calloc (rg, LEN_RG, TY_STRUCT)

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
	    iferr {
		if (Memc[str] == '@') {
		    fd = open (Memc[str+1], READ_ONLY, TEXT_FILE)
		    while (getline (fd, Memc[str]) != EOF) {
			iferr (call rg_add (rg, Memc[str], rmin, rmax))
			    call erract (EA_WARN)
		    }
		    call close (fd)
		} else
		    call rg_add (rg, Memc[str], rmin, rmax)
	    } then
		call erract (EA_WARN)
	}

	call sfree (sp)
	return (rg)
end


# RG_ADD -- Add a range

procedure rg_add (rg, rstr, rmin, rmax)

pointer	rg			# Range descriptor
char	rstr[ARB]		# Range string
int	rmin			# Minimum value
int	rmax			# Maximum value

int	i, j, nrgs, strlen(), ctoi()
int	rval1, rval2
pointer	sp, str, ptr

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

	    ptr = str
	    while (!(IS_WHITE(rstr[i]) || rstr[i]==',' || rstr[i]=='\n' ||
		rstr[i]==EOS)) {
		if (rstr[i] == ':')
		    Memc[ptr] = ' '
		else
		    Memc[ptr] = rstr[i]
	        i = i + 1
		ptr = ptr + 1
	    }
	    Memc[ptr] = EOS

	    # Parse range
	    if (Memc[str] == '@')
		call error (1, "Cannot nest @files")
	    else if (Memc[str] == '*') {
		rval1 = rmin
		rval2 = rmax
	    } else {
		# Get range
		j = 1
		if (ctoi (Memc[str], j, rval1) == 0)
		    call error (1, "Range syntax error")
		if (ctoi (Memc[str], j, rval2) == 0)
		    rval2 = rval1
	    }

	    # Check limits.
	    j = rval1
	    rval1 = min (j, rval2)
	    rval2 = max (j, rval2)
	    if (rval2 >= rmin && rval1 <= rmax) {
		nrgs = RG_NRGS(rg)
		if (mod (nrgs, NRGS) == 0)
		    call realloc (rg, LEN_RG+2*(nrgs+NRGS), TY_STRUCT)
		nrgs = nrgs + 1
		RG_NRGS(rg) = nrgs
		RG_X1(rg, nrgs) = max (rmin, rval1)
		RG_X2(rg, nrgs) = min (rmax, rval2)
		RG_NPTS(rg) = RG_NPTS(rg) +
		    abs (RG_X1(rg, nrgs) - RG_X2(rg, nrgs)) + 1
	    }
	}

	call sfree (sp)
end
