# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<imhdr.h>
include	<imset.h>

define	OUTPUTS		"|none|list|file|"
define	NONE		1		# No output
define	LIST		2		# List output
define	FILE		3		# File output

define	SZ_RANGE	100		# Size of range list
define	SZ_LISTOUT	255		# Size of output list


# T_IMEXTENSIONS -- Expand a template of FITS files into a list of image
# extensions on the standard output and record the number image extensions
# in a parameter.

procedure t_imextensions()

pointer	input			# List of ME file names
int	output			# Output list (none|list|file)
pointer	index			# Range list of extension indexes
pointer	extname			# Patterns for extension names
pointer extver			# Range list of extension versions
int	lindex			# List index number?
int	lname			# List extension name?
int	lver			# List extension version?
pointer	ikparams		# Image kernel parameters

pointer	sp, image, listout
int	list, nimages, fd
int	clgwrd(), btoi(), imextensions(), stropen()
int	imtgetim(), imtlen()
bool	clgetb()
errchk	stropen, fprintf, strclose

begin
	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (index, SZ_LINE, TY_CHAR)
	call salloc (extname, SZ_LINE, TY_CHAR)
	call salloc (extver, SZ_LINE, TY_CHAR)
	call salloc (ikparams, SZ_LINE, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Task parameters
	call clgstr ("input", Memc[input], SZ_LINE)
	output = clgwrd ("output", Memc[image], SZ_FNAME, OUTPUTS)
	call clgstr ("index", Memc[index], SZ_LINE)
	call clgstr ("extname", Memc[extname], SZ_LINE)
	call clgstr ("extver", Memc[extver], SZ_LINE)
	lindex = btoi (clgetb ("lindex"))
	lname = btoi (clgetb ("lname"))
	lver = btoi (clgetb ("lver"))
	call clgstr ("ikparams", Memc[ikparams], SZ_LINE)

	# Get the list.
	list = imextensions (Memc[input], Memc[index], Memc[extname],
	    Memc[extver], lindex, lname, lver, Memc[ikparams], YES)

	# Format the output and set the number of images.
	switch (output) {
	case LIST:
	    call salloc (listout, SZ_LISTOUT, TY_CHAR)
	    iferr {
		fd = stropen (Memc[listout], SZ_LISTOUT, WRITE_ONLY)
		nimages = 0
		while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
		    nimages = nimages + 1
		    if (nimages == 1) {
			call fprintf (fd, "%s")
			    call pargstr (Memc[image])
		    } else {
			call fprintf (fd, ",%s")
			    call pargstr (Memc[image])
		    }
		}
		call strclose (fd)
		call printf ("%s\n")
		    call pargstr (Memc[listout])
	    } then {
		call imtclose (list)
		call sfree (sp)
		call error (1, "Output list format is too long")
	    }
	case FILE:
	    while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
		call printf ("%s\n")
		    call pargstr (Memc[image])
	    }
	}
	call clputi ("nimages", imtlen (list))

	call imtclose (list)
	call sfree (sp)
end


# IMEXTENSIONS -- Expand a template of ME files into a list of image extensions.

int procedure imextensions (files, index, extname, extver, lindex, lname, lver,
	ikparams, err)

char	files[ARB]		#I List of ME files
char	index[ARB]		#I Range list of extension indexes
char	extname[ARB]		#I Patterns for extension names
char	extver[ARB]		#I Range list of extension versions
int	lindex			#I List index number?
int	lname			#I List extension name?
int	lver			#I List extension version?
char	ikparams[ARB]		#I Image kernel parameters
int	err			#I Print errors?
int	list			#O Image list

int	i, fd
pointer	sp, temp, fname, imname, section, rindex, rextver, ikp, str
int	imtopen(), imtgetim()
int	ix_decode_ranges(), nowhite(), open()
errchk	open, imextension, delete

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (ikp, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Expand parameters.
	list = imtopen (files)
	call salloc (rindex, 3*SZ_RANGE, TY_INT)
	if (ix_decode_ranges (index, Memi[rindex], SZ_RANGE, i) == ERR)
	    call error (1, "Bad index range list")

	rextver = NULL
	if (nowhite (extver, Memc[str], SZ_LINE) > 0) {
	    call salloc (rextver, 3*SZ_RANGE, TY_INT)
	    if (ix_decode_ranges (Memc[str], Memi[rextver], SZ_RANGE, i)==ERR)
		call error (1, "Bad extension version range list")
	}
	i = nowhite (ikparams, Memc[ikp], SZ_LINE)

	# Expand ME files into list of image extensions in a temp file.
	call mktemp ("@tmp$iraf", Memc[temp], SZ_FNAME)
	fd = open (Memc[temp+1], NEW_FILE, TEXT_FILE)
	while (imtgetim (list, Memc[fname], SZ_FNAME) != EOF) {
	    call imgimage (Memc[fname], Memc[imname], SZ_FNAME)
	    call imgsection (Memc[fname], Memc[section], SZ_FNAME)
	    call imextension (fd, Memc[imname], rindex, extname, rextver,
		lindex, lname, lver, Memc[ikp], Memc[section], err)
	}
	call imtclose (list)
	call close (fd)

	# Return list.
	list = imtopen (Memc[temp])
	call delete (Memc[temp+1])
	call sfree (sp)
	return (list)
end


# IMEXTENSION -- Expand a single ME file into a list of image extensions.
# The image extensions are written to the input file descriptor.

procedure imextension (fd, fname, index, extname, extver, lindex, lname, lver,
	ikparams, section, err)

int	fd			#I File descriptor for list 
char	fname[SZ_FNAME]		#I File image name (without kernel or image sec)
pointer	index			#I Range list of extension indexes
char	extname[ARB]		#I Pattern for extension names
pointer	extver			#I Range list of extension versions
int	lindex			#I List index number?
int	lname			#I List extension name?
int	lver			#I List extension version?
char	ikparams[ARB]		#I Image kernel parameters
char	section[ARB]		#I Image section
int	err			#I Print errors?

bool	extmatch()
int	i, j, ver, ix_get_next_number(), errcode(), imgeti(), stridxs()
pointer	sp, image, name, str, im, immap()
bool	is_in_range()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	i = -1
	while (ix_get_next_number (Memi[index], i) != EOF) {
	    j = stridxs ("[", fname)
	    if (j > 0) {
		if (i > 0)
		    break
		call strcpy (fname, Memc[image], SZ_FNAME)
	    } else {
		call sprintf (Memc[image], SZ_FNAME, "%s[%d]")
		    call pargstr (fname)
		    call pargi (i)
	    }
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		switch (errcode()) {
		case SYS_FXFRFEOF:
		    break
		case SYS_IKIEXTN:
		    next
		case SYS_IKIOPEN:
		    switch (i) {
		    case 0:
			next
		    case 1:
			if (err == YES)
			    call erract (EA_WARN)
			break
		    default:
			break
		    }
		default:
		    call erract (EA_ERROR)
		}
	    }

	    # Check the extension name.
	    if (extname[1] != EOS) {
		iferr (call imgstr (im, "extname", Memc[name], SZ_LINE)) {
		    Memc[name] = EOS
		    #call imunmap (im)
		    #next
		}
		if (!extmatch (Memc[name], extname)) {
		    call imunmap (im)
		    next
		}
	    }

	    # Check the extension version.
	    if (extver != NULL) {
		iferr (ver = imgeti (im, "extver")) {
		    call imunmap (im)
		    next
		}
		if (!is_in_range (Memi[extver], ver)) {
		    call imunmap (im)
		    next
		}
	    }

	    # Set the extension name and version.
	    if (lname == YES) {
		iferr (call imgstr (im, "extname", Memc[name], SZ_LINE))
		    Memc[name] = EOS
	    } else
		Memc[name] = EOS
	    if (lver == YES) {
		iferr (ver = imgeti (im, "extver"))
		    ver = INDEFI
	    } else
		ver = INDEFI

	    # Write the image name.
	    call fprintf (fd, fname)
	    if (j == 0) {
		if (lindex == YES || (Memc[name] == EOS && IS_INDEFI(ver))) {
		    call fprintf (fd, "[%d]")
			call pargi (i)
		}
		if (Memc[name] != EOS) {
		    call fprintf (fd, "[%s")
			call pargstr (Memc[name])
		    if (!IS_INDEFI(ver)) {
			call fprintf (fd, ",%d")
			    call pargi (ver)
		    }
		    if (ikparams[1] != EOS) {
			call fprintf (fd, ",%s")
			    call pargstr (ikparams)
		    }
		    call fprintf (fd, "]")
		} else if (!IS_INDEFI(ver)) {
		    call fprintf (fd, "[extver=%d")
			call pargi (ver)
		    if (ikparams[1] != EOS) {
			call fprintf (fd, ",%s")
			    call pargstr (ikparams)
		    }
		    call fprintf (fd, "]")
		} else if (ikparams[1] != EOS) {
		    call fprintf (fd, "[%s]")
			call pargstr (ikparams)
		}
	    }
	    call fprintf (fd, "%s")
		call pargstr (section)
	    call fprintf (fd, "\n")
		
	    call imunmap (im)
	}

	call sfree (sp)
end


include	<mach.h>
include	<ctype.h>

define	FIRST	1		# Default starting range
define	LAST	MAX_INT		# Default ending range
define	STEP	1		# Default step
define	EOLIST	-1		# End of list

# IX_DECODE_RANGES -- Parse a string containing a list of integer numbers or
# ranges, delimited by either spaces or commas.  Return as output a list
# of ranges defining a list of numbers, and the count of list numbers.
# Range limits must be positive nonnegative integers.  ERR is returned as
# the function value if a conversion error occurs.  The list of ranges is
# delimited by EOLIST.

int procedure ix_decode_ranges (range_string, ranges, max_ranges, nvalues)

char	range_string[ARB]	# Range string to be decoded
int	ranges[3, max_ranges]	# Range array
int	max_ranges		# Maximum number of ranges
int	nvalues			# The number of values in the ranges

int	ip, nrange, first, last, step, ctoi()

begin
	ip = 1
	nvalues = 0

	do nrange = 1, max_ranges - 1 {
	    # Defaults to all nonnegative integers
	    first = FIRST
	    last = LAST
	    step = STEP

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get first limit.
	    # Must be a number, '-', 'x', or EOS.  If not return ERR.
	    if (range_string[ip] == EOS) {			# end of list
		if (nrange == 1) {
		    # Null string defaults
		    ranges[1, 1] = first
		    ranges[2, 1] = last
		    ranges[3, 1] = step
		    ranges[1, 2] = EOLIST
	    	    nvalues = MAX_INT
		    return (OK)
		} else {
		    ranges[1, nrange] = EOLIST
		    return (OK)
		}
	    } else if (range_string[ip] == '-')
		;
	    else if (range_string[ip] == 'x')
		;
	    else if (IS_DIGIT(range_string[ip])) {		# ,n..
		if (ctoi (range_string, ip, first) == 0)
		    return (ERR)
	    } else
		return (ERR)

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get last limit
	    # Must be '-', or 'x' otherwise last = first.
	    if (range_string[ip] == 'x')
		;
	    else if (range_string[ip] == '-') {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, last) == 0)
		        return (ERR)
		} else if (range_string[ip] == 'x')
		    ;
		else
		    return (ERR)
	    } else
		last = first

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get step.
	    # Must be 'x' or assume default step.
	    if (range_string[ip] == 'x') {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, step) == 0)
		        ;
		    if (step == 0)
			return (ERR)
		} else if (range_string[ip] == '-')
		    ;
		else
		    return (ERR)
	    }

	    # Output the range triple.
	    ranges[1, nrange] = first
	    ranges[2, nrange] = last
	    ranges[3, nrange] = step
	    nvalues = nvalues + abs (last-first) / step + 1
	}

	return (ERR)					# ran out of space
end


# IX_GET_NEXT_NUMBER -- Given a list of ranges and the current file number,
# find and return the next file number.  Selection is done in such a way
# that list numbers are always returned in monotonically increasing order,
# regardless of the order in which the ranges are given.  Duplicate entries
# are ignored.  EOF is returned at the end of the list.

int procedure ix_get_next_number (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Both input and output parameter

int	ip, first, last, step, next_number, remainder

begin
	# If number+1 is anywhere in the list, that is the next number,
	# otherwise the next number is the smallest number in the list which
	# is greater than number+1.

	number = number + 1
	next_number = MAX_INT

	for (ip=1;  ranges[ip] != EOLIST;  ip=ip+3) {
	    first = min (ranges[ip], ranges[ip+1])
	    last = max (ranges[ip], ranges[ip+1])
	    step = ranges[ip+2]
	    if (step == 0)
		call error (1, "Step size of zero in range list")
	    if (number >= first && number <= last) {
		remainder = mod (number - first, step)
		if (remainder == 0)
		    return (number)
		if (number - remainder + step <= last)
		    next_number = number - remainder + step
	    } else if (first > number)
		next_number = min (next_number, first)
	}

	if (next_number == MAX_INT)
	    return (EOF)
	else {
	    number = next_number
	    return (number)
	}
end


# EXTMATCH -- Match extname against a comma-delimited list of patterns.

bool procedure extmatch (extname, patterns)

char	extname[ARB]		#I Extension name to match
char	patterns[ARB]		#I Comma-delimited list of patterns
bool	stat			#O Match?

int	i, j, k, sz_pat, strlen(), patmake(), patmatch(), nowhite()
pointer	sp, patstr, patbuf

begin
	stat = false

	sz_pat = strlen (patterns)
	if (sz_pat == 0)
	    return (stat)
	sz_pat = sz_pat + SZ_LINE

	call smark (sp)
	call salloc (patstr, sz_pat, TY_CHAR)
	call salloc (patbuf, sz_pat, TY_CHAR)

	i = nowhite (patterns, Memc[patstr], sz_pat)
	if (i == 0)
	    stat = true
	else if (i == 1 && Memc[patstr] == '*')
	    stat = true
	else {
	    i = 1
	    for (j=i;; j=j+1) {
		if (patterns[j] != ',' && patterns[j] != EOS)
		    next
		if (j - i > 0) {
		    if (j-i == 1 && patterns[i] == '*') {
			stat = true
			break
		    }
		    call strcpy (patterns[i], Memc[patstr+1], j-i)
		    Memc[patstr] = '^'
		    Memc[patstr+j-i+1] = '$'
		    Memc[patstr+j-i+2] = EOS
		    k = patmake (Memc[patstr], Memc[patbuf], sz_pat)
		    if (patmatch (extname, Memc[patbuf]) > 0) {
			stat = true
			break
		    }
		}
		if (patterns[j] == EOS)
		    break
		i = j + 1
	    }
	}

	call sfree (sp)
	return (stat)
end
