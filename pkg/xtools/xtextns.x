include	<error.h>
include	<pkg/mef.h>
include	<imhdr.h>


define	SZ_RANGE	100		# Size of range list


# XT_EXTNS -- Expand template of files into a list of extensions.
#
# This supports all MEF extension types.  If IMAGE type or any type is
# requested this will also return non-FITS images as well.
#
# This differs from XT_EXTNS1 in that extension zero is not returned
# unless it is a simple image and, in that case, the extension is removed.

int procedure xt_extns (files, exttype, index, extname, extver, lindex, lname,
	lver, dataless, ikparams, err, imext)

char	files[ARB]		#I List of MEF files
char	exttype[ARB]		#I Extension type (or null for all)
char	index[ARB]		#I Range list of extension indexes
char	extname[ARB]		#I Patterns for extension names
char	extver[ARB]		#I Range list of extension versions
int	lindex			#I List index number?
int	lname			#I List extension name?
int	lver			#I List extension version?
int	dataless		#I Include dataless image headers?
char	ikparams[ARB]		#I Image kernel parameters
int	err			#I Print errors?
int	imext			#O Image extensions?
int	list			#O Image list

int	i, j, nphu, nextns, fd
pointer	sp, temp, patbuf, fname, image, im, immap()
int	xt_extns1(), patmake(), gpatmatch(), imtopen(), imtgetim(), open()
errchk	xt_extns1, open, immap, delete

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (patbuf, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Get the list.
	list = xt_extns1 (files, exttype, index, extname, extver, lindex,
	    lname, lver, ikparams, err)

	# Check and edit the list.
	i = patmake ("\[[01]\]", Memc[patbuf], SZ_FNAME)
	nphu = 0
	nextns = 0
	call mktemp ("@tmp$iraf", Memc[temp], SZ_FNAME)
	fd = open (Memc[temp+1], NEW_FILE, TEXT_FILE)
	while (imtgetim (list, Memc[fname], SZ_FNAME) != EOF) {
	    if (dataless == NO) {
		iferr (im = immap (Memc[fname], READ_ONLY, 0))
		    im = NULL
		if (im != NULL) {
		    if (IM_NDIM(im) == 0 || IM_LEN(im,1) == 0) {
			call imunmap (im)
			next
		    }
		    call imunmap (im)
		}
	    }
	    if (gpatmatch (Memc[fname], Memc[patbuf], i, j) > 0) {
	        call strcpy (Memc[fname], Memc[image], SZ_FNAME)
		call strcpy (Memc[image+j], Memc[image+i-1], SZ_FNAME)
		ifnoerr (im = immap (Memc[image], READ_ONLY, 0)) {
		    call strcpy (Memc[image], Memc[fname], SZ_FNAME)
		    call imunmap (im)
		    nphu = nphu + 1
		}
	    }
	    nextns = nextns + 1
	    call fprintf (fd, "%s\n")
		call pargstr (Memc[fname])
	}
	call close (fd)

	# Return new list and extension flag.
	imext = YES
	if (nphu == nextns)
	    imext = NO
	call imtclose (list)
	list = imtopen (Memc[temp])
	call delete (Memc[temp+1])
	call sfree (sp)
	return (list)
end


# XT_IMEXTNS -- Expand a template of MEF files into a list of image extensions.

int procedure xt_imextns (files, index, extname, extver, lindex, lname, lver,
	ikparams, err)

char	files[ARB]		#I List of MEF files
char	index[ARB]		#I Range list of extension indexes
char	extname[ARB]		#I Patterns for extension names
char	extver[ARB]		#I Range list of extension versions
int	lindex			#I List index number?
int	lname			#I List extension name?
int	lver			#I List extension version?
char	ikparams[ARB]		#I Image kernel parameters
int	err			#I Print errors?
int	list			#O Image list

int	xt_extns1()
errchk	xt_extns1

begin
	list = xt_extns1 (files, "IMAGE", index, extname, extver, lindex,
	    lname, lver, ikparams, err)
	return (list)
end


# XT_EXTNS1 -- Expand a template of MEFs into a list of extensions.

int procedure xt_extns1 (files, exttype, index, extname, extver, lindex,
	lname, lver, ikparams, err)

char	files[ARB]		#I List of MEFs
char	exttype[ARB]		#I Desired extension type (or null for all)
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
pointer	sp, temp, fname, rindex, rextver, ikp, str
int	imtopen(), imtgetim()
int	ix_decode_ranges(), decode_ranges(), nowhite(), open()
errchk	open, xt_extn, delete

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
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
	    if (decode_ranges (Memc[str], Memi[rextver], SZ_RANGE, i)==ERR)
		call error (1, "Bad extension version range list")
	}
	i = nowhite (ikparams, Memc[ikp], SZ_LINE)

	# Expand MEFs into list of extensions in a temp file.
	call mktemp ("@tmp$iraf", Memc[temp], SZ_FNAME)
	fd = open (Memc[temp+1], NEW_FILE, TEXT_FILE)
	while (imtgetim (list, Memc[fname], SZ_FNAME) != EOF) {
	    call xt_extn (fd, Memc[fname], exttype, rindex, extname,
	    	rextver, lindex, lname, lver, Memc[ikp], err)
	}
	call imtclose (list)
	call close (fd)

	# Return list.
	list = imtopen (Memc[temp])
	call delete (Memc[temp+1])
	call sfree (sp)
	return (list)
end


# XT_EXTN -- Expand a single MEF into a list of extensions.
# The extensions are written to the input file descriptor.

procedure xt_extn (fd, fname, exttype, indices, extname, extver, lindex,
	lname, lver, ikparams, err)

int	fd			#I File descriptor for list 
char	fname[SZ_FNAME]		#I File name
char	exttype[ARB]		#I Extension type (or null for all)
pointer	indices			#I Range list of extension indexes
char	extname[ARB]		#I Pattern for extension names
pointer	extver			#I Range list of extension versions
int	lindex			#I List index number?
int	lname			#I List extension name?
int	lver			#I List extension version?
char	ikparams[ARB]		#I Image kernel parameters
int	err			#I Print errors?

int	i, j, n, index, ver, stat
pointer	sp, clust, ksec, imsec, name, str
pointer	mef, im

bool	streq(), is_in_range(), xt_extmatch()
int	mef_rdhdr_exnv(), mef_rdhdr_gn(), ix_get_next_number()
pointer	mefopen(), immap()
errchk	mefopen, mef_rdhdr_exnv, mef_rdhdr_gn, immap

begin
	call smark (sp)
	call salloc (clust, SZ_FNAME, TY_CHAR)
	call salloc (ksec, SZ_FNAME, TY_CHAR)
	call salloc (imsec, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Parse the file name syntax.
	call imparse (fname, Memc[clust], SZ_FNAME, Memc[ksec],
	    SZ_FNAME, Memc[imsec], SZ_FNAME, index, ver)

	# Open the file and check the error status.
	iferr (mef = mefopen (Memc[clust], READ_ONLY, 0)) {
	    if (exttype[1] == EOS || !streq (exttype, "IMAGE"))
	        call fprintf (fd, fname)
	    else if (streq (exttype, "IMAGE")) {
		ifnoerr (im = immap (fname, READ_ONLY, 0)) {
		    call imunmap (im)
		    call fprintf (fd, fname)
		}
	    }
	    return
	}

	# Loop through extensions.
	if (Memc[ksec] != EOS || index >= 0)
	    n = 1
	else
	    n = ARB
	j = index
	do i = 1, n {
	    iferr {
		# If a kernel section is given look for the extension/extver.
		if (Memc[ksec] != EOS) {
		    call mef_ksection (Memc[ksec], Memc[name], ver)
		    stat = mef_rdhdr_exnv (mef, Memc[name], ver)

		# If an index is given then look for the indexed extension
		} else if (j >= 0)
		    stat = mef_rdhdr_gn (mef, j)

		# If neither is given look for list of indices.
		else {
		    stat = ix_get_next_number (Memi[indices], index)
		    if (stat != EOF)
			stat = mef_rdhdr_gn (mef, index)
		}
	    } then {
	        # Check if file is an IRAF image.
		if (exttype[1] == EOS || !streq (exttype, "IMAGE"))
			call fprintf (fd, fname)
		else if (streq (exttype, "IMAGE")) {
		    ifnoerr (im = immap (fname, READ_ONLY, 0)) {
			call imunmap (im)
			call fprintf (fd, fname)
		    }
		}
	        stat = EOF
	    }

	    # Finish if EOF is encountered in either indices or file.
	    if (stat == EOF)
	        break

	    # Check the extension type.
	    if (exttype[1] != EOS && !streq (exttype, MEF_EXTTYPE(mef))) {
		if (!streq (exttype, "IMAGE") ||
		    !streq (MEF_EXTTYPE(mef), "SIMPLE")) {
		    # Check for PLIO mask which is a kind of image.
		    if (streq (MEF_EXTTYPE(mef), "BINTABLE")) {
		        call sprintf (Memc[str], SZ_LINE, "%s[%d]")
			    call pargstr (Memc[clust])
			    call pargi (MEF_CGROUP(mef))
			iferr (im = immap (Memc[str], READ_ONLY, 0))
			    im = NULL
			if (im == NULL)
			    next
			else
			    call imunmap (im)
		    }
		}
	    }

	    # Check the extension name.
	    if (!xt_extmatch (MEF_EXTNAME(mef), extname))
		next

	    # Check the extension version.
	    if (extver != NULL) {
	        if (IS_INDEFI(MEF_EXTVER(mef)))
		    next
		if (!is_in_range (Memi[extver], MEF_EXTVER(mef)))
		    next
	    }

	    # Set the extension name and version.
	    if (lname == YES)
	        call strcpy (MEF_EXTNAME(mef), Memc[name], SZ_LINE)
	    else
		Memc[name] = EOS
	    if (lver == YES)
	        ver = MEF_EXTVER(mef)
	    else
		ver = INDEFI

	    # Output the file name with the desired elements.
	    call fprintf (fd, Memc[clust])
	    if (lindex == YES || (Memc[name] == EOS && IS_INDEFI(ver))) {
		call fprintf (fd, "[%d]")
		    call pargi (MEF_CGROUP(mef))
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
	    if (Memc[imsec] != EOS) {
		call fprintf (fd, "%s")
		    call pargstr (Memc[imsec])
	    }
	    call fprintf (fd, "\n")
	}

	# Finish up.
	call mefclose (mef)
	call sfree (sp)
end


include	<mach.h>
include	<ctype.h>

define	FIRST	0		# Default starting range
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


# XT_EXTMATCH -- Match extname against a comma-delimited list of patterns.

bool procedure xt_extmatch (extname, patterns)

char	extname[ARB]		#I Extension name to match
char	patterns[ARB]		#I Comma-delimited list of patterns
bool	stat			#O Match?

int	i, j, k, sz_pat, strlen(), patmake(), patmatch(), nowhite()
pointer	sp, patstr, patbuf

begin
	if (patterns[1] == EOS)
	    return (true)

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
		if (j > 0 && patterns[j] == ',' && patterns[j-1] == '\\')
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
