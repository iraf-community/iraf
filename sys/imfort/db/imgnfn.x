# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<ctype.h>
include	<imhdr.h>
include	"../imfort.h"
include	"idb.h"

.help imgnfn
.nf --------------------------------------------------------------------------
IMGNFN -- Template expansion for header keywords.

	list =	imofnl[su] (im, template)	# open list
	nch =	    imgnfn (im, outstr, maxch)	# get next field name
		    imcfnl (im)			# close list

IMOFNLS opens the list sorted, whereas IMOFNLU opens it unsorted.  Both std.
and user header keywords are included in the list.
.endhelp ---------------------------------------------------------------------

define	MAX_FIELDS	128
define	SZ_SBUF		1024
define	LEN_FNSTRUCT	(10+MAX_FIELDS)

define	FN_NENTRIES	Memi[$1]	# number of field names in list
define	FN_NEXT		Memi[$1+1]	# next string to be returned
define	FN_SBUF		Memi[$1+2]	# pointer to string buffer
			# open
define	FN_STRP		Memi[$1+10+$2-1]	# array of str ptrs
define	FN_FIELDNAME	Memc[FN_STRP($1,$2)]	# reference a string


# IMGNFN -- Get the next header field name matching the given template from an
# image header database.  Sorting of the field list is optional.  A prior call
# to IMOFNL[SU] is necessary to open the sorted or unsorted list.

int procedure imgnfn (fn, outstr, maxch)

pointer	fn			# field name list descriptor
char	outstr[ARB]		# output string
int	maxch

int	strnum
int	gstrcpy()

begin
	strnum = FN_NEXT(fn)
	if (strnum > FN_NENTRIES(fn))
	    return (EOF)
	FN_NEXT(fn) = strnum + 1

	return (gstrcpy (FN_FIELDNAME(fn,strnum), outstr, maxch))
end


# IMOFNLS -- Open a sorted field name list.

pointer procedure imofnls (im, template)

pointer	im			# image descriptor
char	template[ARB]		# field name template
pointer	imofnl()

begin
	return (imofnl (im, template, YES))
end


# IMOFNLU -- Open an unsorted field name list.

pointer procedure imofnlu (im, template)

pointer	im			# image descriptor
char	template[ARB]		# field name template
pointer	imofnl()

begin
	return (imofnl (im, template, NO))
end


# IMCFNL -- Close the image header field name list and return all associated
# storage.

procedure imcfnl (fn)

pointer	fn			# field name list descriptor

begin
	call mfree (FN_SBUF(fn), TY_CHAR)
	call mfree (fn, TY_STRUCT)
end


# IMOFNL -- Open an image header field name list, either sorted or unsorted.
# A template is a list of patterns delimited by commas.

pointer procedure imofnl (im, template, sort)

pointer	im			# image descriptor
char	template[ARB]		# field name template
int	sort			# sort flag

bool	escape
int	tp, nstr, ch, junk, first_string, nstrings, nmatch, i
pointer	sp, ip, op, fn, kwname, sbuf, pattern, patcode, nextch
int	patmake(), patmatch(), strlen()
errchk	syserr

begin
	call smark (sp)
	call salloc (kwname, SZ_FNAME, TY_CHAR)
	call salloc (pattern, SZ_FNAME, TY_CHAR)
	call salloc (patcode, SZ_LINE,  TY_CHAR)

	# Allocate field list descriptor.
	call calloc (fn, LEN_FNSTRUCT, TY_STRUCT)
	call malloc (sbuf, SZ_SBUF, TY_CHAR)

	FN_SBUF(fn) = sbuf
	nextch = sbuf
	nstr = 0
	tp = 1

	# Extract each comma delimited template, expand upon image header
	# field list, sort if desired, and add strings to list.

	while (template[tp] != EOS && template[tp] != '\n') {
	    # Advance to next field.
	    while (IS_WHITE(template[tp]) || template[tp] == ',')
		tp = tp + 1

	    # Extract pattern.  Enclose pattern in ^{} so that the match will
	    # occur only at the beginning of each line and will be case
	    # insensitive (req'd for FITS format).

	    op = pattern
	    Memc[op] = '^'
	    op = op + 1
	    Memc[op] = '{'
	    op = op + 1

	    # A field name of the form "$", "$x", etc. is not matched against
	    # the actual image field list, but is included in the output field
	    # list as a literal.

	    ch = template[tp]
	    escape = (ch == '$')

	    while (! (IS_WHITE(ch) || ch == '\n' || ch == ',' || ch == EOS)) {
		# Map "*" into "?*".
		if (ch == '*') {
		    Memc[op] = '?'
		    op = op + 1
		}

		Memc[op] = ch
		op = op + 1
		tp = tp + 1
		ch = template[tp]
	    }

	    Memc[op] = '}'
	    op = op + 1
	    Memc[op] = EOS

	    # If the pattern is a literal, put it in the output list without
	    # matching it against the image field list.

	    if (escape) {
		# Omit the leading "^{" and the trailing "}".
		ip = pattern + 2
		op = op - 1
		Memc[op] = EOS
		call imfn_putkey (Memc[ip], FN_STRP(fn,1), nstr, nextch, sbuf)

	    } else {
		# Encode pattern.
		junk = patmake (Memc[pattern], Memc[patcode], SZ_LINE)

		# Scan database and extract all field names matching the
		# pattern.  Mark number of first string for the sort.

		first_string = nstr + 1

		# First find any standard header keywords matching the pattern.
		call imfn_stdkeys (im, Memc[patcode], FN_STRP(fn,1), nstr,
		    nextch, sbuf)

		# Now scan the user area.
		for (ip=IM_USERAREA(im);  Memc[ip] != EOS;  ip=ip+1) {
		    # Skip entries that are not keywords.
		    if (Memc[ip+8] != '=')
			next

		    # Extract keyword name.
		    Memc[kwname+8] = EOS
		    do i = 1, 8 {
			ch = Memc[ip+i-1]
			if (ch == ' ') {
			    Memc[kwname+i-1] = EOS
			    break
			} else
			    Memc[kwname+i-1] = ch
		    }

		    # Check for a match.
		    if (Memc[kwname] != EOS) {
			# Put key in list if it matches.
			nmatch = patmatch (Memc[kwname], Memc[patcode]) - 1
			if (nmatch > 0 && nmatch == strlen(Memc[kwname]))
			    call imfn_putkey (Memc[ip],
				FN_STRP(fn,1), nstr, nextch, sbuf)
		    }

		    # Advance to the next record.
		    if (IM_UABLOCKED(im) == YES)
			ip = ip + IDB_RECLEN
		    else {
			while (Memc[ip] != '\n' && Memc[ip] != EOS)
			    ip = ip + 1
		    }

		    if (Memc[ip] == EOS)
			break
		}

		# Sort the newly added keywords.
		nstrings = nstr - first_string + 1
		if (sort == YES && nstrings > 1)
		    call strsrt (FN_STRP(fn,first_string), Memc, nstrings)
	    }
	}

	FN_NENTRIES(fn) = nstr
	FN_NEXT(fn)     = 1

	call sfree (sp)
	return (fn)
end


# IMFN_STDKEYS -- Match a pattern (encoded) against the list of standard header
# keywords, both with and without the "i_" prefix.  Add the full name (with i_
# prefix) of each name matched to the keyword list.  Note that by default,
# only the "user" keywords are matched in this way, although any keyword can
# be accessed if its name is known (i.e., not all keywords are visible).

procedure imfn_stdkeys (im, patcode, strp, nstr, nextch, sbuf)

pointer	im			# image descriptor
char	patcode[ARB]		# encoded pattern
pointer	strp[ARB]		# array of string pointers
int	nstr			# current number of strings
pointer	nextch			# next available char in string buffer
pointer	sbuf			# string buffer

bool	validfield
int	ip, index
pointer	sp, op, key
int	patmatch()
errchk	imfn_putkey

# NOTE index values below depend upon position in this string.
string	keywords "|naxis|naxis1|naxis2|naxis3|pixtype|datamin|datamax|\
ctime|mtime|limtime|title|"

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	call strcpy ("i_", Memc[key], SZ_FNAME)
	index = 1

	for (ip=2;  keywords[ip] != EOS;  ip=ip+1) {
	    # Do not put dimensions NAXIS1, NAXIS2, etc. higher than the
	    # actual image dimension into the matched list.

	    validfield = true
	    if (index >= 2 && index <= 4)
		validfield = (index - 1 <= IM_NDIM(im))

	    # Extract keyword into buffer, after the "i_".
	    for (op=key+2;  keywords[ip] != '|';  op=op+1) {
		Memc[op] = keywords[ip]
		ip = ip + 1
	    }
	    Memc[op] = EOS

	    if (validfield)
		if (patmatch (Memc[key],   patcode) > 0 ||
		    patmatch (Memc[key+2], patcode) > 0) {

		    call imfn_putkey (Memc[key], strp, nstr, nextch, sbuf)
		}

	    index = index + 1
	}

	call sfree (sp)
end


# IMFN_PUTKEY -- Put a keyword into the keyword list.

procedure imfn_putkey (key, strp, nstr, nextch, sbuf)

char	key[ARB]		# keyword name (etc.)
pointer	strp[ARB]		# array of string pointers
int	nstr			# current number of strings
pointer	nextch			# next available char in string buffer
pointer	sbuf			# string buffer

int	ch, ip
errchk	syserr

begin
	# Append keyword to the string buffer.
	nstr = nstr + 1
	if (nstr > MAX_FIELDS)
	    call syserr (SYS_IMFNOVFL)
	strp[nstr] = nextch

	ip = 1
	ch = key[ip]

	while (ch != '=' && ch != ' ' && ch != EOS) {
	    Memc[nextch] = ch
	    nextch = nextch + 1
	    if (nextch >= sbuf + SZ_SBUF)
		call syserr (SYS_IMFNOVFL)
	    ip = ip + 1
	    ch = key[ip]
	}

	Memc[nextch] = EOS
	nextch = nextch + 1
end
