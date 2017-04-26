include <syserr.h>
include	<ctype.h>
include "../ptkeysdef.h"

define	MAX_FIELDS	128
define	SZ_SBUF		1024
define	LEN_FNSTRUCT	(10 + MAX_FIELDS)

define	FN_NENTRIES	Memi[$1]		# number of field names in list
define	FN_NEXT		Memi[$1+1]		# next string to be returned
define	FN_SBUF		Memi[$1+2]		# pointer to string buffer
define	FN_STRP		Memi[$1+10+$2-1]	# array of str ptrs

define	FN_FIELDNAME	Memc[FN_STRP($1,$2)]	# reference a string


# PT_OFNL -- Procedure to decode the template.

pointer procedure pt_ofnl (ap, template)

pointer	ap			# image descriptor
char	template[ARB]		# field name template

int	tp, nstr, ch, junk
pointer	sp, ip, op, rop, fn, pattern, patcode, ranges, nextch
int	patmake(), patmatch()
errchk	syserr

begin
	call smark (sp)
	call salloc (pattern, SZ_FNAME, TY_CHAR)
	call salloc (patcode, SZ_LINE,  TY_CHAR)
	call salloc (ranges, SZ_FNAME, TY_CHAR)

	# Allocate field list descriptor and initialize.
	call calloc (fn, LEN_FNSTRUCT, TY_STRUCT)
	call malloc (FN_SBUF(fn), SZ_SBUF, TY_CHAR)
	nextch = FN_SBUF(fn)
	nstr = 0
	tp = 1

	# Extract each comma delimited template, expand upon the aphot
	# database and add strings to list.

	while (template[tp] != EOS && template[tp] != '\n') {

	    # Advance to next field.
	    while (IS_WHITE(template[tp]) || template[tp] == ',')
		tp = tp + 1

	    # Extract pattern.  Enclose pattern in {} so that the match will
	    # be case insensitive.

	    op = pattern
	    Memc[op] = '^'
	    op = op + 1
	    Memc[op] = '{'
	    op = op + 1

	    # Fetch the pattern.
	    ch = template[tp]
	    while (! (IS_WHITE(ch) || ch == '\n' || ch == ',' || ch == '[' ||
	        ch == EOS)) {

		# Map "*" into "?*".
		if (ch == '*') {
		    Memc[op] = '?'
		    op = op + 1
		}

		# Update.
		Memc[op] = ch
		op = op + 1
		tp = tp + 1
		ch = template[tp]
	    }

	    # Decode ranges.
	    if (ch == '[') {
		rop = ranges
		while (! (ch == '\n' || ch == EOS || ch == ']')) {
		    Memc[rop] = ch
		    rop = rop + 1
		    tp = tp + 1
		    ch = template[tp]
		}
		Memc[rop] = ']'
		rop = rop + 1
		tp = tp + 1
		ch = template[tp]
		while (ch != EOS && ch != '\n' && ch != ',' && IS_DIGIT(ch)) {
		    Memc[rop] = ch
		    tp = tp + 1
		    ch = template[tp]
		    rop = rop + 1
		}
		Memc[rop] = EOS
	    } else
		Memc[ranges] = EOS

	    # Close off the pattern.
	    Memc[op] = '}'
	    op = op + 1
	    Memc[op] = EOS

	    # Encode the pattern.
	    junk = patmake (Memc[pattern], Memc[patcode], SZ_LINE)

	    # Scan database and extract all field names matching the
	    # pattern.

	    for (ip = KY_WORDS(ap) + 1;  Memc[ip] != EOS;  ip = ip + 1) {

		# Put key in list if it matches.
		if (patmatch (Memc[ip], Memc[patcode]) > 0) {
		    call pt_fnputkey (Memc[ip], Memc[ranges], FN_STRP(fn,1),
		        nstr, nextch, FN_SBUF(fn))
		}

		# Advance to the next record.
		while (Memc[ip] != ',' && Memc[ip] != EOS)
		    ip = ip + 1

		# Quit if you hit EOS
		if (Memc[ip] == EOS)
		    break
	    }
	}

	FN_NENTRIES(fn) = nstr
	FN_NEXT(fn)     = 1
	call sfree (sp)
	return (fn)
end


# PT_GNFN -- Get the next field name matching the given template from the
# apphot data base.

int procedure pt_gnfn (fn, outstr, ranges, maxch)

pointer	fn			# field name list descriptor
char	outstr[ARB]		# output string
char	ranges[ARB]		# ranges string
int	maxch			# maximum number of characters

char	left_bkt
int	strnum, findex, nchars
int	gstrcpy(), stridx()
data	left_bkt /'['/

begin
	# Initialize.
	ranges[1] = EOS
	outstr[1] = EOS

	# Check that there is an entry
	strnum = FN_NEXT(fn)
	if (strnum > FN_NENTRIES(fn))
	    return (EOF)

	# Get the next field name.
	nchars = gstrcpy (FN_FIELDNAME(fn,strnum), outstr, maxch)

	# Get the ranges string.
	findex = stridx (left_bkt, outstr)
	if (findex > 0) {
	    call strcpy (outstr[findex], ranges, maxch)
	    outstr[findex] = EOS
	}

	# Increment counter.
	FN_NEXT(fn) = strnum + 1

	return (nchars)
end


# PT_CFNL -- Procedure to close the list.

procedure pt_cfnl (fn)

pointer	fn			# field name list descriptor

begin
	call mfree (FN_SBUF(fn), TY_CHAR)
	call mfree (fn, TY_STRUCT)
end



# PT_FNPUTKEY -- Put a keyword into the keyword list.

procedure pt_fnputkey (key, ranges, strp, nstr, nextch, sbuf)

char	key[ARB]		# keyword name (etc.)
char	ranges[ARB]		# list of ranges
pointer	strp[ARB]		# array of string pointers
int	nstr			# current number of strings
pointer	nextch			# next available char in string buffer
pointer	sbuf			# string buffer

int	ch, ip, rip
errchk	syserr

begin
	# Check size of string buffer.
	nstr = nstr + 1
	if (nstr > MAX_FIELDS)
	    call error (0, "There too many fields in the input template")

	# Initialize.
	strp[nstr] = nextch
	ip = 1
	ch = key[ip]

	# Append keyword to the string buffer.
	while (ch != ',' && ch != ' ' && ch != EOS) {
	    Memc[nextch] = ch
	    nextch = nextch + 1
	    if (nextch >= sbuf + SZ_SBUF)
	        call error (0, "There too many fields in the input template")
	    ip = ip + 1
	    ch = key[ip]
	}

	# Get the ranges information.
	rip = 1
	while (ranges[rip] != EOS) {
	    Memc[nextch] = ranges[rip]
	    nextch = nextch + 1
	    if (nextch >= sbuf + SZ_SBUF)
	        call error (0, "There too many fields in the input template")
	    rip = rip + 1
	}

	Memc[nextch] = EOS
	nextch = nextch + 1
end
