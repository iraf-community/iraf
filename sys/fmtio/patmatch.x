# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pattern.h>
include	<ctype.h>
include	<chars.h>

# PATMATCH.X -- Routines for matching regular expressions (general pattern
# matching).  Adapted from Software Tools.
# 
#	      patsize = patmake (patstr, patbuf, sz_patbuf)
#	   next_char = patmatch (str, patbuf)
#	  next_char = gpatmatch (str, patbuf, first_char, last_char)
#	          ip = patindex (patbuf, index_number)
#
# The pattern string must be encoded with PATMAKE before use.  See also
# STRMATCH, STRNCMP, etc.

# Pattern codes (for encoded patterns).

define	EOP		-1		# end of encoded pattern
define	CHAR		-2		# match char
define	UCHAR		-3		# match either case
define	LCHAR		-4		# match either case
define	BOL		-5		# match at beginning of line
define	EOL		-6		# match at end of line
define	ANY		-7		# "?"
define	WHITESPACE	-8		# "#"
define	CCL		-9		# [...
define	NCCL		-10		# [^...
define	CLOSURE		-11		# "*"
define	INDEX		-12		# % (mark index of ^ in pattern)

define	CH_INDEX	'%'		# move to <chars.h> after a while

# Definitions for the closure structure.

define	CLOSIZE		4		# size of closure structure
define	COUNT		1		# repeat count for matches
define	PREVCL		2		# index of previous closure in pat
define	START		3		# index in str where match starts


# PATMATCH -- Match pattern anywhere on line.  Returns the index of the
# first character AFTER the match, or zero if no match.

int procedure patmatch (str, pat)

char	str[ARB]		# string to be scanned
char	pat[ARB]		# encoded pattern

int	first_char, last_char
int	gpatmatch()

begin
	return (gpatmatch (str, pat, first_char, last_char))
end


# GPATMATCH -- Generalized pattern match.  Matches pattern anywhere on
# line (the first such pattern matched terminates the search).  Function
# return same as for PATMATCH, but also returns indices of the first and
# last characters in the matched substring.

int procedure gpatmatch (str, pat, first_char, last_char)

char	str[ARB]		# string to be scanned
char	pat[ARB]		# encoded pattern
int	first_char		# index of first char matched (output)
int	last_char		# index of last  char matched (output)

int	ip, nchars_matched
int	pat_amatch()				# anchored match

begin
	nchars_matched = 0

	if (pat[1] == BOL) {
	    ip = 1
	    nchars_matched = pat_amatch (str, ip, pat)
	} else {
	    for (ip=1;  str[ip] != EOS;  ip=ip+1) {
		nchars_matched = pat_amatch (str, ip, pat)
		if (nchars_matched > 0)
		    break
	    }
	}

	if (nchars_matched > 0) {
	    first_char = ip
	    last_char = ip + nchars_matched - 1
	    return (last_char + 1)
	} else
	    return (0)
end


# PATINDEX -- Return the index of a marked position in the pattern.  Inclusion
# of the character % in the pattern causes the index of the character following
# the % to be saved in the encoded pattern at patmatch time.  We are called
# after a patmatch operation to scan the pattern and recall the Nth saved index.
# Zero is returned if N is larger than the number of saved index points.

int procedure patindex (pat, n)

char	pat[ARB]		# encoded pattern
int	n			# number of index to be returned

int	pp, ix
int	pat_gsize()

begin
	ix = 1
	for (pp=1;  pat[pp] != EOP;  pp=pp+pat_gsize(pat,pp))
	    if (pat[pp] == INDEX)
		if (ix >= n)
		    return (pat[pp+1])
		else
		    ix = ix + 1

	return (0)
end


# PAT_AMATCH -- Anchored match.  Look for match starting at the given
# offset.  Return the number of characters matched.

int procedure pat_amatch (str, from, pat)

char	str[ARB]		# string to be matched
int	from			# starting at this index
char	pat[ARB]		# encoded pattern

int	ip, pp, offset, stack
int	pat_omatch(), pat_gsize()

begin
	stack = 0
	offset = from				# next unexamined input char

	for (pp=1;  pat[pp] != EOP;  pp = pp + pat_gsize(pat,pp)) {
	    if (pat[pp] == CLOSURE) {		# a closure entry
		stack = pp
		pp = pp + CLOSIZE	
		# Match as many characters as possible, save results
		for (ip=offset;  str[ip] != EOS;  )
		    if (pat_omatch (str, ip, pat, pp) == NO)
			break
		pat[stack+COUNT] = ip - offset
		pat[stack+START] = offset
		offset = ip			# character that made us fail

	    } else if (pat_omatch (str, offset, pat, pp) == NO) {
		for (;  stack > 0;  stack = pat[stack+PREVCL])
		    if (pat[stack+COUNT] > 0)
			break
		if (stack <= 0)			# stack is empty
		    return (0)			# return failure

		pat[stack+COUNT] = pat[stack+COUNT] - 1
		pp = stack + CLOSIZE
		offset = pat[stack+START] + pat[stack+COUNT]
	    }
	}

	return (offset-from)			# successful match
end


# PAT_GSIZE -- Returns size of pattern entry at pat[n].

int procedure pat_gsize (pat, n)

char	pat[ARB]		# encoded pattern
int	n			# pointer into pattern
int	pattern_size

begin
	switch (pat[n]) {
	case CHAR, UCHAR, LCHAR, INDEX:
	    pattern_size = 2
	case BOL, EOL, ANY, WHITESPACE:
	    pattern_size = 1
	case CCL, NCCL:
	    pattern_size = pat[n+1] + 2
	case CLOSURE:					# not used
	    pattern_size = CLOSIZE
	default:
	    call error (0, "In patsize: can't happen.")
	}

	return (pattern_size)
end


# PAT_OMATCH -- Try to match a single pattern at pat[pp].  If match, bump IP
# to point to the next unmatched character.  Return OK if match.

int procedure pat_omatch (str, ip, pat, pp)

char	str[ARB]		# string to be scanned
int	ip			# starting index in string (may be changed)
char	pat[ARB]		# encoded pattern
int	pp			# pointer to next pattern element

char	str_ch
int	bump, pat_locate()

begin
	if (str[ip] == EOS)
	    if (pat[pp] == INDEX) {
		pat[pp+1] = ip
		return (YES)
	    } else if (pat[pp] == EOL) {
		return (YES)
	    } else
		return (NO)

	# Treat CHAR (simple character match) as a special case to speed
	# things up a bit.

	if (pat[pp] == CHAR)
	    if (str[ip] == pat[pp+1]) {
		ip = ip + 1
		return (YES)
	    } else
		return (NO)

	# Compare as indicated by encoded pattern opcode.
	bump = -1

	switch (pat[pp]) {
	case UCHAR:					# match either case
	    str_ch = str[ip]
	    if (IS_LOWER (str_ch))
		str_ch = TO_UPPER (str_ch)
	    if (str_ch == pat[pp+1])
		bump = 1
	case LCHAR:					# match either case
	    str_ch = str[ip]
	    if (IS_UPPER (str_ch))
		str_ch = TO_LOWER (str_ch)
	    if (str_ch == pat[pp+1])
		bump = 1
	case BOL:					# beg. of line
	    if (ip == 1)
		bump = 0
	case EOL:					# end of line
	    if (str[ip] == '\n')
		bump = 0
	case ANY:					# match any char
	    if (str[ip] != '\n')
		bump = 1
	case WHITESPACE:
	    for (bump=0;  IS_WHITE (str[ip+bump]);  bump=bump+1)
		;
	case CCL:					# char class
	    if (pat_locate (str[ip], pat, pp + 1) == YES)
		bump = 1
	case NCCL:					# not in char class
	    if (str[ip] != '\n' && pat_locate (str[ip], pat, pp + 1) == NO)
		bump = 1
	case INDEX:
	    pat[pp+1] = ip
	    bump = 0
	default:
	    call error (0, "In omatch: can't happen.")
	}

	if (bump >= 0) {
	    ip = ip + bump
	    return (YES)
	} else
	    return (NO)
end


# PAT_LOCATE -- Look for c in char class at pat[offset].

int procedure pat_locate (ch, pat, offset)

char	ch			# char to search for
char	pat[ARB]		# encoded pattern
int	offset			# offset of character class in pattern

int	nchars, i

begin
	# Size of class is at pat[offset], characters follow.
	nchars = pat[offset]
	do i = 1, nchars
	    if (ch == pat[offset+i])
		return (YES)

	return (NO)
end


# PATMAKE -- Encode pattern specification string.  Returns the size of
# the encoded pattern string.

int procedure patmake (str, pat, sz_pat)

char	str[ARB]		# pattern to be encoded
char	pat[ARB]		# encoded pattern (output)
int	sz_pat			# max size of the pattern string
int	gpatmake()

begin
	return (gpatmake (str, 1, EOS, pat, sz_pat))
end


# GPATMAKE -- Make pattern from str[from], terminate at delim.

int procedure gpatmake (patstr, from, delim, patbuf, sz_pat)

char	patstr[ARB]		# pattern to be encoded
int	from			# starting index
int	delim			# delimiter character
char	patbuf[ARB]		# put encoded pattern here
int	sz_pat			# max chars in encoded pattern

int	ip, op, last_closure, last_op, l_op
char	cval
bool	ignore_case
int	cctoc(), pat_getccl(), pat_stclos()

begin
	op = 1					      # pat index
	last_op = 1
	last_closure = 0
	ignore_case = false

	for (ip=from;  patstr[ip] != delim && patstr[ip] != EOS;  ip=ip+1) {
	    l_op = op

	    # If CVAL gets set to nonzero it will be deposited in the output
	    # buffer at end of switch.

	    cval = 0

	    switch (patstr[ip]) {
	    case CH_ANY:
		cval = ANY
	    case CH_WHITESPACE:
		cval = WHITESPACE

	    case CH_BOL:
		if (ip == from)
		    cval = BOL
		else {
		    cval = CHAR
		    call chdeposit (cval, patbuf, sz_pat, op)
		    cval = CH_BOL
		}

	    case CH_EOL:
		if (patstr[ip+1] == delim)
		    cval = EOL
		else {
		    cval = CHAR
		    call chdeposit (cval, patbuf, sz_pat, op)
		    cval = CH_EOL
		}

	    case CH_IGNORECASE:
		ignore_case = true
	    case CH_MATCHCASE:
		ignore_case = false

	    case CH_CCL:
		if (pat_getccl (patstr, patbuf, sz_pat, ip, op) == ERR)
		    return (ERR)

	    case CH_CLOSURE:
		# The "closure" of a pattern, e.g., "..*".

		l_op = last_op
		# Convert a pattern such as "*..." into "?*...".
		if (ip == from)				# closure of nothing
		    cval = ANY
		else {
		    switch (patbuf[l_op]) {
		    case BOL, EOL, CLOSURE:
			cval = ANY
		    }
		}

		if (cval != 0)
		    call chdeposit (cval, patbuf, sz_pat, op)
		cval = 0

		last_closure = pat_stclos (patbuf, sz_pat, op, last_op,
		    last_closure)

	    case CH_INDEX:
		# This metacharacter does not match anything, but rather is
		# used to record the index of the marked position in the
		# matched pattern.  The index is recorded in the pattern
		# buffer at match time, to be later recovered with patindex.

		cval = INDEX
		call chdeposit (cval, patbuf, sz_pat, op)
		cval = 0
		call chdeposit (cval, patbuf, sz_pat, op)

	    default:
		# Ordinary character.

		# Deposit command code.
		if (ignore_case) {
		    if (IS_UPPER (patstr[ip]))
			cval = UCHAR
		    else
			cval = LCHAR
		} else
		    cval = CHAR
		call chdeposit (cval, patbuf, sz_pat, op)

		# Set CVAL to actual character value.
		if (patstr[ip] == CH_ESCAPE) {
		    if (cctoc (patstr, ip, cval) == 1)
			cval = patstr[ip]
		    else
			ip = ip - 1
		} else
		    cval = patstr[ip]
	    }

	    # Deposit the character left in CVAL by the code above.
	    if (cval != 0)
		call chdeposit (cval, patbuf, sz_pat, op)

	    last_op = l_op
	}

	# Terminate the pattern.
	cval = EOP
	call chdeposit (cval, patbuf, sz_pat, op)

	if (patstr[ip] != delim || op >= sz_pat)
	    return (ERR)
	else
	    return (op - 1)				# return size patbuf
end


# PAT_GETCCL -- Expand character class at patstr[i] into patbuf[op].

int procedure pat_getccl (patstr, patbuf, sz_pat, ip, op)

char	patstr[ARB], patbuf[ARB]
int	sz_pat, ip, op
char	cval
int	op_start

begin
	ip = ip + 1					# skip over [
	if (patstr[ip] == CH_NOT) {
	    cval = NCCL
	    ip = ip + 1
	} else
	    cval = CCL

	call chdeposit (cval, patbuf, sz_pat, op)

	op_start = op
	cval = 0
	call chdeposit (cval, patbuf, sz_pat, op)	# leave room for count
	call pat_filset (CH_CCLEND, patstr, ip, patbuf, sz_pat, op)
	patbuf[op_start] = op - op_start - 1		# fix up count

	if (patstr[ip] == CH_CCLEND)
	    return (OK)
	else
	    return (ERR)
end


# PAT_STCLOS -- Insert closure entry at patbuf[op].

int procedure pat_stclos (patbuf, sz_pat, op, last_op, last_closure)

char	patbuf[ARB]
int	sz_pat
int	op
int	last_op
int	last_closure

char	cvals[4]
int	next_closure, jp, jt, i

begin
	for (jp=op-1;  jp >= last_op;  jp=jp-1) {	# make a hole
	    jt = min (sz_pat, jp + CLOSIZE)
	    patbuf[jt] = patbuf[jp]
	}

	op = op + CLOSIZE
	next_closure = last_op

	cvals[1] = CLOSURE
	cvals[2] = 0				# COUNT
	cvals[3] = last_closure			# PREVCL
	cvals[4] = 0				# START

	do i = 1, 4
	    call chdeposit (cvals[i], patbuf, sz_pat, last_op)

	return (next_closure)
end


# PAT_FILSET -- Process a character class into a simple list of characters.

procedure pat_filset (delim, patstr, ip, patbuf, sz_pat, op)

int	delim			# character class delimiter character
char	patstr[ARB]		# character class characters
int	ip			# index where they start
char	patbuf[ARB]		# encode character class in this string
int	sz_pat			# max chars out
int	op			# offset into patbuf

char	ch, ch1, ch2
int	cctoc()

begin
	for (;  patstr[ip] != delim && patstr[ip] != EOS;  ip=ip+1) {
	    if (patstr[ip] == ESCAPE) {				# escape seq.
		if (cctoc (patstr, ip, ch) == 1)
		    ch = patstr[ip]
		else
		    ip = ip - 1
		call chdeposit (ch, patbuf, sz_pat, op)

	    } else if (patstr[ip] != CH_RANGE) {
		call chdeposit (patstr[ip], patbuf, sz_pat, op)

	    } else if (op <= 1 || patstr[ip+1] == EOS) {	# literal "-"
		ch = CH_RANGE
		call chdeposit (ch, patbuf, sz_pat, op)

	    } else {
		# Here if char is CH_RANGE, denoting a range of characters to be
		# included in the character class.  Range is valid only if limit
		# chars are both digits, both lower case, or both upper case.

		ch1 = patbuf[op-1]		# not same as patstr[ip-1]
		ch2 = patstr[ip+1]

		if ((IS_DIGIT (ch1) && IS_DIGIT (ch2)) ||
		    (IS_LOWER (ch1) && IS_LOWER (ch2)) ||
		    (IS_UPPER (ch1) && IS_UPPER (ch2))) {
			if (ch1 <= ch2)
			    for (ch=ch1+1;  ch <= ch2;  ch=ch+1)
				call chdeposit (ch, patbuf, sz_pat, op)
			else
			    for (ch=ch1-1;  ch >= ch2;  ch=ch-1)
				call chdeposit (ch, patbuf, sz_pat, op)
			ip = ip + 1
		} else {
		    ch = CH_RANGE
		    call chdeposit (ch, patbuf, sz_pat, op)
		}
	    }
	}
end
