# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"help.h"

.help hinput
.nf __________________________________________________________________________
HINPUT -- Lroff line input procedure.  Called by Lroff to get lines of
input text.  Function varies slightly depending on the Help option.
If printing only single param or single section, our job is to eat all
input which is not part of the indicated section of the help block.
A parameter block begins with ".ls paramname" and ends with a matching
".le" or ".ih", if the text is formatted.  Similarly, the single section
begins with a ".ih" followed by the section name on the next line.
.endhelp ______________________________________________________________________

define	SEARCHING	1
define	IN_BLOCK	2
define	MAXPAT		10


# HINPUT -- Get next input line from the help block.

int procedure hinput (ctrl, lbuf)

pointer	ctrl
char	lbuf[ARB]

bool	formatted
int	fd, level, status, ip
int	getline(), strmatch(), gstrcpy(), stridxs()
int	h_findparam(), h_findsection()
errchk	getline, h_findparam, h_findsection
define	findsec_ 91

begin
	fd = H_IN(ctrl)

	# EOF flag is set after param block has been fully input,
	# but normally before the actual end of file.

	if (H_EOF(ctrl) == YES || H_QUIT(ctrl) == YES)
	    return (EOF)
	else if (H_RAWIN(ctrl) == YES || H_FILTER_INPUT(ctrl) == NO)
	    return (getline (fd, lbuf))

	# We get here only if special processing is required to
	# filter out all but a section of the help text.

	switch (H_STATE(ctrl)) {
	case BOF:
	    # Determine whether or not the help block is formatted.
	    # Read in first line but save it for later and return a
	    # dummy line to Lroff, telling it whether or not the help
	    # block is formatted.

	    repeat {
		if (getline (fd, lbuf) == EOF)
		    return (EOF)
		for (ip=1;  IS_WHITE(lbuf[ip]);  ip=ip+1)
		    ;
	    } until (lbuf[ip] != '\n')
	    call ungetline (fd, lbuf)

	    if (lbuf[1] == '.') {
		formatted = true
		status = gstrcpy (".in 4\n", lbuf, SZ_LINE)
	    } else {
		formatted = false
		status = gstrcpy (".nf\n", lbuf, SZ_LINE)
	    }
	    H_STATE(ctrl) = SEARCHING

	case SEARCHING:
	    status = getline (fd, lbuf)
	    if (H_PARNAME(ctrl) != EOS) {
		status = h_findparam (fd, lbuf, formatted, H_PARNAME(ctrl))
		level = 1
	    } else if (H_SECNAME(ctrl) != EOS)
findsec_	status = h_findsection (fd, lbuf, formatted, H_SECNAME(ctrl))
	    H_STATE(ctrl) = IN_BLOCK

	case IN_BLOCK:
	    # By the time we get here we are in the parameter or single
	    # section.
	    status = getline (fd, lbuf)

	    if (lbuf[1] == '.') {
		if (strmatch (lbuf, "^.{ih}") > 0) {
		    if (stridxs ("|", H_SECNAME(ctrl)) > 0)
			goto findsec_
		    status = EOF
		} else if (H_PARNAME(ctrl) != EOS) {
		    if (strmatch (lbuf, "^.{ls}") > 0)
			level = level + 1
		    else if (strmatch (lbuf, "^.{le}") > 0) {
			level = level - 1
			if (level == 0)
			    status = EOF
		    }
		}
	    }

	default:
	    call error (15, "hinput: unknown input state encountered")
	}

	return (status)
end


# H_FINDPARAM -- If text contains format directives, eat input lines until
# a ".ls" directive is found which contains the param name as a substring.
# If the text is not formatted, search for a line beginning with the pattern.
# We are called with the first line of the file in lbuf.

int procedure h_findparam (fd, lbuf, formatted, param)

int	fd
char	lbuf
bool	formatted
char	param[ARB]

bool	match_found
pointer	sp, pattern
int	getline(), strmatch(), strlen()
errchk	getline

begin
	call smark (sp)
	call salloc (pattern, SZ_FNAME, TY_CHAR)

	match_found = false

	if (formatted) {
	    call sprintf (Memc[pattern], SZ_FNAME, "{%s}")
		call pargstr (param)
	    repeat {
		if (strmatch (lbuf, "^.{ls}") > 0)
		    if (strmatch (lbuf, Memc[pattern]) > 0) {
			match_found = true
			break
		    }
	    } until (getline (fd, lbuf) == EOF)

	} else {
	    call sprintf (Memc[pattern], SZ_FNAME, "^#{%s}")
		call pargstr (param)
	    repeat {
		if (strmatch (lbuf, Memc[pattern]) > 0) {
		    match_found = true
		    break
		}
	    } until (getline (fd, lbuf) == EOF)
	}

	call sfree (sp)
	if (match_found)
	    return (strlen (lbuf))
	else
	    return (EOF)
end


# H_FINDSECTION -- If text contains format directives, eat input lines until
# a ".ih" directive is found for the named section.  If the text is not
# formatted, search for a line beginning with the section name.
# We are called with the first line of the file in lbuf.

int procedure h_findsection (fd, lbuf, formatted, sections)

int	fd			# input file
char	lbuf			# line buffer
bool	formatted		# is help block formatted
char	sections[ARB]		# list of sections "a|b|c"

bool	match_found
int	npat, ip
pointer	sp, patbuf, patoff[MAXPAT], op
bool	h_match()
int	getline(), strmatch(), gstrcpy()
errchk	getline

begin
	call smark (sp)
	call salloc (patbuf, SZ_LINE, TY_CHAR)

	# Process the list of sections into patbuf and patoff, i.e., into a
	# list of EOS delimited strings in the string buffer patbuf.  Each
	# section name or abbreviation is delimited by '|' (or).

	npat = 1
	op = patbuf
	patoff[1] = op

	for (ip=1;  sections[ip] != EOS;  ip=ip+1)
	    switch (sections[ip]) {
	    case '|':
		Memc[op] = EOS
		op = op + 1
		npat = min (MAXPAT, npat + 1)
		patoff[npat] = op
	    default:
		Memc[op] = sections[ip]
		op = op + 1
	    }
	Memc[op] = EOS

	match_found = false

	if (formatted) {
	    repeat {
		if (strmatch (lbuf, "^.{ih}") > 0)
		    if (getline (fd, lbuf) != EOF) {
			match_found = h_match (lbuf, patoff, npat)
			if (match_found)
			    break
		    }
	    } until (getline (fd, lbuf) == EOF)

	} else {
	    repeat {
		match_found = h_match (lbuf, patoff, npat)
		if (match_found)
		    break
	    } until (getline (fd, lbuf) == EOF)
	}


	# If only one section is to be printed, skip the section name, otherwise
	# pass the .ih secname on to the text formatter.

	call sfree (sp)
	if (match_found) {
	    if (npat == 1)
		return (getline (fd, lbuf))
	    else {
		call ungetline (fd, lbuf)
		return (gstrcpy (".ih\n", lbuf, SZ_LINE))
	    }
	} else
	    return (EOF)
end


# H_MATCH -- Match a set of patterns against a line of test, matching only
# at the beginning of line in either case.

bool procedure h_match (lbuf, patoff, npat)

char	lbuf[ARB]		# line of text
pointer	patoff[npat]		# pointers to pattern strings
int	npat			# number of patterns

int	pat
pointer	sp, pattern
int	strmatch()

begin
	call smark (sp)
	call salloc (pattern, SZ_FNAME, TY_CHAR)

	for (pat=1;  pat <= npat;  pat=pat+1) {
	    call sprintf (Memc[pattern], SZ_FNAME, "^{%s}")
		call pargstr (Memc[patoff[pat]])
	    if (strmatch (lbuf, Memc[pattern]) > 0) {
		call sfree (sp)
		return (true)
	    }
	}

	call sfree (sp)
	return (false)
end
