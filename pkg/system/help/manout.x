# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>
include	<ctype.h>
include	"help.h"

.help man_output, man_close
.nf ___________________________________________________________________________
Process a stream of output lines containing forms control codes.  This mode is
used when manual page format is desired.  Input is a sequence of manual pages.
A manual page consists of a formfeed, a two line page header, and then an
arbitrary number of lines of text.  Pages are broken every H_NLPP lines,
the page header is printed at the top of each page, and the page number is
printed at the bottom of each page.  The following special directives may
be inserted in the text as control character forms codes; the forms code must
be the first character in a line but may be followed by text.

	FC_BREAKPAGE		Start a new page, unless already at the
				top of a page.

	FC_TESTPAGE n		Test if there are at least N (binary code)
				lines left on the page.  If no, break the
				page before more text is output.

	FC_STARTKEEP		Start spooling lines into an internal buffer.
				Try to output the keep block all on one page.

	FC_ENDKEEP		Stop spooling and output the keep block.

MAN_CLOSE should be called at program termination to return buffer space and
reinit variables.
.endhelp ______________________________________________________________________

define	LEN_HEADER		2
define	LEN_FOOTER		3
define	LEN_KEEP		1024
define	SZ_PAGELABEL		6
define	SZ_HEADER_GAP		2

define	IN_HEADER		1
define	IN_TEXT			2
define	IN_KEEP			3
define	FLUSH_KEEP		4


# MAN_OUTPUT -- Output a line, optionally preceded by one or more forms control
# characters.  Start a new manual page, save lines in header, save lines in keep
# buffer, or write line to output.

procedure man_output (fd, lbuf, nlines_per_page, lmargin, rmargin)

int	fd			# output file
char	lbuf[ARB]		# next output line
int	nlines_per_page
int	lmargin, rmargin

char	end_keep_chars[3]
int	ip, mode, max_lineno, i

int	nlines_in_keep, nlines_in_header, init
int	lineno, pageno, center_col, nlpp
pointer	header_lines[LEN_HEADER], keep_lines[LEN_KEEP]
common	/hlpman/ lineno, pageno, nlpp, center_col, init,
	nlines_in_header, nlines_in_keep, header_lines, keep_lines

int	stridx()
pointer	man_putstr()
data	end_keep_chars /FC_STARTKEEP, FC_BREAKPAGE, '\f', EOS/

begin
	if (init == YES) {
	    # Initialize internal variables.
	    lineno = 0
	    pageno = 0
	    mode = IN_TEXT
	    nlines_in_header = 0
	    nlines_in_keep = 0
	    nlpp = nlines_per_page
	    max_lineno = max(1, min(LEN_KEEP, nlpp - LEN_FOOTER))
	    center_col = (lmargin + rmargin) / 2
	    init = NO
	}

	for (ip=1;  lbuf[ip] != EOS;  ) {
	    switch (mode) {

	    case IN_HEADER:
		# Just started a new manual page; still accumulating header
		# lines.  Forms control chars not recognized in header area.
		# Output header to start new page as soon as we have all the
		# lines.

		nlines_in_header = nlines_in_header + 1
		header_lines[nlines_in_header] = man_putstr (lbuf[ip])
		if (nlines_in_header == LEN_HEADER) {
		    mode = IN_TEXT
		    call man_breakpage (fd, header_lines, nlines_in_header,
			lineno, pageno, nlpp, center_col)
		    pageno = 1
		}
		break

	    case IN_KEEP:
		# Any forms control character in a keep ends the keep, in case
		# the KE is missing.  Don't bump input pointer unless sentinel
		# is ENDKEEP, else forms control character will be skipped.

		if (lbuf[ip] == FC_ENDKEEP) {
		    mode = FLUSH_KEEP
		    ip = ip + 1
		} else if (stridx (lbuf[ip], end_keep_chars) > 0) {
		    mode = FLUSH_KEEP
		} else if (lineno + nlines_in_keep > max_lineno) {
		    mode = FLUSH_KEEP
		} else {
		    # Put line in keep buffer.
		    nlines_in_keep = nlines_in_keep + 1
		    keep_lines[nlines_in_keep] = man_putstr (lbuf[ip])
		    break
		}

	    case FLUSH_KEEP:
		# Break page and output header if insufficient room left on
		# page.
		if (lineno + nlines_in_keep > max_lineno)
		    call man_breakpage (fd, header_lines, nlines_in_header,
			lineno, pageno, nlpp, center_col)

		# Output contents of keep buffer and return buffer space.
		for (i=1;  i <= nlines_in_keep;  i=i+1) {
		    call putline (fd, Memc[keep_lines[i]])
		    call mfree (keep_lines[i], TY_CHAR)
		    lineno = lineno + 1
		}
		nlines_in_keep = 0
		mode = IN_TEXT

	    case IN_TEXT:
		# Not in any special mode.  Check for forms chars and loop if
		# one is found.  Otherwise output line and bump line pointer.
		# Break page when it fills.

		switch (lbuf[ip]) {
		case FC_BREAKPAGE:
		    call man_breakpage (fd, header_lines, nlines_in_header,
			lineno, pageno, nlpp, center_col)
		    ip = ip + 1

		case FC_TESTPAGE:
		    # The first char following the testpage forms control char
		    # is the number of lines to test for, in binary.

		    if (lineno + lbuf[ip+1] > max_lineno)
			call man_breakpage (fd, header_lines, nlines_in_header,
			    lineno, pageno, nlpp, center_col)
		    ip = ip + 2

		case FC_STARTKEEP:
		    mode = IN_KEEP
		    ip = ip + 1
		case FC_ENDKEEP:
		    ip = ip + 1

		case '\f':
		    # Start accumulating new header.  Page cannot be broken
		    # until we have the header.

		    for (i=1;  i <= nlines_in_header;  i=i+1)
			call mfree (header_lines[i], TY_CHAR)
		    nlines_in_header = 0
		    mode = IN_HEADER
		    ip = ip + 1

		default:
		    # This is the case that gets called most often; output an
		    # ordinary line, not in any special mode.  Eat blank lines
		    # at top of page.

		    if (lineno <= nlines_in_header + SZ_HEADER_GAP + 1) {
			for (i=ip;  IS_WHITE (lbuf[i]);  i=i+1)
			    ;
			if (lbuf[i] == '\n' || lbuf[i] == EOS)
			    break
		    }
		    call putline (fd, lbuf[ip])
		    lineno = lineno + 1
		    if (lineno > max_lineno)
			call man_breakpage (fd, header_lines, nlines_in_header,
			    lineno, pageno, nlpp, center_col)
		    break
		}
	    }
	}	   
end


# MAN_INIT -- Called to initialize the manpage internal data structures.

procedure man_init()

int	i
bool	first_time
data	first_time /true/

int	nlines_in_keep, nlines_in_header, init
int	lineno, pageno, center_col, nlpp
pointer	header_lines[LEN_HEADER], keep_lines[LEN_KEEP]
common	/hlpman/ lineno, pageno, nlpp, center_col, init,
	nlines_in_header, nlines_in_keep, header_lines, keep_lines

begin
	# Clean up in the event of an interrupt.
	if (!first_time) {
	    for (i=1;  i <= nlines_in_header;  i=i+1)
		call mfree (header_lines[i], TY_CHAR)
	    for (i=1;  i <= nlines_in_keep;  i=i+1)
		call mfree (keep_lines[i], TY_CHAR)
	}

	init = YES
	first_time = false
end


# MAN_CLOSE -- Called at program termination to write the page number at the
# bottom of the last manual page.

procedure man_close (fd)

int	fd
int	nlines_in_keep, nlines_in_header, i
int	lineno, pageno, center_col, nlpp, init
pointer	header_lines[LEN_HEADER], keep_lines[LEN_KEEP]
common	/hlpman/ lineno, pageno, nlpp, center_col, init,
	nlines_in_header, nlines_in_keep, header_lines, keep_lines

begin
	if (init == YES)
	    return

	# Trash any data in keep buffer if missing the KE by the time we are
	# called (at program termination).

	call man_breakpage (fd, header_lines, 0, lineno, pageno, nlpp,
	    center_col)

	for (i=1;  i <= nlines_in_header;  i=i+1)
	    call mfree (header_lines[i], TY_CHAR)
	for (i=1;  i <= nlines_in_keep;  i=i+1)
	    call mfree (keep_lines[i], TY_CHAR)

	nlines_in_header = 0
	nlines_in_keep = 0
	init = YES
end


# MAN_BREAKPAGE -- Advance to bottom of page, print page number, bump page
# number, init line counter, formfeed, output page header of new page.

procedure man_breakpage (fd, header_lines, nlines_in_header, lineno, pageno,
			 nlines_per_page, center_col)
int	fd
pointer	header_lines[ARB]
int	nlines_in_header
int	lineno, pageno
int	nlines_per_page
int	center_col

char	pagelabel[SZ_PAGELABEL]
int	i, destcol
int	strlen()

begin
	# Output page footer if there is anything on the page and if there
	# was a page header.

	if (lineno > 0 && nlines_in_header > 0) {
	    while (lineno <= nlines_per_page - 1) {
		call putline (fd, "\n")
		lineno = lineno + 1
	    }

	    call sprintf (pagelabel, SZ_PAGELABEL, "-%d-")
		call pargi (pageno)

	    destcol = max (1, center_col - strlen (pagelabel) / 2)
	    for (i=1;  i < destcol;  i=i+1)
		call putci (fd, ' ')
	    call putline (fd, pagelabel)
	    call putci (fd, '\n')
	}

	# Break page and output page header if there are any header lines.
	# Leave two blank lines between header and first line of text.

	if (nlines_in_header > 0) {
	    if (pageno > 0)
		call putci (fd, '\f')
	    lineno = 1
	    pageno = pageno + 1

	    for (i=1;  i <= nlines_in_header;  i=i+1) {
		call putline (fd, Memc[header_lines[i]])
		lineno = lineno + 1
	    }
	    for (i=1;  i <= SZ_HEADER_GAP;  i=i+1) {
		call putline (fd, "\n")
		lineno = lineno + 1
	   }
	}
end


# MAN_PUTSTR -- Save a string on the heap and return a pointer to the string.

pointer procedure man_putstr (str)

char	str[ARB]
int	nchars
pointer	bp
int	strlen()

begin
	nchars = strlen (str)
	call malloc (bp, nchars, TY_CHAR)
	call strcpy (str, Memc[bp], nchars)

	return (bp)
end
