# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>
include	"lroff.h"

.help input
.nf ___________________________________________________________________________
INPUT -- Read a line of text into the user supplied input buffer.  Convert
any tabs therein to spaces to simplify further processing.  If generation
of standout mode control chars is enabled, map "\f[BIR]" sequences into the
appropriate control chars (defined in <chars.h>), otherwise delete any such
sequences found (these control chars are later mapped by HELP, PAGE, LPRINT
etc. into whatever sequence the actual output device requires).  Return the
number of PRINTABLE chars in the input line.  Control characters are not
counted, but are copied to the output line.  The trailing newline is stripped;
Lroff deals mainly with words, not lines.  Only printable characters are made
to "stand out", i.e., standout mode is always turned off between words and at
the end of a line.
.endhelp ______________________________________________________________________

int procedure input (in, userbuf)

extern	in()
char	userbuf[ARB]

bool	standout_mode_in_effect
char	ch
int	len_inputline, ocol
pointer	sp, lbuf, ip, op
int	stridx(), in()
errchk	salloc, in
include	"lroff.com"

begin
	call smark (sp)
	call salloc (lbuf, SZ_IBUF, TY_CHAR)

	# Get input line and deal with any tab characters therein.
	if (in (in_magic_arg, Memc[lbuf]) == EOF) {
	    call sfree (sp)
	    return (EOF)
	}

	standout_mode_in_effect = false
	len_inputline = 0
	ip = lbuf
	op = 1
	ocol = 0

	# Process the input buffer, converting any "\f?" font escape sequences
	# found.  Terminate when newline is reached.  Delete the newline.
	# Expand all tabs.

	for (ch=Memc[ip];  ch != '\n' && ch != EOS;  ch=Memc[ip]) {
	    if (ch == '\\')
		if (Memc[ip+1] == 'f' && stridx (Memc[ip+2], "BIR") > 0) {
		    # Turn standout mode on or off.  Can only be turned on
		    # if "soflag" is YES.
		    switch (Memc[ip+2]) {
		    case 'B', 'I':			# bold, italic
			if (soflag == YES)
			    standout_mode_enabled = true
		    case 'R':				# roman
			if (standout_mode_in_effect) {
			    userbuf[op] = SO_OFF
			    op = op + 1
			    standout_mode_in_effect = false
			}
			standout_mode_enabled = false
		    }
		    ip = ip + 3				# \f? = 3
		    next
		}

	    # Only make alphanumeric chars "stand out".

	    if (IS_ALNUM(ch)) {
		len_inputline = len_inputline + 1
		ocol = ocol + 1
		if (standout_mode_enabled && !standout_mode_in_effect) {
		    userbuf[op] = SO_ON
		    op = op + 1
		    standout_mode_in_effect = true
		}

	    } else if (ch == '\t') {
		repeat {
		    userbuf[op] = ' '
		    op = op + 1
		    ocol = ocol + 1
		    len_inputline = len_inputline + 1
		} until (ocol > 1 && mod (ocol, TABSIZE) == 0)
		ip = ip + 1
		next

	    } else {
		if (IS_PRINT(ch)) {
		    len_inputline = len_inputline + 1
		    ocol = ocol + 1
		}
		if (standout_mode_in_effect) {
		    userbuf[op] = SO_OFF
		    op = op + 1
		    standout_mode_in_effect = false
		}
	    }

	    userbuf[op] = ch
	    op = op + 1
	    ip = ip + 1
	}

	if (standout_mode_in_effect) {
	    userbuf[op] = SO_OFF
	    op = op + 1
	    standout_mode_in_effect = false
	}
	userbuf[op] = EOS

	call sfree (sp)
	return (len_inputline)
end
