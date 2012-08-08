# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include <psset.h>
include "psio.h"


# PS_OUTPUT -- Output the given line and break, fill if requested.

procedure ps_output (ps, str, fill_flag)

pointer	ps					#I PSIO descriptor
char	str[ARB]				#I text string to write
int	fill_flag				#I fill line flag

char	ch, word[SZ_FNAME]
int	fd, last_nscan, ip, op, curpos
int	i, spacing, nspaces, nwords, ngaps, twidth, len
int	ps_textwidth(), strmatch(), strlen()
errchk	ps_wrtblock, ps_textwidth

define	break_	99

begin
	# Idiot check.
	if (str[1] == EOS)
	    return

	if (PS_INITIALIZED(ps) == NO)
	    call ps_write_prolog (ps)

	# Initialize.
	fd = PS_FD(ps)
	curpos = PS_CLMARGIN(ps)
	last_nscan = 0

        # Trim trailing whitespace or newlines.
        len = strlen (str)
        for ( ; IS_WHITE(str[len]) || str[len] == '\n'; len=len-1)
            str[len] = EOS

	if (fill_flag == NO && strmatch(str, "\\\\f?") != 0) {
	    # No font changes or filling, just dump it as one string.
	    call ps_wrtblock (ps, curpos, str)
	    return
	}

	# Get the number of words in the line.
	nspaces = 0
	for (ip=1; str[ip] != EOS; ip=ip+1) {
	    if (IS_WHITE(str[ip]) && !IS_WHITE(str[ip+1]))
	        nspaces = nspaces + 1
	}
	nwords = nspaces + 1
	ngaps = max (1, nspaces)
	twidth = ps_textwidth (ps, str)

	# Calculate the inter-word spacing.
	if (PS_CFONT(ps) == F_TELETYPE)
	    spacing = FIXED_WIDTH
	else
	    spacing = SPACE_WIDTH

	if (fill_flag == YES)
	    spacing = spacing + (PS_LINE_WIDTH(ps) - twidth) / ngaps

	# Set the base font for the line
	if (PS_SFONT(ps) != NULL)
	    ch = PS_SFONT_CH(ps)
	else
	    ch = PS_CFONT_CH(ps)
	call fprintf (fd, "%c\n")
	    call pargc (ch)

	# Process the words on the line.
	ip = 1
	do i = 1, nwords {

	    if (str[ip] == EOS)
		break

	    # Collect chars up to the end of the word.
	    for (op=1; str[ip] != EOS && str[ip] != ' '; op=op+1) {
		word[op] = str[ip]
		ip = ip + 1
	    }
	    word[op] = EOS
	    twidth = ps_textwidth (ps, word)

	    # if we're filling, force the right-justification of the last
	    # word to cover for any roundoff in the spacing computation.
	    if (fill_flag == YES && i == nwords)
	    	curpos = PS_CRMPOS(ps) - twidth

	    # Write it out, handling font changes.
	    if (op > 1)
	        call ps_wrtblock (ps, curpos, word)

	    # Increment the position for the next word.
	    curpos = curpos + twidth

	    # Increment for the spaces between words.
	    for ( ; IS_WHITE(str[ip]) && str[ip] != EOS; ip=ip+1)
	        curpos = curpos + spacing
	}
end


# PS_WRTBLOCK -- Write a block of text at the given position.  We escape the
# parenthesis here since they were needed in computing the width.

procedure ps_wrtblock (ps, curpos, str)

pointer	ps					#I PSIO descriptor
int	curpos					#I X position of text
char	str[ARB]				#I string to write

char	word[SZ_WORD], line[SZ_LINE]
int	i, fd, ip, pos, st, en, gstrmatch()
int	ps_textwidth(), ps_getfont()
errchk	ps_setfont, ps_textwidth

begin
	fd = PS_FD(ps)

	call aclrc (word, SZ_WORD)
	call aclrc (line, SZ_LINE)

	if (gstrmatch (str, "\\\\f?", st, en) == 0) {
	    # No font changes so just output the word.
	    call ps_escText (str, line, SZ_LINE)
	    call fprintf (fd, "%d (%s) S\n")
		call pargi (curpos)
		call pargstr (line)

	} else {
	    # We have a font change.  Collect all chars up to the font change
	    # and output as an atom.  Change the font, and repeat until we
	    # use up the string.

	    pos = curpos
	    i = 1
	    for (ip=1; str[ip] != EOS; ip=ip+1) {
		if (str[ip] == '\\' && str[ip+1] == 'f') {
		    if (word[1] != EOS) {
			word[i] = EOS
	    		call ps_esctext (word, line, SZ_LINE)
	    		call fprintf (fd, "%d (%s) S\n")
			    call pargi (pos)
			    call pargstr (line)
			pos = pos + ps_textwidth (ps, word)
		    }
		    iferr (call ps_setfont (ps, ps_getfont(ps, str[ip+2])))
		        break;
		    ip = ip + 2
		    i = 1
		    word[1] = EOS
	 	} else {
		    word[i] = str[ip]
		    i = i + 1
		}
	    }
 	    word[i] = EOS

	    if (word[1] != EOS) {
    		call ps_esctext (word, line, SZ_LINE)
    		call fprintf (fd, "%d (%s) S\n")
		    call pargi (pos)
		    call pargstr (line)
		pos = pos + ps_textwidth (ps, word)
	    }
	}
end


# PS_ESCTEXT -- Escape the parenthesis in a text string.

procedure ps_esctext (in, out, maxch)

char	in[ARB]					#I input text
char	out[ARB]				#O output text
int	maxch					#I max characters

int	ip, op

begin
	ip = 1
	op = 1
	while (in[ip] != EOS) {
	    if (in[ip] == '(' || in[ip] == ')' || in[ip] == '\\') {
		out[op] = '\\'
		op = op + 1
	    }
	    out[op] = in[ip]
	    op = op + 1
	    ip = ip + 1
	}
	out[op] = EOS
end
