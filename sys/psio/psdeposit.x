# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include "psio.h"


# PS_DEPOSIT -- Deposit a line of text to the output buffer.  When the output
# width exceeds the permanent right margin the line us flushed to the output
# file and the x-position reset to the current left margin, the y-pos is 
# moved to the next line dependent on the font size.   Remaining words in the
# line buffer to added to the next line buffer. 

procedure ps_deposit (ps, line)

pointer	ps					#I PSIO descriptor
char	line[ARB]				#I text line

char	word[SZ_FNAME]
pointer	wbuf, wp
int	i, ip, start_ip
int	len, width, curpos, rmargin

int	strlen(), ps_textwidth(), ps_chwidth()
errchk	ps_chwidth, ps_textwidth, ps_linebreak

begin
	# Process the line, collect all the words that will fit on the
	# line and add to the word buffer.  When the line fills output
	# it, otherwise fill the buffer.

	wbuf = PS_WBPTR(ps)
	wp = PS_WBPTR(ps) + strlen (Memc[wbuf])
	curpos = PS_CLMARGIN(ps) + ps_textwidth (ps, Memc[wbuf])
	rmargin = PS_CRMPOS(ps)
	len = strlen (line)

	# Trim trailing whitespace or newlines.
	for ( ; IS_WHITE(line[len]) || line[len] == '\n'; len=len-1)
	    line[len] = EOS

	# Take care of any leading whitespace.  Tabs are treated as
	# spaces, we assume the caller has 'detabbed' the line before
	# we are called.
	for (ip=1; IS_WHITE(line[ip]); ip=ip+1)
	    width = width + ps_chwidth (line[ip], PS_CFONT(ps))
	if (PS_JUSTIFY(ps) == NO)
	    curpos = curpos + width

	# Process the rest of the line.
	for (; ip <= len; ip=ip+1) {

	    # Get the next word on the line and it's length.
	    start_ip = ip
	    for (i=1; !IS_WHITE(line[ip]) && line[ip] != EOS; i=i+1) {
		word[i] = line[ip]
		ip = ip + 1
	    }
	    word[i] = EOS
	    len = ps_textwidth (ps, word)

	    if (curpos + len > rmargin) {
		# We would overflow the line so break it here.
		len = strlen (Memc[wbuf])
		Memc[wbuf+len-1] = EOS
		call ps_linebreak (ps, PS_JUSTIFY(ps))

		call aclrc (Memc[wbuf], SZ_FNAME)
		call strcpy (line[start_ip], Memc[wbuf], SZ_LINE)
		call strcat (" ", Memc[wbuf], SZ_LINE)
		return

	    } else {
		# Copy the word to the buffer and update the position.
		call strcat (word, Memc[wbuf], SZ_LINE)
		curpos = curpos + len
	 	wp = wp + strlen (word)
	    }

	    # Get the spaces between words.
	    for (; IS_WHITE(line[ip]) && line[ip] != EOS; ip=ip+1) {
		curpos = curpos + ps_chwidth (line[ip], PS_CFONT(ps))
		Memc[wp] = line[ip]
		wp = wp + 1
	    }

	    if (line[ip] == EOS)
		break
	    else
	        ip = ip - 1
	}

	call strcat (" ", Memc[wbuf], SZ_LINE)
	PS_XPOS(ps) = curpos
end
