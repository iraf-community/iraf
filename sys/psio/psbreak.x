# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <psset.h>
include "psio.h"


# PS_LINEBREAK -- Break the current line regardless of whether it has been
# filled.  The fill_flag says whether to fill the current line to be right 
# justified.  May be called to simply output the current line buffer.

procedure ps_linebreak (ps, fill_flag)

pointer	ps					#I PSIO descriptor
int	fill_flag				#I fill line flag

begin
	iferr (call ps_output (ps, Memc[PS_WBPTR(ps)], fill_flag))
	    return

	# Do a variable spacing depending on whether we're within unformatted
	# text where the font is smaller, or outputting a regular line.

	if (PS_CFONT(ps) == F_TELETYPE)
            PS_YPOS(ps) = PS_YPOS(ps) - ((LINE_HEIGHT-2) * RESOLUTION)
	else 
            PS_YPOS(ps) = PS_YPOS(ps) - (LINE_HEIGHT * RESOLUTION)

	# Check for a page break.
        if (PS_YPOS(ps) <= PS_PBMARGIN(ps))
            call ps_pagebreak (ps)
	else {
	    call fprintf (PS_FD(ps), "%d V\n")
	        call pargi (PS_YPOS(ps))
	}

        # Reset the X position to current left margin.
        PS_XPOS(ps) = PS_CLMARGIN(ps)

	# Clear the word buffer.
	call aclrc (Memc[PS_WBPTR(ps)], SZ_LINE)
end


# PS_NEWLINE -- Output a newline (vertical space actually).

procedure ps_newline (ps)

pointer	ps					#I PSIO descriptor

begin
	# Check for a page break.
        PS_YPOS(ps) = PS_YPOS(ps) - ((LINE_HEIGHT-4) * RESOLUTION)
        if (PS_YPOS(ps) <= PS_PBMARGIN(ps))
            call ps_pagebreak (ps)
	else {
	    call fprintf (PS_FD(ps), "%d V\n")
	        call pargi (PS_YPOS(ps))
	}

        # Reset the X position to current left margin.
        PS_XPOS(ps) = PS_CLMARGIN(ps)
end


# PS_PAGEBREAK -- Break the current page regardless of whether it has been
# filled.

procedure ps_pagebreak (ps)

pointer	ps					#I PSIO descriptor

begin
	PS_PNUM(ps) = PS_PNUM(ps) + 1
	call fprintf (PS_FD(ps), "EP\n%%%%Page: %d %d\nBP\n")
	    call pargi (PS_PNUM(ps))
	    call pargi (PS_PNUM(ps))

	PS_YPOS(ps) = (PS_PHEIGHT(ps) * RESOLUTION) - PS_PTMARGIN(ps)
	call ps_ypos (ps, PS_YPOS(ps))
end
