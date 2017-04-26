# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <psset.h>
include "psio.h"


# PS_PAGE_SIZE -- Set the page size (letter|legal|a4|b5).

procedure ps_page_size (ps, page)

pointer	ps					#I PSIO descriptor
int	page					#I page type

begin
	if (PS_INITIALIZED(ps) == YES)
	    return

	if (page >= PAGE_LETTER && page <= PAGE_B5) {
	    switch (page) {
	    case PAGE_LETTER:
	        PS_PAGE(ps)    = PAGE_LETTER
	        PS_PWIDTH(ps)  = LETTER_WIDTH
	        PS_PHEIGHT(ps) = LETTER_HEIGHT
	    case PAGE_LEGAL:
	        PS_PAGE(ps)    = PAGE_LEGAL
	        PS_PWIDTH(ps)  = LEGAL_WIDTH
	        PS_PHEIGHT(ps) = LEGAL_HEIGHT
	    case PAGE_A4:
	        PS_PAGE(ps)    = PAGE_A4
	        PS_PWIDTH(ps)  = A4_WIDTH
	        PS_PHEIGHT(ps) = A4_HEIGHT
	    case PAGE_B5:
	        PS_PAGE(ps)    = PAGE_B5
	        PS_PWIDTH(ps)  = B5_WIDTH
	        PS_PHEIGHT(ps) = B5_HEIGHT
	    default:
	        call eprintf (
		    "Warning (PSIO): attempt to set illegal page size.")
	    }
	}
end


# PS_FONT_SIZE -- Set the default font size to use (default = 10 points).

procedure ps_font_size (ps, font_size)

pointer	ps					#I PSIO descriptor
int	font_size				#I default font size

begin
	if (PS_INITIALIZED(ps) == YES)
	    return

	PS_FONTSZ(ps) = font_size
end


# PS_HEADER -- Set the header tag strings.

procedure ps_header (ps, ledge, center, redge)

pointer	ps					#I PSIO descriptor
char	ledge[ARB]				#I left edge text
char	center[ARB]				#I center text
char	redge[ARB]				#I right edge text

begin
	if (PS_INITIALIZED(ps) == YES)
	    return

	if (ledge[1] != EOS)
	    call strcpy (ledge, HLEDGE(ps), SZ_WORD)
	if (center[1] != EOS)
	    call strcpy (center, HCENTER(ps), SZ_WORD)
	if (redge[1] != EOS)
	    call strcpy (redge, HREDGE(ps), SZ_WORD)
end


# PS_FOOTER -- Set the footer tag strings.

procedure ps_footer (ps, ledge, center, redge)

pointer	ps					#I PSIO descriptor
char	ledge[ARB]				#I left edge text
char	center[ARB]				#I center text
char	redge[ARB]				#I right edge text

begin
	if (PS_INITIALIZED(ps) == YES)
	    return

	if (ledge[1] != EOS)
	    call strcpy (ledge, FLEDGE(ps), SZ_WORD)
	if (center[1] != EOS)
	    call strcpy (center, FCENTER(ps), SZ_WORD)
	if (redge[1] != EOS) {
	    call strcpy (redge, FREDGE(ps), SZ_WORD)
	    PS_NUMBER(ps) = NO
	}
end


# PS_SETMARGINS -- Set the permanent page margins (in inches).

procedure ps_setmargins (ps, left, right, top, bottom)

pointer	ps					#I PSIO descriptor
real	left, right, top, bottom		#I margins

int	scale

begin
	if (PS_INITIALIZED(ps) == YES)
	    return

	PS_LMARGIN(ps)	= left
	PS_RMARGIN(ps)	= right
	PS_TMARGIN(ps)	= top
	PS_BMARGIN(ps)	= bottom

        # Set the margin values.
        scale = PPI * RESOLUTION
        PS_PLMARGIN(ps) = left * scale       	# perm. L margin     (points)
        PS_PRMARGIN(ps) = right * scale       	# perm. R margin     (points)
        PS_PTMARGIN(ps) = top * scale       	# perm. T margin     (points)
        PS_PBMARGIN(ps) = bottom * scale       	# perm. B margin     (points)

        PS_CLMARGIN(ps) = PS_PLMARGIN(ps)       # current L margin   (points)
        PS_CRMARGIN(ps) = PS_PRMARGIN(ps)       # current R margin   (points)
end
