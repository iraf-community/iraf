# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <psset.h>
include "psio.h"


# PS_SETFONT -- Set the font to be used.

procedure ps_setfont (ps, font)

pointer	ps					#I PSIO descriptor
int	font					#I font type

int	old_font
char	old_font_ch, ps_fontchar()
errchk	syserr

begin
	old_font = PS_CFONT(ps)
	old_font_ch = PS_CFONT_CH(ps)

	switch (font) {
	case F_ROMAN:
	    PS_CFONT(ps) = F_ROMAN
	    PS_CFONT_CH(ps) = 'R'
	    call fprintf (PS_FD(ps), "R ")
	case F_ITALIC:
	    PS_CFONT(ps) = F_ITALIC
	    PS_CFONT_CH(ps) = 'I'
	    call fprintf (PS_FD(ps), "I ")
	case F_BOLD:
	    PS_CFONT(ps) = F_BOLD
	    PS_CFONT_CH(ps) = 'B'
	    call fprintf (PS_FD(ps), "B ")
	case F_TELETYPE:
	    PS_CFONT(ps) = F_TELETYPE
	    PS_CFONT_CH(ps) = 'T'
	    call fprintf (PS_FD(ps), "T ")
	case F_PREVIOUS:
	    if (PS_SFONT(ps) != NULL) {
	        call fprintf (PS_FD(ps), "%c ")
		    call pargc (ps_fontchar (ps, PS_SFONT(ps)))
	    } else {
	        call fprintf (PS_FD(ps), "%c ")
		    call pargc (ps_fontchar (ps, PS_PFONT(ps)))
	    }
	default:
	    call syserr (SYS_PSFONT)
	}

	PS_PFONT(ps) = old_font
	PS_PFONT_CH(ps) = old_font_ch
end


# PS_SPFONT -- Set the special font to be used.

procedure ps_spfont (ps, font)

pointer	ps					#I PSIO descriptor
int	font					#I font type

errchk	syserr

begin
	if (font == NULL) {
	    PS_SFONT(ps) = NULL
	    PS_SFONT_CH(ps) = EOS
	    call fprintf (PS_FD(ps), "R ")
	    return
	}

	switch (font) {
	case F_ROMAN:
	    PS_SFONT(ps) = F_ROMAN
	    PS_SFONT_CH(ps) = 'R'
	    call fprintf (PS_FD(ps), "R ")
	case F_ITALIC:
	    PS_SFONT(ps) = F_ITALIC
	    PS_SFONT_CH(ps) = 'I'
	    call fprintf (PS_FD(ps), "I ")
	case F_BOLD:
	    PS_SFONT(ps) = F_BOLD
	    PS_SFONT_CH(ps) = 'B'
	    call fprintf (PS_FD(ps), "B ")
	case F_TELETYPE:
	    PS_SFONT(ps) = F_TELETYPE
	    PS_SFONT_CH(ps) = 'T'
	    call fprintf (PS_FD(ps), "T ")
	default:
	    call syserr (SYS_PSSPFONT)
	}
end


# PS_GETFONT -- Given the font character in a "\fN" string return the font
# type code. 

int procedure ps_getfont (ps, font_char)

pointer	ps					#I PSIO descriptor
char	font_char				#I font type character

begin
	switch (font_char) {
	case 'R':
	    return (F_ROMAN)
	case 'B':
	    return (F_BOLD)
	case 'I':
	    return (F_ITALIC)
	case 'T':
	    return (F_TELETYPE)
	case 'P':
	    return (F_PREVIOUS)
	default:
	    return (PS_CFONT(ps))
	}
end


# PS_FONTCHAR -- Given the font code return the character for it.

char procedure ps_fontchar (ps, font)

pointer	ps					#I PSTIO descriptor
int	font					#I font type character

begin
	switch (font) {
	case F_ROMAN:
	    return ('R')
	case F_BOLD:
	    return ('B')
	case F_ITALIC:
	    return ('I')
	case F_TELETYPE:
	    return ('T')
	case F_PREVIOUS:
	    return ('P')
	default:
	    return (PS_CFONT_CH(ps))
	}
end
