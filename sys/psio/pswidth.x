# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <ctype.h>
include <psset.h>
include "psio.h"


# PS_TEXTWIDTH -- Return the length in PS pixels of the given string.  We 
# handle font changes here and keep track of the current and previous font.

int procedure ps_textwidth (ps, str)

pointer	ps					#I package pointer
char	str[ARB]				#I string to check

int	ip, width, f_current
int	ps_getfont(), ps_chwidth()
errchk	ps_chwidth

begin
	# Initialize.
	width = 0
	f_current = PS_CFONT(ps)

	# Now process the word, computing the width of each character and
	# returning the total width.  Handle inline font changes.

	for (ip=1; str[ip] != EOS; ip=ip+1) {

	    # Handle any font changes.
	    if (str[ip] == '\\' && str[ip+1] == 'f') {
	        f_current = ps_getfont (ps, str[ip+2])
	        ip = ip + 2
	    } else if ((str[ip] == '\\' && str[ip+1] == '(') ||
	    	       (str[ip] == '\\' && str[ip+1] == ')')) {
		# Skip over escaped parens in width computation.
	        ip = ip + 1
	        width = width + ps_chwidth (str[ip], f_current)
	    } else
	        width = width + ps_chwidth (str[ip], f_current)
	}

	return (width)
end


# PS_CHWIDTH -- Given the font type and a character return the width.

int procedure ps_chwidth (ch, font)

char	ch					#I character
int	font					#I font type character

errchk	syserr
include	"font.com"

begin
	if (ch < START_CH || ch > END_CH)
	    return (0)

	switch (font) {
	case F_ROMAN: 	
	    return (roman[(ch-START_CH+1)])
	case F_BOLD: 	
	    return (bold[(ch-START_CH+1)])
	case F_ITALIC: 	
	    return (italic[(ch-START_CH+1)])
	case F_TELETYPE: 	
	    return (FIXED_WIDTH)
	case F_PREVIOUS: 	
	    return (FIXED_WIDTH)
	default:
	    call syserr (SYS_PSFONT)
	}
end
