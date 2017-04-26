include	<ttyset.h>
include "../curses.h"

# PS_BEGIN -- Initialize physical screen display
#
# B.Simon	18-Jan-89	Original
# B.Simon	26-Sep-90	Updated to use screen buffer

procedure ps_begin ()

#--
include	"screen.com"

char	ch
data	ch	/ EOS /

string	nomove  "Terminal does not support cursor movement (cm)"

bool	ttygetb()
int	ttopen(), ttystati()
pointer	ttyodes()

begin
	# Initialize global variables

	ks[1] = EOS
	ke[1] = EOS
	keych = EOF

	currow = GIANT
	curcol = GIANT

	keytab = NULL
	termscr = NULL
	htext = NULL

	term = ttyodes ("terminal")
	lines = ttystati (term, TTY_NLINES)
	cols = ttystati (term, TTY_NCOLS)

	if (! ttygetb (term, "cm")) {
	    call ttyclose (term)
	    call error (1, nomove)
	}

	ttyin = ttopen ("dev$tty", READ_ONLY)
	ttyout = ttopen ("dev$tty", APPEND)

	# Allocate memory for screen and fill with EOS

	call malloc (termscr, lines*cols, TY_CHAR)
	call amovkc (ch, Memc[termscr], lines*cols)

	# Initialize display

	call ps_sendcap ("ti", 1)
	call ps_sendcap ("vs", 1)
	
end
