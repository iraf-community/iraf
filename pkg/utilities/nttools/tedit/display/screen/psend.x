# PS_END -- Terminate physical display package
#
# B.Simon	18-Jan-89	Original
# B.Simon	26-Sep-90	Updated to use screen buffer

procedure ps_end()

#--
include	"screen.com"

char	newline
data	newline	/ '\n' /

begin
	# Restore terminal to original state and release termcap structure

	if (term != NULL) {
	    call ps_sendcap ("ve", 1)
	    call ps_sendcap ("te", 1)

	    if (ttyout != NULL) {
		call ttygoto (ttyout, term, 1, lines)
		call putc (ttyout, newline)
	    }
	    call ttyclose (term)
	    term = NULL
	}

	# Release screen memory

	if (termscr != NULL) {
	    call mfree (termscr, TY_CHAR)
	    termscr = NULL
	}

	# Close terminal descriptor

	if (ttyin != NULL) {
	    call close (ttyin)
	    ttyin = NULL
	}

	if (ttyout != NULL) {
	    call close (ttyout)
	    ttyout = NULL
	}

end
