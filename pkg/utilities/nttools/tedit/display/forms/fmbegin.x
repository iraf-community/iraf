# FM_BEGIN -- Initialize form handler
#
# B.Simon	25-Jan-88	Original
# B.Simon	11-Oct-90	Rewritten to use curses

procedure fm_begin ()

#--
include	"forms.com"

begin
	# Initialize curses

	call initscr	

	# Initialize global variables

	helpwin = 0

end
