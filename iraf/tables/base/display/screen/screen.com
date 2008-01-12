# SCREEN.COM -- Global variables used by the screen routines

pointer	keytab		# Table of function key sequences
pointer	termscr		# Copy of terminal screen contents
pointer	htext		# Function key help text
pointer	term		# The termcap data structure
int	ttyin		# Input file descriptor for the terminal
int	ttyout		# Output file descriptor for the terminal
int	lines		# The number of lines on the screen
int	cols		# The number of columns on the screen
int	currow		# Cursor row
int	curcol		# Cursor column
int	keych		# Pushed back keyboard character
char	ks[7]		# Initialize keypad sequence
char	ke[7]		# Terminate keypad sequence

common /screen/  keytab, termscr, htext, ttyin, ttyout, term, 
		 lines, cols, currow, curcol, keych, ks, ke 
