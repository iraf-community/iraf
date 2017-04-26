# WINDOW.COM -- Tracks windows associated with table editor

int	wprompt		# The prompt window
int	nscreens	# Number of screens currently displayed
int	scrlist[MAXSCR] # List of screens, from top to bottom

common	/tedwin/ wprompt, nscreens, scrlist

