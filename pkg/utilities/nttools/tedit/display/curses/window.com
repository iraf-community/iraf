# WINDOW.COM -- Global variables used by the curses subroutines

int	saved		# Save rectangle under window when creating
int	echoed		# Echo characters
pointer	warray[MAXWIN]	# Array holding window descriptors

common	/window/	saved, echoed, warray
