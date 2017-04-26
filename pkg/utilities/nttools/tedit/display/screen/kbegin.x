include	<fset.h>
include	"../curses.h"

# K_BEGIN -- Initialize keyboard
#
# B.Simon	23-Jan-89	Original

procedure k_begin (cmdlist)

char	cmdlist[ARB]	# i: List of editing commands
#--
include	"screen.com"

int	klen
int	strlen()

extern	k_error

begin
	# NOTE: ps_begin must be called before calling this procedure

	# Put terminal in raw mode

	call fseti (ttyin, F_RAW, YES)

	# Set up error exit routine

	call onerror (k_error)

	# Set up function key table and help screen

	call k_compile (cmdlist)

	# Send initialize keypad sequence to terminal

	klen = strlen (ks)
	if (klen > 0)
	    call ttywrite (ttyout, term, ks, klen, 1)
	
end
