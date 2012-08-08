include "../curses.h"
include "window.h"

# GETCH -- Get a character and display it in the window

int procedure getch ()

# B.Simon	01-Oct-90	Original

#--
int	wgetch()

begin
	return (wgetch (STDSCR))
end

int procedure wgetch (win)

pointer	win		# i: Window descriptor
#--
include "window.com"

char	str[1]
int	ch
pointer pwin

int	k_get()

begin
	str[1] = EOS
	str[2] = EOS
	pwin = warray[win]

	if (WIN_FUNC(pwin) == NULL) {
	    call ps_synch
	    ch = k_get ()

	    if (ch < K_BASE) {
		str[1] = ch
		call waddstr (win, str)
	    }

	} else {
	    call zcall3 (WIN_FUNC(pwin), win, str, 1)
	    if (str[1] == EOS) {
		ch = k_get ()  # get pushed back character
	    } else {
	        ch = str[1]
	    }
	}

	return (ch)
end
