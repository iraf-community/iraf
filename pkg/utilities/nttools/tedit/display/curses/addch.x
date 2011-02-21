include "../curses.h"

# ADDCH -- Add a character to the standard screen
#
# B.Simon	01-Oct-90	Original

procedure addch (ch)

char	ch		# i: Character to add
#--
char	str[1]

begin
	str[1] = ch
	str[2] = EOS
	call waddstr (STDSCR, str)
end

procedure waddch (win, ch)

int	win		# i: Window descriptor
char	ch		# i: Character to add
#--
char	str[1]

begin
	str[1] = ch
	str[2] = EOS
	call waddstr (win, str)
end
