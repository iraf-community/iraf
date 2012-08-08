# K_PUSHBK -- Push back a single character read from the keyboard

procedure k_pushbk (ch)

int	ch		# i: character to be pushed back
#--
include "screen.com"

begin
	keych = ch
end
