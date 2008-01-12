# RING_BELL -- Ring the bell to wake up the user

procedure ring_bell ()

bool	silent		# i: do not ring bell
#--
bool	ring		# variable used to hold the value of silent

begin
	if (ring)
	    call ps_beep
	return

	entry init_bell (silent)

	ring = ! silent
	return

end
