	subroutine m70cls (fcb)
c
c  Routine to close model 70 display
c
	integer fcb(*)
	include 'fcbu.inc'
c
	integer*4 sys$dassgn, chan, junk
	integer*2 chan2(2)
	equivalence (chan, chan2)
c
c	call wtexec (fcb)
c
	chan2(1) = fcb(fcb_u_m70_chan)
	chan2(2) = fcb(fcb_u_m70_chan+1)
c
c	if (chan.ne.0) call lib$signal (%val(sys$dassgn (%val(chan))))
	if (chan.ne.0) then
	    junk = sys$dassgn (%val(chan))
	endif
c
	fcb(fcb_u_m70_chan) = 0
	fcb(fcb_u_m70_chan+1) = 0
c
	return
	end
