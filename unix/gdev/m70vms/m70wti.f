	subroutine m70wti (fcb, func, time, button, x, y)
c
c     This routine waits for the appropriate interupt from the  
c     Unibus M70 interface card, then returns button and cursor
c     information.
c
c
c     parameter descriptions:
c
c        fcb   is a system info. array.
c
c        func  is interpreted:
c                 0 ==> wait for button push.
c                 1 ==> wait for cursor move.
c                 2 ==> wait for button push or cursor move.
c
	integer fcb(*), func, time, button, x, y
c
	include 'fcbu.inc'
	external io$_rewindoff
	integer*4 sys$qiow, mask, chan, iosb(2)
	integer*2 chan2(2)
	equivalence (chan, chan2)
c
	chan2(1) = fcb(fcb_u_m70_chan)	! get M70 channel
	chan2(2) = fcb(fcb_u_m70_chan+1)
c
	if (func.eq.0) then
	  mask = '0400'x		! wait for button
	elseif (func.eq.1) then
	  mask = '0800'x		! wait for trackball
	elseif (func.eq.2) then
	  mask = '0C00'x		! wait for button or trackball
	else
	  mask = '0C00'x
	endif
c
	status = sys$qiow (, %val(chan), io$_rewindoff,
	1		iosb,,,%val(mask),,,,,)
c
c  Get button word and X-Y position of cursor
c
	call rbutn (fcb, button, x, y)
c
	return
	end
