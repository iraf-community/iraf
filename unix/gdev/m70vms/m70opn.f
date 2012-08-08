	subroutine m70opn (FCB, error)
C
c  Routine to open model 70
c
c	Error is returned as:
c	   -1 = display open 
c	    0 = OK
c	    1 = timeout
c	    2 = invalid or non-responding device 
c	    >= 1000 : machine dependent error number
c
	integer fcb(*), error
	include 'fcbu.inc'
C
	integer*4 sys$assign, chan, status
	integer*2 chan2(2), name2(2), stat(2)
	byte name1(4)
	character*4 m70
	equivalence (chan, chan2), (status, stat(1))
	equivalence (name1, name2), (name1, m70)
C
	name2(1) = fcb(fcb_u_m70_name)
	name2(2) = fcb(fcb_u_m70_name+1)
	chan2(1) = fcb(fcb_u_m70_chan)
	chan2(2) = fcb(fcb_u_m70_chan+1)
c
	if (chan.eq.0) then
	  status = sys$assign ('_'//m70//':', chan,,)
	  if (status) then
	    fcb(fcb_u_m70_chan) = chan2(1)
	    fcb(fcb_u_m70_chan+1) = chan2(2)
	    error = 0
	  else
	    error = 1000 + stat(1)
	  endif
	else
	  error = -1
	endif
c
	return
	end
