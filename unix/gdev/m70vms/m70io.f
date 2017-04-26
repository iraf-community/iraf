	subroutine m70io (fcb, data, count, read, opcd, iosb, error)
c
c     Routine does io between M70 and a VAX
c
c     Parameters:
c
c        fcb      function communications block.
c
c        data     input/output buffer.
c
c        count    number of words to read/write.
c
c        read     0 -> write, 1 -> read.
c
c        opcd     0 -> qio with efn = 1 and wait for completion
c                 1 -> qio with efn = 2
c                 2 -> qio with efn = 3
c                 
c	iosb	I/O status quadword
c
c	error    -1 => display not available
c		  0 => success
c	          1 => time/out
c	          2 => invalid or non-responding device
c	          1000 > machine dependent error code
c
	integer fcb(*), data(1), count, read, opcd, error
	integer*4 iosb(2)
c
	include 'fcbu.inc'
	external io$_writevblk, io$_readvblk, ss$_timeout, ss$_powerfail
	integer*4 chan, sys$qio, sys$waitfr, func, status, l_iosb(2)
	integer*2 chan2(2), stat(2)
	equivalence (chan, chan2), (status, stat(1))
c
	chan2(1) = fcb(fcb_u_m70_chan)
	chan2(2) = fcb(fcb_u_m70_chan+1)
c
	if (read.eq.1) then
	  func = %loc(io$_readvblk)
	else
	  func = %loc(io$_writevblk)
	endif
c
	if (opcd.eq.0) then
	  status = sys$qio (%val(opcd+1), %val(chan), %val(func),
	1		l_iosb,,, data, %val(2*count),,,,)
	  if (status) then
	     status = sys$waitfr (%val(1))
	     if (l_iosb(1)) then
	        error = 0
	     else
	        stat(1) = lib$match_cond 
	1               (iosb, ss$_timeout, ss$_powerfail)
	        if (stat(1) .eq. 0) then
	           error = 1000 + iosb(1)
	        else
	           error = stat(1)
	        endif
	     endif
	  else
	     error = 1000 + stat(1)
	  endif
	else
	  status = sys$qio (%val(opcd+1), %val(chan), %val(func),
	1		iosb,,, data, %val(2*count),,,,)
	  if (status) then
	     error = 0
	  else
	     error = 1000 + stat(1)
	  endif
	endif
c
	return
	end
