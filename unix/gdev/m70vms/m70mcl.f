	subroutine m70mcl (fcb, error)
c
c     master clear model 70
c
	integer fcb(*), error
c
	include 'fcbu.inc'
	external io$_rewind, ss$_normal
	integer*4 status, iosb(2), chan, sys$qiow
	integer*2 chan2(2), iostat, stat
	equivalence (chan, chan2), (iosb, iostat)
        equivalence (status, stat)
        external ss$_timeout, ss$_powerfail
c
	chan2(1) = fcb(fcb_u_m70_chan)
	chan2(2) = fcb(fcb_u_m70_chan+1)
c
	status = sys$qiow (, %val(chan), io$_rewind, iosb,,,,,,,,)
	if (status) then
           if (iosb(1)) then
              error = 0
           else
              status = lib$match_cond (iosb, ss$_timeout, ss$_powerfail)
              if (status .eq. 0) then
                 error = 1000 + iostat
              else
                 error = status
              endif
           endif
        else
           error = 1000 + stat
        endif
c
	return
	end
