	subroutine m70wt (fcb, bfnum, bfcnt, iosb, error)
c
c     routine to wait for completion of buffer write on pdp-11's
c
c        fcb      function communication block.
c        bfnum    used to determine event flag to wait for
c        bfcnt    number of words in buffer.  should be -1 indicating
c                 i/o pending.  reset to zero when i/o completed.
c        iosb     i/o status block
c        error    0 success, -1 not acquired, 1 timeout,
c                 2 invalid device, 1000+n system dep. error
c
	integer fcb(*)
	integer bfnum, bfcnt, error
	integer*4 iosb(2), status
	integer*2 stat(2)
c
	integer*4 sys$waitfr
	equivalence (status, stat(1))
	external ss$_timeout, ss$_powerfail
c
c     is this wait required?
c
	error = 0
	if (bfcnt .ge. 0) return
c
	status = sys$waitfr (%val(bfnum+1))
	if (status) then
	   bfcnt = 0
	   if (.not. iosb(1)) then
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
c
	return
	end
