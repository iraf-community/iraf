	subroutine m70get (fcb, error)
c
c  Routine to get (allocate) the model 70
c
c     arguments:
c
c        fcb      function communications block
c
c        error    -2 => device already allocated
c                 -1 => m70 not acquired
c                  0 => success
c                  1 => timeout
c                  2 => invalid device or powerfail
c                  >=1000 machine dependent error number
c
	integer fcb(*), error
c
	include 'fcbu.inc'
	external ss$_normal, ss$_devalloc
	integer*4 len,status, sys$alloc
	integer*2 nam2(2), stat
	byte nam(4)
	character name*4, result*8
	equivalence (nam2, nam), (name, nam), (status, stat)
c
	nam2(1) = fcb(fcb_u_m70_name)
	nam2(2) = fcb(fcb_u_m70_name+1)
c
	status = sys$alloc (name, len, result,)
	if (status.ne.%loc(ss$_normal)) then
            if (status .eq. %loc(ss$_devalloc)) then
               error = -2
            else
	       error = 1000 + stat
            endif
	else
	  call m70opn (fcb, error)
          if (error .ne. 0) return
	  call m70mcl (fcb, error)
	endif
c
	return
	end
