	subroutine m70rel (fcb)
c
c     routine to release(DEALLOCATE) the model 70
c
	integer fcb(*)
c
	include 'fcbu.inc'
	integer*2 dev2(2)
	byte dev(4)
	character*4 m70
	equivalence (dev2,dev), (m70,dev)
c
	call m70cls (fcb)
	dev2(1) = fcb(fcb_u_m70_name)
	dev2(2) = fcb(fcb_u_m70_name+1)
	call sys$dalloc ('_'//m70//':',)
c
	return
	end
