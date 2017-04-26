C
C  F77SKYBOT -- Fortran example task of VOClient SkyBoT interface
C
C  M.Fitzpatrick, NOAO, Aug 2006

	program f77skybot

	double precision ra, dec, sr, epoch, vmag
	character 	name*15
	integer		skybot, nobjs, len, i

 	ra = 0.0
 	dec = 0.0
 	sr = 600.0
 	epoch = 2454545.0

	print *, "#"
	print *, "# ra=", ra, "  dec=", dec, " sr=", sr, " epoch=", epoch
	print *, "#"
	call vfskybot (ra, dec, sr, sr, epoch, skybot)
	call vfskybotnobjs (skybot, nobjs)

	print *, "#"
	print *, "# Found ", nobjs, " objects"
	print *, "#"

	do 10 i = 1, nobjs
	  call vfskybotstr (skybot, "name", i, name, len)
	  call vfskybotdbl (skybot, "ra", i, ra)
	  call vfskybotdbl (skybot, "dec", i, dec)
	  call vfskybotdbl (skybot, "vmag", i, vmag)

	  print *, "Obj: ", name, "  ra=", ra, "  dec=", dec, " Mv=", vmag

10	continue

	stop
	end
