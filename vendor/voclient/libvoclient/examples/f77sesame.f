C
C  F77SESAME -- Fortran example task of VOClient Sesame interface
C
C  M.Fitzpatrick, NOAO, Jul 2006

	program f77sesame

	double precision ra, dec
	character 	target*10, pos*24
	integer		sr, len

 	target = "ngc1234"

	call vfnameresolver (target, sr)
	call vfresolverra (sr, ra)
	call vfresolverdec (sr, dec)
	call vfresolverpos (sr, pos, len)

	print *, "Target: ", target, "  ra=", ra, "  dec=", dec
	print *, "            (", pos,")"

	stop
	end
