
c-------------------------------------------------------------------------
c Compute machine epsilon, i.e, the smallest real or double precision
c number EPS such that (1.0 + EPS > 1.0).  This calculation is tricky
c because of the optimizations performed by some compilers, and because
c a comparison performed in registers may be done to a higher precision
c than one involving variables.  This program contains some minor
c violations of the F78 standard.
c-------------------------------------------------------------------------


	program epsilo

	real seps
	double precision deps

		write (*,*) 'Calculate Machine Epsilon ------'
		call cseps (seps)
		call cdeps (deps)
		write (*,*) '   single precision epsilon: ', seps
		write (*,*) '   double precision epsilon: ', deps

		write (*,*) ' '
		write (*,*) 'Verify Values -----'

		write (*, '(''     enter s.p. epsilon: '',$)')
		read (*,*) seps
		if (1.0 + seps .gt. 1.0) then
		    write (*,*) '     ok'
		else
		    write (*,*) '     not ok'
		endif

		write (*, '(''     enter d.p. epsilon: '',$)')
		read (*,*) deps
		if (1.0 + deps .gt. 1.0) then
		    write (*,*) '     ok'
		else
		    write (*,*) '     not ok'
		endif

		stop
	end


c -- Compute the single precision epsilon.

	subroutine cseps (seps)

	real	seps
	real	sval
	double	precision dval
	logical	sgt
	common	/eps/ sval, dval
	save	/eps/

		sval = 1.0
 10		seps = sval
		sval = sval / 2.0
		if (sgt (1.0)) then
		    goto 10
		endif
	end



c -- Is SVAL + 1.0 greater than 1.0?

	logical function sgt (value)

	real	value, sval, stemp
	double precision dval
	common	/eps/ sval, dval
	save	/eps/

		stemp = sval + 1.0
		sgt = (stemp .gt. value)
	end



c -- Compute the double precision epsilon.

	subroutine cdeps (deps)

	double	precision deps
	double	precision dval
	real	sval
	logical	dgt
	common	/eps/ sval, dval
	save	/eps/

		dval = 1.0d0
 10		deps = dval
		dval = dval / 2.0d0
		if (dgt (1.0d0)) then
		    goto 10
		endif
	end


c -- Is DVAL + 1.0 greater than 1.0?

	logical function dgt (value)

	double	precision value
	double	precision dval, dtemp
	real	sval
	common	/eps/ sval, dval
	save	/eps/

		dtemp = dval + 1.0d0
		dgt = (dtemp .gt. value)
	end
