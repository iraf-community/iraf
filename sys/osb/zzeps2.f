c-------------------------------------------------------------------------
c ZZEPS2.F -- Alternate version of ZZEPS.  This version may avoid problems
c seen on some systems of excess precision causing an artificially large
c value of the single precision epsilon to be computed, due to the epsilon
c value being computed in registers.  Use whichever version produces the
c smaller epsilon.
c
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
		if (sgt (sval + 1.0, 1.0)) then
		    goto 10
		endif
	end



c -- Is SVAL + 1.0 greater than 1.0?

	logical function sgt (value, ref)

	real	value, ref

		sgt = (value .gt. ref)
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
		if (dgt (dval + 1.0d0, 1.0d0)) then
		    goto 10
		endif
	end


c -- Is DVAL + 1.0 greater than 1.0?

	logical function dgt (value, ref)

	double	precision value, ref

		dgt = (value .gt. ref)
	end
