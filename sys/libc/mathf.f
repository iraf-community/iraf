c
c	MATH -- C callable math functions.  This is the only portable way
c	to access the Fortran intrinsic functions from C.  As a local
c	optimization it is possible to add defines to map these external
c	names onto the local Fortran library functions, but since C is not
c	generally used intensively for computations and all floating point
c	is done in double precision anyway, it is probably not worth it.
c

	integer function xnint (x)
	double precision x
		xnint = nint (x)
	end

	double precision function xexp (x)
	double precision x
		xexp = exp(x)
	end

	double precision function xlog (x)
	double precision x
		xlog = log(x)
	end

	double precision function xlog10 (x)
	double precision x
		xlog10 = log10(x)
	end

	double precision function xpow (x, y)
	double precision x
	double precision y
		xpow = x ** y
	end

	double precision function xsqrt (x)
	double precision x
		xsqrt = sqrt(x)
	end

	double precision function xsin (x)
	double precision x
		xsin = sin(x)
	end

	double precision function xcos (x)
	double precision x
		xcos = cos(x)
	end

	double precision function xtan (x)
	double precision x
		xtan = tan(x)
	end

	double precision function xasin (x)
	double precision x
		xasin = asin(x)
	end

	double precision function xacos (x)
	double precision x
		xacos = acos(x)
	end

	double precision function xatan (x)
	double precision x
		xatan = atan(x)
	end

	double precision function xatan2 (x, y)
	double precision x
	double precision y
		xatan2 = atan2(x,y)
	end
