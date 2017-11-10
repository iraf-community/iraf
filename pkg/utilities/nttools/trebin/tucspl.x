# Setup spline interpolation
#
# Copyright (c) 2017 Ole Streicher

# Solve a tridiagonal matrix (Thomas algorithm).
#
# Reference: Conte, S.D., and deBoor, C., Elementary Numerical Analysis,
#            McGraw-Hill, New York, 1972.
#
procedure solve_tridiagonal(n, a, b, c, d)
int	  n
double	  a[n]	# subdiagonal
double	  b[n]	# diagonal
double	  c[n]	# superdiagonal
double	  d[n]	# in: right side with constants. out: solution

int i
pointer cs, ds, sp
begin
    call smark(sp)
    call salloc(cs, n, TY_DOUBLE)
    call salloc(ds, n, TY_DOUBLE)

    # Forward step
    Memd[cs] = c[1] / b[1]
    do i = 2, n-1 {
       Memd[cs+i-1] = c[i] / (b[i] - a[i] * Memd[cs+i-2])
    }
    Memd[ds] = d[1] / b[1]
    do i = 2, n {
       Memd[ds+i-1] = (d[i] - a[i] * Memd[ds+i-2]) / (b[i] - a[i] * Memd[cs+i-2])
    }

    # Back substitution
    d[n] = Memd[ds+n-1]
    do i = n-1, 1, -1 {
        d[i] = Memd[ds+i-1] - Memd[cs+i-1] * d[i+1]
    }

    call sfree(sp)
end


# Compute the derivative K at each point in the array.  This is the
# initialization needed in preparation for interpolating using cubic
# splines by the subroutine TUISPL. Input and output are all double
# precision.
#
# Reference: De Boor, Carl, et al. A practical guide to splines.
#            Vol. 27. New York: Springer-Verlag, 1978.
#
# The interface is adopted from IRAFs original tucspl() routine.
# The "work" scratch array is gone, however.
#
procedure tucspl(xa, ya, n, k)

int	  n	# number of elements in each array
double	  xa[n]	# array of independent-variable values
double	  ya[n]	# array of dependent-variable values
double	  k[n]	# derivative of YA at each point

int	  i
double	  xl, xu
pointer	  sp, a, b, c

begin
    call smark(sp)
    call salloc(a, n, TY_DOUBLE)
    call salloc(b, n, TY_DOUBLE)
    call salloc(c, n, TY_DOUBLE)

    xu = xa[2] - xa[1]
    Memd[a] = 0.
    Memd[b] = 2./ xu
    Memd[c] = 1. / xu
    k[1] = 3 * (ya[2] - ya[1]) / xu**2

    do i=2, n-1 {
        xl = xa[i] - xa[i-1]
	xu = xa[i+1] - xa[i]
	Memd[a+i-1] = 1. / xl
	Memd[b+i-1] = 2. / xl + 2. / xu
	Memd[c+i-1] = 1. / xu
	k[i] = 3 * ( (ya[i] - ya[i-1])/xl**2 + (ya[i+1] - ya[i])/xu**2 )
    }

    xl = xa[n] - xa[n-1]
    Memd[a+n-1] = 1. / xl
    Memd[b+n-1] = 2. / xl
    Memd[c+n-1] = 0.
    k[n] = 3 * (ya[n] - ya[n-1]) / xl**2

    # Solve triangular matrix
    # This replaces the right side with the second derivatives.
    call solve_tridiagonal(n, Memd[a], Memd[b], Memd[c], k)

    call sfree(sp)
end

