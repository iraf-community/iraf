# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


define	MAXPTS		1024

task	band

# This procedure solves for the natural cubic spline which interpolates
# the curve y[i] = 100., for i = 1 to n.  The matrix is of length n+2,
# and has a bandwidth of 3.  Lawsons and Hansons routines BNDACC and 
# BNDSOL (modified to return an error code IER) are used to solve the
# system.  The computations are carried out in single precision.


procedure band()

real	g[MAXPTS,4], c[MAXPTS], rnorm
int	ip, ir, i, n, ier, geti()
real	marktime, cptime()

begin
	n = min (MAXPTS, geti ("npts"))
	marktime = cptime()

	ip = 1
	ir = 1

	g[ir,1] = 6.			# first row
	g[ir,2] = -12.
	g[ir,3] = 6.
	g[ir,4] = 0.
	call bndacc (g, MAXPTS, 3, ip, ir, 1, 1)

	do i = 2, n-1 {			# tridiagonal elements
	    g[ir,1] = 1.
	    g[ir,2] = 4.
	    g[ir,3] = 1.
	    g[ir,4] = 100.
	    call bndacc (g, MAXPTS, 3, ip, ir, 1, i-1)
	}

	g[ir,1] = 6.			# last row
	g[ir,2] = -12.
	g[ir,3] = 6.
	g[ir,4] = 0.
	call bndacc (g, MAXPTS, 3, ip, ir, 1, n-2)

	call printf ("matrix accumulation took %8.2f cpu seconds\n")
	    call pargr (cptime() - marktime)
	marktime = cptime()

					# solve for the b-spline coeff C
	call bndsol (1, g, MAXPTS, 3, ip, ir, c, n, rnorm, ier)

	call printf ("solution took %8.2f cpu seconds (rnorm=%g, ier=%d)\n")
	    call pargr (cptime() - marktime)
	    call pargr (rnorm)
	    call pargi (ier)

	call printf ("selected coefficients:\n")
	for (i=1;  i <= n;) {		# print the first and last 4 coeff
	    call printf ("%8d%15.6f\n")
		call pargi (i)
		call pargr (c[i])
	    if (i == 4)
		i = max(i+1, n-3)
	    else
		i = i + 1
	}
end
