# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


define	M	256
define	N	256

task	lsq

# This procedure fits a natural cubic spline to an array of n data points.
# The system being solved is a tridiagonal matrix of n+2 rows.  The system
# is solved by Lawsons and Hansons routine HFTI, which solves a general
# m by n linear system of equations.  This is enormous overkill for this
# problem (see "band.x"), but serves to give timing estimates for the code.


procedure lsq()

real	a[M,N], b[M], tau, rnorm, h[N], g[N]
int	krank, ip[N]
int	i, j, m, n, geti()
real	marktime, cptime()

begin
	m = min (M, geti ("npts"))	# size of matrix
	n = min (N, m)
	tau = 1e-6

	do j = 1, n			# set up b-spline matrix
	    do i = 1, m
		a[i,j] = 0.

	a[1,1] = 6.			# first row
	a[1,2] = -12.
	a[1,3] = 6.

	a[m,n] = 6.			# last row
	a[m,n-1] = -12.
	a[m,n-2] = 6.

	do j = 2, m-1 {			# tridiagonal elements
	    a[j,j-1] = 1.
	    a[j,j] = 4.
	    a[j,j+1] = 1.
	}

	b[1] = 0.			# natural spline bndry conditions
	b[m] = 0.

	do i = 2, m-1			# set up data vector
	    b[i] = 100.

	marktime = cptime()
	call hfti (a,M,m,n, b,1,1, tau, krank,rnorm, h,g,ip)

	call printf ("took %8.2f cpu seconds (krank=%d, rnorm=%g)\n")
	    call pargr (cptime() - marktime)
	    call pargi (krank)
	    call pargr (rnorm)

	call printf ("selected coefficients:\n")
	for (i=1;  i <= m;) {		# print first, last 4 coeff
	    call printf ("%8d%15.5f\n")
		call pargi (i)
		call pargr (b[i])
	    if (i == 4)
		i = max(i+1, m-3)
	    else
		i = i + 1
	}
end
