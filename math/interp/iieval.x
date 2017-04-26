# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help
		procedures to evaluate interpolants

     These procedures are seperated from the other programs in order
to make it easier to optimise these in case it becomes necessary to
obtain the fastest possible interpolants.  The interpolation package
spends most of its time in these routines.
.endhelp

procedure iievne(x,y,n,a)   # nearest neighbor

real	x[ARB]	# x values, must be within [1,n]
real	y[ARB]	# interpolated values returned to user
int	n	# number of x values
real	a[ARB]	# data to be interpolated

int	i

begin
	do i = 1, n
	    y[i] = a[int(x[i] + 0.5)]
	return
end

procedure iievli(x,y,n,a)   # linear

real	x[ARB]	# x values, must be within [1,n]
real	y[ARB]	# interpolated values returned to user
int	n	# number of x values
real	a[ARB]	# data to be interpolated

int	i,nx

begin
	
	do i = 1,n {
	    nx = x[i]
	    y[i] =  (x[i] - nx) * a[nx + 1] + (nx + 1 - x[i]) * a[nx]
	}
	return
end

procedure iievp3(x,y,n,a)   # interior third order polynomial

real	x[ARB]	# x values, must be within [1,n]
real	y[ARB]	# interpolated values returned to user
int	n	# number of x values
real	a[ARB]	# data to be interpolated from a[0] to a[n+2]

int	i,nx,nxold
real	s,t,cd20,cd21

begin
	nxold = -1
	do i = 1,n {
	    nx = x[i]
	    s = x[i] - nx
	    t = 1. - s
	    if (nx != nxold) {

		# second central differences:
		cd20 = 1./6. * (a[nx+1] - 2. * a[nx] + a[nx-1])
		cd21 = 1./6. * (a[nx+2] - 2. * a[nx+1] + a[nx])
		nxold = nx
	    }
	    y[i] = s * (a[nx+1] + (s * s - 1.) * cd21) +
		    t * (a[nx] + (t * t - 1.) * cd20)

	}
	return
end

procedure iievp5(x,y,n,a)   # interior fifth order polynomial

real	x[ARB]	# x values, must be within [1,n]
real	y[ARB]	# interpolated values returned to user
int	n	# number of x values
real	a[ARB]	# data to be interpolated - from a[-1] to a[n+3]

int	i,nx,nxold
real	s,t,cd20,cd21,cd40,cd41

begin
	nxold = -1
	do i = 1,n {
	    nx = x[i]
	    s = x[i] - nx
	    t = 1. - s
	    if (nx != nxold) {
		cd20 = 1./6. * (a[nx+1] - 2. * a[nx] + a[nx-1])
		cd21 = 1./6. * (a[nx+2] - 2. * a[nx+1] + a[nx])

		# fourth central differences
		cd40 = 1./120. * (a[nx-2] - 4. * a[nx-1] +
			6. * a[nx] - 4. * a[nx+1] + a[nx+2])
		cd41 = 1./120. * (a[nx-1] - 4. * a[nx] +
			6. * a[nx+1] - 4. * a[nx+2] + a[nx+3])
		nxold = nx
	    }
	    y[i] = s * (a[nx+1] + (s * s - 1.) *
			 (cd21 + (s * s - 4.) * cd41)) +
		   t * (a[nx] + (t * t - 1.) *
			 (cd20 + (t * t - 4.) * cd40)) 
	}
	return
end

procedure iievs3(x,y,n,a)   # cubic spline evaluator

real	x[ARB]	# x values, must be within [1,n]
real	y[ARB]	# interpolated values returned to user
int	n	# number of x values
real	a[ARB]	# basis spline coefficients - from a[0] to a[n+1]

int	i,nx,nxold
real	s,c0,c1,c2,c3

begin
	nxold = -1
	do i = 1,n {
	    nx = x[i]
	    s = x[i] - nx
	    if (nx != nxold) {

		# convert b-spline coeff's to poly. coeff's
		c0 = a[nx-1] + 4. * a[nx] + a[nx+1]
		c1 = 3. * (a[nx+1] - a[nx-1])
		c2 = 3. * (a[nx-1] - 2. * a[nx] + a[nx+1])
		c3 = -a[nx-1] + 3. * a[nx] - 3. * a[nx+1] + a[nx+2]
		nxold = nx
	    }
	    y[i] = c0 + s * (c1 + s * (c2 + s * c3) )
	}
	return
end
