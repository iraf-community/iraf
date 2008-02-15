# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GS_BPOL -- Procedure to evaluate all the non-zero polynomial functions for
# a set of points and given order.

procedure rgs_bpol (x, npts, order, k1, k2, basis)

real	x[npts]		# array of data points
int	npts		# number of points
int	order		# order of polynomial, order = 1, constant
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions

int	bptr, k

begin
	bptr = 1
	do k = 1, order {

	    if (k == 1)
	        call amovkr (1.0, basis, npts)
	    else if (k == 2)
		call amovr (x, basis[bptr], npts)
	    else 
		call amulr (basis[bptr-npts], x, basis[bptr], npts)
		
	    bptr = bptr + npts
	}
end

# GS_BCHEB -- Procedure to evaluate all the non-zero Chebyshev functions for
# a set of points and given order.

procedure rgs_bcheb (x, npts, order, k1, k2, basis)

real	x[npts]		# array of data points
int	npts		# number of points
int	order		# order of polynomial, order = 1, constant
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions

int	k, bptr

begin
	bptr = 1
	do k = 1, order {

	    if (k == 1)
	        call amovkr (1.0, basis, npts)
	    else if (k == 2)
		call altar (x, basis[bptr], npts, k1, k2)
	    else {
		call amulr (basis[1+npts], basis[bptr-npts], basis[bptr],
		    npts)
		call amulkr (basis[bptr], 2.0, basis[bptr], npts)
		call asubr (basis[bptr], basis[bptr-2*npts], basis[bptr], npts)
	    }
		
	    bptr = bptr + npts
	}
end


# GS_BLEG -- Procedure to evaluate all the non zero Legendre function
# for a given order and set of points.

procedure rgs_bleg (x, npts, order, k1, k2, basis)

real	x[npts]		# number of data points
int	npts		# number of points
int	order		# order of polynomial, 1 is a constant
real	k1, k2		# normalizing constants
real	basis[ARB]	# array of basis functions

int	k, bptr
real	ri, ri1, ri2

begin
	bptr = 1
	do k = 1, order {

	    if (k == 1)
		call amovkr (1.0, basis, npts)
	    else if (k == 2)
		call altar (x, basis[bptr], npts, k1, k2)
	    else {
		ri = k
		ri1 = (2. * ri - 3.) / (ri - 1.)
		ri2 = - (ri - 2.) / (ri - 1.)
		call amulr (basis[1+npts], basis[bptr-npts], basis[bptr],
		    npts)
		call awsur (basis[bptr], basis[bptr-2*npts],
			basis[bptr], npts, ri1, ri2)
	    }
			
	    bptr = bptr + npts
	}
end
