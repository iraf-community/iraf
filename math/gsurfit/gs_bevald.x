# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GS_BPOL -- Procedure to evaluate all the non-zero polynomial functions for
# a set of points and given order.

procedure dgs_bpol (x, npts, order, k1, k2, basis)

double	x[npts]		# array of data points
int	npts		# number of points
int	order		# order of polynomial, order = 1, constant
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions

int	bptr, k

begin
	bptr = 1
	do k = 1, order {

	    if (k == 1)
	        call amovkd (1.0d0, basis, npts)
	    else if (k == 2)
		call amovd (x, basis[bptr], npts)
	    else 
		call amuld (basis[bptr-npts], x, basis[bptr], npts)
		
	    bptr = bptr + npts
	}
end

# GS_BCHEB -- Procedure to evaluate all the non-zero Chebyshev functions for
# a set of points and given order.

procedure dgs_bcheb (x, npts, order, k1, k2, basis)

double	x[npts]		# array of data points
int	npts		# number of points
int	order		# order of polynomial, order = 1, constant
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions

int	k, bptr

begin
	bptr = 1
	do k = 1, order {

	    if (k == 1)
		call amovkd (1.0d0, basis, npts)
	    else if (k == 2)
		call altad (x, basis[bptr], npts, k1, k2)
	    else {
		call amuld (basis[1+npts], basis[bptr-npts], basis[bptr],
		    npts)
		call amulkd (basis[bptr], 2.0d0, basis[bptr], npts)
		call asubd (basis[bptr], basis[bptr-2*npts], basis[bptr], npts)
	    }
		
	    bptr = bptr + npts
	}
end


# GS_BLEG -- Procedure to evaluate all the non zero Legendre function
# for a given order and set of points.

procedure dgs_bleg (x, npts, order, k1, k2, basis)

double	x[npts]		# number of data points
int	npts		# number of points
int	order		# order of polynomial, 1 is a constant
double	k1, k2		# normalizing constants
double	basis[ARB]	# array of basis functions

int	k, bptr
double	ri, ri1, ri2

begin
	bptr = 1
	do k = 1, order {

	    if (k == 1)
		call amovkd (1.0d0, basis, npts)
	    else if (k == 2)
		call altad (x, basis[bptr], npts, k1, k2)
	    else {
		ri = k
		ri1 = (2.0d0 * ri - 3.0d0) / (ri - 1.0d0)
		ri2 = - (ri - 2.0d0) / (ri - 1.0d0)
		call amuld (basis[1+npts], basis[bptr-npts], basis[bptr],
		    npts)
		call awsud (basis[bptr], basis[bptr-2*npts],
			basis[bptr], npts, ri1, ri2)
	    }
			
	    bptr = bptr + npts
	}
end
