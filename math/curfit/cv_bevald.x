# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CV_BCHEB -- Procedure to evaluate all the non-zero Chebyshev functions for
# a set of points and given order.

procedure dcv_bcheb (x, npts, order, k1, k2, basis)

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

# CV_BLEG -- Procedure to evaluate all the non zero Legendre function
# for a given order and set of points.

procedure dcv_bleg (x, npts, order, k1, k2, basis)

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

# CV_BSPLINE1 -- Evaluate all the non-zero spline1 functions for a set
# of points.

procedure dcv_bspline1 (x, npts, npieces, k1, k2, basis, left)

double	x[npts]		# set of data points
int	npts		# number of points
int	npieces		# number of polynomial pieces minus 1
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions
int	left[ARB]	# indices of the appropriate spline functions

int	k

begin
	call altad (x, basis[1+npts], npts, k1, k2)
	call achtdi (basis[1+npts], left, npts)
	call aminki (left, npieces, left, npts)

	do k = 1, npts {
	    basis[npts+k] = basis[npts+k] - left[k]
	    basis[k] = 1.0d0 - basis[npts+k]
	}
end

# CV_BSPLINE3 --  Procedure to evaluate all the non-zero basis functions
# for a cubic spline.

procedure dcv_bspline3 (x, npts, npieces, k1, k2, basis, left)

double	x[npts]		# array of data points
int	npts		# number of data points
int	npieces		# number of polynomial pieces minus 1
double	k1, k2		# normalizing constants
double	basis[ARB]	# array of basis functions
int	left[ARB]	# array of indices for first non-zero spline

int	i
pointer	sp, sx, tx

begin

	# allocate space
	call smark (sp)

	call salloc (sx, npts, TY_DOUBLE)
	call salloc (tx, npts, TY_DOUBLE)

	# calculate the index of the first non-zero coeff
	call altad (x, Memd[sx], npts, k1, k2)
	call achtdi (Memd[sx], left, npts)
	call aminki (left, npieces, left, npts)

	# normalize x to 0 to 1
	do i = 1, npts {
	    Memd[sx+i-1] = Memd[sx+i-1] - left[i]
	    Memd[tx+i-1] = 1.0d0 - Memd[sx+i-1]
	}

	# calculate the basis function
	call apowkd (Memd[tx], 3, basis, npts)
	do i = 1, npts {
	    basis[npts+i] = 1.0d0 + Memd[tx+i-1] * (3.0d0 + Memd[tx+i-1] *
	        (3.0d0 - 3.0d0 * Memd[tx+i-1]))
	    basis[2*npts+i] = 1.0d0 + Memd[sx+i-1] * (3.0d0 + Memd[sx+i-1] *
	        (3.0d0 - 3.0d0 * Memd[sx+i-1]))
	}
	call apowkd (Memd[sx], 3, basis[1+3*npts], npts)

	# release space
	call sfree (sp)
end
