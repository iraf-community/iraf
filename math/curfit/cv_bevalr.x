# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CV_BCHEB -- Procedure to evaluate all the non-zero Chebyshev functions for
# a set of points and given order.

procedure rcv_bcheb (x, npts, order, k1, k2, basis)

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
		call amovkr (real(1.0), basis, npts)
	    else if (k == 2)
		call altar (x, basis[bptr], npts, k1, k2)
	    else {
		call amulr (basis[1+npts], basis[bptr-npts], basis[bptr],
				npts)
		call amulkr (basis[bptr], real(2.0), basis[bptr], npts)
		call asubr (basis[bptr], basis[bptr-2*npts], basis[bptr], npts)
	    }
	    bptr = bptr + npts
	}
end


# CV_BLEG -- Procedure to evaluate all the non zero Legendre function
# for a given order and set of points.

procedure rcv_bleg (x, npts, order, k1, k2, basis)

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
		call amovkr (real(1.0), basis, npts)
	    else if (k == 2)
		call altar (x, basis[bptr], npts, k1, k2)
	    else {
		ri = k
		ri1 = (real(2.0) * ri - real(3.0)) / (ri - real(1.0))
		ri2 = - (ri - real(2.0)) / (ri - real(1.0))
		call amulr (basis[1+npts], basis[bptr-npts], basis[bptr],
		    npts)
		call awsur (basis[bptr], basis[bptr-2*npts],
		    basis[bptr], npts, ri1, ri2)
	    }
	    bptr = bptr + npts
	}
end


# CV_BSPLINE1 -- Evaluate all the non-zero spline1 functions for a set
# of points.

procedure rcv_bspline1 (x, npts, npieces, k1, k2, basis, left)

real	x[npts]		# set of data points
int	npts		# number of points
int	npieces		# number of polynomial pieces minus 1
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions
int	left[ARB]	# indices of the appropriate spline functions

int	k

begin
	call altar (x, basis[1+npts], npts, k1, k2)
	call achtri (basis[1+npts], left, npts)
	call aminki (left, npieces, left, npts)

	do k = 1, npts {
	    basis[npts+k] = max (real(0.0), min (real(1.0),
	        basis[npts+k] - left[k]))
	    basis[k] = max (real(0.0), min (real(1.0), real(1.0) -
	        basis[npts+k]))
	}
end


# CV_BSPLINE3 --  Procedure to evaluate all the non-zero basis functions
# for a cubic spline.

procedure rcv_bspline3 (x, npts, npieces, k1, k2, basis, left)

real	x[npts]		# array of data points
int	npts		# number of data points
int	npieces		# number of polynomial pieces minus 1
real	k1, k2		# normalizing constants
real	basis[ARB]	# array of basis functions
int	left[ARB]	# array of indices for first non-zero spline

int	i
pointer	sp, sx, tx
real	dsx, dtx

begin
	# allocate space
	call smark (sp)
	call salloc (sx, npts, TY_REAL)
	call salloc (tx, npts, TY_REAL)

	# calculate the index of the first non-zero coeff
	call altar (x, Memr[sx], npts, k1, k2)
	call achtri (Memr[sx], left, npts)
	call aminki (left, npieces, left, npts)

	do i = 1, npts {
	    Memr[sx+i-1] = max (real(0.0), min (real(1.0),
	        Memr[sx+i-1] - left[i]))
	    Memr[tx+i-1] = max (real(0.0), min (real(1.0), real(1.0) -
	        Memr[sx+i-1]))
	}

	# calculate the basis function
	#call apowk$t (Mem$t[tx], 3, basis, npts)
	do i = 1, npts {
	    dsx = Memr[sx+i-1]
	    dtx = Memr[tx+i-1]
	    basis[i] = dtx * dtx * dtx
	    basis[npts+i] = real(1.0) + dtx * (real(3.0) + dtx *
	        (real(3.0) - real(3.0) * dtx))
	    basis[2*npts+i] = real(1.0) + dsx * (real(3.0) + dsx *
	        (real(3.0) - real(3.0) * dsx))
	    basis[3*npts+i] = dsx * dsx * dsx
	}
	#call apowk$t (Mem$t[sx], 3, basis[1+3*npts], npts)

	# release space
	call sfree (sp)
end
