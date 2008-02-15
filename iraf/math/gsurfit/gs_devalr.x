# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GS_DPOL -- Procedure to evaluate the polynomial derivative basis functions.

procedure rgs_dpol (x, npts, order, nder, k1, k2, basis)

real	x[npts]		# array of data points
int	npts		# number of points
int	order		# order of new polynomial, order = 1, constant
int	nder		# order of derivative, order = 0, no derivative
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions

size_t	sz_val
int	bptr, k, kk
real	fac

begin
	# Optimize for oth and first derivatives.
	if (nder == 0) {
	    call rgs_bpol (x, npts, order, k1, k2, basis)
	    return
	} else if (nder == 1) {
	    call rgs_bpol (x, npts, order, k1, k2, basis)
	    do k = 1, order {
		call amulkr(basis[1+(k-1)*npts], real (k),
		    basis[1+(k-1)*npts], npts)
	    }
	    return
	}

	# Compute the polynomials.
	bptr = 1
	do k = 1, order {
	    if (k == 1) {
		call amovkr (real(1.0), basis, npts)
	    } else if (k == 2) {
		sz_val = npts
		call amovr (x, basis[bptr], sz_val)
	    } else {
		call amulr (basis[bptr-npts], x, basis[bptr], npts)
	    }
	    bptr = bptr + npts
	}

	# Apply the derivative factor.
	bptr = 1
	do k = 1, order {
	    if (k == 1) {
	        fac = real(1.0)
		do kk = 2, nder
		    fac = fac * real (kk)
	    } else {
	        fac = real(1.0)
		do kk = k +  nder - 1, k, -1 
	            fac = fac * real(kk)
	    }
	    call amulkr (basis[bptr], fac, basis[bptr], npts)
	    bptr = bptr + npts
	}
end


# GS_DCHEB -- Procedure to evaluate the chebyshev polynomial derivative
# basis functions using the usual recursion relation.

procedure rgs_dcheb (x, npts, order, nder, k1, k2, basis)

real	x[npts]		# array of data points
int	npts		# number of points
int	order		# order of polynomial, order = 1, constant
int	nder		# order of derivative, order = 0, no derivative
real	k1, k2		# normalizing constants
real	basis[ARB]	# basis functions

size_t	sz_val
int	i, k
pointer	fn, dfn, xnorm, bptr, fptr
real	fac

begin
	# Optimze the no derivatives case.
	if (nder == 0) {
	    call rgs_bcheb (x, npts, order, k1, k2, basis)
	    return
	}

	# Allocate working space for the basis functions and derivatives.
	sz_val = npts * (order + nder)
	call calloc (fn, sz_val, TY_REAL)
	call calloc (dfn, sz_val, TY_REAL)

	# Compute the normalized x values.
	sz_val = npts
	call malloc (xnorm, sz_val, TY_REAL)
        call altar (x, Memr[xnorm], npts, k1, k2)

	# Compute the current solution.
        bptr = fn
        do k = 1, order + nder {
	    if (k == 1) {
	        call amovkr (real(1.0), Memr[bptr], npts)
	    } else if (k == 2) {
	        sz_val = npts
	        call amovr (Memr[xnorm], Memr[bptr], sz_val)
	    } else {
	        call amulr (Memr[xnorm], Memr[bptr-npts], Memr[bptr], npts)
	        call amulkr (Memr[bptr], real(2.0), Memr[bptr], npts)
		call asubr (Memr[bptr], Memr[bptr-2*npts], Memr[bptr], npts)
	    }
	    bptr = bptr + npts
        }

	# Compute the derivative basis functions.
	do i = 1, nder {

	    # Compute the derivatives.
	    bptr = fn
	    fptr = dfn
	    do k = 1, order + nder {
		if (k == 1)
		    call amovkr (real(0.0), Memr[fptr], npts]
		else if (k == 2) {
		    if (i == 1)
		        call amovkr (real(1.0), Memr[fptr], npts]
		    else
		        call amovkr (real(0.0), Memr[fptr], npts]
		} else {
	            call amulr (Memr[xnorm], Memr[fptr-npts], Memr[fptr],
		        npts)
	            call amulkr (Memr[fptr], real(2.0), Memr[fptr], npts)
		    call asubr (Memr[fptr], Memr[fptr-2*npts], Memr[fptr],
		        npts)
		    fac = real (2.0) * real (i)
		    call awsur (Memr[bptr-npts], Memr[fptr], Memr[fptr],
			npts, fac, real(1.0))
		    
		}
	        bptr = bptr + npts
	        fptr = fptr + npts
	    }

	    # Make the derivatives the old solution
	    if (i < nder) {
		sz_val = npts * (order + nder)
		call amovr (Memr[dfn], Memr[fn], sz_val)
	    }
	}

	# Copy the solution into the basis functions.
	sz_val = order * npts
	call amovr (Memr[dfn+nder*npts], basis[1], sz_val)

	call mfree (xnorm, TY_REAL)
	call mfree (fn, TY_REAL)
	call mfree (dfn, TY_REAL)
end


# GS_DLEG -- Procedure to evaluate the Legendre polynomial derivative basis
# functions using the usual recursion relation.

procedure rgs_dleg (x, npts, order, nder, k1, k2, basis)

real	x[npts]		# number of data points
int	npts		# number of points
int	order		# order of new polynomial, 1 is a constant
int	nder		# order of derivate, 0 is no derivative
real	k1, k2		# normalizing constants
real	basis[ARB]	# array of basis functions

size_t	sz_val
int	i, k
pointer	fn, dfn, xnorm, bptr, fptr
real	ri, ri1, ri2, fac

begin
	# Optimze the no derivatives case.
	if (nder == 0) {
	    call rgs_bleg (x, npts, order, k1, k2, basis)
	    return
	}

	# Allocate working space for the basis functions and derivatives.
	sz_val = npts * (order + nder)
	call calloc (fn, sz_val, TY_REAL)
	call calloc (dfn, sz_val, TY_REAL)

	# Compute the normalized x values.
	sz_val = npts
	call malloc (xnorm, sz_val, TY_REAL)
        call altar (x, Memr[xnorm], npts, k1, k2)

	# Compute the basis functions.
	bptr = fn
	do k = 1, order + nder {
	    if (k == 1) {
		call amovkr (real(1.0), Memr[bptr], npts)
	    } else if (k == 2) {
		sz_val = npts
		call amovr (Memr[xnorm], Memr[bptr], sz_val)
	    } else {
		ri = k
		ri1 = (real(2.0) * ri - real(3.0)) / (ri - real(1.0))
		ri2 = - (ri - real(2.0)) / (ri - real(1.0))
		call amulr (Memr[xnorm], Memr[bptr-npts], Memr[bptr], npts)
		call awsur (Memr[bptr], Memr[bptr-2*npts], Memr[bptr],
		    npts, ri1, ri2)
	    }
	    bptr = bptr + npts
	}

	# Compute the derivative basis functions.
	do i = 1, nder {

	    # Compute the derivatives.
	    bptr = fn
	    fptr = dfn
	    do k = 1, order + nder {
		if (k == 1)
		    call amovkr (real(0.0), Memr[fptr], npts]
		else if (k == 2) {
		    if (i == 1)
		        call amovkr (real(1.0), Memr[fptr], npts]
		    else
		        call amovkr (real(0.0), Memr[fptr], npts]
		} else {
		    ri = k
		    ri1 = (real(2.0) * ri - real(3.0)) / (ri - real(1.0))
		    ri2 = - (ri - real(2.0)) / (ri - real(1.0))
		    call amulr (Memr[xnorm], Memr[fptr-npts], Memr[fptr],
		        npts)
		    call awsur (Memr[fptr], Memr[fptr-2*npts], Memr[fptr],
		        npts, ri1, ri2)
		    fac = ri1 * real (i)
		    call awsur (Memr[bptr-npts], Memr[fptr], Memr[fptr],
			npts, fac, real(1.0))
		    
		}
	        bptr = bptr + npts
	        fptr = fptr + npts
	    }

	    # Make the derivatives the old solution
	    if (i < nder) {
		sz_val = npts * (order + nder)
		call amovr (Memr[dfn], Memr[fn], sz_val)
	    }
	}

	# Copy the solution into the basis functions.
	sz_val = order * npts
	call amovr (Memr[dfn+nder*npts], basis[1], sz_val)

	call mfree (xnorm, TY_REAL)
	call mfree (fn, TY_REAL)
	call mfree (dfn, TY_REAL)
end
