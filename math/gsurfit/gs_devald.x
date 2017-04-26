# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GS_DPOL -- Procedure to evaluate the polynomial derivative basis functions.

procedure dgs_dpol (x, npts, order, nder, k1, k2, basis)

double	x[npts]		# array of data points
int	npts		# number of points
int	order		# order of new polynomial, order = 1, constant
int	nder		# order of derivative, order = 0, no derivative
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions

int	bptr, k, kk
double	fac

begin
	# Optimize for oth and first derivatives.
	if (nder == 0) {
	    call dgs_bpol (x, npts, order, k1, k2, basis)
	    return
	} else if (nder == 1) {
	    call dgs_bpol (x, npts, order, k1, k2, basis)
	    do k = 1, order {
		call amulkd(basis[1+(k-1)*npts], double (k),
		    basis[1+(k-1)*npts], npts)
	    }
	    return
	}

	# Compute the polynomials.
	bptr = 1
	do k = 1, order {
	    if (k == 1)
		call amovkd (double(1.0), basis, npts)
	    else if (k == 2)
		call amovd (x, basis[bptr], npts)
	    else 
		call amuld (basis[bptr-npts], x, basis[bptr], npts)
	    bptr = bptr + npts
	}

	# Apply the derivative factor.
	bptr = 1
	do k = 1, order {
	    if (k == 1) {
	        fac = double(1.0)
		do kk = 2, nder
		    fac = fac * double (kk)
	    } else {
	        fac = double(1.0)
		do kk = k +  nder - 1, k, -1 
	            fac = fac * double(kk)
	    }
	    call amulkd (basis[bptr], fac, basis[bptr], npts)
	    bptr = bptr + npts
	}
end


# GS_DCHEB -- Procedure to evaluate the chebyshev polynomial derivative
# basis functions using the usual recursion relation.

procedure dgs_dcheb (x, npts, order, nder, k1, k2, basis)

double	x[npts]		# array of data points
int	npts		# number of points
int	order		# order of polynomial, order = 1, constant
int	nder		# order of derivative, order = 0, no derivative
double	k1, k2		# normalizing constants
double	basis[ARB]	# basis functions

int	i, k
pointer	fn, dfn, xnorm, bptr, fptr
double	fac

begin
	# Optimze the no derivatives case.
	if (nder == 0) {
	    call dgs_bcheb (x, npts, order, k1, k2, basis)
	    return
	}

	# Allocate working space for the basis functions and derivatives.
	call calloc (fn, npts * (order + nder), TY_DOUBLE)
	call calloc (dfn, npts * (order + nder), TY_DOUBLE)

	# Compute the normalized x values.
	call malloc (xnorm, npts, TY_DOUBLE)
        call altad (x, Memd[xnorm], npts, k1, k2)

	# Compute the current solution.
        bptr = fn
        do k = 1, order + nder {
	    if (k == 1)
	        call amovkd (double(1.0), Memd[bptr], npts)
	    else if (k == 2)
	        call amovd (Memd[xnorm], Memd[bptr], npts)
	    else {
	        call amuld (Memd[xnorm], Memd[bptr-npts], Memd[bptr], npts)
	        call amulkd (Memd[bptr], double(2.0), Memd[bptr], npts)
		call asubd (Memd[bptr], Memd[bptr-2*npts], Memd[bptr], npts)
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
		    call amovkd (double(0.0), Memd[fptr], npts)
		else if (k == 2) {
		    if (i == 1)
		        call amovkd (double(1.0), Memd[fptr], npts)
		    else
		        call amovkd (double(0.0), Memd[fptr], npts)
		} else {
	            call amuld (Memd[xnorm], Memd[fptr-npts], Memd[fptr],
		        npts)
	            call amulkd (Memd[fptr], double(2.0), Memd[fptr], npts)
		    call asubd (Memd[fptr], Memd[fptr-2*npts], Memd[fptr],
		        npts)
		    fac = double (2.0) * double (i)
		    call awsud (Memd[bptr-npts], Memd[fptr], Memd[fptr],
			npts, fac, double(1.0))
		    
		}
	        bptr = bptr + npts
	        fptr = fptr + npts
	    }

	    # Make the derivatives the old solution
	    if (i < nder)
		call amovd (Memd[dfn], Memd[fn], npts * (order + nder))
	}

	# Copy the solution into the basis functions.
	call amovd (Memd[dfn+nder*npts], basis[1], order * npts)

	call mfree (xnorm, TY_DOUBLE)
	call mfree (fn, TY_DOUBLE)
	call mfree (dfn, TY_DOUBLE)
end


# GS_DLEG -- Procedure to evaluate the Legendre polynomial derivative basis
# functions using the usual recursion relation.

procedure dgs_dleg (x, npts, order, nder, k1, k2, basis)

double	x[npts]		# number of data points
int	npts		# number of points
int	order		# order of new polynomial, 1 is a constant
int	nder		# order of derivate, 0 is no derivative
double	k1, k2		# normalizing constants
double	basis[ARB]	# array of basis functions

int	i, k
pointer	fn, dfn, xnorm, bptr, fptr
double	ri, ri1, ri2, fac

begin
	# Optimze the no derivatives case.
	if (nder == 0) {
	    call dgs_bleg (x, npts, order, k1, k2, basis)
	    return
	}

	# Allocate working space for the basis functions and derivatives.
	call calloc (fn, npts * (order + nder), TY_DOUBLE)
	call calloc (dfn, npts * (order + nder), TY_DOUBLE)

	# Compute the normalized x values.
	call malloc (xnorm, npts, TY_DOUBLE)
        call altad (x, Memd[xnorm], npts, k1, k2)

	# Compute the basis functions.
	bptr = fn
	do k = 1, order + nder {
	    if (k == 1)
		call amovkd (double(1.0), Memd[bptr], npts)
	    else if (k == 2)
		call amovd (Memd[xnorm], Memd[bptr], npts)
	    else {
		ri = k
		ri1 = (double(2.0) * ri - double(3.0)) / (ri - double(1.0))
		ri2 = - (ri - double(2.0)) / (ri - double(1.0))
		call amuld (Memd[xnorm], Memd[bptr-npts], Memd[bptr], npts)
		call awsud (Memd[bptr], Memd[bptr-2*npts], Memd[bptr],
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
		    call amovkd (double(0.0), Memd[fptr], npts)
		else if (k == 2) {
		    if (i == 1)
		        call amovkd (double(1.0), Memd[fptr], npts)
		    else
		        call amovkd (double(0.0), Memd[fptr], npts)
		} else {
		    ri = k
		    ri1 = (double(2.0) * ri - double(3.0)) / (ri - double(1.0))
		    ri2 = - (ri - double(2.0)) / (ri - double(1.0))
		    call amuld (Memd[xnorm], Memd[fptr-npts], Memd[fptr],
		        npts)
		    call awsud (Memd[fptr], Memd[fptr-2*npts], Memd[fptr],
		        npts, ri1, ri2)
		    fac = ri1 * double (i)
		    call awsud (Memd[bptr-npts], Memd[fptr], Memd[fptr],
			npts, fac, double(1.0))
		    
		}
	        bptr = bptr + npts
	        fptr = fptr + npts
	    }

	    # Make the derivatives the old solution
	    if (i < nder)
		call amovd (Memd[dfn], Memd[fn], npts * (order + nder))
	}

	# Copy the solution into the basis functions.
	call amovd (Memd[dfn+nder*npts], basis[1], order * npts)

	call mfree (xnorm, TY_DOUBLE)
	call mfree (fn, TY_DOUBLE)
	call mfree (dfn, TY_DOUBLE)
end
