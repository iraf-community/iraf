# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math/curfit.h>

include	"dcurfitdef.h"

# CVPOWER -- Convert legendre or chebyshev coeffecients to power series.

procedure dcvpower (cv, ps_coeff, ncoeff)

pointer	cv				# Pointer to curfit structure
int	ncoeff				# Number of coefficients in fit
double	ps_coeff[ncoeff]		# Power series coefficients (output)

pointer	sp, cf_coeff, elm
int	function
int	dcvstati()

begin
	call smark (sp)
	call salloc (elm, ncoeff ** 2, TY_DOUBLE)
	call salloc (cf_coeff, ncoeff, TY_DOUBLE)
	function = dcvstati (cv, CVTYPE)
	call amovkd (0.0d0, Memd[elm], ncoeff ** 2)

	if (function != LEGENDRE && function != CHEBYSHEV) {
	    call eprintf ("Cannot convert coefficients - wrong function type\n")
	    call amovkd (INDEFR, ps_coeff, ncoeff)
	    call sfree (sp)
	    return
	}

	# Get existing coefficients
	call dcvcoeff (cv, Memd[cf_coeff], ncoeff)

	switch (function){
	case (LEGENDRE):
	    call dcv_legen (Memd[elm], Memd[cf_coeff], ps_coeff, ncoeff)
	case (CHEBYSHEV):
	    call dcv_cheby (Memd[elm], Memd[cf_coeff], ps_coeff, ncoeff)
	}

	# Normalize coefficients
	call dcv_normalize (cv, ps_coeff, ncoeff)

	call sfree (sp)
end


# CV_LEGEN -- Convert legendre coeffecients to power series coefficients.
# Scaling the coefficients from -1,+1 to the full data range is done in a 
# seperate procedure (cf_normalize).  Summation notation for Legendre series
# taken from Arfken, page 536, equation 12.8.

procedure dcv_legen (matrix, cf_coeff, ps_coeff, ncoeff)

int	ncoeff
double	matrix[ncoeff, ncoeff]
double	cf_coeff[ncoeff]
double	ps_coeff[ncoeff]

int	s, n, r, i
double	sum

double	dcv_legcoeff()

begin
	# Calculate matrix elements.
	do s = 0, ncoeff - 1 {
	    if (mod (s, 2) == 0) 
	        r = s / 2
	    else 
	        r = (s - 1) / 2

	    do n = 0, r
		matrix[s+1, (s+1) - (2*n)] = dcv_legcoeff (n, s)
	}

	# Multiply matrix columns by curfit coefficients and sum.
	do n = 1, ncoeff {
	    sum = 0.0
	    do i = 1, ncoeff
	        sum = sum + (matrix[i,n] * cf_coeff[i])
	    ps_coeff[n] = sum
	}
end


# CV_LEGCOEFF -- calculate matrix elements for converting legendre coefficients
# to powers of x.

double procedure dcv_legcoeff (k, n)

int	k
int	n

double	fcn, sum1, divisor

double	dcv_factorial()

begin
	sum1 = ((-1) ** k) * dcv_factorial (2 * n - 2 * k)
	divisor = (2**n) * dcv_factorial (k) * dcv_factorial (n-k) * 
	    dcv_factorial (n - 2*k)
	fcn = sum1 / divisor

	return (fcn)
end


# CV_CHEBY -- Convert chebyshev coeffecients to power series coefficients.
# Scaling the coefficients from -1,+1 to the full data range is done in a 
# seperate procedure (cf_normalize).  Summation notation for Chebyshev series 
# from Arfken, page 628, equation 13.83

procedure dcv_cheby (matrix, cf_coeff, ps_coeff, ncoeff)

int	ncoeff				# Number of coefficients
double	matrix[ncoeff, ncoeff]		# Work array for matrix elements
double	cf_coeff[ncoeff]		# Input curfit coefficients
double	ps_coeff[ncoeff]		# Output power series coefficients

int	s, n, m, i
double	sum

double	dcv_chebcoeff()

begin
	# Set first matrix element.
	matrix[1,1] = 1.0d0

	# Calculate remaining matrix elements.
	do s = 1, ncoeff - 1 {
	    if (mod (s, 2) == 0)
	        n = s / 2
	    else 
	        n = (s - 1) / 2

	    do m = 0, n
		matrix[(s+1),(s+1)-(2*m)] = (double(s)/2.0) *
		    dcv_chebcoeff (m, s)
	}

	# Multiply matrix columns by curfit coefficients and sum.
	do n = 1, ncoeff {
	    sum = 0.0
	    do i = 1, ncoeff
	        sum = sum + (matrix[i,n] * cf_coeff[i])
	    ps_coeff[n] = sum
	}
end


# CV_CHEBCOEFF -- calculate matrix elements for converting chebyshev 
# coefficients to powers of x.

double procedure dcv_chebcoeff (m, n)

int	m	# Summation notation index
int	n	# Summation notation index

double	fcn, sum1, divisor
double	dcv_factorial()

begin
	sum1 = ((-1) ** m) * dcv_factorial (n - m - 1) * (2 ** (n - (2*m)))
	divisor = dcv_factorial (n - (2*m)) * dcv_factorial (m)
	fcn = sum1 / divisor

	return (fcn)
end


# CV_NORMALIZE -- Return coefficients scaled to full data range.

procedure dcv_normalize (cv, ps_coeff, ncoeff)

pointer	cv			# Pointer to curfit structure
int	ncoeff			# Number of coefficients in fit
double	ps_coeff[ncoeff]	# Power series coefficients

pointer	sp, elm, index
int	n, i, k
double	k1, k2, bc, sum

double	dcv_bcoeff()

begin
	# Need space for ncoeff**2 matrix elements
	call smark (sp)
	call salloc (elm, ncoeff ** 2, TY_DOUBLE)

	k1 = CV_RANGE(cv)
	k2 = k1 * CV_MAXMIN(cv)

	# Fill matrix, after zeroing it 
	call amovkd (0.0d0, Memd[elm], ncoeff ** 2)
	do n = 1, ncoeff {
	    k = n - 1
	    do i = 0, k {
		bc = dcv_bcoeff (k, i)
		index = elm + k * ncoeff + i
		Memd[index] =  bc * ps_coeff[n] * (k1 ** i) * (k2 ** (k-i))
	    }
	}

	# Now sum along matrix columns to get coefficient of individual 
	# powers of x.
	do n = 1, ncoeff {
	   sum = 0.0d0
	   do i = 1, ncoeff {
	       index = elm + (n-1) + (i-1) * ncoeff
	       sum = sum + Memd[index]
	    }
	    ps_coeff[n] = sum
	}

	call sfree (sp)
end


# CV_BCOEFF -- calculate and return binomial coefficient as function value.

double procedure dcv_bcoeff (n, i)

int	n
int	i

double	dcv_factorial()

begin
	if (i == 0)
	    return (1.0d0)
	else if (n == i)
	    return (1.0d0)
	else
	    return (dcv_factorial (n) / (dcv_factorial (n - i) *
	        dcv_factorial (i)))
end


# CV_FACTORIAL -- calculate factorial of argument and return as function value.

double procedure dcv_factorial (n)

int	n

int	i
double	fact

begin
	if (n == 0)
	    return (1.0d0)
	else {
	    fact = 1.0d0
	    do i = n, 1, -1
	        fact = fact * double (i)
	    return (fact)
	}
end
