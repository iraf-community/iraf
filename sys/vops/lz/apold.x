# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# APOL -- Evaluate a polynomial at X, given the coefficients of the polynomial
# in COEFF and returning the computed value as the function value.

double procedure apold (x, coeff, ncoeff)

double	x		# point at which the polynomial is to be evaluated
double	coeff[ncoeff]	# coefficients of the polynomial, lower orders first
int	ncoeff

int	i
double	pow, sum

begin
	sum = coeff[1]
	pow = x

	do i = 2, ncoeff {
	    sum = sum + pow * coeff[i]
	    pow = pow * x
	}

	return (sum)
end
