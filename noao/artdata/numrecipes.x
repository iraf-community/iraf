# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>

# POIDEV -- Returns Poisson deviates for a given mean.
# GASDEV -- Return a normally distributed deviate of zero mean and unit var.


# POIDEV -- Returns Poisson deviates for a given mean.
# The real value returned is an integer.
#
# Copyright(c) 2017 Anastasia Galkin
# References:
#   for lambda <= 30: Donald E. Knuth (1969). Seminumerical
#                     Algorithms. The Art of Computer Programming,
#                     Volume 2. Addison Wesley.
#   for lambda  > 30: A. C. Atkinson (1978), The Computer Generation
#                     of Poisson Random Variables, Journal of the
#                     Royal Statistical Society Series C (Applied
#                     Statistics) Vol. 28, No. 1. (1979) (pp 29-35)

real procedure poidev (xm, seed)

real	xm		# Poisson mean
long	seed		# Random number seed

real	c, beta, alpha, k, x, p, e, n, u, lhs, rhs, v, r,y
real	urand(), log(), exp(), gammln()
begin
    if (xm < 0.) {
        return 0.0
    } else if (xm < 30.) {
	x = 0.
 	p = 1.
 	e = exp(-xm)
 	while (p > e) {
 	    r = urand(seed)
	    p = p * r
 	    x = x + 1.
	}
 	return x - 1.
    } else {
        c = 0.767 - 3.36 / xm
 	beta = PI / sqrt(3. * xm)
 	alpha = beta * xm
 	k = log(c) - xm - log(beta)
	repeat {
	    u = urand(seed)
	    x = (alpha - log((1. - u) / u)) / beta
	    n = int(x + 0.5)
	    if (n >= 0.) {
	        v = urand(seed)
		y = alpha - beta * x
		lhs = y + log(v / (1. + exp(y)) ** 2)
		rhs = k + n * log(xm) - gammln(n + 1.)
	    }
	} until (lhs <= rhs)
	return n
    }
end


# GASDEV -- Return a normally distributed deviate with zero mean and unit
# variance.  The method computes two deviates simultaneously.
#
# Copyright(c) 2017 Anastasia Galkin
# Reference: G. E. P. Box and Mervin E. Muller, A Note on the Generation of
#            Random Normal Deviates, The Annals of Mathematical Statistics
#            (1958), Vol. 29, No. 2 pp. 610–611

real procedure gasdev (seed)

long	seed

int	count
data	count/0/

real	u1, u2, x
real	urand()

begin
	if (count == 0) {
	        u1 = 1. - urand (seed)
 	        u2 = urand (seed)
		x = sqrt(-2 * log(u1)) * cos(2*PI*u2);
		count = 1
	} else {
		x = sqrt(-2 * log(u1)) * sin(2*PI*u2);
		count = 0
	}
	return (x)
end
