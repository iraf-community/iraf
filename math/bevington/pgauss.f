c function pgauss.f
c
c source
c   Bevington, page 45.
c
c purpose
c   evaluate gaussian probability function
c
c usage
c   result = pgauss (x, averag, sigma)
c
c description of parameters
c   x      - value for which probability is to be evaluated
c   averag - mean of distribution
c   sigma  - standard deviation of distribution
c
c subroutines and function subprograms required
c   none
c
	function pgauss (x,averag,sigma)
	double precision z
1	z=(x-averag)/sigma
2	pgauss=0.3989422804/sigma*dexp(-(z**2)/2.)
	return
	end
