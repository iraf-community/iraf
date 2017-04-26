c function agauss.f
c
c source
c   Bevington, page 48.
c
c purpose
c   evaluate integral of gaussian probability function
c
c usage
c   result = agauss (x, averag, sigma)
c
c description of parameters
c   x      - limit for integral
c   averag - mean of distribution
c   sigma  - standard deviation of distribution
c   integration range is averag +/- z*sigma
c      where z = abs(x-averag)/sigma
c
c subroutines and function subprograms required
c   none
c
	function agauss (x,averag,sigma)
	double precision z,y2,term,sum,denom
11	z=abs(x-averag)/sigma
	agauss=0.
	if (z) 42,42,21
21	term=0.7071067812*z
22	sum=term
	y2=(z**2)/2.
	denom=1.
c
c accumulate sums of terms
c
31	denom=denom+2.
32	term=term*(y2*2./denom)
33	sum=sum+term
	if (term/sum-1.e-10) 41,41,31
41	agauss=1.128379167*sum*dexp(-y2)
42	return
	end
