c function ppoiss.f
c
c source
c   Bevington, page 39.
c
c purpose
c   evaluate poisson probability function
c
c usage
c   result = ppoiss (nobs, averag)
c
c description of parameters
c   nobs   - number of items observed
c   averag - mean of distribution
c
c subroutines and function subprograms required
c   factor (n)
c      calculates n factorial for integers
c
	function ppoiss (nobs,averag)
1	ppoiss=((averag**nobs)/factor(nobs))*exp(-averag)
	return
	end
