c function ploren.f
c
c source
c   Bevington, page 51.
c
c purpose
c   evaluate lorentzian probability function
c
c usage
c   result = ploren (x, averag, width)
c
c description of parameters
c   x      - value for which probability is to be evaluated
c   averag - mean of distribution
c   width  - full width at half maximum of distribution
c
c subroutines and function subprograms required
c   none
c
	function ploren (x,averag,width)
1	ploren=0.1591549431*width/((x-averag)**2+(width/2.)**2)
	return
	end
