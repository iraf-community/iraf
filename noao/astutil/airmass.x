# AIRMASS -- Compute the airmass at a given elevation above the horizon.
# Airmass formulation from Allen "Astrophysical Quantities" 1973 p.125,133.

procedure t_airmass()

real	elevation, airmass, scale
real	x, radians_per_degree
bool	clgetb()
real	clgetr()
data	radians_per_degree /57.29577951D0/

begin
	# Get elevation in either degrees or radians and the scale factor
	# for the Earth's atmosphere.
	elevation = clgetr ("elevation")
	if (!clgetb ("radians"))
	    elevation = elevation / radians_per_degree
	scale = clgetr ("scale")

	x  = scale * sin (elevation)
	airmass = sqrt (x**2 + 2*scale + 1) - x

	call printf ("airmass %.5g at an elevation of ")
	    call pargr (airmass)
	call printf ("%.5g degrees (%.5g radians) above horizon\n")
	    call pargr (elevation * radians_per_degree)
	    call pargr (elevation)

	# Store airmass back in a parameter so that it can be accessed from
	# the CL.
	call clputr ("airmass", airmass)
end
