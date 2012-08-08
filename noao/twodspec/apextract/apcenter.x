include	<pkg/center1d.h>

# AP_CENTER -- Locate the center of an emission profile.  This is done
# using the CENTER1D algorithm.  The procedure gets the centering
# parameters using CL queries.  If the center is not found because of the
# RADIUS or THRESHOLD centering criteria then INDEF is returned.

real procedure ap_center (x, data, npts)

real	x		# Initial guess
real	data[npts]	# Data
int	npts		# Number of data points

real	width		# Centering width
real	radius		# Centering radius
real	threshold	# Detection threshold

real	apgetr(), center1d()

begin
	width = apgetr ("width")
	radius = apgetr ("radius")
	threshold = apgetr ("threshold")

	return (center1d (x, data, npts, width, EMISSION, radius, threshold))
end
