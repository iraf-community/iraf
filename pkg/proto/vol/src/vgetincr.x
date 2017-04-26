include "pvol.h"


# VGETINCREM -- Get list of input voxel band & line indices that contribute to
# current ray, using simple incremental digital differential analyzer.

procedure vgetincrem (tx1,ty1, tx2,ty2, nx,ny, maxvox, nvox, xind, yind)
double	tx1,ty1		# (in)  starting coordinate of ray
double	tx2,ty2		# (in)  ending coordinate of ray
int	nx,ny		# (in)  dimensions of working plane (1:nx, 1:ny)
int	maxvox		# (in)  max dimension of output index arrays
int	nvox		# (out) count of indices for current ray
int	xind[ARB]	# (out) array of input voxel band indices
int	yind[ARB]	# (out) array of input voxel line indices

real	x1,y1, x2,y2, dy,dx, adx,ady, x,y, length
int	i, tvox, xi, yi

int	vsign()

begin
	# Going between integer and floating point grid representations
	# is tricky, especially for symmetrical rotation angles aligned with
	# the grid nodes.  Rounding from double to single precision here
	# is the only way I could get things to work for all possible angles
	# and grid dimensions.

	x1 = tx1
	y1 = ty1
	x2 = tx2
	y2 = ty2
	dx = x2 - x1
	dy = y2 - y1
	adx = abs (dx)
	ady = abs (dy)

	# Approximate the line length.
	if (adx >= ady)
	    length = adx
	else
	    length = ady
	tvox = int (length) + 1
	if (tvox > maxvox)
	    call error (0, "VGETINCREM:  nvox > maxvox")
	
	# Select the larger of dx or dy to be one raster unit.
	dx = dx / length
	dy = dy / length

	# Round values; using vsign function makes this work in all quadrants.
	x = x1 + 0.5 * vsign (dx)
	y = y1 + 0.5 * vsign (dy)

	# Boundary-extend if coming FROM +x or +y and if rounding would not
	# take us out of range.
	if (dx == -1.0 || dy == -1.0) {
	    if (!((int(x-dx) <= 0 || int(x-dx) > nx) ||
		(int(y-dy) <= 0 || int(y-dy) > ny))) {
		x = x - dx
		y = y - dy
	    }
	}

	# Fill in the integer grid coordinates.
	nvox = 0
	do i = 1, tvox {
	    xi = nx - int(x) + 1	# yes, we want to truncate here
	    yi = int (y)
	    if (1 <= xi && xi <= nx && 1 <= yi && yi <= ny) {
		nvox = nvox + 1
		xind[nvox] = xi
		yind[nvox] = yi
	    }
	    x = x + dx
	    y = y + dy
	}
end


# VSIGN -- Return -1, 0, +1 if val is <0, =0, >0.

int procedure vsign (val)
real	val

begin
	if (val < 0.0)
	    return (-1)
	else if (val == 0.0)
	    return (0)
	else
	    return (1)
end
