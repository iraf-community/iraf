include "../lib/polyphot.h"

# AP_YSHIFT -- Shift a polygon to a new center position.

procedure ap_yshift (py, x, y, nver, newx, newy)

pointer	py		# pointer to the polyphot structure
real	x[ARB]		# x coordinates of the vertices
real	y[ARB]		# y coordinates of the vertices
int	nver		# number of vertices
real	newx, newy	# new x and y coordinates of center

real	xshift, yshift
real	apstatr()

begin
	call apsetr (py, PYX, apstatr (py, PYCX))
	call apsetr (py, PYY, apstatr (py, PYCY))

	xshift = newx - apstatr (py, PYCX)
	yshift = newy - apstatr (py, PYCY)
	call aaddkr (x, xshift, x, nver)
	call aaddkr (y, yshift, y, nver)

	call apsetr (py, PYCX, newx)
	call apsetr (py, PYCY, newy)
end
