include "../lib/apphot.h"
include "../lib/polyphot.h"

# AP_YSHIFT -- Shift a polygon to a new center position.

procedure ap_yshift (py, im, x, y, nver, newx, newy)

pointer	py		# pointer to the polyphot structure
pointer	im		# the input image descriptor
real	x[ARB]		# x coordinates of the vertices
real	y[ARB]		# y coordinates of the vertices
int	nver		# number of vertices
real	newx, newy	# new x and y coordinates of center

real	xshift, yshift
real	apstatr()
int	apstati()

begin
	call apsetr (py, PYX, apstatr (py, PYCX))
	call apsetr (py, PYY, apstatr (py, PYCY))

	xshift = newx - apstatr (py, PYCX)
	yshift = newy - apstatr (py, PYCY)
	call aaddkr (x, xshift, x, nver)
	call aaddkr (y, yshift, y, nver)
	call apsetr (py, PYCX, newx)
	call apsetr (py, PYCY, newy)

        switch (apstati(py,WCSOUT)) {
        case WCS_WORLD, WCS_PHYSICAL:
            call ap_ltoo (py, newx, newy, xshift, yshift, 1)
        case WCS_TV:
            call ap_ltov (im, newx, newy, xshift, yshift, 1)
        default:
	    xshift = newx
	    yshift = newy
        }
        call apsetr (py, OPYCX, xshift)
        call apsetr (py, OPYCY, yshift)
end
