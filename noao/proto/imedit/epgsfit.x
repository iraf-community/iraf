include	<math/gsurfit.h>
include	"epix.h"
 
# EP_GSFIT -- Fit a surface to the background annulus.
 
procedure ep_gsfit (ep, data, mask, x, y, w, nx, ny, gs)
 
pointer	ep			# EPIX structure
real	data[nx,ny]		# Data subraster
int	mask[nx,ny]		# Mask subraster
real	x[nx,ny]		# X positions
real	y[nx,ny]		# Y positions
real	w[nx,ny]		# Weights
int	nx, ny			# Subraster size
pointer	gs			# Surface pointer (returned)
 
int	i, j, nw
 
begin
	do i = 1, nx
	    x[i,1] = i
	do j = 1, ny {
	    call amovr (x, x[1,j], nx)
	    call amovkr (real (j), y[1,j], nx)
	    nw = nw + nx
	}
 
	nw = 0
	do j = 1, ny
	    do i = 1, nx {
	        if (mask[i,j] == 2) {
		    w[i,j] = 1.
		    nw = nw + 1
	        }
	        else
		    w[i,j] = 0.
	    }
 
	if (nw < 8) {
	    call eprintf ("ERROR: No background points for fit\n")
	    gs = NULL
	    return
	}
 
	i = EP_XORDER(ep)
	j = EP_YORDER(ep)
	repeat {
	    call gsinit (gs, GS_POLYNOMIAL, i, j, YES,
	        1., real (nx), 1., real (ny))
	    call gsfit (gs, x, y, data, w, nx * ny, WTS_USER, nw)
	    if (nw == OK)
		break
	    i = max (1, i - 1)
	    j = max (1, j - 1)
	}
end

