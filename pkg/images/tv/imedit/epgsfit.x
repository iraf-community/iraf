include	<math/gsurfit.h>
include	"epix.h"
 
# EP_GSFIT -- Fit the background annulus.
 
procedure ep_gsfit (ep, data, mask, x, y, w, nx, ny, gs)
 
pointer	ep			# EPIX structure
real	data[nx,ny]		# Data subraster
int	mask[nx,ny]		# Mask subraster
real	x[nx,ny]		# X positions
real	y[nx,ny]		# Y positions
real	w[nx,ny]		# Weights
int	nx, ny			# Subraster size
pointer	gs			# Surface pointer (returned)
 
int	i, j, n, npts, xo, yo
pointer	sp, work
real	amedr()
 
begin
	call smark (sp)
	call salloc (work, nx * ny, TY_REAL)

	gs = NULL
	npts = nx * ny

	if (EP_XORDER(ep) == 0 || EP_YORDER(ep) == 0) {
	    n = 0
	    do j = 1, ny {
		do i = 1, nx {
		    if (mask[i,j] == 2) {
			Memr[work+n] = data[i,j]
			n = n + 1
		    }
		}
	    }
	    call amovkr (amedr (Memr[work], n), Memr[work], npts)
	    xo = 1
	    yo = 1
	} else {
	    call amovr (data, Memr[work], npts)
	    xo = EP_XORDER(ep)
	    yo = EP_YORDER(ep)
	}

	n = 0
	do j = 1, ny {
	    do i = 1, nx {
		x[i,j] = i
		y[i,j] = j
		if (mask[i,j] == 2) {
		    w[i,j] = 1.
		    n = n + 1
		} else
		    w[i,j] = 0.
	    }
	}
 
	if (n > 7) {
	    repeat {
		call gsinit (gs, GS_POLYNOMIAL, xo, yo, YES,
		    1., real (nx), 1., real (ny))
		call gsfit (gs, x, y, Memr[work], w, npts, WTS_USER, n)
		if (n == OK)
		    break
		xo = max (1, xo - 1)
		yo = max (1, yo - 1)
	    }
	} else
	    call eprintf ("ERROR: Insufficient background points\n")

	call sfree (sp)
end
