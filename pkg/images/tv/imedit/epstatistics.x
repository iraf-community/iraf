include	"epix.h"

# EP_STATISTICS -- Compute and print statistics for the input aperture.

procedure ep_statistics (ep, ap, xa, ya, xb, yb, box)

pointer ep			# EPIX structure
int	ap			# Aperture type
int	xa, ya, xb, yb		# Aperture coordinates
int	box			# Print box?

int	i, x1, x2, y1, y2
pointer	mask, x, y, w, gs

begin
	i = max (5., abs (EP_SEARCH(ep))+EP_BUFFER(ep)+EP_WIDTH(ep)+1)
	x1 = min (xa, xb) - i
	x2 = max (xa, xb) + i
	y1 = min (ya, xb) - i
	y2 = max (ya, yb) + i
	EP_OUTDATA(ep) = NULL
	call ep_gindata (ep, x1, x2, y1, y2)
	if (EP_INDATA(ep) != NULL) {
	    call malloc (mask, EP_NPTS(ep), TY_INT)
	    call malloc (x, EP_NPTS(ep), TY_INT)
	    call malloc (y, EP_NPTS(ep), TY_INT)
	    call malloc (w, EP_NPTS(ep), TY_INT)

	    call ep_search (ep, Memr[EP_INDATA(ep)], EP_NX(ep),
		EP_NY(ep), ap, xa, ya, xb, yb)
	    call ep_mask (ep, mask, ap, xa, ya, xb, yb)
	    call ep_gsfit (ep, Memr[EP_INDATA(ep)], Memi[mask],
		Memr[x], Memr[y], Memr[w], EP_NX(ep), EP_NY(ep), gs)
	    call ep_statistics1 (Memr[EP_INDATA(ep)], Memi[mask],
		EP_NX(ep), EP_NY(ep), EP_X1(ep), EP_Y1(ep),
		(xa+xb)/2, (ya+yb)/2, gs)
	    if (box == YES)
	        call ep_box (Memr[EP_INDATA(ep)], EP_NX(ep), EP_NY(ep),
		    EP_X1(ep), EP_Y1(ep), xa, ya, xb, yb)

	    call mfree (mask, TY_INT)
	    call mfree (x, TY_INT)
	    call mfree (y, TY_INT)
	    call mfree (w, TY_INT)
	    call gsfree (gs)
	}
end


# EP_STATISTICS1 -- Compute and print statistics.

procedure ep_statistics1 (data, mask, nx, ny, x1, y1, x, y, gs)

real	data[nx,ny]		# Input data subraster
int	mask[nx,ny]		# Mask subraster
int	nx, ny			# Size of subraster
int	x1, y1			# Origin of subraster
int	x, y			# Center of object
pointer	gs			# GSURFIT pointer

int	i, j, area, nsky
real	flux, sky, sigma, d, gseval()

begin
	flux = 0.
	area = 0
	sky = 0.
	sigma = 0.
	nsky = 0

	do j = 1, ny {
	    do i = 1, nx {
	        if (mask[i,j] == 1) {
		    d = data[i,j]
		    if (gs != NULL)
		        d = d - gseval (gs, real (i), real (j))
		    flux = flux + d
		    area = area + 1
	        } else if (mask[i,j] == 2) {
		    d = data[i,j] - gseval (gs, real (i), real (j))
		    sky = sky + data[i,j]
		    sigma = sigma + d * d
		    nsky = nsky + 1
	        }
	    }
	}

	call printf ("x=%d y=%d z=%d mean=%g area=%d")
	    call pargi (x)
	    call pargi (y)
	    call pargr (data[x-x1+1,y-y1+1])
	    call pargr (flux / area)
	    call pargi (area)

	if (nsky > 0) {
	    call printf (" sky=%g sigma=%g nsky=%d")
		call pargr (sky / nsky)
		call pargr (sqrt (sigma / nsky))
		call pargi (nsky)
	}

	call printf ("\n")
end


# EP_BOX -- Print box of pixel values.

procedure ep_box (data, nx, ny, xo, yo, xa, ya, xb, yb)

real	data[nx,ny]		# Input data subraster
int	nx, ny			# Size of subraster
int	xo, yo			# Origin of subraster
int	xa, ya, xb, yb		# Aperture

int	i, j, x1, x2, y1, y2, x, y

begin
	x1 = min (xa, xb)
	x2 = max (xa, xb)
	y1 = min (ya, yb)
	y2 = max (ya, yb)
	if (x2 - x1 + 1 <= 10) {
	    x1 = max (xo, x1 - 1)
	    x2 = min (xo + nx - 1, x2 + 1)
	}
	y1 = max (yo, y1 - 1)
	y2 = min (yo + ny - 1, y2 + 1)

	call printf ("%4w") 
	do x = x1, x2 {
	    call printf (" %4d ")
		call pargi (x)
	}
	call printf ("\n")

	do y = y2, y1, -1 {
	    call printf ("%4d")
		call pargi (y)
	    j = y - yo + 1
	    do x = x1, x2 {
		i = x - xo + 1
		call printf (" %5g")
		    call pargr (data[i,j])
	    }
	    call printf ("\n")
	}
end
