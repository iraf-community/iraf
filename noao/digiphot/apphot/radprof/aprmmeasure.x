include <math/curfit.h>

# AP_RMMEASURE -- Procedure to compute the sums and areas inside the apertures.

procedure ap_rmmeasure (pixels, nx, ny, wx, wy, aperts, sums, areas, naperts)

real	pixels[nx,ARB]		# subraster pixel values
int	nx, ny			# dimensions of the subraster
real	wx, wy			# center of subraster
real	aperts[ARB]		# array of apertures
double	sums[ARB]		# array of sums
double	areas[ARB]		# aperture areas
int	naperts			# number of apertures

int	i, j, k
double	fctn
real	apmaxsq, dy2, r2, r

begin
	# Initialize.
	apmaxsq = (aperts[naperts] + 0.5) ** 2
	call aclrd (sums, naperts)
	call aclrd (areas, naperts)

	# Loop over the pixels.
	do j = 1, ny {
	    dy2 = (j - wy) ** 2
	    do i = 1, nx {
		r2 = (i - wx) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		r = sqrt (r2) - 0.5
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    sums[k] = sums[k] + fctn * pixels[i,j]
		    areas[k] = areas[k] + fctn
		}
	    }
	}
end


# AP_BRMMEASURE -- Procedure to compute the sums and areas inside the apertures.

procedure ap_brmmeasure (pixels, nx, ny, wx, wy, datamin, datamax, aperts,
	sums, areas, naperts, minapert)

real	pixels[nx,ARB]		# subraster pixel values
int	nx, ny			# dimensions of the subraster
real	wx, wy			# center of subraster
real	datamin			# minimum good data value
real	datamax			# maximum good data value
real	aperts[ARB]		# array of apertures
double	sums[ARB]		# array of sums
double	areas[ARB]		# aperture areas
int	naperts			# number of apertures
int	minapert		# minimum number of apertures

int	i, j, k, kindex
double	fctn
real	apmaxsq, dy2, r2, r, pixval

begin
	# Initialize.
	apmaxsq = (aperts[naperts] + 0.5) ** 2
	call aclrd (sums, naperts)
	call aclrd (areas, naperts)
	minapert = naperts + 1

	# Loop over the pixels.
	do j = 1, ny {
	    dy2 = (j - wy) ** 2
	    do i = 1, nx {
		r2 = (i - wx) ** 2 + dy2
		if (r2 > apmaxsq)
		    next
		r = sqrt (r2) - 0.5
		kindex = naperts + 1
		pixval = pixels[i,j]
		do k = 1, naperts {
		    if (r > aperts[k])
			next
		    kindex = min (k, kindex)
		    fctn = max (0.0, min (1.0, aperts[k] - r))
		    sums[k] = sums[k] + fctn * pixval
		    areas[k] = areas[k] + fctn
		}
		if (kindex < minapert) {
		    if (pixval < datamin || pixval > datamax)
			minapert = kindex
		}
	    }
	}
end
