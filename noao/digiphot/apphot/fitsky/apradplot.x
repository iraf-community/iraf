include "../lib/fitsky.h"

# AP_RADPLOT -- Procedure to compute the mode, sigma, and skew of the sky by
# eye using a radial profile plot of the sky pixels and cursor readback.

int procedure ap_radplot (gd, gt, skypix, coords, index, nskypix, sxc, syc,
	snx, sny, scale, sky_mode, sky_skew, sky_sigma, nsky,
	nsky_reject)

pointer	gd			# pointer to graphics stream
pointer	gt			# pointer to gtools structure
real	skypix[ARB]		# array of sky pixels
int	coords[ARB]		# array of sky coordinates
int	index[ARB]		# the index array
int	nskypix			# number of sky pixels
real	sxc, syc		# sky subraster center
int	snx, sny		# sky subraster size
real	scale			# the image scale
real	sky_mode		# computed sky value
real	sky_sigma		# computed sigma of sky pixels
real	sky_skew		# computed skew of sky pixels
int	nsky			# number of sky pixels used in fit
int	nsky_reject		# number of rejected sky pixels

double	sumpx, sumsqpx, sumcbpx
int	wcs, key
pointer	sp, r, cmd
real	wx, wy, xmin, xmax, ymin, ymax, u1, u2, v1, v2, x1, x2, y1, y2
real	sky_zero
int	clgcur()
real	ap_asumr()

begin
	if (gd == NULL)
	    return (AP_NOGRAPHICS)

	# Initialize.
	nsky = nskypix
	nsky_reject = 0
	sky_mode = INDEFR
	sky_sigma = INDEFR
	sky_skew = INDEFR
	if (nskypix <= 0)
	    return (AP_SKY_OUTOFBOUNDS)

	call smark (sp)
	call salloc (r, nskypix, TY_REAL)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Compute an initial guess at the data characteristics.
	nsky = nskypix
	sky_zero = ap_asumr (skypix, index, nskypix) / nskypix
	call apfimoments (skypix, index, nskypix, sky_zero, sumpx, sumsqpx,
	    sumcbpx, sky_mode, sky_sigma, sky_skew)

	# Store the old window and viewport coordinates.
	call ggview (gd, u1, u2, v1, v2)
	call ggwind (gd, x1, x2, y1, y2)

	# Compute the radial profile
	call ap_xytor (coords, index, Memr[r], nskypix, sxc, syc, snx)
	call alimr (Memr[r], nskypix, xmin, xmax)
	call alimr (skypix, nskypix, ymin, ymax)

	# Plot the radial profile.
	call gclear (gd)
	call ap_rset (gd, gt, xmin, xmax, ymin, ymax, scale)
	call ap_plotrad (gd, gt, Memr[r], skypix, nskypix, "plus")

	# Mark the sky level with the cursor.
	call printf ("Mark sky level (%g) [space=mark,q=quit,:.help=help]:")
	    call pargr (sky_mode)
	call gscur (gd, (xmin + xmax) / 2.0, sky_mode)
	while (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) !=
	    EOF) {
	    if (key == 'q')
		break
	    else
		sky_mode = wy
	    call printf ("Mark sky level (%g) [space=mark,q=quit,:.help=help]:")
	        call pargr (sky_mode)
	    call gscur (gd, (xmin + xmax) / 2.0, sky_mode)
	}

	# Store the old window and viewport coordinates.
	call gsview (gd, u1, u2, v1, v2)
	call gswind (gd, x1, x2, y1, y2)

	call sfree (sp)
	return (AP_OK)
end
