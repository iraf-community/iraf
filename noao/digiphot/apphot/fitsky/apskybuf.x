include <imhdr.h>
include <math.h>
include <mach.h>
include "../lib/apphotdef.h"
include "../lib/fitskydef.h"
include "../lib/fitsky.h"

# APSKYBUF -- Procedure to fetch the sky pixels given the pointer to the
# IRAF image, the coordinates of the center and the size of the apphot
# sky annulus.

int procedure apskybuf (ap, im, wx, wy)

pointer	ap		# pointer to apphot structure
pointer	im		# pointer to the IRAF image
real	wx, wy		# center coordinates

int	lenbuf
pointer	sky
real	annulus, dannulus, datamin, datamax
int	ap_skypix(), ap_bskypix()

begin
	# Check for 0 radius annulus.
	sky = AP_PSKY(ap)
	annulus = AP_ANNULUS(sky) * AP_SCALE(ap)
	dannulus = AP_DANNULUS(sky) * AP_SCALE(ap)
	if (dannulus <= 0.0)
	    return (AP_NOSKYAREA)

	# Allocate space for sky pixels.
	lenbuf = PI * (2.0 * annulus + dannulus + 1.0) * (dannulus + 0.5)

	if (lenbuf != AP_LENSKYBUF(sky)) {
	    if (AP_SKYPIX(sky) != NULL)
		call mfree (AP_SKYPIX(sky), TY_REAL)
	    call malloc (AP_SKYPIX(sky), lenbuf, TY_REAL)
	    if (AP_COORDS(sky) != NULL)
		call mfree (AP_COORDS(sky), TY_INT)
	    call malloc (AP_COORDS(sky), lenbuf, TY_INT)
	    if (AP_INDEX(sky) != NULL)
		call mfree (AP_INDEX(sky), TY_INT)
	    call malloc (AP_INDEX(sky), lenbuf, TY_INT)
	    if (AP_SWGT(sky) != NULL)
		call mfree (AP_SWGT(sky), TY_REAL)
	    call malloc (AP_SWGT(sky), lenbuf, TY_REAL)
	    AP_LENSKYBUF(sky) = lenbuf
	}

	# Fetch the sky pixels.
	if (IS_INDEFR(AP_DATAMIN(ap)) && IS_INDEFR(AP_DATAMAX(ap))) {
	    AP_NSKYPIX(sky) = ap_skypix (im, wx, wy, annulus, (annulus +
	        dannulus), Memr[AP_SKYPIX(sky)], Memi[AP_COORDS(sky)],
		AP_SXC(sky), AP_SYC(sky), AP_SNX(sky), AP_SNY(sky))
	    AP_NBADSKYPIX(sky) = 0
	} else {
	    if (IS_INDEFR(AP_DATAMIN(ap)))
		datamin = -MAX_REAL
	    else
		datamin = AP_DATAMIN(ap)
	    if (IS_INDEFR(AP_DATAMAX(ap)))
		datamax = MAX_REAL
	    else
		datamax = AP_DATAMAX(ap)
	    AP_NSKYPIX(sky) = ap_bskypix (im, wx, wy, annulus, (annulus +
	        dannulus), datamin, datamax, Memr[AP_SKYPIX(sky)],
		Memi[AP_COORDS(sky)], AP_SXC(sky), AP_SYC(sky), AP_SNX(sky),
		AP_SNY(sky), AP_NBADSKYPIX(sky))
	}

	if (AP_NSKYPIX(sky) <= 0) {
	    if (AP_NBADSKYPIX(sky) <= 0)
	        return (AP_SKY_OUTOFBOUNDS)
	    else
	        return (AP_NSKY_TOO_SMALL)
	} else
	    return (AP_OK)
end


# AP_SKYPIX -- Procedure to fetch the sky pixels from the image

int procedure ap_skypix (im, wx, wy, rin, rout, skypix, coords, xc, yc,
        nx, ny)

pointer	im		# pointer to IRAF image
real	wx, wy		# center of sky annulus
real	rin, rout	# inner and outer radius of sky annulus
real	skypix[ARB]	# skypixels
int	coords[ARB]	# sky subraster coordinates [i + nx * (j - 1)]
real	xc, yc		# center of sky subraster
int	nx, ny		# max dimensions of sky subraster (output)

int	i, j, ncols, nlines, c1, c2, l1, l2, nskypix
pointer	buf
real	xc1, xc2, xl1, xl2, rin2, rout2, rj2, r2
pointer	imgs2r()

#pointer	tbuf

begin
	if (rout <= rin)
	    return (0)

	# Test for out of bounds sky regions.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xc1 = wx - rout
	xc2 = wx + rout
	xl1 = wy - rout
	xl2 = wy + rout
	if (xc2 < 1.0 || xc1 > real (ncols) || xl2 < 1.0 || xl1 > real (nlines))
	    return (0)

	# Compute the column and line limits.
	c1 = max (1.0, min (real (ncols), wx - rout)) + 0.5
	c2 = min (real (ncols), max (1.0, wx + rout)) + 0.5
	l1 = max (1.0, min (real (nlines), wy - rout)) + 0.5
	l2 = min (real (nlines), max (1.0, wy + rout)) + 0.5
	nx = c2 - c1 + 1
	ny = l2 - l1 + 1
	xc = wx - c1 + 1
	yc = wy - l1 + 1

	# Fetch the sky pixels.
	rin2 = rin ** 2
	rout2 = rout ** 2
	nskypix = 0

	do j = l1, l2 {
	    buf = imgs2r (im, c1, c2, j, j)
	    rj2 = (wy - j) ** 2
	    do i = c1, c2 {
	        r2 = (wx - i) ** 2 + rj2
		if (r2 > rin2 && r2 <= rout2) {
		    skypix[nskypix+1] = Memr[buf+i-c1]
		    coords[nskypix+1] = (i - c1 + 1) + nx * (j - l1)
		    nskypix = nskypix + 1
		}
	    }
	}

	#buf = imgs2r (im, c1, c2, l1, l2)
	#tbuf = buf
	#do j = l1,  l2 {
	    #rj2 = (wy - j) ** 2
	    #do i = c1, c2 {
	        #r2 = (wx - i) ** 2 + rj2
		#if (r2 > rin2 && r2 <= rout2) {
		    #skypix[nskypix+1] = Memr[tbuf+i-c1]
		    #coords[nskypix+1] = (i - c1 + 1) + nx * (j - l1)
		    #nskypix = nskypix + 1
		#}
	    #}
	    #tbuf = tbuf + nx
	#}

	return (nskypix)
end


# AP_BSKYPIX -- Procedure to fetch the sky pixels from the image

int procedure ap_bskypix (im, wx, wy, rin, rout, datamin, datamax,
	skypix, coords, xc, yc, nx, ny, nbad)

pointer	im		# pointer to IRAF image
real	wx, wy		# center of sky annulus
real	rin, rout	# inner and outer radius of sky annulus
real	datamin		# minimum good value
real	datamax		# maximum good value
real	skypix[ARB]	# skypixels
int	coords[ARB]	# sky subraster coordinates [i + nx * (j - 1)]
real	xc, yc		# center of sky subraster
int	nx, ny		# max dimensions of sky subraster (output)
int	nbad		# number of bad pixels

int	i, j, ncols, nlines, c1, c2, l1, l2, nskypix
pointer	buf
real	xc1, xc2, xl1, xl2, rin2, rout2, rj2, r2, pixval
pointer	imgs2r()

#pointer	tbuf

begin
	if (rout <= rin)
	    return (0)

	# Test for out of bounds sky regions.
	ncols = IM_LEN(im,1)
	nlines = IM_LEN(im,2)
	xc1 = wx - rout
	xc2 = wx + rout
	xl1 = wy - rout
	xl2 = wy + rout
	if (xc2 < 1.0 || xc1 > real (ncols) || xl2 < 1.0 || xl1 > real (nlines))
	    return (0)

	# Compute the column and line limits.
	c1 = max (1.0, min (real (ncols), wx - rout)) + 0.5
	c2 = min (real (ncols), max (1.0, wx + rout)) + 0.5
	l1 = max (1.0, min (real (nlines), wy - rout)) + 0.5
	l2 = min (real (nlines), max (1.0, wy + rout)) + 0.5
	nx = c2 - c1 + 1
	ny = l2 - l1 + 1
	xc = wx - c1 + 1
	yc = wy - l1 + 1

	rin2 = rin ** 2
	rout2 = rout ** 2
	nskypix = 0
	nbad = 0

	# Fetch the sky pixels.
	do j = l1, l2 {
	    buf = imgs2r (im, c1, c2, j, j)
	    rj2 = (wy - j) ** 2
	    do i = c1, c2 {
	        r2 = (wx - i) ** 2 + rj2
		if (r2 > rin2 && r2 <= rout2) {
		    pixval = Memr[buf+i-c1] 
		    if (pixval < datamin || pixval > datamax)
		        nbad = nbad + 1
		    else {
		        skypix[nskypix+1] = pixval
		        coords[nskypix+1] = (i - c1 + 1) + nx * (j - l1)
		        nskypix = nskypix + 1
		    }
		}
	    }
	}

	#buf = imgs2r (im, c1, c2, l1, l2)
	#tbuf = buf
	#do j = l1, l2 {
	    #rj2 = (wy - j) ** 2
	    #do i = c1, c2 {
	        #r2 = (wx - i) ** 2 + rj2
		#if (r2 > rin2 && r2 <= rout2) {
		    #pixval = Memr[tbuf+i-c1] 
		    #if (pixval < datamin || pixval > datamax)
		        #nbad = nbad + 1
		    #else {
		        #skypix[nskypix+1] = pixval
		        #coords[nskypix+1] = (i - c1 + 1) + nx * (j - l1)
		        #nskypix = nskypix + 1
		    #}
		#}
	    #}
	    #tbuf = tbuf + nx
	#}

	return (nskypix)
end
