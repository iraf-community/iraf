# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include <ctype.h>
include "imwcs.h"
include	"mwcs.h"

.help WFTPV
.nf -------------------------------------------------------------------------
# WFTPV -- WCS function driver for the TPV polynomial projection.

Driver routines:

	FN_INIT		 wf_tpv_init (fc, dir)
	FN_DESTROY	 wf_tpv_destroy (fc)
	FN_FWD		 wf_tpv_fwd (fc, v1, v2)
	FN_INV		 wf_tpv_inv (fc, v1, v2)

.endhelp --------------------------------------------------------------------

define  MAX_NITER       20

# Driver specific fields of function call (FC) descriptor.
define	FC_IRA		Memi[$1+FCU]		# RA axis (1 or 2)
define	FC_IDEC		Memi[$1+FCU+1]		# DEC axis (1 or 2)
define	FC_COSDEC	Memd[P2D($1+FCU+2)]	# cosine(dec)
define	FC_SINDEC	Memd[P2D($1+FCU+4)]	# sine(dec)
define	FC_W		Memd[P2D($1+FCU+6)+($2)-1] # W (CRVAL) for each axis
define	FC_NPRA		Memi[$1+FCU+10]		# poly order (0-39)
define	FC_NPDEC	Memi[$1+FCU+11]		# poly order (0-39)
define	FC_PV		Memi[$1+FCU+12]		# pointer to PV data (double)

define	FC_A		Memd[FC_PV($1)+($2)]	# RA coefficient
define	FC_B		Memd[FC_PV($1)+40+($2)]	# DEC coefficient

# WF_TPV_INIT -- Initialize the tan polynomial forward or inverse
# transform. Initialization for this transformation consists of, determining
# which axis is RA / LON and which is DEC / LAT, computing the celestial
# longitude and colatitude of the native pole, reading in the the native
# longitude of the pole of the celestial coordinate system LONGPOLE from the
# attribute list, precomputing the Euler angles and various intermediary
# functions of the reference coordinates, reading in the projection parameter
# RO from the attribute list, reading in up to ten polynomial coefficients,
# and, for polynomial orders greater than 2 computing the colatitude and radius
# of the  first point of inflection. If LONGPOLE is undefined then a value of
# 180.0 degrees is assumed. If RO is undefined a value of 180.0 / PI is
# assumed. If the polynomial coefficients are all zero then an error condition
# is posted. If the order of the polynomial is 2 or greater and there is no
# point of inflection an error condition is posted. The TPV projection with
# an order of 1 and 0th and 1st coefficients of 0.0 and 1.0 respectively is
# equivalent to the ARC projtection. In order to determine the axis order,
# the parameter "axtype={ra|dec} {xlon|xlat}" must have been set in the
# attribute list for the function. The LONGPOLE and RO parameters may be set
# in either or both of the axes attribute lists, but the value in the RA axis
# attribute list takes precedence. 

procedure wf_tpv_init (fc, dir)

pointer	fc			#I pointer to FC descriptor
int	dir			#I direction of transform

int	i, ualen, index, ip
double	dec, dval
pointer	ct, mw, wp, wv, im, idb, rp

int	idb_nextcard(), itoc(), ctod()
pointer	idb_open()
errchk	wf_decaxis()

begin
	# Allocate PV storage.  This is freed in wf_tpv_destroy.
	call calloc (FC_PV(fc), 80, TY_DOUBLE)

	# Set non-zero defaults.
	FC_NPRA(fc) = 1
	FC_NPDEC(fc) = 1
	FC_A(fc,1) = 1D0
	FC_B(fc,1) = 1D0

	# Get the required mwcs pointers.

	# Determine which is the DEC axis, and hence the axis order.
	call wf_decaxis (fc, FC_IRA(fc), FC_IDEC(fc))

	# Get the value of W for each axis, i.e. the world coordinates at
	# the reference point.

	ct = FC_CT(fc)
	mw = CT_MW(ct)
	wp = FC_WCS(fc)
	wv = MI_DBUF(mw) + WCS_W(wp) - 1
	do i = 1, 2
	    FC_W(fc,i) = Memd[wv+CT_AXIS(ct,FC_AXIS(fc,i))-1]

        # Precompute the sin and cos of the declination at the reference pixel.
	dec = DEGTORAD(FC_W(fc,FC_IDEC(fc)))
	FC_COSDEC(fc) = cos(dec)
	FC_SINDEC(fc) = sin(dec)

        # Read through the fits header once more and pick up the PV cards.
        # Read the values and store them, keeping track of what is
        # the highest order coefficient.

	im = MI_REFIM(mw)
	idb = idb_open(im,ualen)
	while (idb_nextcard(idb,rp) != EOF) {
	    if (Memc[rp] != 'P' || Memc[rp+1] != 'V' || Memc[rp+3] != '_')
	        next
	    if (Memc[rp+2] != '1' && Memc[rp+2] != '2')
	        next
            if (! IS_DIGIT(Memc[rp+4]))
                next
            index = TO_INTEG(Memc[rp+4])
            do i = 5,7 {
                if (! IS_DIGIT(Memc[rp+i]))
		    break
		else
		    index = 10*index + TO_INTEG(Memc[rp+i])
            }
	    if (index > 39)
	        next
            ip = IDB_STARTVALUE
            if (ctod(Memc[rp],ip,dval) <= 0) 
	        dval = 0.0d0
	    i = TO_INTEG(Memc[rp+2])
	    if (i == FC_IRA(fc)) {
		FC_A(fc,index) = dval
		if (index > FC_NPRA(fc))
		    FC_NPRA(fc) = double(index)
	    } else {
		FC_B(fc,index) = dval
		if (index > FC_NPDEC(fc))
		    FC_NPDEC(fc) = double(index)
	    }
        }
	call idb_close(idb)

end


# WF_TPV_FWD -- Forward transform (physical to world) for the tangent plane
# with polynomial distortion.

procedure wf_tpv_fwd (fc, p, w)

pointer	fc			#I pointer to FC descriptor
double	p[2]			#I physical coordinates (x, y)
double	w[2]			#O world coordinates (ra, dec)

double  x, y, z, a, b, ra, dec

begin
	# Compute the standard coordinates.

	x = p[1]
	y = p[2]
	call tpv_poly (fc, x, y, a, b)

        # Rotate the rectangular coordinate system of the vector [1,xi,eta]
        # by the declination so that the X axis will pass through the equator.

        a = DEGTORAD(a)
        b = DEGTORAD(b)

        x = FC_COSDEC(fc) - b * FC_SINDEC(fc)
        y = a
        z = FC_SINDEC(fc) + b * FC_COSDEC(fc)

        # Compute RA and DEC in radians.
        if (x == 0.0D0 && y == 0.0D0)
            ra = 0.0D0
        else
            ra = atan2 (y, x)
        dec = atan2 (z, sqrt (x*x + y*y))

        # Return RA and DEC in degrees.
        dec = RADTODEG(dec)
        ra  = RADTODEG(ra) + FC_W(fc,FC_IRA(fc))

        if (ra < 0D0)
            ra = ra + 360D0
        else if (ra > 360D0)
            ra = ra - 360D0

        w[FC_IRA(fc)]  = ra
        w[FC_IDEC(fc)] = dec

end


# WF_TPV_INV -- Inverse transform (world to physical) for the tangent plane
# projection with polynomials.

procedure wf_tpv_inv (fc, w, p)

pointer	fc			#I pointer to FC descriptor
double	w[2]			#I input world (RA, DEC) coordinates
double	p[2]			#I output physical coordinates

int	ira, idec, niter
double	ra, dec
double	cosra, cosdec, sinra, sindec, cosdist
double	a, b, x, y, f, g, fx, gx, fy, gy, denom, dx, dy, dmax

begin
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	ra  = DEGTORAD (w[ira] - FC_W(fc,ira))
	dec = DEGTORAD (w[idec])

	cosra = cos (ra)
	sinra = sin (ra)
	cosdec = cos (dec)
	sindec = sin (dec)
	cosdist = sindec * FC_SINDEC(fc) + cosdec * FC_COSDEC(fc) * cosra

	a = RADTODEG(cosdec * sinra / cosdist)
	b = RADTODEG((sindec * FC_COSDEC(fc) - cosdec * FC_SINDEC(fc) * cosra) /
	    cosdist)

	x = a
	y = b
	dmax = 30. / 3600.
	niter = 0

	repeat {
	    call tpv_poly (fc, x, y, f, g)
	    call tpv_der (fc, x, y, fx, gx, fy, gy)
	    
	    f = f - a
	    g = g - b

	    denom = fx * gy - fy * gx
	    if (denom == 0.0d0)
		break
		
	    dx = (-f * gy + g * fy) / denom
	    dy = (-g * fx + f * gx) / denom
	    x = x + max (-dmax, min (dmax, dx))
	    y = y + max (-dmax, min (dmax, dy))
	    
	    if (max (abs (dx), abs (dy), abs(f), abs(g)) < 2.0d-7)
		break

	    niter = niter + 1

	} until (niter >= MAX_NITER)

	p[ira]  = x
	p[idec] = y

end


# WF_TPV_DESTROY -- Free up the distortion surface pointers.

procedure wf_tpv_destroy (fc)

pointer fc                      #I pointer to the FC descriptor

begin
	call mfree (FC_PV(fc), TY_DOUBLE)
end


# TPV_POLY -- Evaluate TPV polynomial (x,y -> xi,eta)

procedure tpv_poly (fc, x, y, a, b)

pointer	fc			#I pointer to FC descriptor
double	x, y			#I physical coordinates
double	a, b			#O standard coordinates (xi, eta) in deg

int	n
double	r, r2, r3, r5, r7, x2, x3, x4, x5, x6, x7, y2, y3, y4, y5, y6, y7

begin
	# Compute the standard coordinates.
	# This depends on undefined coefficients being zero.

	x2 = x * x
	y2 = y * y
	r2 = x2 + y2
	r = sqrt (r2)
	n = max (FC_NPRA(fc), FC_NPDEC(fc))
	
	a = FC_A(fc,0) + FC_A(fc,1) * x + FC_A(fc,2) * y + FC_A(fc,3) * r
	b = FC_B(fc,0) + FC_B(fc,1) * y + FC_B(fc,2) * x + FC_B(fc,3) * r
	if (n <= 3)
	    return
	a = a + FC_A(fc,4) * x2 + FC_A(fc,5) * x*y + FC_A(fc,6) * y2
	b = b + FC_B(fc,4) * y2 + FC_B(fc,5) * x*y + FC_B(fc,6) * x2
	if (n <= 6)
	    return
	x3 = x * x2
	y3 = y * y2
	r3 = r * r2
	a = a + FC_A(fc,7) * x3
	b = b + FC_B(fc,7) * y3
	a = a + FC_A(fc,8) * x2*y
	b = b + FC_B(fc,8) * y2*x
	a = a + FC_A(fc,9) * x*y2
	b = b + FC_B(fc,9) * y*x2
	a = a + FC_A(fc,10) * y3
	b = b + FC_B(fc,10) * x3
	a = a + FC_A(fc,11) * r3
	b = b + FC_B(fc,11) * r3
	if (n <= 11)
	    return
	x4 = x * x3
	y4 = y * y3
	a = a + FC_A(fc,12) * x4
	b = b + FC_B(fc,12) * y4
	a = a + FC_A(fc,13) * x3*y
	b = b + FC_B(fc,13) * y3*x
	a = a + FC_A(fc,14) * x2*y2
	b = b + FC_B(fc,14) * y2*x2
	a = a + FC_A(fc,15) * x*y3
	b = b + FC_B(fc,15) * y*x3
	a = a + FC_A(fc,16) * y4
	b = b + FC_B(fc,16) * x4
	if (n <= 16)
	    return
	x5 = x * x4
	y5 = y * y4
	r5 = r3 * r2
	a = a + FC_A(fc,17) * x5
	b = b + FC_B(fc,17) * y5
	a = a + FC_A(fc,18) * x4*y
	b = b + FC_B(fc,18) * y4*x
	a = a + FC_A(fc,19) * x3*y2
	b = b + FC_B(fc,19) * y3*x2
	a = a + FC_A(fc,20) * x2*y3
	b = b + FC_B(fc,20) * y2*x3
	a = a + FC_A(fc,21) * x*y4
	b = b + FC_B(fc,21) * y*x4
	a = a + FC_A(fc,22) * y5
	b = b + FC_B(fc,22) * x5
	a = a + FC_A(fc,23) * r5
	b = b + FC_B(fc,23) * r5
	if (n <= 23)
	    return
	x6 = x * x5
	y6 = y * y5
	a = a + FC_A(fc,14) * x6
	b = b + FC_B(fc,24) * y6
	a = a + FC_A(fc,25) * x5*y
	b = b + FC_B(fc,25) * y5*x
	a = a + FC_A(fc,26) * x4*y2
	b = b + FC_B(fc,26) * y4*x2
	a = a + FC_A(fc,27) * x3*y3
	b = b + FC_B(fc,27) * y3*x3
	a = a + FC_A(fc,28) * x2*y4
	b = b + FC_B(fc,28) * y2*x4
	a = a + FC_A(fc,29) * x*y5
	b = b + FC_B(fc,29) * y*x5
	a = a + FC_A(fc,30) * y6
	b = b + FC_B(fc,30) * x6
	if (n <= 30)
	    return
	x7 = x * x6
	y7 = y * y6
	r7 = r5 * r2
	a = a + FC_A(fc,31) * x7
	b = b + FC_B(fc,31) * y7
	a = a + FC_A(fc,32) * x6*y
	b = b + FC_B(fc,32) * y6*x
	a = a + FC_A(fc,33) * x5*y2
	b = b + FC_B(fc,33) * y5*x2
	a = a + FC_A(fc,34) * x4*y3
	b = b + FC_B(fc,34) * y4*x3
	a = a + FC_A(fc,35) * x3*y4
	b = b + FC_B(fc,35) * y3*x4
	a = a + FC_A(fc,36) * x2*y5
	b = b + FC_B(fc,36) * y2*x5
	a = a + FC_A(fc,37) * x*y6
	b = b + FC_B(fc,37) * y*x6
	a = a + FC_A(fc,38) * y7
	b = b + FC_B(fc,38) * x7
	a = a + FC_A(fc,39) * r7
	b = b + FC_B(fc,39) * r7

end


# TPV_DER -- Evaluate TPV polynomial (x,y -> xi,eta)

procedure tpv_der (fc, x, y, ax, bx, ay, by)

pointer	fc			#I pointer to FC descriptor
double	x, y			#I physical coordinates
double	ax, bx			#O standard coordinates (xi, eta) in deg
double	ay, by			#O standard coordinates (xi, eta) in deg

int	n
double	r, r2, r4, r6, x2, x3, x4, x5, x6, y2, y3, y4, y5, y6, rx, ry

begin
	x2 = x * x
	y2 = y * y
	r2 = x2 + y2
	r = sqrt (r2)
	if (r < 2.0d-7) {
	    rx = 1D0
	    ry = 1D0
	} else {
	    rx = x / r
	    ry = y / r
	}
	n = max (FC_NPRA(fc), FC_NPDEC(fc))
	
	ax = FC_A(fc,1) + FC_A(fc,3) * rx
	by = FC_B(fc,1) + FC_B(fc,3) * ry
	ay = FC_A(fc,2) + FC_A(fc,3) * ry
	bx = FC_B(fc,2) + FC_B(fc,3) * rx
	if (n <= 3)
	    return
	ax = ax + 2 * FC_A(fc,4) * x + FC_A(fc,5) * y
	by = by + 2 * FC_B(fc,4) * y + FC_B(fc,5) * x
	ay = ay + FC_A(fc,5) * x + 2 * FC_A(fc,6) * y
	bx = bx + FC_B(fc,5) * y + 2 * FC_B(fc,6) * x
	if (n <= 6)
	    return
	ax = ax + 3 * FC_A(fc,7) * x2
	by = by + 3 * FC_B(fc,7) * y2
	ax = ax + 2 * FC_A(fc,8) * x*y
	by = by + 2 * FC_B(fc,8) * y*x
	ax = ax + FC_A(fc,9) * y2
	by = by + FC_B(fc,9) * x2
	ax = ax + 3 * FC_A(fc,11) * r2 * rx
	by = by + 3 * FC_B(fc,11) * r2 * ry
	ay = ay + FC_A(fc,8) * x2
	bx = bx + FC_B(fc,8) * y2
	ay = ay + 2 * FC_A(fc,9) * x*y
	bx = bx + 2 * FC_B(fc,9) * y*x
	ay = ay + 3 * FC_A(fc,10) * y2
	bx = bx + 3 * FC_B(fc,10) * x2
	ay = ay + 3 * FC_A(fc,11) * r2 * ry
	bx = bx + 3 * FC_B(fc,11) * r2 * rx
	if (n <= 11)
	    return
	x3 = x * x2
	y3 = y * y2
	ax = ax + 4 * FC_A(fc,12) * x3
	by = by + 4 * FC_B(fc,12) * y3
	ax = ax + 3 * FC_A(fc,13) * x2*y
	by = by + 3 * FC_B(fc,13) * y2*x
	ax = ax + 2 * FC_A(fc,14) * x*y2
	by = by + 2 * FC_B(fc,14) * y*x2
	ax = ax + FC_A(fc,15) * y3
	by = by + FC_B(fc,15) * x3
	ay = ay + FC_A(fc,13) * x3
	bx = bx + FC_B(fc,13) * y3
	ay = ay + 2 * FC_A(fc,14) * x2*y
	bx = bx + 2 * FC_B(fc,14) * y2*x
	ay = ay + 3 * FC_A(fc,15) * x*y2
	bx = bx + 3 * FC_B(fc,15) * y*x2
	ay = ay + 4 * FC_A(fc,16) * y3
	bx = bx + 4 * FC_B(fc,16) * x3
	if (n <= 16)
	    return
	x4 = x * x3
	y4 = y * y3
	r4 = r2 * r2
	ax = ax + 5 * FC_A(fc,17) * x4
	by = by + 5 * FC_B(fc,17) * y4
	ax = ax + 4 * FC_A(fc,18) * x3*y
	by = by + 4 * FC_B(fc,18) * y3*x
	ax = ax + 3 * FC_A(fc,19) * x2*y2
	by = by + 3 * FC_B(fc,19) * y2*x2
	ax = ax + 2 * FC_A(fc,20) * x*y3
	by = by + 2 * FC_B(fc,20) * y*x3
	ax = ax + FC_A(fc,21) * y4
	by = by + FC_B(fc,21) * x4
	ax = ax + 5 * FC_A(fc,23) * r4 * rx
	by = by + 5 * FC_B(fc,23) * r4 * ry
	ay = ay + FC_A(fc,18) * x4
	bx = bx + FC_B(fc,18) * y4
	ay = ay + 2 * FC_A(fc,19) * x3*y
	bx = bx + 2 * FC_B(fc,19) * y3*x
	ay = ay + 3 * FC_A(fc,20) * x2*y2
	bx = bx + 3 * FC_B(fc,20) * y2*x2
	ay = ay + 4 * FC_A(fc,21) * x*y3
	bx = bx + 4 * FC_B(fc,21) * y*x3
	ay = ay + 5 * FC_A(fc,22) * y4
	bx = bx + 5 * FC_B(fc,22) * x4
	ay = ay + 5 * FC_A(fc,23) * r4 * ry
	bx = bx + 5 * FC_B(fc,23) * r4 * rx
	if (n <= 23)
	    return
	x5 = x * x4
	y5 = y * y4
	ax = ax + 6 * FC_A(fc,14) * x5
	by = by + 6 * FC_B(fc,24) * y5
	ax = ax + 5 * FC_A(fc,25) * x4*y
	by = by + 5 * FC_B(fc,25) * y4*x
	ax = ax + 4 * FC_A(fc,26) * x3*y2
	by = by + 4 * FC_B(fc,26) * y3*x2
	ax = ax + 3 * FC_A(fc,27) * x2*y3
	by = by + 3 * FC_B(fc,27) * y2*x3
	ax = ax + 2 * FC_A(fc,28) * x*y4
	by = by + 2 * FC_B(fc,28) * y*x4
	ax = ax + FC_A(fc,29) * y5
	by = by + FC_B(fc,29) * x5
	ay = ay + FC_A(fc,25) * x5
	bx = bx + FC_B(fc,25) * y5
	ay = ay + 2 * FC_A(fc,26) * x4*y
	bx = bx + 2 * FC_B(fc,26) * y4*x
	ay = ay + 3 * FC_A(fc,27) * x3*y2
	bx = bx + 3 * FC_B(fc,27) * y3*x2
	ay = ay + 4 * FC_A(fc,28) * x2*y3
	bx = bx + 4 * FC_B(fc,28) * y2*x3
	ay = ay + 5 * FC_A(fc,29) * x*y4
	bx = bx + 5 * FC_B(fc,29) * y*x4
	ay = ay + 6 * FC_A(fc,30) * y5
	bx = bx + 6 * FC_B(fc,30) * x5
	if (n <= 30)
	    return
	x6 = x * x5
	y6 = y * y5
	r6 = r4 * r2
	ax = ax + 7 * FC_A(fc,31) * x6
	by = by + 7 * FC_B(fc,31) * y6
	ax = ax + 6 * FC_A(fc,32) * x5*y
	by = by + 6 * FC_B(fc,32) * y5*x
	ax = ax + 5 * FC_A(fc,33) * x4*y2
	by = by + 5 * FC_B(fc,33) * y4*x2
	ax = ax + 4 * FC_A(fc,34) * x3*y3
	by = by + 4 * FC_B(fc,34) * y3*x3
	ax = ax + 3 * FC_A(fc,35) * x2*y4
	by = by + 3 * FC_B(fc,35) * y2*x4
	ax = ax + 2 * FC_A(fc,36) * x*y5
	by = by + 2 * FC_B(fc,36) * y*x5
	ax = ax + FC_A(fc,37) * y6
	by = by + FC_B(fc,37) * x6
	ax = ax + 7 * FC_A(fc,39) * r6 * rx
	by = by + 7 * FC_B(fc,39) * r6 * ry
	ay = ay + FC_A(fc,32) * x6
	bx = bx + FC_B(fc,32) * y6
	ay = ay + 2 * FC_A(fc,33) * x5*y
	bx = bx + 2 * FC_B(fc,33) * y5*x
	ay = ay + 3 * FC_A(fc,34) * x4*y2
	bx = bx + 3 * FC_B(fc,34) * y4*x2
	ay = ay + 4 * FC_A(fc,35) * x3*y3
	bx = bx + 4 * FC_B(fc,35) * y3*x3
	ay = ay + 5 * FC_A(fc,36) * x2*y4
	bx = bx + 5 * FC_B(fc,36) * y2*x4
	ay = ay + 6 * FC_A(fc,37) * x*y5
	bx = bx + 6 * FC_B(fc,37) * y*x5
	ay = ay + 7 * FC_A(fc,38) * y6
	bx = bx + 7 * FC_B(fc,38) * x6
	ay = ay + 7 * FC_A(fc,39) * r6 * ry
	bx = bx + 7 * FC_B(fc,39) * r6 * rx

end
