# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include <ctype.h>
include "imwcs.h"
include	"mwcs.h"

.help WFZPN
.nf -------------------------------------------------------------------------
# WFZPN -- WCS function driver for the zenithal / azimuthal polynomial 
# projection.

Driver routines:

	FN_INIT		    wf_zpn_init (fc, dir)
	FN_DESTROY	 wf_zpn_destroy (fc)
	FN_FWD		     wf_zpn_fwd (fc, v1, v2)
	FN_INV		     wf_zpn_inv (fc, v1, v2)

.endhelp --------------------------------------------------------------------

define  MAX_NITER       20

# Driver specific fields of function call (FC) descriptor.
define  FC_LNGCOR       Memi[$1+FCU]                    # RA axis correction
define  FC_LATCOR       Memi[$1+FCU+1]                  # DEC axis correction
define	FC_IRA		Memi[$1+FCU+2]			# RA axis (1 or 2)
define	FC_IDEC		Memi[$1+FCU+3]			# DEC axis (1 or 2)
define	FC_NP		Memd[P2D($1+FCU+4)]		# poly order (0-9)
define	FC_LONGP	Memd[P2D($1+FCU+6)]		# LONGPOLE (rads)
define	FC_COLATP	Memd[P2D($1+FCU+8)]		# (90 - DEC) (rads)
define	FC_COSLATP	Memd[P2D($1+FCU+10)]		# cosine (90 - DEC)
define	FC_SINLATP	Memd[P2D($1+FCU+12)]		# sine (90 - DEC)
define	FC_SPHTOL	Memd[P2D($1+FCU+14)]		# trig tolerance
define	FC_PC		Memd[P2D($1+FCU+16)+($2)]	# poly coefficients (9)
define	FC_RODEG	Memd[P2D($1+FCU+36)]		# RO (degs)
define	FC_ZD		Memd[P2D($1+FCU+38)]		# colat of FIP (degs)
define	FC_R		Memd[P2D($1+FCU+40)]		# radius of FIP (degs)
define	FC_BADCVAL	Memd[P2D($1+FCU+42)]		# Bad coordinate value
define	FC_W		Memd[P2D($1+FCU+44)+($2)-1]	# CRVAL (axis 1 and 2)


# WF_ZPN_INIT -- Initialize the zenithal/azimuthal polynomial forward or inverse
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
# point of inflection an error condition is posted. The ZPN projection with
# an order of 1 and 0th and 1st coefficients of 0.0 and 1.0 respectively is
# equivalent to the ARC projtection. In order to determine the axis order,
# the parameter "axtype={ra|dec} {xlon|xlat}" must have been set in the
# attribute list for the function. The LONGPOLE and RO parameters may be set
# in either or both of the axes attribute lists, but the value in the RA axis
# attribute list takes precedence. 

procedure wf_zpn_init (fc, dir)

pointer	fc			#I pointer to FC descriptor
int	dir			#I direction of transform

int	i, j, np, szatstr, maxorder, ualen, index, ip
double	dec, zd1, d1, zd2, d2, zd, d, r, tol, dval
pointer	sp, atname, atvalue, ct, mw, wp, wv, im, idb, rp
char    compare[4]
bool    match
int	ctod(), strlen(), idb_nextcard(), itoc()
pointer	wf_gsopen(), idb_open()
data	tol/1.0d-13/
errchk	wf_decaxis(), mw_gwattrs()

begin
	# Allocate space for the attribute string.
	call smark (sp)
	call salloc (atname, SZ_ATNAME, TY_CHAR)
	call salloc (atvalue, SZ_LINE, TY_CHAR)

	# Get the required mwcs pointers.
	ct = FC_CT(fc)
	mw = CT_MW(ct)
	wp = FC_WCS(fc)
	im = MI_REFIM(mw)

	# Determine which is the DEC axis, and hence the axis order.
	call wf_decaxis (fc, FC_IRA(fc), FC_IDEC(fc))

	# Get the value of W for each axis, i.e. the world coordinates at
	# the reference point.

	wv = MI_DBUF(mw) + WCS_W(wp) - 1
	do i = 1, 2
	    FC_W(fc,i) = Memd[wv+CT_AXIS(ct,FC_AXIS(fc,i))-1]

	# Get the celestial coordinates of the native pole which are in
	# this case the ra and 90 - dec of the reference point.

	dec = DDEGTORAD(90.0d0 - FC_W(fc,FC_IDEC(fc)))

	# Determine the native longitude of the pole of the celestial
	# coordinate system corresponding to the FITS keyword LONGPOLE.
	# This number has no default and should normally be set to 180
	# degrees. Search both axes for this quantity.

	FC_LONGP(fc) = DDEGTORAD(180.0d0)

	# Precompute the trigomometric functions used by the spherical geometry
	# code to improve efficiency.

	FC_COLATP(fc) = dec
	FC_COSLATP(fc) = cos(dec)
	FC_SINLATP(fc) = sin(dec)

	# Fetch the RO projection parameter which is the radius of the
	# generating sphere for the projection. If RO is absent which
	# is the usual case set it to 180 / PI. Search both axes for
	# this quantity.

	FC_RODEG(fc) = 180.0d0/DPI
        szatstr = SZ_LINE

	# Fetch the longitude correction surface. Note that the attribute
	# string may be of any length so the length of atvalue may have
	# to be adjusted.

        FC_LNGCOR(fc) = NULL

	# Fetch the latitude correction surface. Note that the attribute
	# string may be of any length so the length of atvalue may have
	# to be adjusted.

        FC_LATCOR(fc) = NULL

        # Read through the fits header once more and pick up the PV matrix
        # cards.  Read the values and store them, keeping track of what is
        # the highest order coefficient. With this projection only the dec
	# axis coefficients matter. Technically we can have up to 99 
	# coefficients. But we restrict this to 10 for the moment.

        maxorder = -1
	idb = idb_open(im,ualen)
	compare[1] = 'P'
        compare[2] = 'V'
        i = itoc(FC_IDEC(fc),compare[3],1)
	compare[4] = '_'
	while (idb_nextcard(idb,rp) != EOF) {
            match = true
	    do i = 0,3 {
	        if (Memc[rp+i] != compare[i+1]) {
		    match = false
		    break;
                }
            }
            if (! match) 
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
	    if (index > 9)
	        next
            ip = IDB_STARTVALUE
            if (ctod(Memc[rp],ip,dval) <= 0) 
	        dval = 0.0d0
	    if (index > maxorder)
	        maxorder = index
	    FC_PC(fc,index) = dval
        }
	call idb_close(idb)

	# If all the coefficients are 0.0 the polynomial is undefined.
	if (maxorder < 0) {
	    call sfree (sp)
	    call error (0, "WFT_ZPN_INIT: The polynomial is undefined")
	}

	# Determine the number of coefficients.
	FC_NP(fc) = double(maxorder)
	np = maxorder

	if (np >= 3) {
	    # Find the point of inflection closest to the pole.
	    zd1 = 0.0d0
	    d1 = FC_PC(fc,1)
	    if (d1 <= 0.0d0) {
		call sfree (sp)
	        call error (0,
		    "WFT_ZPN_INIT: The point of inflection does not exist")
	    }

	    # Find the point where the derivative first goes negative.
	    do i = 1, 180 {
		zd2 = DPI * double (i) / 180.0d0
		d2 = 0.0d0
		do j = np, 1, -1
		    d2 = d2 * zd2 + j * FC_PC(fc,j)
		if (d2 <= 0.0d0)
		    break
		zd1 = zd2
		d1 = d2
	    }

	    # Find where the derivative is 0.
	    if (d2 <= 0.0d0) {
		do i = 1, 10 {
		    zd = zd1 - d1 * (zd2 - zd1) / (d2 - d1)
		    d = 0.0d0
		    do j = np, 1, -1
			d = d * zd + j * FC_PC(fc,j)
		    if (abs(d) < tol)
			break
		    if (d < 0.0d0) {
			zd2 = zd
			d2 = d
		    } else {
			zd1 = zd
			d1 = d
		    }
		}

	    # No negative derivative.
	    } else
		zd = DPI

	    r = 0.0d0
	    do j = np, 0, -1
		r = r * zd + FC_PC(fc,j)
	    FC_ZD(fc) = zd
	    FC_R(fc) = r
	}

	# Set the spherical trigonometric tolerance.
	FC_SPHTOL(fc) = 1.0d-5

	# Set the bad coordinate value.
	FC_BADCVAL(fc) = INDEFD

	# Free working space.
	call sfree (sp)

end


# WF_ZPN_FWD -- Forward transform (physical to world) for the zenithal /
# azimuthal polynomial projection.

procedure wf_zpn_fwd (fc, p, w)

pointer	fc			#I pointer to FC descriptor
double	p[2]			#I physical coordinates (x, y)
double	w[2]			#O world coordinates (ra, dec)

int	ira, idec, i, j, k
double	x, y, r, zd, a, b, c, d, zd1, zd2, r1, r2, lambda, rt, tol
double	phi, theta, costhe, sinthe, dphi, cosphi, sinphi,  ra, dec, dlng, z
double	wf_gseval()
data	tol/1.0d-13/

define	phitheta_	11

begin
	# Get the axis numbers.
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	# Compute native spherical coordinates PHI and THETA in degrees from
	# the projected coordinates. This is the projection part of the
	# computation.

	k = nint (FC_NP(fc))
        if (FC_LNGCOR(fc) == NULL)
            x = p[ira]
        else
            x = p[ira] + wf_gseval (FC_LNGCOR(fc), p[ira], p[idec])
        if (FC_LATCOR(fc) == NULL)
            y = p[idec]
        else
            y = p[idec] + wf_gseval (FC_LATCOR(fc), p[ira], p[idec])
	r = sqrt (x * x + y * y) / FC_RODEG(fc)

	# Solve.

	# Constant no solution
	if (k < 1) {
	    w[ira] = FC_BADCVAL(fc)
	    w[idec] = FC_BADCVAL(fc)
	    return

	# Linear.
	} else if (k == 1) {
	    zd = (r - FC_PC(fc,0)) / FC_PC(fc,1)

	# Quadratic.
	} else if (k == 2) {

	    a = FC_PC(fc,2) 
	    b = FC_PC(fc,1) 
	    c = FC_PC(fc,0) - r 
	    d = b * b - 4.0d0 * a * c
	    if (d < 0.0d0) {
	        w[ira] = FC_BADCVAL(fc)
	        w[idec] = FC_BADCVAL(fc)
	        return
	    }
	    d = sqrt (d)

	    # Choose solution closet to the pole.
	    zd1 = (-b + d) / (2.0d0 * a)
	    zd2 = (-b - d) / (2.0d0 * a)
	    zd = min (zd1, zd2)
	    if (zd < -tol)
		zd = max (zd1, zd2)
	    if (zd < 0.0d0) {
		if (zd < -tol) {
	            w[ira] = FC_BADCVAL(fc)
	            w[idec] = FC_BADCVAL(fc)
	            return
		}
		zd = 0.0d0
	    } else if (zd > DPI) {
		if (zd > (DPI + tol)) {
	            w[ira] = FC_BADCVAL(fc)
	            w[idec] = FC_BADCVAL(fc)
	            return
		}
		zd = DPI
	    }

	# Higher order solve iteratively.
	} else {

	    zd1 = 0.0d0
	    r1 = FC_PC(fc,0)
	    zd2 = FC_ZD(fc)
	    r2 = FC_R(fc)

	    if (r < r1) {
		if (r < (r1 - tol)) {
	            w[ira] = FC_BADCVAL(fc)
	            w[idec] = FC_BADCVAL(fc)
	            return
		}
		zd = zd1
		goto phitheta_
	    } else if (r > r2) {
		if (r > (r2 + tol)) {
	            w[ira] = FC_BADCVAL(fc)
	            w[idec] = FC_BADCVAL(fc)
	            return
		}
		zd = zd2
		goto phitheta_
	    } else {
		do j = 1, 100 {
		    lambda = (r2 - r) / (r2 - r1)
		    if (lambda < 0.1d0)
			lambda = 0.1d0
		    else if (lambda > 0.9d0)
			lambda = 0.9d0
		    zd = zd2 - lambda * (zd2 - zd1)
		    rt = 0.0d0
		    do i = k, 0, -1
			rt = (rt * zd) + FC_PC(fc,i)
		    if (rt < r) {
			if ((r - rt) < tol)
			    goto phitheta_
			r1 = rt
			zd1 = zd
		    } else {
			if ((rt - r) < tol)
			    goto phitheta_
			r2 = rt
			zd2 = zd
		    }
		    if (abs(zd2 - zd1) < tol)
			goto phitheta_
		}
	    }

	}

phitheta_

	# Compute PHI.
	if (r == 0.0d0)
	    phi = 0.0d0
	else
	    phi = atan2 (x, -y)

	# Compute THETA.
	theta = DHALFPI - zd

	# Compute the celestial coordinates RA and DEC from the native
	# coordinates PHI and THETA. This is the spherical geometry part
	# of the computation.

	costhe = cos (theta)
	sinthe = sin (theta)
	dphi = phi - FC_LONGP(fc)
	cosphi = cos (dphi)
	sinphi = sin (dphi)

	# Compute the RA.
        x = sinthe * FC_SINLATP(fc) - costhe * FC_COSLATP(fc) * cosphi
        if (abs (x) < FC_SPHTOL(fc))
            x = -cos (theta + FC_COLATP(fc)) + costhe * FC_COSLATP(fc) *
                (1.0d0 - cosphi)
        y = -costhe * sinphi
        if (x != 0.0d0 || y != 0.0d0) {
            dlng = atan2 (y, x)
        } else {
            dlng = dphi + DPI
        }
        ra =  FC_W(fc,ira) + DRADTODEG(dlng)

	# Normalize the RA.
	if (FC_W(fc,ira) >= 0.0d0) {
	    if (ra < 0.0d0)
		ra = ra + 360.0d0
	} else {
	    if (ra > 0.0d0)
		ra = ra - 360.0d0
	}
        if (ra > 360.0d0)
            ra = ra - 360.0d0
        else if (ra < -360.0d0)
            ra = ra + 360.0d0

	# Compute the DEC.
        if (mod (dphi, DPI) == 0.0d0) {
            dec = DRADTODEG(theta + cosphi * FC_COLATP(fc))
            if (dec > 90.0d0)
                dec = 180.0d0 - dec
            if (dec < -90.0d0)
                dec = -180.0d0 - dec
        } else {
            z = sinthe * FC_COSLATP(fc) + costhe * FC_SINLATP(fc) * cosphi
            if (abs(z) > 0.99d0) {
                if (z >= 0.0d0)
                    dec = DRADTODEG(acos (sqrt(x * x + y * y)))
                else
                    dec = DRADTODEG(-acos (sqrt(x * x + y * y)))
            } else
                dec = DRADTODEG(asin (z))
        }

	# Store the results.
	w[ira]  = ra
	w[idec] = dec
end


# WF_ZPN_INV -- Inverse transform (world to physical) for the zenithal /
# azimuthal polynomial projection.

procedure wf_zpn_inv (fc, w, p)

pointer	fc			#I pointer to FC descriptor
double	w[2]			#I input world (RA, DEC) coordinates
double	p[2]			#I output physical coordinates

int	ira, idec, i, niter
double	ra, dec, cosdec, sindec, cosra, sinra, x, y, phi, theta, s, r, dphi, z
double	xm, ym, f, fx, fy, g, gx, gy, denom, dx, dy
double	wf_gseval(), wf_gsder()

begin
	# Get the axes numbers.
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	# Compute the transformation from celestial coordinates RA and
	# DEC to native coordinates PHI and THETA. This is the spherical
	# geometry part of the transformation.

	ra  = DDEGTORAD (w[ira] - FC_W(fc,ira))
	dec = DDEGTORAD (w[idec])
	cosra = cos (ra)
	sinra = sin (ra)
	cosdec = cos (dec)
	sindec = sin (dec)

	# Compute PHI.
        x = sindec * FC_SINLATP(fc) - cosdec * FC_COSLATP(fc) * cosra
        if (abs(x) < FC_SPHTOL(fc))
            x = -cos (dec + FC_COLATP(fc)) + cosdec * FC_COSLATP(fc) *
                (1.0d0 - cosra)
        y = -cosdec * sinra
        if (x != 0.0d0 || y != 0.0d0)
            dphi = atan2 (y, x)
        else
            dphi = ra - DPI
        phi = FC_LONGP(fc) + dphi
        if (phi > DPI)
            phi = phi - DTWOPI
        else if (phi < -DPI)
            phi = phi + DTWOPI

	# Compute THETA.
        if (mod (ra, DPI) ==0.0) {
            theta = dec + cosra * FC_COLATP(fc)
            if (theta > DHALFPI)
                theta = DPI - theta
            if (theta < -DHALFPI)
                theta = -DPI - theta
        } else {
            z = sindec * FC_COSLATP(fc) + cosdec * FC_SINLATP(fc) * cosra
            if (abs (z) > 0.99d0) {
                if (z >= 0.0)
                    theta = acos (sqrt(x * x + y * y))
                else
                    theta = -acos (sqrt(x * x + y * y))
            } else
                theta = asin (z)
        }

	# Compute the transformation from native coordinates PHI and THETA
	# to projected coordinates X and Y.

	s = DHALFPI - theta
	r = 0.0d0
	do i = 9, 0, -1
	    r = r * s + FC_PC(fc,i)
	r = FC_RODEG(fc) * r

        if (FC_LNGCOR(fc) == NULL && FC_LATCOR(fc) == NULL) {
	    p[ira]  = r * sin (phi)
	    p[idec] = -r * cos (phi)

	} else {
            xm  = r * sin (phi)
            ym = -r * cos (phi)
            x = xm
            y = ym
            niter = 0

            repeat {
                if (FC_LNGCOR(fc) != NULL) {
                    f = x + wf_gseval (FC_LNGCOR(fc), x, y) - xm
                    fx = wf_gsder (FC_LNGCOR(fc), x, y, 1, 0)
                    fx = 1.0 + fx
                    fy = wf_gsder (FC_LNGCOR(fc), x, y, 0, 1)
                } else {
                    f = x - xm
                    fx = 1.0
                    fy = 0.0
                }
                if (FC_LATCOR(fc) != NULL) {
                    g = y + wf_gseval (FC_LATCOR(fc), x, y) - ym
                    gx = wf_gsder (FC_LATCOR(fc), x, y, 1, 0)
                    gy = wf_gsder (FC_LATCOR(fc), x, y, 0, 1)
                    gy = 1.0 + gy
                } else {
                    g = y - ym
                    gx = 0.0
                    gy = 1.0
                }
                denom = fx * gy - fy * gx
                if (denom == 0.0d0)
                    break
                dx = (-f * gy + g * fy) / denom
                dy = (-g * fx + f * gx) / denom
                x = x + max (-1.0D0, min (1.0D0, dx))
                y = y + max (-1.0D0, min (1.0D0, dy))
                if (max (abs (dx), abs (dy), abs(f), abs(g)) < 2.80d-7)
                    break

                niter = niter + 1

            } until (niter >= MAX_NITER)

            p[ira]  = x
            p[idec] = y
	}
end


# WF_ZPN_DESTROY -- Free up the distortion surface pointers.

procedure wf_zpn_destroy (fc)

pointer fc                      #I pointer to the FC descriptor

begin
        if (FC_LNGCOR(fc) != NULL)
            call wf_gsclose (FC_LNGCOR(fc))
        if (FC_LATCOR(fc) != NULL)
            call wf_gsclose (FC_LATCOR(fc))
end
