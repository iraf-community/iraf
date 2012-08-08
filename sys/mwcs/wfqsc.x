# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	"mwcs.h"

.help WFQSC
.nf -------------------------------------------------------------------------
WFQSC -- WCS function driver for quadrilateralized spherical cube projection.

Driver routines:

	FN_INIT		    wf_qsc_init (fc, dir)
	FN_DESTROY			(none)
	FN_FWD		     wf_qsc_fwd (fc, v1, v2)
	FN_INV		     wf_qsc_inv (fc, v1, v2)

.endhelp --------------------------------------------------------------------

# Driver specific fields of function call (FC) descriptor.
define	FC_IRA		Memi[$1+FCU]		     # RA axis (1 or 2)
define	FC_IDEC		Memi[$1+FCU+1]		     # DEC axis (1 or 2)
define  FC_NATRA        Memd[P2D($1+FCU+2)]          # RA of native pole (rads)
define  FC_NATDEC       Memd[P2D($1+FCU+4)]          # DEC of native pole (rads)
define  FC_LONGP        Memd[P2D($1+FCU+6)]          # LONGPOLE (rads)
define  FC_COSDEC       Memd[P2D($1+FCU+8)]          # cosine (NATDEC)
define  FC_SINDEC       Memd[P2D($1+FCU+10)]         # sine (NATDEC)
define  FC_SPHTOL       Memd[P2D($1+FCU+12)]         # trig tolerance
define	FC_RODEG	Memd[P2D($1+FCU+14)]	     # RO (degs)
define	FC_C1		Memd[P2D($1+FCU+16)]	     # RO * (PI / 4)
define	FC_C2		Memd[P2D($1+FCU+18)]	     # (4 / PI) / RO
define	FC_BADCVAL	Memd[P2D($1+FCU+20)]	     # bad coordinate value
define	FC_W		Memd[P2D($1+FCU+22)+($2)-1]  # CRVAL axis (1 and 2)


# WF_QSC_INIT -- Initialize the forward or inverse quarilateralized spherical
# cube projection transform. Initialization for this transformation consists
# of, determining which axis is RA / LON and which is DEC / LAT, reading in
# the native longitude and latitude of the pole in celestial coordinates
# LONGPOLE and LATPOLE from the attribute list, computing the celestial
# longitude and colatitude of the native pole, precomputing the Euler angles
# and various intermediary functions of the reference point, reading in the
# projection parameter RO from the attribute list, and precomputing the various
# required intermediate quantities. If LONGPOLE is undefined then a value of
# 180.0 degrees is assumed if the celestial latitude is less than 0, otherwise
# 0 degrees is assumed. If LATPOLE is undefined the most northerly of the two
# possible solutions is chosen, otherwise the solution closest to LATPOLE is
# chosen. If RO is undefined a value of 180.0 / PI is assumed. In order to
# determine the axis order, the parameter "axtype={ra|dec} {xlon|xlat}" must
# have been set in the attribute list for the function. The LONGPOLE, LATPOLE,
# and RO parameters may be set in either or both of the axes attribute lists,
# but the value in the RA axis attribute list takes precedence. 

procedure wf_qsc_init (fc, dir)

pointer	fc			#I pointer to FC descriptor
int	dir			#I direction of transform

int	i
double  dec, latpole, theta0, clat0, slat0, cphip, sphip, cthe0, sthe0, x, y, z
double  u, v, latp1, latp2, latp, maxlat, tol
pointer	sp, atvalue, ct, mw, wp, wv
int	ctod()
data    tol/1.0d-10/
errchk	wf_decaxis(), mw_gwattrs()

begin
	# Allocate space for the attribute string.
	call smark (sp)
	call salloc (atvalue, SZ_LINE, TY_CHAR)

	# Get the required mwcs pointers.
	ct = FC_CT(fc)
	mw = CT_MW(ct)
	wp = FC_WCS(fc)

	# Determine which is the DEC axis, and hence the axis order.
	call wf_decaxis (fc, FC_IRA(fc), FC_IDEC(fc))

	# Get the value of W for each axis, i.e. the world coordinates at
	# the reference point.

	wv = MI_DBUF(mw) + WCS_W(wp) - 1
	do i = 1, 2
	    FC_W(fc,i) = Memd[wv+CT_AXIS(ct,FC_AXIS(fc,i))-1]

        # Determine the native longitude and latitude of the pole of the
        # celestial coordinate system corresponding to the FITS keywords
        # LONGPOLE and LATPOLE. LONGPOLE has no default but will be set
        # to 180 or 0 depending on the value of the declination of the
        # reference point. LATPOLE has no default but will be set depending
        # on the values of LONGPOLE and the reference declination.

        iferr {
            call mw_gwattrs (mw, FC_IRA(fc), "longpole", Memc[atvalue], SZ_LINE)
        } then {
            iferr {
                call mw_gwattrs (mw, FC_IDEC(fc), "longpole", Memc[atvalue],
                    SZ_LINE)
            } then {
                FC_LONGP(fc) = INDEFD
            } else {
                i = 1
                if (ctod (Memc[atvalue], i, FC_LONGP(fc)) <= 0)
                    FC_LONGP(fc) = INDEFD
            }
        } else {
            i = 1
            if (ctod (Memc[atvalue], i, FC_LONGP(fc)) <= 0)
                FC_LONGP(fc) = INDEFD
        }

        iferr {
            call mw_gwattrs (mw, FC_IRA(fc), "latpole", Memc[atvalue], SZ_LINE)
        } then {
            iferr {
                call mw_gwattrs (mw, FC_IDEC(fc), "latpole", Memc[atvalue],
                    SZ_LINE)
            } then {
                latpole = INDEFD
            } else {
                i = 1
                if (ctod (Memc[atvalue], i, latpole) <= 0)
                    latpole = INDEFD
            }
        } else {
            i = 1
            if (ctod (Memc[atvalue], i, latpole) <= 0)
                latpole = INDEFD
        }

        # Fetch the RO projection parameter which is the radius of the
        # generating sphere for the projection. If RO is absent which
        # is the usual case set it to 180 / PI. Search both axes for
        # this quantity.

        iferr {
            call mw_gwattrs (mw, FC_IRA(fc), "ro", Memc[atvalue], SZ_LINE)
        } then {
            iferr {
                call mw_gwattrs (mw, FC_IDEC(fc), "ro", Memc[atvalue],
                    SZ_LINE)
            } then {
                FC_RODEG(fc) = 180.0d0 / DPI
            } else {
                i = 1
                if (ctod (Memc[atvalue], i, FC_RODEG(fc)) <= 0)
                    FC_RODEG(fc) = 180.0d0 / DPI
            }
        } else {
            i = 1
            if (ctod (Memc[atvalue], i, FC_RODEG(fc)) <= 0)
                FC_RODEG(fc) = 180.0d0 / DPI
        }

        # Compute the native longitude of the celestial pole.
        dec = DDEGTORAD(FC_W(fc,FC_IDEC(fc)))
        theta0 = 0.0d0
        if (IS_INDEFD(FC_LONGP(fc))) {
            if (dec < theta0)
                FC_LONGP(fc) = DPI
            else
                FC_LONGP(fc) = 0.0d0
        } else
            FC_LONGP(fc) = DDEGTORAD(FC_LONGP(fc))

        # Compute the celestial longitude and latitude of the native pole.
        clat0 = cos (dec)
        slat0 = sin (dec)
        cphip = cos (FC_LONGP(fc))
        sphip = sin (FC_LONGP(fc))
        cthe0 = cos (theta0)
        sthe0 = sin (theta0)

        x = cthe0 * cphip
        y = sthe0
        z = sqrt (x * x + y * y)

        # The latitude of the native pole is determined by LATPOLE in this
        # case.
        if (z == 0.0d0) {

            if (slat0 != 0.0d0)
                call error (0, "WF_QSC_INIT: Invalid projection parameters")
            if (IS_INDEFD(latpole))
                latp = 999.0d0
            else
                latp = DDEGTORAD(latpole)

        } else {
            if (abs (slat0 / z) > 1.0d0)
                call error (0, "WF_QSC_INIT: Invalid projection parameters")

            u = atan2 (y, x)
            v = acos (slat0 / z)
            latp1 = u + v
            if (latp1 > DPI)
                latp1 = latp1 - DTWOPI
            else if (latp1 < -DPI)
                latp1 = latp1 + DTWOPI


            latp2 = u - v
            if (latp2 > DPI)
                latp2 = latp2 - DTWOPI
            else if (latp2 < -DPI)
                latp2 = latp2 + DTWOPI

            if (IS_INDEFD(latpole))
                maxlat = 999.0d0
            else
                maxlat = DDEGTORAD(latpole)
            if (abs (maxlat - latp1) < abs (maxlat - latp2)) {
                if (abs (latp1) < (DHALFPI + tol))
                    latp = latp1
                else
                    latp = latp2
            } else {
                if (abs (latp2) < (DHALFPI + tol))
                    latp = latp2
                else
                    latp = latp1
            }
        }
        FC_NATDEC(fc) = DHALFPI - latp

        z = cos (latp) * clat0
        if (abs(z) < tol) {

            # Celestial pole at the reference point.
            if (abs(clat0) < tol) {
                FC_NATRA(fc) = DDEGTORAD(FC_W(fc,FC_IRA(fc)))
                FC_NATDEC(fc) = DHALFPI - theta0
            # Celestial pole at the native north pole.
            } else if (latp > 0.0d0) {
                FC_NATRA(fc) = DDEGTORAD(FC_W(fc,FC_IRA(fc))) + FC_LONGP(fc) -
                    DPI
                FC_NATDEC(fc) = 0.0d0
            # Celestial pole at the native south pole.
            } else if (latp < 0.0d0) {
                FC_NATRA(fc) = DDEGTORAD(FC_W(fc,FC_IRA(fc))) - FC_LONGP(fc)
                FC_NATDEC(fc) = DPI
            }

        } else {
            x = (sthe0 - sin (latp) * slat0) / z
            y = sphip * cthe0 / clat0
            if (x == 0.0d0 && y == 0.0d0)
                call error (0, "WF_QSC_INIT: Invalid projection parameters")
            FC_NATRA(fc) = DDEGTORAD(FC_W(fc,FC_IRA(fc))) - atan2 (y,x)
        }

        if (FC_W(fc,FC_IRA(fc)) >= 0.0d0) {
            if (FC_NATRA(fc) < 0.0d0)
                FC_NATRA(fc) = FC_NATRA(fc) + DTWOPI
        } else {
            if (FC_NATRA(fc) > 0.0d0)
                FC_NATRA(fc) = FC_NATRA(fc) - DTWOPI
        }
        FC_COSDEC(fc) = cos (FC_NATDEC(fc))
        FC_SINDEC(fc) = sin (FC_NATDEC(fc))

        # Check for ill-conditioned parameters.
        if (abs(latp) > (DHALFPI+tol))
            call error (0, "WF_QCS_INIT: Invalid projection parameters")

	# Compute the required intermediate quantities.
	FC_C1(fc) = FC_RODEG(fc) * (DPI / 4.0d0)
	FC_C2(fc) = 1.0d0 / FC_C1(fc)

        # Set the bad coordinate value.
        FC_SPHTOL(fc) = 1.0d-5

	# Set the bad coordinate value.
	FC_BADCVAL(fc) = INDEFD

	# Free working space.
	call sfree (sp)
end


# WF_QSC_FWD -- Forward transform (physical to world) for the quarilateralized
# spherical projection.

procedure wf_qsc_fwd (fc, p, w)

pointer	fc			#I pointer to FC descriptor
double	p[2]			#I physical coordinates (x, y)
double	w[2]			#O world coordinates (ra, dec)

int	ira, idec, face, direct
double	l, m, n, phi, theta, costhe, sinthe, dphi, cosphi, sinphi, x, y, z
double	xf, yf, rho, chi, psi, tol, wconst, ra, dec, dlng, rhu
data	tol /1.0d-12/

begin
	# Get the axis numbers.
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	# Compute native spherical coordinates PHI and THETA in degrees from
	# the projected coordinates. This is the projection part of the
	# computation.

	xf = p[ira] * FC_C2(fc)
	yf = p[idec] * FC_C2(fc)
	if (xf > 5.0d0) {
	    face = 4
	    xf = xf - 6.0d0
	} else if (xf > 3.0d0) {
	    face = 3
	    xf = xf - 4.0d0
	} else if (xf > 1.0d0) {
	    face = 2
	    xf = xf - 2.0d0
	} else if (yf > 1.0d0) {
	    face = 0
	    yf = yf - 2.0d0
	} else if (yf < -1.0d0) {
	    face = 5
	    yf = yf + 2.0d0
	} else {
	    face = 1
	}

	if (abs(xf) > abs(yf))
	    direct = YES
	else
	    direct = NO
	if (direct == YES) {
	    if (xf == 0.0d0) {
		psi = 0.0d0
		chi = 1.0d0
		rho = 1.0d0
		rhu = 0.0d0
	    } else {
		wconst = DDEGTORAD(15.0d0 * yf / xf)
		psi = sin (wconst) / (cos (wconst) - 1.0d0 / DSQRTOF2)
		chi = 1.0d0 + psi * psi
		rhu = xf * xf * (1.0d0 - 1.0d0 / sqrt (1.0d0 + chi))
		rho = 1.0d0 - rhu
	    }
	} else {
	    if (yf == 0.0d0) {
		psi = 0.0d0
		chi = 1.0d0
		rho = 1.0d0
		rhu = 0.0d0
	    } else {
		wconst = DDEGTORAD(15.0d0 * xf / yf)
		psi = sin (wconst) / (cos (wconst) - 1.0d0 / DSQRTOF2)
		chi = 1.0d0 + psi * psi
		rhu = yf * yf * (1.0d0 - 1.0d0 / sqrt (1.0d0 + chi))
		rho = 1.0d0 - rhu
	    }
	}

	if (rho < -1.0d0) {
	    if (rho < (-1.0d0 - tol)) {
		w[ira] = FC_BADCVAL(fc)
		w[idec] = FC_BADCVAL(fc)
		return
	    }
	    rho = -1.0d0
	    rhu = 2.0d0
	    wconst = 0.0d0
	} else {
	    wconst = sqrt (rhu * (2.0d0 - rhu) / chi)
	}

	switch (face) {
	case 0:
	    n = rho
	    if (direct == YES) {
		l = wconst
		if (xf < 0.0d0)
		    l = -l
		m = -l * psi
	    } else {
		m = wconst
		if (yf > 0.0d0)
		    m = -m
		l = -m * psi
	    }

	case 1:
	    m = rho
	    if (direct == YES) {
		l = wconst
		if (xf < 0.0d0)
		    l = -l
		n = l * psi
	    } else {
		n = wconst
		if (yf < 0.0d0)
		    n = -n
		l = n * psi
	    }

	case 2:
	    l = rho
	    if (direct == YES) {
		m = wconst
		if (xf > 0.0d0)
		    m = -m
		n = -m * psi
	    } else {
		n = wconst
		if (yf < 0.0d0)
		    n = -n
		m = -n * psi
	    }

	case 3:
	    m = -rho
	    if (direct == YES) {
		l = wconst
		if (xf > 0.0d0)
		    l = -l
		n = -l * psi
	    } else {
		n = wconst
		if (yf < 0.0d0)
		    n = -n
		l = -n * psi
	    }

	case 4:
	    l = -rho
	    if (direct == YES) {
		m = wconst
		if (xf < 0.0d0)
		    m = -m
		n = m * psi
	    } else {
		n = wconst
		if (yf < 0.0d0)
		    n = -n
		m = n * psi
	    }

	case 5:
	    n = -rho
	    if (direct == YES) {
		l = wconst
		if (xf < 0.0d0)
		    l = -l
		m = l * psi
	    } else {
		m = wconst
		if (yf < 0.0d0)
		    m = -m
		l = m * psi
	    }
	}

	# Compute PHI.
	if (l == 0.0d0 && m == 0.0d0)
	    phi = 0.0d0
	else
	    phi = atan2 (l, m)

	# Compute THETA.
	theta = asin(n)

	# Compute the celestial coordinates RA and DEC from the native
	# coordinates PHI and THETA. This is the spherical geometry part
	# of the computation.

	costhe = cos (theta)
	sinthe = sin (theta)
	dphi = phi - FC_LONGP(fc)
	cosphi = cos (dphi)
	sinphi = sin (dphi)

	# Compute the RA.
	x = sinthe * FC_SINDEC(fc) - costhe * FC_COSDEC(fc) * cosphi
        if (abs (x) < FC_SPHTOL(fc))
            x = -cos (theta + FC_NATDEC(fc)) + costhe * FC_COSDEC(fc) *
                (1.0d0 - cosphi)
	y = -costhe * sinphi
	if (x != 0.0d0 || y != 0.0d0) {
	    dlng = atan2 (y, x)
	} else {
	    dlng = dphi + DPI
	}
	ra = DRADTODEG(FC_NATRA(fc) + dlng)

	# Normalize the RA.
        if (FC_NATRA(fc) >= 0.0d0) {
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
            dec = DRADTODEG(theta + cosphi * FC_NATDEC(fc))
            if (dec > 90.0d0)
                dec = 180.0d0 - dec
            if (dec < -90.0d0)
                dec = -180.0d0 - dec
        } else {
            z = sinthe * FC_COSDEC(fc) + costhe * FC_SINDEC(fc) * cosphi
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


# WF_QSC_INV -- Inverse transform (world to physical) for the quadilateralized
# spherical projection.

procedure wf_qsc_inv (fc, w, p)

pointer	fc			#I pointer to FC descriptor
double	w[2]			#I input world (RA, DEC) coordinates
double	p[2]			#I output physical coordinates

int	ira, idec, face
double	ra, dec, cosdec, sindec, cosra, sinra, x, y, z, phi, theta, dphi
double	costhe, eta, l, m, n, rho, xi, tol, x0, y0, psi, chi, xf, yf
double	pconst, t, rhu
data	tol /1.0d-12/

begin
	# Get the axes numbers.
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	# Compute the transformation from celestial coordinates RA and
	# DEC to native coordinates PHI and THETA. This is the spherical
	# geometry part of the transformation.

	ra  = DDEGTORAD (w[ira]) - FC_NATRA(fc)
	dec = DDEGTORAD (w[idec])
	cosra = cos (ra)
	sinra = sin (ra)
	cosdec = cos (dec)
	sindec = sin (dec)

	# Compute PHI.
	x = sindec * FC_SINDEC(fc) - cosdec * FC_COSDEC(fc) * cosra
        if (abs(x) < FC_SPHTOL(fc))
            x = -cos (dec + FC_NATDEC(fc)) + cosdec * FC_COSDEC(fc) *
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
        if (mod (ra, DPI) == 0.0) {
            theta = dec + cosra * FC_NATDEC(fc)
            if (theta > DHALFPI)
                theta = DPI - theta
            if (theta < -DHALFPI)
                theta = -DPI - theta
        } else {
            z = sindec * FC_COSDEC(fc) + cosdec * FC_SINDEC(fc) * cosra
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
	if (abs(theta) == DHALFPI) {
	    p[ira] = 0.0d0
	    if (theta >= 0.0d0)
	        p[idec] = 2.0d0 * FC_C1(fc) 
	    else
	        p[idec] = -2.0d0 * FC_C1(fc) 
	    return
	}

	costhe = cos (theta)
	l = costhe * sin (phi)
	m = costhe * cos (phi)
	n = sin (theta)

	face = 0
	rho = n
	if (m > rho) {
	    face = 1 
	    rho = m
	}
	if (l > rho) {
	    face = 2
	    rho = l
	}
	if (-m > rho) {
	    face = 3
	    rho = -m
	}
	if (-l > rho) {
	    face = 4
	    rho = -l
	}
	if (-n > rho) {
	    face = 5
	    rho = -n
	}
	rhu = 1.0d0 - rho

	switch (face) {
	case 0:
	    xi = l
	    eta = -m
	    if (rhu < 1.0d-8) {
		t = (DHALFPI - theta)
		rhu = t * t / 2.0d0
	    }
	    x0 = 0.0d0
	    y0 = 2.0d0
	case 1:
	    xi = l
	    eta = n
	    if (rhu < 1.0d-8) {
		t = theta
		pconst = mod (phi, DTWOPI)
		if (pconst < -DPI)
		     pconst = pconst + DTWOPI
		if (pconst > DPI)
		     pconst = pconst - DTWOPI
		rhu = (pconst * pconst + t * t) / 2.0d0
	    }
	    x0 = 0.0d0
	    y0 = 0.0d0
	case 2:
	    xi = -m
	    eta = n
	    if (rhu < 1.0d-8) {
		t = theta
		pconst = mod (phi, DTWOPI)
		if (pconst < -DPI)
		     pconst = pconst + DTWOPI
		pconst = (DHALFPI - pconst)
		rhu = (pconst * pconst + t * t) / 2.0d0
	    }
	    x0 = 2.0d0
	    y0 = 0.0d0
	case 3:
	    xi = -l
	    eta = n
	    if (rhu < 1.0d-8) {
		t = theta
		pconst = mod (phi, DTWOPI)
		if (pconst < 0.0d0)
		     pconst = pconst + DTWOPI
		pconst = (DPI - pconst)
		rhu = (pconst * pconst + t * t) / 2.0d0
	    }
	    x0 = 4.0d0
	    y0 = 0.0d0
	case 4:
	    xi = m
	    eta = n
	    if (rhu < 1.0d-8) {
		t = theta
		pconst = mod (phi, DTWOPI)
		if (pconst > DPI)
		     pconst = pconst - DTWOPI
		pconst = (DHALFPI + pconst)
		rhu = (pconst * pconst + t * t) / 2.0d0
	    }
	    x0 = 6.0d0
	    y0 = 0.0d0
	case 5:
	    xi = l
	    eta = m
	    if (rhu < 1.0d-8) {
		t = (DHALFPI + theta)
		rhu = t * t / 2.0d0
	    }
	    x0 = 0.0d0
	    y0 = -2.0d0
	}

	if (xi == 0.0d0 && eta == 0.0d0) {
	    xf = 0.0d0
	    yf = 0.0d0
	} else if (-xi >= abs(eta)) {
	    psi = eta / xi
	    chi = 1.0d0 + psi * psi
	    xf = -sqrt (rhu / (1.0d0 - 1.0d0 / sqrt (1.0d0 + chi)))
	    yf = (xf / 15.0d0) * DRADTODEG ((atan (psi) - asin (psi /
	        sqrt (chi + chi))))
	} else if (xi >= abs(eta)) {
	    psi = eta / xi
	    chi = 1.0d0 + psi * psi
	    xf = sqrt (rhu / (1.0d0 - 1.0d0 / sqrt (1.0d0 + chi)))
	    yf = (xf / 15.0d0) * DRADTODEG ((atan (psi) - asin (psi /
	        sqrt (chi + chi))))
	} else if (-eta > abs (xi)) {
	    psi = xi / eta
	    chi = 1.0d0 + psi * psi
	    yf = -sqrt (rhu / (1.0d0 - 1.0d0 / sqrt (1.0d0 + chi)))
	    xf = (yf / 15.0d0) * DRADTODEG ((atan (psi) - asin (psi /
	        sqrt (chi + chi))))
	} else if (eta > abs (xi)) {
	    psi = xi / eta
	    chi = 1.0d0 + psi * psi
	    yf = sqrt (rhu / (1.0d0 - 1.0d0 / sqrt (1.0d0 + chi)))
	    xf = (yf / 15.0d0) * DRADTODEG ((atan (psi) - asin (psi /
	        sqrt (chi + chi))))
	}

	if (abs(xf) > 1.0d0) {
	    if (abs(xf) > (1.0d0 + tol)) {
		p[ira] = FC_BADCVAL(fc)
		p[idec] = FC_BADCVAL(fc)
		return
	    }
	    if (xf >= 0.0d0)
		xf = 1.0d0
	    else
		xf = -1.0d0
	}
	if (abs(yf) > 1.0d0) {
	    if (abs(yf) > (1.0d0 + tol)) {
		p[ira] = FC_BADCVAL(fc)
		p[idec] = FC_BADCVAL(fc)
		return
	    }
	    if (yf >= 0.0d0)
		yf = 1.0d0
	    else
		yf = -1.0d0
	}

	p[ira] = FC_C1(fc) * (x0 + xf)
	p[idec] = FC_C1(fc) * (y0 + yf)
end
