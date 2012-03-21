# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	"mwcs.h"

.help WFTNX
.nf -------------------------------------------------------------------------
WFTNX -- WCS function driver for the gnomonic projection.

Driver routines:

	FN_INIT		    wf_tn_init (fc, dir)
	FN_DESTROY       wf_tnx_destroy (fc)
	FN_FWD		     wf_tnx_fwd (fc, v1, v2)
	FN_INV		     wf_tnx_inv (fc, v1, v2)

.endhelp --------------------------------------------------------------------

define	MAX_NITER	20

# Driver specific fields of function call (FC) descriptor.
define	FC_LNGCOR	Memi[$1+FCU]			# RA axis (1 or 2)
define	FC_LATCOR	Memi[$1+FCU+1]			# DEC axis (1 or 2)
define	FC_IRA		Memi[$1+FCU+2]			# RA axis (1 or 2)
define	FC_IDEC		Memi[$1+FCU+3]			# DEC axis (1 or 2)
define	FC_LONGP	Memd[P2D($1+FCU+4)]		# LONGPOLE (rads)
define	FC_COLATP	Memd[P2D($1+FCU+6)]		# (90 - DEC) (rads)
define	FC_COSLATP	Memd[P2D($1+FCU+8)]		# cosine (90 - DEC)
define	FC_SINLATP	Memd[P2D($1+FCU+10)]		# sine (90 - DEC)
define	FC_SPHTOL	Memd[P2D($1+FCU+12)]		# trig toleracne
define	FC_RODEG	Memd[P2D($1+FCU+14)]		# RO (degs)
define	FC_BADCVAL	Memd[P2D($1+FCU+16)]		# Bad coordinate value
define	FC_W		Memd[P2D($1+FCU+18)+($2)-1] 	# CRVAL (axis 1 and 2)


# WF_TNX_INIT -- Initialize the gnomonic forward or inverse transform.
# Initialization for this transformation consists of, determining which
# axis is RA / LON and which is DEC / LAT, computing the celestial longitude
# and colatitude of the native pole, reading in the the native longitude
# of the pole of the celestial coordinate system LONGPOLE from the attribute
# list, precomputing Euler angles and various intermediaries derived from the
# coordinate reference values, and reading in the projection parameter RO
# from the attribute list. If LONGPOLE is undefined then a value of 180.0
# degrees is assumed. If RO is undefined a value of 180.0 / PI is assumed.
# The TAN projection is equivalent to the AZP projection with MU set to 0.0.
# In order to determine the axis order, the parameter "axtype={ra|dec}
# {xlon|glat}{xlon|elat}" must have been set in the attribute list for the
# function. The LONGPOLE and RO parameters may be set in either or both of
# the axes attribute lists, but the value in the RA axis attribute list takes
# precedence. 

procedure wf_tnx_init (fc, dir)

pointer	fc			#I pointer to FC descriptor
int	dir			#I direction of transform

int	i, szatstr
double	dec
pointer	atvalue, ct, mw, wp, wv
int	ctod(), strlen()
pointer	wf_gsopen()
errchk	wf_decaxis(), mw_gwattrs()

begin
	# Allocate space for the attribute string.
	call malloc (atvalue, SZ_LINE, TY_CHAR)

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

	# Get the celestial coordinates of the native pole which are in
	# this case the ra and 90 - dec of the reference point.

	dec = DDEGTORAD(90.0d0 - FC_W(fc,FC_IDEC(fc)))

	# Determine the native longitude of the pole of the celestial
	# coordinate system corresponding to the FITS keyword LONGPOLE.
	# This number has no default and should normally be set to 180
	# degrees. Search both axes for this quantity.

	iferr {
	    call mw_gwattrs (mw, FC_IRA(fc), "longpole", Memc[atvalue], SZ_LINE)
	} then {
	    iferr {
	        call mw_gwattrs (mw, FC_IDEC(fc), "longpole", Memc[atvalue],
		    SZ_LINE)
	    } then {
		FC_LONGP(fc) = 180.0d0
	    } else {
	        i = 1
	        if (ctod (Memc[atvalue], i, FC_LONGP(fc)) <= 0)
		    FC_LONGP(fc) = 180.0d0
	        if (IS_INDEFD(FC_LONGP(fc)))
	            FC_LONGP(fc) = 180.0d0
	    }
	} else {
	    i = 1
	    if (ctod (Memc[atvalue], i, FC_LONGP(fc)) <= 0)
		FC_LONGP(fc) = 180.0d0
	    if (IS_INDEFD(FC_LONGP(fc)))
	        FC_LONGP(fc) = 180.0d0
	}
	FC_LONGP(fc) = DDEGTORAD(FC_LONGP(fc))

	# Precompute the trigomometric functions used by the spherical geometry
	# code to improve efficiency.

	FC_COLATP(fc) = dec
	FC_COSLATP(fc) = cos(dec)
	FC_SINLATP(fc) = sin(dec)

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

	szatstr = SZ_LINE

	# Fetch the longitude correction surface. Note that the attribute
	# string may be of any length so the length of atvalue may have
	# to be adjusted.

	iferr {
	    repeat {
	        call mw_gwattrs (mw, FC_IRA(fc), "lngcor", Memc[atvalue],
		    szatstr)
		if (strlen (Memc[atvalue]) < szatstr)
		    break
		szatstr = szatstr + SZ_LINE 
		call realloc (atvalue, szatstr, TY_CHAR)

	    }
	} then {
	    FC_LNGCOR(fc) = NULL
	} else {
	    FC_LNGCOR(fc) = wf_gsopen (Memc[atvalue])
	}

	# Fetch the latitude correction surface. Note that the attribute
	# string may be of any length so the length of atvalue may have
	# to be adjusted.

	iferr {
	    repeat {
	        call mw_gwattrs (mw, FC_IDEC(fc), "latcor", Memc[atvalue],
		    szatstr)
		if (strlen (Memc[atvalue]) < szatstr)
		    break
		szatstr = szatstr + SZ_LINE 
		call realloc (atvalue, szatstr, TY_CHAR)
	    }
	} then {
	    FC_LATCOR(fc) = NULL
	} else {
	    FC_LATCOR(fc) = wf_gsopen (Memc[atvalue])
	}

	# Set the small angle spherical trigonometry tolerance.
	FC_SPHTOL(fc) = 1.0d-5

	# Set the bad coordinate value.
	FC_BADCVAL(fc) = INDEFD

	# Free working space.
	call mfree (atvalue, TY_CHAR)
end


# WF_TNX_FWD -- Forward transform (physical to world) gnomonic projection.

procedure wf_tnx_fwd (fc, p, w)

pointer	fc			#I pointer to FC descriptor
double	p[2]			#I physical coordinates (x, y)
double	w[2]			#O world coordinates (ra, dec)

int	ira, idec
double	x, y, r, phi, theta, costhe, sinthe, dphi, cosphi, sinphi, dlng, z
double	ra, dec
double	wf_gseval()

begin
	# Get the axis numbers.
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	# Compute native spherical coordinates PHI and THETA in degrees from
	# the projected coordinates. This is the projection part of the
	# computation.

	if (FC_LNGCOR(fc) == NULL)
	    x = p[ira]
	else
	    x = p[ira] + wf_gseval (FC_LNGCOR(fc), p[ira], p[idec])
	if (FC_LATCOR(fc) == NULL)
	    y = p[idec]
	else
	    y = p[idec] + wf_gseval (FC_LATCOR(fc), p[ira], p[idec])
	r = sqrt (x * x + y * y)

	# Compute PHI.
	if (r == 0.0d0)
	    phi = 0.0d0
	else
	    phi = atan2 (x, -y)

	# Compute THETA.
	theta = atan2 (FC_RODEG(fc), r)

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

	# Normalize RA.
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


# WF_TNX_INV -- Inverse transform (world to physical) for the gnomic
# projection.

procedure wf_tnx_inv (fc, w, p)

pointer	fc			#I pointer to FC descriptor
double	w[2]			#I input world (RA, DEC) coordinates
double	p[2]			#I output physical coordinates

int	ira, idec, niter
double	ra, dec, cosdec, sindec, cosra, sinra, x, y, phi, theta, s, r, dphi, z
double	xm, ym, f, fx, fy, g, gx, gy, denom, dx, dy, dmax
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

	s = sin (theta)
	if (s == 0.0d0) {
	    p[ira] = FC_BADCVAL(fc)
	    p[idec] = FC_BADCVAL(fc)
	} else {
	    r = FC_RODEG(fc) * cos (theta) / s
	    if (FC_LNGCOR(fc) == NULL && FC_LATCOR(fc) == NULL) {
	        p[ira]  = r * sin (phi)
	        p[idec] = -r * cos (phi)
	    } else {
	        xm  = r * sin (phi)
	        ym = -r * cos (phi)
		x = xm
		y = ym
		niter = 0
		dmax = 30. / 3600.
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
		    x = x + max (-dmax, min (dmax, dx))
		    y = y + max (-dmax, min (dmax, dy))
		    if (max (abs (dx), abs (dy), abs(f), abs(g)) < 2.80d-7)
			break

		    niter = niter + 1

		} until (niter >= MAX_NITER)

	        p[ira]  = x
	        p[idec] = y
	    }
	}
end


# WF_TNX_DESTROY -- Free up the distortion surface pointers.

procedure wf_tnx_destroy (fc)

pointer	fc			#I pointer to the FC descriptor

begin
	if (FC_LNGCOR(fc) != NULL)
	    call wf_gsclose (FC_LNGCOR(fc))
	if (FC_LATCOR(fc) != NULL)
	    call wf_gsclose (FC_LATCOR(fc))
end
