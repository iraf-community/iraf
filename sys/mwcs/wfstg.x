# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	"mwcs.h"

.help WFSTG
.nf -------------------------------------------------------------------------
WFSTG -- WCS function driver for the stereographic projection.

Driver routines:

	FN_INIT		    wf_stg_init (fc, dir)
	FN_DESTROY			(none)
	FN_FWD		     wf_stg_fwd (fc, v1, v2)
	FN_INV		     wf_stg_inv (fc, v1, v2)

.endhelp --------------------------------------------------------------------

# Driver specific fields of function call (FC) descriptor.
define	FC_IRA		Memi[$1+FCU]			# RA axis (1 or 2)
define	FC_IDEC		Memi[$1+FCU+1]			# DEC axis (1 or 2)
define	FC_LONGP	Memd[P2D($1+FCU+2)]		# LONGPOLE (rads)
define	FC_COLATP	Memd[P2D($1+FCU+4)]		# (90 - DEC) (rads)
define	FC_COSLATP	Memd[P2D($1+FCU+6)]		# cosine (90 - DEC)
define	FC_SINLATP	Memd[P2D($1+FCU+8)]		# sine (90 - DEC)
define	FC_SPHTOL	Memd[P2D($1+FCU+10)]		# trig tolerance
define	FC_RODEG	Memd[P2D($1+FCU+12)]		# RO (degs)
define	FC_2RODEG	Memd[P2D($1+FCU+14)]		# 2 * RO (degs)
define	FC_REC2RODEG	Memd[P2D($1+FCU+16)]		# 1 / (2 * RO) (degs)
define	FC_BADCVAL	Memd[P2D($1+FCU+18)]		# Bad coordinate value
define	FC_W		Memd[P2D($1+FCU+20)+($2)-1] 	# CRVAL (axis 1 and 2)


# WF_STG_INIT -- Initialize the stereographic forward or inverse transform.
# Initialization for this transformation consists of, determining which
# axis is RA / LON and which is DEC / LAT, computing the celestial longitude
# and colatitude of the native pole, reading in the the native longitude of the
# pole of the celestial coordinate system LONGPOLE from the attribute list,
# precomputing the Euler angles and various intermediary functions of the
# reference coordinates, reading in the projection parameter RO from the
# attribute list, and precomputing some intermediate parameters. If LONGPOLE
# is undefined then a value of 180.0 degrees is assumed. If RO is undefined a
# value of 180.0 / PI is assumed. The STG projection is equivalent to the AZP
# projection with MU set to 1.0. In order to determine the axis order, the
# parameter "axtype={ra|dec} {xlon|xlat}" must have been set in the attribute
# list for the function. The LONGPOLE and RO parameters may be set in either
# or both of the axes attribute lists, but the value in the RA axis attribute
# list takes precedence. 

procedure wf_stg_init (fc, dir)

pointer	fc			#I pointer to FC descriptor
int	dir			#I direction of transform

int	i
double	dec
pointer	sp, atvalue, ct, mw, wp, wv
int	ctod()
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
	FC_2RODEG(fc) = 2.0d0 * FC_RODEG(fc)
	FC_REC2RODEG(fc) = 1.0d0 / FC_2RODEG(fc)

	# Fetch the spherical trigonometry tolerance.
	FC_SPHTOL(fc) = 1.0d-5

	# Fetch the bad coordinate value.
	FC_BADCVAL(fc) = INDEFD

	# Free working space.
	call sfree (sp)
end


# WF_STG_FWD -- Forward transform (physical to world) for the stereographic
# projection.

procedure wf_stg_fwd (fc, p, w)

pointer	fc			#I pointer to FC descriptor
double	p[2]			#I physical coordinates (x, y)
double	w[2]			#O world coordinates (ra, dec)

int	ira, idec
double	x, y, r, phi, theta, costhe, sinthe, dphi, cosphi, sinphi,  ra, dec
double	dlng, z

begin
	# Get the axis numbers.
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	# Compute native spherical coordinates PHI and THETA in degrees from
	# the projected coordinates. This is the projection part of the
	# computation.

	x = p[ira]
	y = p[idec]
	r = sqrt (x * x + y * y)

	# Compute PHI.
	if (r == 0.0d0)
	    phi = 0.0d0
	else
	    phi = atan2 (x, -y)

	# Compute THETA.
	theta = DHALFPI - 2.0d0 * atan (r * FC_REC2RODEG(fc))

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


# WF_STG_INV -- Inverse transform (world to physical) for the stereographic
# projection.

procedure wf_stg_inv (fc, w, p)

pointer	fc			#I pointer to FC descriptor
double	w[2]			#I input world (RA, DEC) coordinates
double	p[2]			#I output physical coordinates

int	ira, idec
double	ra, dec, cosdec, sindec, cosra, sinra, x, y, phi, theta, s, r, dphi, z

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

	s = 1.0d0 + sin (theta)
	if (s == 0.0d0) {
	    p[ira]  = FC_BADCVAL(fc)
	    p[idec] = FC_BADCVAL(fc)
	} else {
	    r = FC_2RODEG(fc) * cos (theta) / s
	    p[ira]  = r * sin (phi)
	    p[idec] = -r * cos (phi)
	}
end
