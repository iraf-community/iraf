# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	"mwcs.h"

.help WFTAN
.nf -------------------------------------------------------------------------
WFTAN -- WCS function driver for the tangent plane (gnonomic) projection.

Driver routines:

	FN_INIT		    wf_tan_init (fc, dir)
	FN_DESTROY			(none)
	FN_FWD		     wf_tan_fwd (fc, v1, v2)
	FN_INV		     wf_tan_inv (fc, v1, v2)

.endhelp --------------------------------------------------------------------

# Driver specific fields of function call (FC) descriptor.
define	FC_IRA		Memi[$1+FCU]		# RA axis (1 or 2)
define	FC_IDEC		Memi[$1+FCU+1]		# DEC axis (1 or 2)
define	FC_COSDEC	Memd[P2D($1+FCU+2)]	# cosine(dec)
define	FC_SINDEC	Memd[P2D($1+FCU+4)]	# sine(dec)
define	FC_W		Memd[P2D($1+FCU+6)+($2)-1] # W (CRVAL) for each axis


# WF_TAN_INIT -- Initialize the tangent plane forward or inverse transform.
# Initialization for this transformation consists of determining which axis
# is RA and which is DEC, and precomputing the sine and cosine of the
# declination at the reference point.  In order to determine the axis order,
# the parameter "axtype={ra|dec}" must have been set in the attribute list
# for the function.

procedure wf_tan_init (fc, dir)

pointer	fc			#I pointer to FC descriptor
int	dir			#I direction of transform

int	i
double	dec
pointer	ct, mw, wp, wv
errchk	wf_decaxis

begin
	ct = FC_CT(fc)
	mw = CT_MW(ct)
	wp = FC_WCS(fc)

	# Determine which is the DEC axis, and hence the axis order.
	call wf_decaxis (fc, FC_IRA(fc), FC_IDEC(fc))

	# Get the value of W for each axis, i.e., the world coordinate at
	# the reference point.

	wv = MI_DBUF(mw) + WCS_W(wp) - 1
	do i = 1, 2
	    FC_W(fc,i) = Memd[wv+CT_AXIS(ct,FC_AXIS(fc,i))-1]

	# Precompute the sin and cos of the declination at the reference pixel.
	dec = DEGTORAD(FC_W(fc,FC_IDEC(fc)))
	FC_COSDEC(fc) = cos(dec)
	FC_SINDEC(fc) = sin(dec)
end


# WF_TAN_FWD -- Forward transform (physical to world), tangent plane
# projection.  Based on code from STScI, Hodge et. al.

procedure wf_tan_fwd (fc, p, w)

pointer	fc			#I pointer to FC descriptor
double	p[2]			#I physical coordinates (xi, eta)
double	w[2]			#O world coordinates (ra, dec)

int	ira, idec
double	xi, eta, x, y, z, ra, dec

begin
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	xi  = DEGTORAD(p[ira])
	eta = DEGTORAD(p[idec])

	# Rotate the rectangular coordinate system of the vector [1,xi,eta]
	# by the declination so that the X axis will pass through the equator.

	x = FC_COSDEC(fc) - eta * FC_SINDEC(fc)
	y = xi
	z = FC_SINDEC(fc) + eta * FC_COSDEC(fc)

	# Compute RA and DEC in radians.
	if (x == 0.0D0 && y == 0.0D0)
	    ra = 0.0D0
	else
	    ra = atan2 (y, x)
	dec = atan2 (z, sqrt (x*x + y*y))

	# Return RA and DEC in degrees.
	dec = RADTODEG(dec)
	ra  = RADTODEG(ra) + FC_W(fc,ira)

	if (ra < 0)
	    ra = ra + 360D0
	else if (ra > 360D0)
	    ra = ra - 360D0

	w[ira]  = ra
	w[idec] = dec
end


# WF_TAN_INV -- Inverse transform (world to physical) for the tangent plane
# projection.  Based on code from STScI, Hodge et. al.

procedure wf_tan_inv (fc, w, p)

pointer	fc			#I pointer to FC descriptor
double	w[2]			#I input world (RA, DEC) coordinates
double	p[2]			#I output physical coordinates

int	ira, idec
double	ra, dec, xi, eta
double	cosra, cosdec, sinra, sindec, cosdist

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

	xi = cosdec * sinra / cosdist
	eta = (sindec * FC_COSDEC(fc) - cosdec * FC_SINDEC(fc) * cosra) /
	    cosdist

	p[ira]  = RADTODEG(xi)
	p[idec] = RADTODEG(eta)
end
