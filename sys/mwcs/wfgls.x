# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<math.h>
include	"mwcs.h"

.help WFGLS
.nf -------------------------------------------------------------------------
WFGLS -- WCS function driver for the global sinusoidal projection.

Driver routines:

	FN_INIT		    wf_gls_init (fc, dir)
	FN_DESTROY			(none)
	FN_FWD		     wf_gls_fwd (fc, v1, v2)
	FN_INV		     wf_gls_inv (fc, v1, v2)

.endhelp --------------------------------------------------------------------

# Driver specific fields of function call (FC) descriptor.
define	FC_IRA		Memi[$1+FCU]		# RA axis (1 or 2)
define	FC_IDEC		Memi[$1+FCU+1]		# DEC axis (1 or 2)
define	FC_COSDEC	Memd[P2D($1+FCU+2)]	# cosine(dec)
define	FC_SINDEC	Memd[P2D($1+FCU+4)]	# sine(dec)
define	FC_W		Memd[P2D($1+FCU+6)+($2)-1] # W (CRVAL) for each axis


# WF_GLS_INIT -- Initialize the global sinusoidal forward or inverse transform.
# Initialization for this transformation consists of determining which axis
# is RA and which is DEC, and precomputing the sine and cosine of the
# declination at the reference point.  In order to determine the axis order,
# the parameter "axtype={ra|dec}" must have been set in the attribute list
# for the function.
# NOTE:  This is identical to wf_tan_init.

procedure wf_gls_init (fc, dir)

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


# WF_GLS_FWD -- Forward transform (physical to world), global sinusoidal
# projection.  Based on code from Eric Greisen, AIPS Memo No. 27.

procedure wf_gls_fwd (fc, p, w)

pointer	fc			#I pointer to FC descriptor
double	p[2]			#I physical coordinates (xi, eta)
double	w[2]			#O world coordinates (ra, dec)

int	ira, idec
double	ra, dec, cosdec

begin
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	dec = p[idec] + FC_W(fc,idec)			# still in degrees

	cosdec = cos (DEGTORAD(dec))
	if (cosdec > 0.d0)
	    ra = p[ira] / cosdec + FC_W(fc,ira)		# still in degrees
	else
	    ra = 0.d0

	if (ra < 0.d0)
	    ra = ra + 360.D0
	else if (ra > 360.D0)
	    ra = ra - 360.D0

	w[ira]  = ra
	w[idec] = dec
end


# WF_GLS_INV -- Inverse transform (world to physical) for the global sinusoidal
# projection.  Based on code from Eric Greisen, AIPS Memo No. 27.

procedure wf_gls_inv (fc, w, p)

pointer	fc			#I pointer to FC descriptor
double	w[2]			#I input world (RA, DEC) coordinates
double	p[2]			#O output physical coordinates

int	ira, idec
double	ra, dec

begin
	ira = FC_IRA(fc)
	idec = FC_IDEC(fc)

	ra  = w[ira] - FC_W(fc,ira)
	dec = DEGTORAD (w[idec])

	# Put delta RA on the interval (-180,+180].
	if (ra <= -180.d0)
	    ra = ra + 360.d0
	if (ra > 180.d0)
	    ra = ra - 360.d0

	p[ira]  = ra * cos (dec)
	p[idec] = w[idec] - FC_W(fc,idec)
end
