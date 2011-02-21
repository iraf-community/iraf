include	<error.h>
include	<math.h>

define	GR_LEN		24
define	GR_W		Memr[P2R($1)]	# Wavelength
define	GR_O		Memi[$1+1]	# Order
define	GR_P		Memr[P2R($1+2)]	# Blaze peak scale factor
define	GR_WB		Memr[P2R($1+3)]	# First order wavelength at blaze (A)
define	GR_DB		Memr[P2R($1+4)]	# First order dispersion at blaze (A/mm)
define	GR_OREF		Memi[$1+5]	# Reference order
define	GR_F		Memr[P2R($1+6)]	# Focal length (mm)
define	GR_G		Memr[P2R($1+7)]	# Ruling (groves/A)
define	GR_BLAZE	Memr[P2R($1+8)]	# Blaze angle (rad)
define	GR_N		Memr[P2R($1+9)]	# Index of refraction
define	GR_PHI		Memr[P2R($1+10)]	# Alpha - beta (rad)
define	GR_ALPHA	Memr[P2R($1+11)]	# Incident angle (rad)
define	GR_BETA		Memr[P2R($1+12)]	# Diffraction angle (rad)
define	GR_TYPE		Memr[P2R($1+13)]	# 1=Reflection, -1=Transmission
define	GR_FULL		Memi[$1+14]		# Full solution?

define	GR_PIS		Memr[P2R($1+15)]	# PI/G*S
define	GR_CA		Memr[P2R($1+16)]	# cos (ALPHA)
define	GR_SA		Memr[P2R($1+17)]	# sin (ALPHA)
define	GR_CB		Memr[P2R($1+18)]	# cos (BETA)
define	GR_TB		Memr[P2R($1+19)]	# tan (BETA)
define	GR_SE		Memr[P2R($1+20)]	# sin (ALPHA - BLAZE)
define	GR_CE		Memr[P2R($1+21)]	# cos (ALPHA - BLAZE)
define	GR_CBLZ		Memr[P2R($1+22)]	# cos (BLAZE)
define	GR_T2BLZ	Memr[P2R($1+23)]	# tan (2 * BLAZE)


# Definitions of INDEF parameter flags.
define	F	1B		# Focal length
define	G	2B		# Groves	
define	T	4B		# Blaze angle
define	A	10B		# Incident angle
define	B	20B		# Diffracted angle
define	P	40B		# Incident - diffracted
define	W	100B		# Wavelength
define	D	200B		# Dispersions
define	N	400B		# Index of refraction

# Combinations
define	FG	3B
define	FT	5B
define	FA	11B
define	FW	101B
define	GT	6B
define	GA	12B
define	GW	102B
define	GD	202B
define	TA	14B
define	TAW	114B
define	TAD	214B
define	TB	24B
define	TP	44B
define	TW	104B
define	TD	204B
define	AB	30B
define	AP	50B
define	AW	110B
define	AD	210B
define	BP	140B
define	WD	300B
define	ABP	70B
define	GTA	16B


# GRATING_OPEN -- Open grating structure.
# Check and derive grating parameters.
#
# This procedure hasn't yet been fixed up for grisms (index of refraction
# is not accounted for) so all input parameters should be defined with
# alpha=beta=blaze=prism angle.

pointer procedure gr_open (w, o, p, wb, db, oref, f, gmm, blaze, n, phi,
	alpha, beta, mode, full)

real	w		#I Wavelength (A)
int	o		#I Order
real	p		#I Blaze peak scale factor
real	f		#I Focal length (mm)
real	wb		#I Blaze wavelength (A)
real	db		#I Blaze dispersion (A/mm)
int	oref		#I Reference order
real	gmm		#I Groves (groves/mm)
real	blaze		#I Blaze angle (deg)
real	n		#I Index of refraction for grism
real	phi		#I Incident - diffracted (deg)
real	alpha		#I Incident angle (deg)
real	beta		#I Diffracted angle (deg)
int	mode		#I 1 = incident > diffracted
int	full		#I Do full solution?
pointer	gr		#O Grating pointer

int	flags
real	x
define	err_	10

begin
	call malloc (gr, GR_LEN, TY_STRUCT)

	GR_W(gr) = w
	GR_O(gr) = o
	GR_P(gr) = p

	GR_F(gr) = f
	GR_G(gr) = gmm
	GR_BLAZE(gr) = blaze
	GR_N(gr) = n
	GR_PHI(gr) = phi
	GR_ALPHA(gr) = alpha
	GR_BETA(gr) = beta

	GR_OREF(gr) = oref
	GR_WB(gr) = wb
	GR_DB(gr) = db

	# The grating is reflection unless the index of refraction is not
	# 1, the blaze dispersion is negative, beta is greater than
	# 180 degrees, or the incident and diffraction angles are the same.

	if (GR_N(gr) == 1.)
	    GR_TYPE(gr) = 1
	else if (!IS_INDEF(GR_N(gr)))
	    GR_TYPE(gr) = -1
	else {
	    if (!IS_INDEF(GR_BETA(gr))
		&& (GR_BETA(gr) > 180. || GR_BETA(gr) < -180))
		GR_TYPE(gr) = -1
	    else if (!IS_INDEF(GR_DB(gr)) && GR_DB(gr) < 0.)
		GR_TYPE(gr) = -1
	    else if (!IS_INDEF(GR_ALPHA(gr)) && !IS_INDEF(GR_BETA(gr)) &&
		GR_ALPHA(gr) == GR_BETA(gr))
		GR_TYPE(gr) = -1
	    else if (GR_PHI(gr) == 0.)
		GR_TYPE(gr) = -1
	    else
		GR_TYPE(gr) = 1
	}

	# Set INDEF values to reasonable defaults.  Convert degrees to radians.
	if (IS_INDEF(GR_P(gr)))
	    GR_P(gr) = 1
	if (!IS_INDEF(GR_WB(gr))) {
	    if (GR_WB(gr) <= 0.)
		GR_WB(gr) = INDEF
	    else
		GR_WB(gr) = GR_WB(gr) * GR_OREF(gr)
	}
	if (!IS_INDEF(GR_DB(gr)))
	    GR_DB(gr) = GR_TYPE(gr) * GR_OREF(gr) * abs (GR_DB(gr))
	if (!IS_INDEF(GR_F(gr))) {
	    if (GR_F(gr) <= 0.)
		GR_F(gr) = INDEF
	}
	if (!IS_INDEF(GR_G(gr))) {
	    if (GR_G(gr) <= 0.)
		GR_G(gr) = INDEF
	    else
		GR_G(gr) = GR_G(gr) / 1e7
	}
	if (!IS_INDEF(GR_PHI(gr)))
	    GR_PHI(gr) = DEGTORAD (GR_PHI(gr))
	if (!IS_INDEF(GR_ALPHA(gr)))
	    GR_ALPHA(gr) = DEGTORAD (GR_ALPHA(gr))
	if (!IS_INDEF(GR_BETA(gr))) {
	    GR_BETA(gr) = DEGTORAD (GR_BETA(gr))
	    if (GR_BETA(gr) > HALFPI)
		GR_BETA(gr) = GR_BETA(gr) - PI
	    else if (GR_BETA(gr) < -HALFPI)
		GR_BETA(gr) = GR_BETA(gr) + PI
	}
	if (!IS_INDEF(GR_BLAZE(gr)))
	    GR_BLAZE(gr) = DEGTORAD (GR_BLAZE(gr))

	# Compute missing angles, if possible,  based on the other angles.
	# This assumes the given values are for the blaze peak.

	flags = 0
	if (IS_INDEF(GR_BLAZE(gr)))
	    flags = flags + T
	if (IS_INDEF(GR_ALPHA(gr)))
	    flags = flags + A
	if (IS_INDEF(GR_BETA(gr)))
	    flags = flags + B
	if (IS_INDEF(GR_PHI(gr)))
	    flags = flags + P

	switch (flags) {
	case T, P, TP:
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_BLAZE(gr) = (GR_ALPHA(gr) + GR_BETA(gr)) / 2.
	case A, TA:
	    GR_ALPHA(gr) = GR_BETA(gr) + GR_PHI(gr)
	    GR_BLAZE(gr) = (GR_ALPHA(gr) + GR_BETA(gr)) / 2.
	case AB:
	    if (mode == 1) {
		GR_ALPHA(gr) = GR_BLAZE(gr) + GR_PHI(gr)/2.
		GR_BETA(gr) = GR_BLAZE(gr) - GR_PHI(gr)/2.
	    } else {
		GR_ALPHA(gr) = GR_BLAZE(gr) - GR_PHI(gr)/2.
		GR_BETA(gr) = GR_BLAZE(gr) + GR_PHI(gr)/2.
	    }
	case AP:
	    GR_ALPHA(gr) = 2 * GR_BLAZE(gr) - GR_BETA(gr)
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	case B, TB:
	    GR_BETA(gr) = GR_ALPHA(gr) - GR_PHI(gr)
	    GR_BLAZE(gr) = (GR_ALPHA(gr) + GR_BETA(gr)) / 2.
	case BP:
	    GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	case ABP:
	    GR_ALPHA(gr) = GR_BLAZE(gr)
	    GR_BETA(gr) = GR_BLAZE(gr)
	    GR_PHI(gr) = 0.
	}

	# Compute index of refraction if possible.
	if (IS_INDEF(GR_N(gr))) {
	    if (GR_TYPE(gr) == 1)
		GR_N(gr) = 1
	    else if (!IS_INDEF(GR_WB(gr)) && !IS_INDEF(GR_G(gr)) &&
		!IS_INDEF(GR_BLAZE(gr)))
		GR_N(gr) = (GR_G(gr) * GR_WB(gr)) / sin (GR_BLAZE(gr)) + 1
	}

	# Compute other parameters if possible.
	flags = 0
	if (IS_INDEF(GR_F(gr)))
	    flags = flags + F
	if (IS_INDEF(GR_G(gr)))
	    flags = flags + G
	if (IS_INDEF(GR_BLAZE(gr)))
	    flags = flags + T
	if (IS_INDEF(GR_ALPHA(gr)))
	    flags = flags + A
	if (IS_INDEF(GR_WB(gr)))
	    flags = flags + W
	if (IS_INDEF(GR_DB(gr)))
	    flags = flags + D
	if (IS_INDEF(GR_N(gr)))
	    flags = flags + N

	switch (flags) {
	case 0, F, G, T, A, N, W, D:
	    switch (flags) {
	    case F:
		GR_F(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) /
		    (GR_G(gr) * GR_DB(gr))
	    case G: 
		GR_G(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		    GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_WB(gr)
		if (GR_G(gr) == 0.)
		    GR_G(gr) = INDEF
	    case T:
		if (GR_ALPHA(gr) > PI) {
		    x = GR_G(gr) * GR_WB(gr) / (2 * cos (GR_ALPHA(gr)))
		    if (abs (x) > 1.)
			goto err_
		    GR_BLAZE(gr) = asin (x)
		    GR_ALPHA(gr) = GR_ALPHA(gr) - TWOPI + GR_BLAZE(gr)
		} else {
		    x = GR_G(gr) * GR_WB(gr) - GR_N(gr) * sin (GR_ALPHA(gr))
		    if (abs (x) > 1.)
			goto err_
		    GR_BLAZE(gr) = (GR_ALPHA(gr) + asin (x)) / 2
		}
		GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
		GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    case A:
		x = GR_TYPE(gr) * GR_G(gr) * GR_WB(gr) / (2 * sin(GR_BLAZE(gr)))
		if (abs (x) > 1.)
		    goto err_
		if (mode == 1) {
		    GR_ALPHA(gr) = GR_BLAZE(gr) + acos (x)
		    GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
		} else {
		    GR_BETA(gr) = GR_BLAZE(gr) + acos (x)
		    GR_ALPHA(gr) = 2 * GR_BLAZE(gr) - GR_BETA(gr)
		}
		GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    case N:
		GR_N(gr) = (GR_G(gr) * GR_WB(gr)) / sin (GR_BLAZE(gr)) + 1
	    }
	    GR_WB(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_G(gr)
	    GR_DB(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_F(gr)*GR_G(gr))
	case FG:
	    x = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_WB(gr)
	    if (x == 0.)
		goto err_
	    GR_G(gr) = x
	    GR_F(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_G(gr) * GR_DB(gr))
	case FT:
	    if (GR_ALPHA(gr) > PI) {
		x = GR_TYPE(gr) * GR_G(gr) * GR_WB(gr) /
		    (2 * cos (GR_ALPHA(gr)))
		if (abs (x) > 1.)
		    goto err_
		GR_BLAZE(gr) = asin (x)
		GR_ALPHA(gr) = GR_ALPHA(gr) - TWOPI + GR_BLAZE(gr)
	    } else {
		x = GR_TYPE(gr) * GR_G(gr) * GR_WB(gr) -
		    GR_N(gr) * sin (GR_ALPHA(gr))
		if (abs (x) > 1.)
		    goto err_
		GR_BLAZE(gr) = (GR_ALPHA(gr) + asin (x)) / 2
	    }
	    GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_F(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_G(gr) * GR_DB(gr))
	case FA:
	    x = GR_TYPE(gr) * GR_G(gr) * GR_WB(gr) / (2 * sin (GR_BLAZE(gr)))
	    if (abs (x) > 1.)
		goto err_
	    if (mode == 1) {
		GR_ALPHA(gr) = GR_BLAZE(gr) + acos (x)
		GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    } else {
		GR_BETA(gr) = GR_BLAZE(gr) + acos (x)
		GR_ALPHA(gr) = 2 * GR_BLAZE(gr) - GR_BETA(gr)
	    }
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_F(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_G(gr) * GR_DB(gr))
	case FW:
	    GR_WB(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_G(gr)
	    GR_F(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_G(gr) * GR_DB(gr))
	case GT:
	    x = GR_TYPE(gr) * GR_F(gr) * GR_DB(gr) / GR_WB(gr)
	    if (GR_ALPHA(gr) > PI) {
		GR_BLAZE(gr) = atan (1 / (2 * x - tan (GR_ALPHA(gr))))
		GR_ALPHA(gr) = GR_ALPHA(gr) - TWOPI + GR_BLAZE(gr)
	    } else {
		x = (tan (GR_ALPHA(gr)) - x) / (1 + 2 * x * tan (GR_ALPHA(gr)))
		GR_BLAZE(gr) = atan (x + sqrt (1 + x * x))
	    }
	    GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_G(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_WB(gr)
	case GA:
	    GR_ALPHA(gr) = GR_BLAZE(gr) +
		atan (2 * GR_TYPE(gr) * GR_F(gr) * GR_DB(gr) /
		GR_WB(gr) - 1 / tan (GR_BLAZE(gr)))
	    GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_G(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_WB(gr)
	case GW:
	    GR_G(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_F(gr) * GR_DB(gr))
	    if (GR_G(gr) == 0.)
		GR_G(gr) = INDEF
	    else
		GR_WB(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		    GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_G(gr)
	case GD:
	    x = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_WB(gr)
	    if (x == 0.)
		goto err_
	    GR_G(gr) = x
	    GR_DB(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_F(gr) * GR_G(gr))
	case TAD:
	    if (IS_INDEF(GR_PHI(gr)))
		GR_PHI(gr) = 0.
	    x = GR_G(gr) * GR_WB(gr) / (2 * cos (GR_PHI(gr)/2))
	    if (mode == 1) {
		GR_ALPHA(gr) = GR_PHI(gr)/2 + asin (x)
		GR_BETA(gr) = GR_ALPHA(gr) - GR_PHI(gr)
	    } else {
		GR_BETA(gr) = GR_PHI(gr)/2 + asin (x)
		GR_ALPHA(gr) = GR_BETA(gr) - GR_PHI(gr)
	    }
	    GR_BLAZE(gr) = (GR_ALPHA(gr) + GR_BETA(gr)) / 2
	    GR_DB(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) /
		(GR_F(gr) * GR_G(gr))
	case TAW:
	    if (!IS_INDEF(GR_PHI(gr))) {
		x = GR_TYPE(gr) * GR_F(gr) * GR_G(gr) * GR_DB(gr)
		if (abs (x) > 1.) {
		    if (abs (x) > 1.1)
			goto err_
		    else {
			x = 1. 
			GR_DB(gr) = x / (GR_F(gr) * GR_G(gr))
		    }
		}
		if (mode == 1) {
		    GR_BETA(gr) = acos (x)
		    GR_ALPHA(gr) = GR_BETA(gr) + GR_PHI(gr) 
		} else {
		    GR_ALPHA(gr) = acos (x)
		    GR_BETA(gr) = GR_ALPHA(gr) + GR_PHI(gr) 
		}
		GR_BLAZE(gr) = (GR_ALPHA(gr) + GR_BETA(gr)) / 2
		GR_WB(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		    GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_G(gr)
	    }
	case TA:
	    if (!IS_INDEF(GR_PHI(gr))) {
		x = GR_G(gr) * GR_WB(gr) / (2 * cos (GR_PHI(gr)/2))
		if (mode == 1) {
		    GR_ALPHA(gr) = GR_PHI(gr)/2 + asin (x)
		    GR_BETA(gr) = GR_ALPHA(gr) - GR_PHI(gr)
		} else {
		    GR_BETA(gr) = GR_PHI(gr)/2 + asin (x)
		    GR_ALPHA(gr) = GR_BETA(gr) - GR_PHI(gr)
		}
		GR_BLAZE(gr) = (GR_ALPHA(gr) + GR_BETA(gr)) / 2
		GR_DB(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) /
		    (GR_F(gr) * GR_G(gr))
	    } else {
		x = GR_TYPE(gr) * GR_F(gr) * GR_G(gr) * GR_DB(gr)
		if (abs (x) > 1.) {
		    if (abs (x) > 1.1)
			goto err_
		    else {
			x = 1. 
			GR_DB(gr) = x / (GR_F(gr) * GR_G(gr))
		    }
		}
		GR_BETA(gr) = acos (x)
		x = GR_G(gr) * GR_WB(gr) - GR_TYPE(gr) * sin (GR_BETA(gr))
		if (abs (x) > 1.)
		    goto err_
		GR_ALPHA(gr) = asin (x)
		GR_BLAZE(gr) = (acos (GR_TYPE(gr) * GR_F(gr) * GR_G(gr) *
		    GR_DB(gr)) + GR_ALPHA(gr)) / 2
		GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
		GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    }
	case TW:
	    GR_BLAZE(gr) = (GR_ALPHA(gr) +
		acos (GR_TYPE(gr) * GR_F(gr) * GR_G(gr) * GR_DB(gr))) / 2
	    GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_WB(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_G(gr)
	case TD:
	    if (GR_ALPHA(gr) > PI) {
		x = GR_G(gr) * GR_WB(gr) / (2 * cos (GR_ALPHA(gr)))
		if (abs (x) > 1.)
		    goto err_
		GR_BLAZE(gr) = asin (x)
		GR_ALPHA(gr) = GR_ALPHA(gr) - TWOPI + GR_BLAZE(gr)
	    } else {
		x = GR_G(gr) * GR_WB(gr) - GR_N(gr) * sin (GR_ALPHA(gr))
		if (abs (x) > 1.)
		    goto err_
		GR_BLAZE(gr) = (GR_ALPHA(gr) + asin (x)) / 2
	    }
	    GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_DB(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_F(gr) * GR_G(gr))
	case AW:
	    x = GR_TYPE(gr) * GR_F(gr) * GR_G(gr) * GR_DB(gr)
	    if (abs (x) > 1.)
		goto err_
	    GR_ALPHA(gr) = 2 * GR_BLAZE(gr) - acos (x)
	    GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_WB(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) + sin (GR_BETA(gr))) /
		GR_G(gr)
	case AD:
	    x = GR_G(gr) * GR_WB(gr) / (2 * sin (GR_BLAZE(gr)))
	    if (abs (x) > 1.)
		goto err_
	    if (mode == 1) {
		GR_ALPHA(gr) = GR_BLAZE(gr) + acos (x)
		GR_BETA(gr) = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    } else {
		GR_BETA(gr) = GR_BLAZE(gr) + acos (x)
		GR_ALPHA(gr) = 2 * GR_BLAZE(gr) - GR_BETA(gr)
	    }
	    GR_PHI(gr) = GR_ALPHA(gr) - GR_BETA(gr)
	    GR_DB(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_F(gr) * GR_G(gr))
	case WD:
	    GR_WB(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_G(gr)
	    GR_DB(gr) = GR_TYPE(gr) * cos (GR_BETA(gr)) / (GR_F(gr) * GR_G(gr))
	case GTA:
	    # Assume beta=alpha=blaze.
	    x = (GR_TYPE(gr) * GR_WB(gr)) /
		((GR_N(gr) + GR_TYPE(gr)) * GR_F(gr) * GR_DB(gr))
	    GR_BETA(gr) = atan (x)
	    GR_ALPHA(gr) = GR_BETA(gr)
	    GR_BLAZE(gr) = GR_BETA(gr)
	    GR_PHI(gr) = 0
	    GR_G(gr) = (GR_N(gr) * sin (GR_ALPHA(gr)) +
		GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_WB(gr)
	}

	# The result should give the blaze wavelength and dispersion.
	# If this cannot be computed then it is an error.

	if (IS_INDEF(GR_WB(gr)) || IS_INDEF(GR_DB(gr))) {
	    call gr_close (gr)
	    call mfree (gr, TY_STRUCT)
	    call error (1,
		"Insufficient information to to resolve grating parameters")
	}
			   
	# If all the other parameters cannot be computed then use linear.
	if (full==NO || IS_INDEF(GR_F(gr)) || IS_INDEF(GR_G(gr)) ||
	    IS_INDEF(GR_BETA(gr)) || IS_INDEF(GR_PHI(gr)) ||
	    IS_INDEF(GR_N(gr))) {
	    GR_FULL(gr) = NO
	    if (IS_INDEF(GR_F(gr)))
		GR_F(gr) = 1
	    GR_W(gr) = GR_WB(gr)
	    GR_O(gr) = GR_OREF(gr)
	    GR_CE(gr) = PI * GR_DB(gr)
	    GR_CA(gr) = 1.
	    GR_CB(gr) = 1.

	# A full grating solution is possible.
	} else {
	    GR_FULL(gr) = YES

	    # Set the order and wavelength to the blaze values if not given.
	    if (IS_INDEF(GR_W(gr))) {
		if (!IS_INDEFI(GR_O(gr)))
		    GR_W(gr) = GR_WB(gr) / GR_O(gr)
		else
		    GR_W(gr) = GR_WB(gr) / GR_OREF(gr)
	    }
	    if (IS_INDEFI(GR_O(gr)))
		GR_O(gr) = max (1, nint (GR_WB(gr) / GR_W(gr)))

	    # Convert to incident angle at desired wavelength.
	    if (GR_PHI(gr) != 0.) {
		x = GR_G(gr) * GR_O(gr) * GR_W(gr) /
		    (2 * cos (GR_PHI(gr)/2))
		if (abs (x) > 1.)
		    goto err_
		if (mode == 1) {
		    GR_ALPHA(gr) = asin (x) + GR_PHI(gr) / 2
		    GR_BETA(gr) = asin (x) - GR_PHI(gr) / 2
		} else {
		    GR_ALPHA(gr) = asin (x) - GR_PHI(gr) / 2
		    GR_BETA(gr) = asin (x) + GR_PHI(gr) / 2
		}
	    } else {
		x = (GR_G(gr) * GR_O(gr) * GR_W(gr) -
		    GR_TYPE(gr) * sin (GR_BETA(gr))) / GR_N(gr)
		if (abs (x) > 1.)
		    goto err_
		GR_ALPHA(gr) = asin (x)
	    }

	    # The following parameters are for efficiency.  Beta terms are
	    # for the blaze peak diffraction.

	    x = 2 * GR_BLAZE(gr) - GR_ALPHA(gr)
	    GR_CA(gr) = cos (GR_ALPHA(gr))
	    GR_SA(gr) = sin (GR_ALPHA(gr))
	    GR_CB(gr) = GR_TYPE(gr) * cos (x)
	    GR_TB(gr) = tan (x)
	    GR_SE(gr) = sin (GR_ALPHA(gr) - GR_BLAZE(gr))
	    GR_CE(gr) = cos (GR_ALPHA(gr) - GR_BLAZE(gr))
	    GR_CBLZ(gr) = cos (GR_BLAZE(gr))
	    GR_T2BLZ(gr) = tan (2 * GR_BLAZE(gr))
	    if (GR_ALPHA(gr) > x)
		GR_PIS(gr) = PI / GR_G(gr) * GR_CA(gr)
	    else
		GR_PIS(gr) = PI / GR_G(gr) * GR_CBLZ(gr)
	}

	return (gr)

err_	call error (2, "Impossible combination of grating parameters")
end


# GR_CLOSE -- Free grating structure.

procedure gr_close (gr)

pointer	gr		#I Grating pointer

begin
	call mfree (gr, TY_STRUCT)
end

# GR_GETR -- Get grating parameter.

real procedure gr_getr (gr, param)

pointer	gr		#I Grating pointer
char	param[ARB]	#I Parameter name

bool	streq()

begin
	if (gr == NULL)
	    return (INDEF)

	switch (param[1]) {
	case 'a':
	    if (streq (param, "alpha")) {
		if (IS_INDEFR(GR_ALPHA(gr)))
		    return (GR_ALPHA(gr))
		else
		    return (RADTODEG(GR_ALPHA(gr)))
	    }
	case 'b':
	    if (streq (param, "blaze")) {
		if (IS_INDEFR(GR_BLAZE(gr)))
		    return (GR_BLAZE(gr))
		else
		    return (RADTODEG(GR_BLAZE(gr)))
	    } else if (streq (param, "beta")) {
		if (IS_INDEFR(GR_BETA(gr)))
		    return (GR_BETA(gr))
		else
		    return (RADTODEG(GR_BETA(gr)))
	    }
	case 'd':
	    if (streq (param, "dblaze"))
		return (GR_DB(gr) / GR_OREF(gr))
	    if (streq (param, "dispersion")) {
		if (GR_FULL(gr) == NO)
		    return (GR_DB(gr) / GR_O(gr))
		else
		    return (GR_CB(gr) / (GR_G(gr) * GR_O(gr) *GR_F(gr)))
	    }
	case 'f':
	    if (streq (param, "full"))
		return (real (GR_FULL(gr)))
	    else if (streq (param, "f"))
		return (GR_F(gr))
	case 'g':
	    if (streq (param, "g")) {
		if (IS_INDEFR(GR_G(gr)))
		    return (GR_G(gr))
		else
		    return (GR_G(gr) * 1E7)
	    }
	case 'm':
	    if (streq (param, "mag"))
		return (GR_CA(gr) / GR_CB(gr))
	case 'n':
	    if (streq (param, "n"))
		return (GR_N(gr))
	case 'o':
	    if (streq (param, "order"))
		return (real (GR_O(gr)))
	    if (streq (param, "oref"))
		return (real (GR_OREF(gr)))
	case 'p':
	    if (streq (param, "phi")) {
		if (IS_INDEFR(GR_PHI(gr)))
		    return (GR_PHI(gr))
		else
		    return (RADTODEG(GR_PHI(gr)))
	    }
	case 't':
	    if (streq (param, "tilt")) {
		if (GR_FULL(gr) == NO)
		    return (INDEFR)
		else
		    return (RADTODEG((GR_ALPHA(gr)+GR_TYPE(gr)*GR_BETA(gr))/2))
	    }
	case 'w':
	    if (streq (param, "wavelength"))
		return (GR_W(gr))
	    if (streq (param, "wblaze"))
		return (GR_WB(gr) / GR_OREF(gr))
	}
	call error (1, "gr_getr: unknown parameter")
end


# GR_LIST -- List grating parameters.

procedure gr_list (gr, order, col)

pointer	gr		#I Grating pointer
int	order		#I Order
int	col		#I Column to indent

begin
	if (gr == NULL)
	    return

	if (GR_FULL(gr) == NO) {
	    call printf ("%*tReference order = %d\n")
		call pargi (col)
		call pargi (order)
	    call printf (
		"%*tBlaze wavelength of reference order = %.6g Angstroms\n")
		call pargi (col)
		call pargr (GR_WB(gr) / order)
	    call printf (
		"%*tBlaze dispersion of reference order = %.4g Angstroms/mm\n")
		call pargi (col)
		call pargr (GR_DB(gr) / order)
	} else  {
	    call printf ("%*tFocal length = %d mm\n")
		call pargi (col)
		call pargr (GR_F(gr))
	    call printf ("%*tGrating = %.1f grooves/mm\n")
		call pargi (col)
		call pargr (GR_G(gr) * 1e7)
	    call printf ("%*tBlaze angle = %.1f degrees\n")
		call pargi (col)
		call pargr (RADTODEG(GR_BLAZE(gr)))
	    call printf ("%*tIncident to diffracted angle = %.1f degrees\n")
		call pargi (col)
		call pargr (RADTODEG(GR_PHI(gr)))
	    call printf ("%*tReference order = %d\n")
		call pargi (col)
		call pargi (order)
	    call printf (
		"%*tBlaze wavelength = %.6g Angstroms\n")
		call pargi (col)
		call pargr (GR_WB(gr) / order)
	    call printf (
		"%*tBlaze dispersion = %.4g Angstroms/mm\n")
		call pargi (col)
		call pargr (GR_DB(gr) / order)

	    call printf ("\n%*tCentral wavelength = %.6g Angstroms\n")
		call pargi (col)
		call pargr (GR_W(gr))
	    call printf ("%*tCentral dispersion = %.4g Angstroms/mm\n")
		call pargi (col)
		call pargr (GR_TYPE(gr) * cos (GR_BETA(gr)) /
		    (GR_G(gr) * order *GR_F(gr)))
	    call printf ("%*tOrder = %d\n")
		call pargi (col)
		call pargi (GR_O(gr))
	    call printf ("%*tTilt = %.1f degrees\n")
		call pargi (col)
		call pargr (RADTODEG(GR_ALPHA(gr) - GR_PHI(gr) / 2))
	    call printf ("%*tGrating magnification = %.2f\n")
		call pargi (col)
		call pargr (GR_CA(gr)/GR_CB(gr))
	    call printf ("%*tIncidence angle = %.1f degrees\n")
		call pargi (col)
		call pargr (RADTODEG(GR_ALPHA(gr)))
	    call printf ("%*tDiffracted angle = %.1f degrees\n")
		call pargi (col)
		call pargr (RADTODEG(GR_BETA(gr)))
	}
end


# GR_W2PSI -- Given wavelength return tan(psi).

real procedure gr_w2psi (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	x		#O Pixel position

real	gr_sinbeta()

begin
	if (GR_FULL(gr) == NO)
	    return ((m*w - GR_WB(gr)) / GR_DB(gr) * GR_F(gr))

	x = gr_sinbeta (gr, w, m)
	if (!IS_INDEF(x)) {
	    x = x / sqrt (1 - x * x)
	    x = (x - GR_TB(gr)) / (1 + x * GR_TB(gr))
	}
	return (x)

end


# GR_W2PSIR -- Given wavelength return tan(psi) of reflected component.

real procedure gr_w2psir (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	x		#O Pixel position

real	gr_sinbeta()

begin
	x = gr_sinbeta (gr, w, m)
	if (!IS_INDEF(x)) {
	    #x = x / sqrt (1 - x * x)
	    #x = (x - GR_TB(gr)) / (1 + x * GR_TB(gr))
	    #x = (x + GR_T2BLZ(gr)) / (1 - x * GR_T2BLZ(gr))
	    x = PI - asin(x)
	    x = 2 * GR_BLAZE(gr) - x - GR_BETA(gr)
	    x = tan (x)
	}
	return (x)
end


# GR_DELTA -- Given pixel position and wavelength return blaze function
# phase angle.

real procedure gr_delta (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	d		#O Delta

real	tanpsi, cospsi, sinpsi, gr_w2psi()

begin
	tanpsi = gr_w2psi (gr, w, m)
	if (IS_INDEF(tanpsi))
	    return (INDEF)

	if (GR_FULL(gr) == NO)
	    d = PI * GR_DB(gr) / w * tanpsi
	else {
	    cospsi = 1 / sqrt (1 + tanpsi * tanpsi)
	    sinpsi = tanpsi * cospsi
	    d = GR_PIS(gr) / w * (GR_CE(gr) * sinpsi + GR_SE(gr) * (1 - cospsi))
	}
	if (abs(d) < 0.01) {
	    if (d < 0)
		d = -0.01
	    else
		d = 0.01
	}
	return (d)
end


# GR_DELTAR -- Blaze function phase angle for reflected component.

real procedure gr_deltar (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	d		#O Delta

real	tanpsi, cospsi, sinpsi, gr_w2psir()

begin
	tanpsi = gr_w2psir (gr, w, m)
	if (IS_INDEF(tanpsi))
	    return (INDEF)

	if (GR_FULL(gr) == NO)
	    d = PI * GR_DB(gr) / w * tanpsi
	else {
	    cospsi = 1 / sqrt (1 + tanpsi * tanpsi)
	    sinpsi = tanpsi * cospsi
	    d = GR_PIS(gr) / w * (GR_CE(gr) * sinpsi + GR_SE(gr) * (1 - cospsi))
	}
	if (abs(d) < 0.01) {
	    if (d < 0)
		d = -0.01
	    else
		d = 0.01
	}
	return (d)
end


# GR_BLAZE -- Blaze pattern at given wavelength.

real procedure gr_blaze (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	val		#O Blaze pattern

real	d, gr_delta()

begin
	d = gr_delta (gr, w, m)
	if (IS_INDEF(d))
	    val = 0.
	else
	    val = (sin (d) / d) ** 2
	return (val)
end


# GR_PEAK -- Blaze peak corrected for light defracted into other orders.

real procedure gr_peak (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength (A)
int	m		#I Order
real	val		#O Blaze peak

int	i, j
real	frac, p
real	gr_delta(), gr_deltar()

begin
	# Find the absolute response of the gratings at the reference
	# blaze peak.

	p = gr_delta (gr, w, m)
	if (IS_INDEF(p))
	    return (INDEF)
	val = (sin (p) / p) ** 2
	frac = 0.
	do i = m - 1, 1, -1 {
	    p = gr_delta (gr, w, i)
	    if (IS_INDEF(p))
		break
	    frac = frac + (sin (p) / p) ** 2
	    if (abs (p) > 1000.)
		break
	}
	do i = m + 1, ARB {
	    p = gr_delta (gr, w, i)
	    if (IS_INDEF(p))
		break
	    frac = frac + (sin (p) / p) ** 2
	    if (abs (p) > 1000.)
		break
	}

	if (GR_FULL(gr) == YES && GR_TYPE(gr) > 0) {
	    j = (GR_N(gr) * GR_SA(gr) + GR_TYPE(gr)) / GR_G(gr) / w
	    do i = j+1, ARB, 1 {
		p = gr_deltar (gr, w, i)
		if (IS_INDEF(p))
		    break
		frac = frac + (sin (p) / p) ** 2
		if (abs (p) > 1000.)
		    break
	    }
	}

	val = val / (val + frac) 

	# Shadowing
	if (GR_ALPHA(gr) < GR_BETA(gr) && GR_TYPE(gr) > 0)
	    val = val * abs (GR_CA(gr) / GR_CB(gr))

	return (val)
end


# GR_EFF -- Efficiency at specified wavelength and order.

real procedure gr_eff (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	eff		#O Efficiency

real	gr_blaze(), gr_peak()

begin
	if (gr == NULL)
	    return (INDEF)

	if (GR_FULL(gr) == NO)
	    return (GR_P(gr))

	eff = gr_blaze (gr, w, m)
	if (eff > 0.)
	    eff =  eff * GR_P(gr) * gr_peak (gr, GR_WB(gr)/m, m)

	return (eff)
end


# GR_W2DW -- Grating dispersion = cos (beta(w,m)) / (g * m * f)
# This is corrected to a detector plane.

real procedure gr_w2dw (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	disp		#O Dispersion (A/mm)

real	gr_sinbeta(), gr_w2x()

begin
	if (GR_FULL(gr) == NO)
	    return (GR_DB(gr) / m)
	
	disp = gr_sinbeta (gr, w, m)
	if (!IS_INDEF(disp)) {
	    disp = sqrt (1 - disp * disp) / (GR_G(gr) * m * GR_F(gr))
	    disp = disp / (1 + (gr_w2x (gr, w, m) / GR_F(gr))**2)
	}
	return (disp)
end


# GR_X2W -- Wavelength at given position relative to center.

real procedure gr_x2w (gr, x, m)

pointer	gr		#I Grating pointer
real	x		#I Pixel position (mm from center)
int	m		#I Order
real	w		#O Wavelength (Angstroms)

begin
	if (GR_FULL(gr) == NO) {
	    w = GR_WB(gr) + GR_DB(gr) / m * x
	    return (w)
	}

	w = x / GR_F(gr)
	w = atan (w) + GR_BETA(gr)
	w = (GR_N(gr) * GR_SA(gr) + GR_TYPE(gr) * sin (w)) /
	    (GR_G(gr) * m)
	return (w)
end


# GR_W2X -- Position at given wavelength.

real procedure gr_w2x (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength (Angstroms)
int	m		#I Order
real	x		#I Pixel position (mm from center)

begin
	if (GR_FULL(gr) == NO) {
	    x = (w - GR_WB(gr)) * m / GR_DB(gr)
	    return (x)
	}

	x = (w * m * GR_G(gr) - GR_N(gr) * GR_SA(gr)) / GR_TYPE(gr)
	x = asin (x) - GR_BETA(gr)
	x = x * GR_F(gr)
	return (x)
end


# GR_MAG -- Grating magnification = cos (alpha) / cos (beta(w,m))

real procedure gr_mag (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	mag		#O mag

real	gr_sinbeta()

begin
	mag = gr_sinbeta (gr, w, m)
	if (!IS_INDEF(mag))
	    mag = GR_CA(gr) / sqrt (1 - mag * mag)
	return (mag)
end


# GR_TILT -- Grating tilt = (alpha + beta) / 2

real procedure gr_tilt (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	tilt		#O tilt

real	gr_sinbeta()

begin
	tilt = gr_sinbeta (gr, w, m)
	if (!IS_INDEF(tilt))
	    tilt = (GR_ALPHA(gr) + GR_TYPE(gr)*asin(tilt)) / 2
	return (tilt)
end


# GR_SINBETA -- sin(beta(w,m)) = g * m * w - n * sin(alpha)
# n is index of refraction which is different from 1 for a grism.

real procedure gr_sinbeta (gr, w, m)

pointer	gr		#I Grating pointer
real	w		#I Wavelength
int	m		#I Order
real	sb		#O sin(beta)

begin
	if (gr == NULL)
	    return (INDEF)
	if (GR_FULL(gr) == NO)
	    return (INDEF)

	sb = (GR_G(gr) * m * w - GR_N(gr) * GR_SA(gr)) / GR_TYPE(gr)
	if (abs(sb) >= 1.)
	    sb = INDEF

	return (sb)
end
