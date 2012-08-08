include	<gset.h>
include	<smw.h>
include	"identify.h"

# ID_CENTER -- Locate the center of a feature.

double procedure id_center (id, x, n, width, type, interactive)

pointer	id				# ID pointer
double	x[n]				# Initial guess
int	n				# Number of features
real	width				# Feature width
int	type				# Feature type
int	interactive			# Interactive?

int	np1
real	value

real	center1d()
double	smw_c1trand()

begin
	switch (type) {
	case EMISSION, ABSORPTION:
	    np1 = NP1(ID_SH(id)) - 1
	    value = smw_c1trand (ID_PL(id), x[1]) - np1
	    value = center1d (value, IMDATA(id,1), ID_NPTS(id),
		width, type, ID_CRADIUS(id), ID_THRESHOLD(id))
	    if (IS_INDEF(value))
		return (INDEFD)
	    else
		return (smw_c1trand (ID_LP(id), double(value+np1)))
	case GEMISSION, GABSORPTION:
	    iferr (call id_gcenter (id, x, n, INDEF, INDEF, INDEF, INDEF,
		width, type, interactive))
		return (INDEFD)
	    return (x[1])
	}
end


# ID_GCENTER -- Locate the center of a feature.

procedure id_gcenter (id, x, ng, xa, ya, xb, yb, width, type, interactive)

pointer	id				# ID pointer
double	x[ng]				# Initial guess
int	ng				# Number of features
real	xa, ya, xb, yb			# Background points
real	width				# Feature width
int	type				# Feature type
int	interactive			# Draw gaussian fit?

int	i, np1, x1, x2
real	ag, bg, pix, w, y
pointer	sp, xr, xg, yg, sg, gp

bool	fp_equalr()
real	id_model()
double	smw_c1trand(), id_fitpt()

errchk	gcenter1d

begin
	call smark (sp)
	call salloc (xr, ng, TY_REAL)
	call salloc (xg, ng, TY_REAL)
	call salloc (yg, ng, TY_REAL)
	call salloc (sg, ng, TY_REAL)

	np1 = NP1(ID_SH(id)) - 1

	# Compute background in logical units.
	if (IS_INDEF(xa) || IS_INDEF(ya) || IS_INDEF(xb) || IS_INDEF(yb)) {
	    ag = INDEF
	    bg = INDEF
	} else {
	    ag = smw_c1trand (ID_PL(id), double(xa)) - np1
	    bg = smw_c1trand (ID_PL(id), double(xb)) - np1
	    if (!fp_equalr (ag, bg)) {
		bg = (yb - ya) / (bg - ag)
		ag = ya - bg * ag
	    } else {
		ag = INDEF
		bg = INDEF
	    }
	}

	do i = 1, ng
	    Memr[xr+i-1] = smw_c1trand (ID_PL(id), x[i]) - np1
 	call gcenter1d (Memr[xr], ng, IMDATA(id,1), ID_NPTS(id),
	    width, type, ID_CRADIUS(id), ID_THRESHOLD(id),
	    x1, x2, Memr[xg], Memr[yg], Memr[sg], ag, bg)
	do i = 1, ng
	    x[i] = smw_c1trand (ID_LP(id), double(Memr[xg+i-1]+np1))

	if (interactive == YES) {
	    gp = ID_GP(id)
	    call gseti (gp, G_PLTYPE, 2)
	    call gseti (gp, G_PLCOLOR, 2)
	    pix = x1
	    w = id_fitpt (id, smw_c1trand (ID_LP(id), double (pix+np1)))
	    y = id_model (pix, Memr[xg], Memr[yg], Memr[sg], ng) + ag +
		bg * pix
	    call gamove (gp, w, y)
	    for (pix = x1; pix <= x2; pix = pix + .1) {
		w = id_fitpt (id, smw_c1trand (ID_LP(id), double (pix+np1)))
		y = id_model (pix, Memr[xg], Memr[yg], Memr[sg], ng) +
		    ag + bg * pix
		call gadraw (gp, w, y)
	    }
	    call gseti (gp, G_PLTYPE, 3)
	    call gseti (gp, G_PLCOLOR, 3)
	    pix = x1
	    w = id_fitpt (id, smw_c1trand (ID_LP(id), double (pix+np1)))
	    y = ag + bg * pix
	    call gamove (gp, w, y)
	    pix = x2
	    w = id_fitpt (id, smw_c1trand (ID_LP(id), double (pix+np1)))
	    y = ag + bg * pix
	    call gadraw (gp, w, y)
	    call gseti (gp, G_PLTYPE, 1)
	    call gseti (gp, G_PLCOLOR, 1)
	    call gflush (gp)
	}

	call sfree (sp)
end


define MIN_WIDTH	3.		# Minimum centering width


# GCENTER1D -- Locate the center of a one dimensional feature by guassian fit.
# A value of INDEF is returned in the centering fails for any reason.
# This procedure just sets up the data and adjusts for emission or
# absorption features.  The actual centering is done by GFIT.
# If width <= 1 return the nearest minima or maxima.

procedure gcenter1d (x, ng, data, npts, width, type, radius, threshold,
	x1, x2, xg, yg, sg, ag, bg)

real	x[ng]				# Initial guess
int	ng				# Number of gaussians
real	data[npts]			# Data points
int	npts				# Number of data points
real	width				# Feature width
int	type				# Feature type
real	radius				# Centering radius
real	threshold			# Minimum range in feature
int	x1, x2				# Fitting region
real	xg[ng], yg[ng], sg[ng], ag, bg	# Gaussian parameters

int	i, nx, xa, xb
real	a, b, c, d, rad, wid, ya, yb, chisq
pointer	xfit

errchk	id_mr_dofit

begin
	# Check starting values.
	do i = 1, ng
	    if (IS_INDEF(x[i]) || (x[i] < 1) || (x[i] > npts))
		call error (1, "Invalid starting values")

	# Set minimum width and error radius.  The minimum in the error radius
	# is for defining the data window.  The user error radius is used to
	# check for an error in the derived center at the end of the centering.

	wid = max (width, MIN_WIDTH)
	rad = max (2., radius)

	# Determine the pixel value range around the initial center, including
	# the width and error radius buffer.  Check for a minimum range.

	call alimr (x, ng, c, d)
	x1 = max (1., c - wid / 2 - rad - wid)
	x2 = min (real (npts), d + wid / 2 + rad + wid + 1)
	nx = x2 - x1 + 1
	call alimr (data[x1], nx, a, b)
	if (b - a < threshold)
	    call error (1, "Data range below threshold")

	# Allocate memory for the continuum subtracted data vector.  The X
	# range is just large enough to include the error radius and the
	# half width.

	x1 = max (1., c - wid / 2 - rad)
	x2 = min (real (npts), d + wid / 2 + rad + 1)
	nx = x2 - x1 + 1

	# Make the centering data positive, subtract the continuum, and
	# apply a threshold to eliminate noise spikes.

	xa = nint(c)
	ya = data[xa]
	xb = nint(d)
	yb = data[xb]
	switch (type) {
	case GEMISSION:
	    for (i = xa; i >= x1; i=i-1)
		if (data[i] < ya) {
		    xa = i
		    ya = data[i]
		}
	    for (i = xb; i <= x2; i=i+1)
		if (data[i] < yb) {
		    xb = i
		    yb = data[i]
		}
	case GABSORPTION:
	    for (i = xa; i >= x1; i=i-1)
		if (data[i] > ya) {
		    xa = i
		    ya = data[i]
		}
	    for (i = xb; i <= x2; i=i+1)
		if (data[i] > yb) {
		    xb = i
		    yb = data[i]
		}
	default:
	    call error (0, "Unknown feature type")
	}

	# Set initial gaussian parameters.
	if (IS_INDEF(ag) || IS_INDEF(bg)) {
	    if (xa == xb)
		call error (1, "Can't determine background")
	    bg = (yb-ya) / (xb-xa)
	    ag = ya - bg * xa
	}
	do i = 1, ng {
	    xg[i] = x[i]
	    yg[i] = data[nint(x[i])] - ag - bg * x[i]
	    sg[i] = width / 6.
	}

	# Determine the center.
	call malloc (xfit, nx, TY_REAL)
	do i = x1, x2
	    Memr[xfit+i-x1] = i

	call id_mr_dofit (0, 3, 3, Memr[xfit], data[x1], nx,
	    ag, bg, xg, yg, sg, ng, chisq)

	# Check user centering error radius.
	do i = 1, ng {
	    if (!IS_INDEF(xg[i])) {
		if (abs (x[i] - xg[i]) > radius)
		    call error (2, "Error radius exceeded")
	    }
	}

	# Free memory and return the center position.
	call mfree (xfit, TY_REAL)
end
