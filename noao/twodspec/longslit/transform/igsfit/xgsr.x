include <math/gsurfit.h>

# XGS -- These routines provide an interface between real input data and
# the double precision surface fitting.  Rather than make the input data
# be double precision we only want the internal surface fitting arithmetic
# to be double.  But the surface fitting package only provides real
# arithmetic for real input and double precision arithmetic for double
# precision input.  Hence these interfaces.  Note that the save and restore
# functions use double precision.

# XGSINIT --  Procedure to initialize the surface descriptor.

procedure xgsinit (sf, surface_type, xorder, yorder, xterms, xmin, xmax,
    ymin, ymax)

pointer	sf		# surface descriptor
int	surface_type	# type of surface to be fitted
int	xorder		# x order of surface to be fit
int	yorder		# y order of surface to be fit
int	xterms		# presence of cross terms
real    xmin		# minimum value of x
real	xmax		# maximum value of x
real	ymin		# minimum value of y
real	ymax		# maximum value of y

begin
	call gsinit (sf, surface_type, xorder, yorder, xterms, xmin, xmax,
	    ymin, ymax)
end


# XGSFIT -- Procedure to solve the normal equations for a surface.

procedure xgsfit (sf, x, y, z, w, npts, wtflag, ier)

pointer	sf		# surface descriptor
real	x[npts]		# array of x values
real	y[npts]		# array of y values
real	z[npts]		# data array
real	w[npts]		# array of weights
int	npts		# number of data points
int	wtflag		# type of weighting
int	ier		# ier = OK, everything OK
			# ier = SINGULAR, matrix is singular, 1 or more
			# coefficients are 0.
			# ier = NO_DEG_FREEDOM, too few points to solve matrix

begin
	call gsfit (sf, x, y, z, w, npts, wtflag, ier)
end


# XGSVECTOR -- Procedure to evaluate the fitted surface at an array of points.

procedure xgsvector (sf, x, y, zfit, npts)

pointer	sf		# pointer to surface descriptor structure
real	x[ARB]		# x value
real	y[ARB]		# y value
real	zfit[ARB]	# fits surface values
int	npts		# number of data points

begin
	call gsvector (sf, x, y, z, npts)
end


# XGSEVAL -- Procedure to evaluate the fitted surface at a single point.

real procedure xgseval (sf, x, y)

pointer	sf		# pointer to surface descriptor structure
real	x		# x value
real	y		# y value

real	gseval()

begin
	return (gseval (sf, x, y))
end


# XGSADD -- Procedure to add the fits from two surfaces together.

procedure xgsadd (sf1, sf2, sf3)

pointer	sf1		# pointer to the first surface
pointer	sf2		# pointer to the second surface
pointer	sf3		# pointer to the output surface

begin
	call gsadd (sf1, sf2, sf3)
end


# XGSFREE -- Procedure to free the surface descriptor

procedure xgsfree (sf)

pointer	sf	# the surface descriptor

begin
	call gsfree (sf)
end


# XGSGCOEFF -- Procedure to fetch a particular coefficient.

real procedure xgsgcoeff (sf, xorder, yorder)

pointer	sf		# pointer to the surface fitting descriptor
int	xorder		# X order of desired coefficent
int	yorder		# Y order of desired coefficent

double	gsgcoeff()

begin
	return (gsgcoeff (sf, xorder, yorder))
end


# XGSSCOEFF -- Procedure to set a particular coefficient.

procedure xgsscoeff (sf, xorder, yorder, coeff)

pointer	sf		# pointer to the surface fitting descriptor
int	xorder		# X order of desired coefficent
int	yorder		# Y order of desired coefficent
real	coeff		# Coefficient value

begin
	call gsscoeff (sf, xorder, yorder, coeff)
end


# XGSGETR -- Procedure to fetch a real gsurfit parameter

real procedure xgsgetr (sf, parameter)

pointer	sf		# pointer to the surface fit
int	parameter	# parameter to be fetched

real	gsgetr()

begin
	return (gsgetr (sf, parameter))
end


# XGSGETI -- Procedure to fetch an integer parameter

int procedure xgsgeti (sf, parameter)

pointer sf		# pointer to the surface fit
int	parameter	# integer parameter

int	gsgeti()

begin
	return (gsgeti (sf, parameter))
end


# XGSSAVE -- Procedure to save the surface fit for later use by the
# evaluate routines.
#
# NOTE THAT THIS USES DOUBLE PRECISION FOR THE COEFFICIENTS.

procedure xgssave (sf, fit)

pointer	sf		# pointer to the surface descriptor
double	fit[ARB]	# array for storing fit

int	nsave, gsgeti()
pointer	sp, rfit

begin
	nsave = gsgeti (sf, GSNSAVE)
	call smark (sp)
	call salloc (rfit, nsave, TY_REAL)
	call gssave (sf, Memr[rfit])
	call achtrd (Memr[rfit], fit, nsave)
	call sfree (sp)
end


# XGSRESTORE -- Procedure to restore the surface fit stored by GSSAVE
# to the surface descriptor for use by the evaluating routines.
#
# NOTE THAT THIS USES DOUBLE PRECISION FOR THE COEFFICIENTS.

procedure xgsrestore (sf, fit)

pointer	sf		# surface descriptor
double	fit[ARB]	# array containing the surface parameters and

int	nsave, gsgeti()
pointer	sp, rfit

begin
	call smark (sp)
	call salloc (rfit, 100, TY_REAL)
	call achtdr (fit, Memr[rfit], 9)
	call gsrestore (sf, Memr[fit])
	nsave = gsgeti (sf, GSNSAVE)
	call achtdr (fit, Memr[rfit], nsave)
	call gsrestore (sf, Memr[fit])
	call sfree (sp)
end


# XGSDER -- Procedure to calculate a new surface which is a derivative of
# the previous surface

procedure xgsder (sf1, x, y, zfit, npts, nxd, nyd)

pointer	sf1		# pointer to the previous surface
real	x[npts]		# x values
real	y[npts]		# y values
real	zfit[npts]	# fitted values
int	npts		# number of points
int	nxd, nyd	# order of the derivatives in x and y

begin
	call gsder (sf1, x, y, z, npts, nxd, nyd)
end
