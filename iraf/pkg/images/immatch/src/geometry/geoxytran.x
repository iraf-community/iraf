include <ctype.h>
include <mach.h>
include <math.h>
include <math/gsurfit.h>

define GEO_LINEAR       1               # Linear transformation only
define GEO_DISTORTION   2               # Distortion correction only
define GEO_GEOMETRIC    3               # Full transformation



# GEO_LINIT -- Initialize the linear part of the transformation.

procedure geo_linitr (sx1, sy1, sx2, sy2)

pointer	sx1, sy1	#I/O pointers to the linear x and y  surfaces
pointer	sx2, sy2	#I/O pointer to the distortion x and y surfaces

real	xmag, ymag, xrot, yrot, xref, yref, xout, yout, xshift, yshift
real	clgetr(), gseval()

begin
	# Initialize the surfaces.
	call gsinit (sx1, GS_POLYNOMIAL, 2, 2, GS_XNONE, -MAX_REAL, MAX_REAL,
	    -MAX_REAL, MAX_REAL)
	call gsinit (sy1, GS_POLYNOMIAL, 2, 2, GS_XNONE, -MAX_REAL, MAX_REAL,
	    -MAX_REAL, MAX_REAL)
	sx2 = NULL
	sy2 = NULL

	# Get the magnification parameters.
	xmag = clgetr ("xmag")
	if (IS_INDEFR(xmag))
	    xmag = real(1.0)
	ymag = clgetr ("ymag")
	if (IS_INDEFR(ymag))
	    ymag = real(1.0)

	# Get the rotation parameters.
	xrot = clgetr ("xrot")
	if (IS_INDEFR(xrot))
	    xrot = real(0.0)
	xrot = -DEGTORAD(xrot)
	yrot = clgetr ("yrot")
	if (IS_INDEFR(yrot))
	    yrot = real(0.0)
	yrot = -DEGTORAD(yrot)

	# Set the magnification and rotation coefficients.
	call geo_rotmagr (sx1, sy1, xmag, ymag, xrot, yrot)

	# Compute the origin of the reference coordinates.
	xref = clgetr ("xref")
	if (IS_INDEFR(xref))
	    xref = real(0.0)
	yref = clgetr ("yref")
	if (IS_INDEFR(yref))
	    yref = real(0.0)

	# Compute the corresponding input coordinates.
	xout = clgetr ("xout")
	if (IS_INDEFR(xout))
	    xout = gseval (sx1, xref, yref)
	yout = clgetr ("yout")
	if (IS_INDEFR(yout))
	    yout = gseval (sy1, xref, yref)

	# Set the shifts.
	xshift = clgetr ("xshift")
	yshift = clgetr ("yshift")
	if (IS_INDEFR(xshift))
	    xshift = xout - gseval (sx1, xref, yref)
	if (IS_INDEFR(yshift))
	    yshift = yout - gseval (sy1, xref, yref)
	call geo_xyshiftr (sx1, sy1, xshift, yshift)
end


# GEO_SFREE -- Free the x and y surface fitting descriptors.

procedure geo_sfreer (sx1, sy1, sx2, sy2)

pointer	sx1, sy1	#I/O pointers to the linear x and y  surfaces
pointer	sx2, sy2	#I/O pointer to the distortion x and y surfaces

begin
        call gsfree (sx1)
        call gsfree (sy1)
        if (sx2 != NULL)
	    call gsfree (sx2)
        if (sy2 != NULL)
	    call gsfree (sy2)
end


# GEO_SINIT --  Read the surface fits from the database file and make
# any requested changes.

procedure geo_sinitr (dt, record, geometry, sx1, sy1, sx2, sy2)

pointer	dt			#I pointer to database file produced by geomap
char	record[ARB]		#I the name of the database record
int	geometry		#I the type of geometry to be computed
pointer	sx1, sy1		#O pointers to the linear x and y surfaces
pointer	sx2, sy2		#O pointers to the x and y distortion surfaces

int	i, rec, ncoeff, junk
real	xmag, ymag, xrot, yrot, xref, yref, xout, yout, xshift, yshift
pointer	newsx1, newsy1, xcoeff, ycoeff
int	dtlocate(), dtscan(), dtgeti()
real	clgetr()
errchk	gsrestore

begin
	# Locate record.
	rec = dtlocate (dt, record)

	# Get linear part of fit.
	ncoeff = dtgeti (dt, rec, "surface1")
	call malloc (xcoeff, ncoeff, TY_REAL)
	call malloc (ycoeff, ncoeff, TY_REAL)
	do i = 1, ncoeff {
	    junk = dtscan (dt)
	    call gargr (Memr[xcoeff+i-1])
	    call gargr (Memr[ycoeff+i-1])
	}

	# Restore linear part of fit.
	call gsrestore (sx1, Memr[xcoeff])
	call gsrestore (sy1, Memr[ycoeff])

	# Get geometric transformation.
	xmag = clgetr ("xmag")
	ymag = clgetr ("ymag")
	xrot = clgetr ("xrotation")
	yrot = clgetr ("yrotation")
	xout = clgetr ("xout")
	yout = clgetr ("yout")
	xref = clgetr ("xref")
	yref = clgetr ("yref")
	xshift = clgetr ("xshift")
	yshift = clgetr ("yshift")

	# Get set to adjust linear part of the fit.
	call gscopy (sx1, newsx1)
	call gscopy (sy1, newsy1)

	if (geometry == GEO_DISTORTION)
	    call geo_rotmagr (newsx1, newsy1, real(1.0), real(1.0),
	        real(0.0), real(0.0))
	 else if (! IS_INDEFR(xmag) || ! IS_INDEFR(ymag) || 
		! IS_INDEFR(xrot) || ! IS_INDEFR(yrot))
	    call geo_drotmagr (dt, rec, newsx1, newsy1, xmag, ymag,
		    xrot, yrot)
	call geo_dxyshiftr (dt, rec, newsx1, newsy1, xout, yout, xref, yref,
	    xshift, yshift)
	call gssave (newsx1, Memr[xcoeff])
	call gssave (newsy1, Memr[ycoeff])

	# Get distortion part of fit.
	ncoeff = dtgeti (dt, rec, "surface2")
	if (ncoeff > 0 && (geometry == GEO_GEOMETRIC ||
	    geometry == GEO_DISTORTION)) {

	    call realloc (xcoeff, ncoeff, TY_REAL)
	    call realloc (ycoeff, ncoeff, TY_REAL)
	    do i = 1, ncoeff {
		junk = dtscan (dt)
		call gargr (Memr[xcoeff+i-1])
		call gargr (Memr[ycoeff+i-1])
	    }

	    # Restore distortion part of fit.
	    iferr {
	        call gsrestore (sx2, Memr[xcoeff])
	    } then {
		call mfree (sx2, TY_STRUCT)
		sx2 = NULL
	    }
	    iferr {
	        call gsrestore (sy2, Memr[ycoeff])
	    } then {
		call mfree (sy2, TY_STRUCT)
		sy2 = NULL
	    }

	} else {
	    sx2 = NULL
	    sy2 = NULL
	}

	# Redefine the linear surfaces.
	call gsfree (sx1)
	call gscopy (newsx1, sx1)
	call gsfree (newsx1)
	call gsfree (sy1)
	call gscopy (newsy1, sy1)
	call gsfree (newsy1)

	# Cleanup.
	call mfree (xcoeff, TY_REAL)
	call mfree (ycoeff, TY_REAL)
end


# GEO_DO_TRANSFORM -- The linear transformation is performed in this procedure.
# First the coordinates are scaled, then rotated and translated.  The
# transformed coordinates are returned.

procedure geo_do_transformr (x, y, xt, yt, sx1, sy1, sx2, sy2)

real	x, y			# initial positions
real	xt, yt			# transformed positions
pointer	sx1, sy1		# pointer to linear surfaces
pointer	sx2, sy2		# pointer to distortion surfaces

real	gseval()

begin
	xt = gseval (sx1, x, y)
	if (sx2 != NULL)
	    xt = xt + gseval (sx2, x, y)
	yt = gseval (sy1, x, y)
	if (sy2 != NULL)
	    yt = yt + gseval (sy2, x, y)
end



# GEO_LINIT -- Initialize the linear part of the transformation.

procedure geo_linitd (sx1, sy1, sx2, sy2)

pointer	sx1, sy1	#I/O pointers to the linear x and y  surfaces
pointer	sx2, sy2	#I/O pointer to the distortion x and y surfaces

double	xmag, ymag, xrot, yrot, xref, yref, xout, yout, xshift, yshift
double	clgetd(), dgseval()

begin
	# Initialize the surfaces.
	call dgsinit (sx1, GS_POLYNOMIAL, 2, 2, GS_XNONE, double (-MAX_REAL),
	    double (MAX_REAL), double (-MAX_REAL), double (MAX_REAL))
	call dgsinit (sy1, GS_POLYNOMIAL, 2, 2, GS_XNONE, double (-MAX_REAL),
	    double (MAX_REAL), double (-MAX_REAL), double (MAX_REAL))
	sx2 = NULL
	sy2 = NULL

	# Get the magnification parameters.
	xmag = clgetd ("xmag")
	if (IS_INDEFD(xmag))
	    xmag = double(1.0)
	ymag = clgetd ("ymag")
	if (IS_INDEFD(ymag))
	    ymag = double(1.0)

	# Get the rotation parameters.
	xrot = clgetd ("xrot")
	if (IS_INDEFD(xrot))
	    xrot = double(0.0)
	xrot = -DEGTORAD(xrot)
	yrot = clgetd ("yrot")
	if (IS_INDEFD(yrot))
	    yrot = double(0.0)
	yrot = -DEGTORAD(yrot)

	# Set the magnification and rotation coefficients.
	call geo_rotmagd (sx1, sy1, xmag, ymag, xrot, yrot)

	# Compute the origin of the reference coordinates.
	xref = clgetd ("xref")
	if (IS_INDEFD(xref))
	    xref = double(0.0)
	yref = clgetd ("yref")
	if (IS_INDEFD(yref))
	    yref = double(0.0)

	# Compute the corresponding input coordinates.
	xout = clgetd ("xout")
	if (IS_INDEFD(xout))
	    xout = dgseval (sx1, xref, yref)
	yout = clgetd ("yout")
	if (IS_INDEFD(yout))
	    yout = dgseval (sy1, xref, yref)

	# Set the shifts.
	xshift = clgetd ("xshift")
	yshift = clgetd ("yshift")
	if (IS_INDEFD(xshift))
	    xshift = xout - dgseval (sx1, xref, yref)
	if (IS_INDEFD(yshift))
	    yshift = yout - dgseval (sy1, xref, yref)
	call geo_xyshiftd (sx1, sy1, xshift, yshift)
end


# GEO_SFREE -- Free the x and y surface fitting descriptors.

procedure geo_sfreed (sx1, sy1, sx2, sy2)

pointer	sx1, sy1	#I/O pointers to the linear x and y  surfaces
pointer	sx2, sy2	#I/O pointer to the distortion x and y surfaces

begin
        call dgsfree (sx1)
        call dgsfree (sy1)
        if (sx2 != NULL)
	    call dgsfree (sx2)
        if (sy2 != NULL)
	    call dgsfree (sy2)
end


# GEO_SINIT --  Read the surface fits from the database file and make
# any requested changes.

procedure geo_sinitd (dt, record, geometry, sx1, sy1, sx2, sy2)

pointer	dt			#I pointer to database file produced by geomap
char	record[ARB]		#I the name of the database record
int	geometry		#I the type of geometry to be computed
pointer	sx1, sy1		#O pointers to the linear x and y surfaces
pointer	sx2, sy2		#O pointers to the x and y distortion surfaces

int	i, rec, ncoeff, junk
double	xmag, ymag, xrot, yrot, xref, yref, xout, yout, xshift, yshift
pointer	newsx1, newsy1, xcoeff, ycoeff
int	dtlocate(), dtscan(), dtgeti()
double	clgetd()
errchk	dgsrestore

begin
	# Locate record.
	rec = dtlocate (dt, record)

	# Get linear part of fit.
	ncoeff = dtgeti (dt, rec, "surface1")
	call malloc (xcoeff, ncoeff, TY_DOUBLE)
	call malloc (ycoeff, ncoeff, TY_DOUBLE)
	do i = 1, ncoeff {
	    junk = dtscan (dt)
	    call gargd (Memd[xcoeff+i-1])
	    call gargd (Memd[ycoeff+i-1])
	}

	# Restore linear part of fit.
	call dgsrestore (sx1, Memd[xcoeff])
	call dgsrestore (sy1, Memd[ycoeff])

	# Get geometric transformation.
	xmag = clgetd ("xmag")
	ymag = clgetd ("ymag")
	xrot = clgetd ("xrotation")
	yrot = clgetd ("yrotation")
	xout = clgetd ("xout")
	yout = clgetd ("yout")
	xref = clgetd ("xref")
	yref = clgetd ("yref")
	xshift = clgetd ("xshift")
	yshift = clgetd ("yshift")

	# Get set to adjust linear part of the fit.
	call dgscopy (sx1, newsx1)
	call dgscopy (sy1, newsy1)

	if (geometry == GEO_DISTORTION)
	    call geo_rotmagd (newsx1, newsy1, double(1.0), double(1.0),
	        double(0.0), double(0.0))
	 else if (! IS_INDEFD(xmag) || ! IS_INDEFD(ymag) || 
		! IS_INDEFD(xrot) || ! IS_INDEFD(yrot))
	    call geo_drotmagd (dt, rec, newsx1, newsy1, xmag, ymag,
		    xrot, yrot)
	call geo_dxyshiftd (dt, rec, newsx1, newsy1, xout, yout, xref, yref,
	    xshift, yshift)
	call dgssave (newsx1, Memd[xcoeff])
	call dgssave (newsy1, Memd[ycoeff])

	# Get distortion part of fit.
	ncoeff = dtgeti (dt, rec, "surface2")
	if (ncoeff > 0 && (geometry == GEO_GEOMETRIC ||
	    geometry == GEO_DISTORTION)) {

	    call realloc (xcoeff, ncoeff, TY_DOUBLE)
	    call realloc (ycoeff, ncoeff, TY_DOUBLE)
	    do i = 1, ncoeff {
		junk = dtscan (dt)
		call gargd (Memd[xcoeff+i-1])
		call gargd (Memd[ycoeff+i-1])
	    }

	    # Restore distortion part of fit.
	    iferr {
	        call dgsrestore (sx2, Memd[xcoeff])
	    } then {
		call mfree (sx2, TY_STRUCT)
		sx2 = NULL
	    }
	    iferr {
	        call dgsrestore (sy2, Memd[ycoeff])
	    } then {
		call mfree (sy2, TY_STRUCT)
		sy2 = NULL
	    }

	} else {
	    sx2 = NULL
	    sy2 = NULL
	}

	# Redefine the linear surfaces.
	call dgsfree (sx1)
	call dgscopy (newsx1, sx1)
	call dgsfree (newsx1)
	call dgsfree (sy1)
	call dgscopy (newsy1, sy1)
	call dgsfree (newsy1)

	# Cleanup.
	call mfree (xcoeff, TY_DOUBLE)
	call mfree (ycoeff, TY_DOUBLE)
end


# GEO_DO_TRANSFORM -- The linear transformation is performed in this procedure.
# First the coordinates are scaled, then rotated and translated.  The
# transformed coordinates are returned.

procedure geo_do_transformd (x, y, xt, yt, sx1, sy1, sx2, sy2)

double	x, y			# initial positions
double	xt, yt			# transformed positions
pointer	sx1, sy1		# pointer to linear surfaces
pointer	sx2, sy2		# pointer to distortion surfaces

double	dgseval()

begin
	xt = dgseval (sx1, x, y)
	if (sx2 != NULL)
	    xt = xt + dgseval (sx2, x, y)
	yt = dgseval (sy1, x, y)
	if (sy2 != NULL)
	    yt = yt + dgseval (sy2, x, y)
end


