include <math.h>
include <math/gsurfit.h>



# GEO_DROTMAG -- Adjust the coefficients  of the fit using the database file.

procedure geo_drotmagr (dt, rec, sx1, sy1, xmag, ymag, xrot, yrot)

pointer	dt		#I pointer to the text database file
int	rec		#I record number
pointer	sx1, sy1	#I/O pointers to the x and y linear surfaces
real	xmag, ymag	#I/O the x and y magnification
real	xrot, yrot	#I/O the x and y axis rotation

real	dtgetr()

begin
	if (IS_INDEFR(xmag))
	    xmag = real (dtgetr (dt, rec, "xmag"))
	if (IS_INDEFR(ymag))
	    ymag = real (dtgetr (dt, rec, "ymag"))
	if (IS_INDEFR(xrot))
	    xrot = DEGTORAD (real(dtgetr (dt, rec, "xrotation")))
	else
	    xrot = DEGTORAD(xrot)
	if (IS_INDEFR(yrot))
	    yrot = DEGTORAD (real (dtgetr (dt, rec, "yrotation")))
	else
	    yrot = DEGTORAD(yrot)
	call geo_rotmagr (sx1, sy1, xmag, ymag, xrot, yrot)
end


# GEO_DXYSHIFT -- Adjust the fitted xy shift using the database file.

procedure geo_dxyshiftr (dt, rec, sx1, sy1, xout, yout, xref, yref,
	xshift, yshift)

pointer	dt		#I pointer to the text file database
int	rec		#I the database record
pointer	sx1, sy1	#I/O pointers to the x and y linear surfaces
real	xout, yout	#I the input coordinate system origin
real	xref, yref	#I the reference coordinate system origin
real	xshift, yshift	#I the origin shift in input coordinates

real	gsgetr(), gseval()

begin
	if (IS_INDEFR(xref))
	    xref = (gsgetr (sx1, GSXMIN) + gsgetr (sx1, GSXMAX)) / 2.0
	if (IS_INDEFR(yref))
	    yref = (gsgetr (sy1, GSYMIN) + gsgetr (sy1, GSYMAX)) / 2.0

	if (IS_INDEFR(xout))
	    xout = gseval (sx1, xref, yref)
	if (IS_INDEFR(yout))
	    yout = gseval (sy1, xref, yref)

	if (IS_INDEFR(xshift))
	    xshift = xout - gseval (sx1, xref, yref)
	if (IS_INDEFR(yshift))
	    yshift = yout - gseval (sy1, xref, yref)

	call geo_xyshiftr (sx1, sy1, xshift, yshift)
end


# GEO_ROTMAG -- Edit the coefficients of the linear surface which determine
# magnification and rotation.

procedure geo_rotmagr (sx1, sy1, xscale, yscale, xrotation, yrotation)

pointer	sx1, sy1		#I/O pointers to the linear x and y surfaces
real	xscale, yscale		#I the x and y scales
real	xrotation,yrotation	#I the x and y axis  rotation angles in radians

real	cosx, sinx, cosy, siny, xrange, yrange
int	ncoeff
pointer	sp, xcoeff, ycoeff
real	gsgetr()
int	gsgeti()

begin
	# Get the current solution.
	call smark (sp)
	ncoeff = max (gsgeti (sx1, GSNSAVE), gsgeti (sy1, GSNSAVE))
	call salloc (xcoeff, ncoeff, TY_REAL)
	call salloc (ycoeff, ncoeff, TY_REAL)
	call gssave (sx1, Memr[xcoeff])
	call gssave (sy1, Memr[ycoeff])

	# Define the scaling parameters.
	cosx = cos (xrotation)
	sinx = sin (xrotation)
	cosy = cos (yrotation)
	siny = sin (yrotation)

	# Calculate coefficients.
	Memr[xcoeff+GS_SAVECOEFF+1] =  xscale * cosx
	Memr[xcoeff+GS_SAVECOEFF+2] =  yscale * siny
	Memr[ycoeff+GS_SAVECOEFF+1] = -xscale * sinx
	Memr[ycoeff+GS_SAVECOEFF+2] =  yscale * cosy

	# Normalize coefficients for-non polynomial functions.
	if (gsgeti (sx1, GSTYPE) != GS_POLYNOMIAL) {
	    xrange = gsgetr (sx1, GSXMAX) - gsgetr (sx1, GSXMIN)
	    Memr[xcoeff+GS_SAVECOEFF+1] = Memr[xcoeff+GS_SAVECOEFF+1] *
	        xrange / 2.d0
	    Memr[xcoeff+GS_SAVECOEFF+2] = Memr[xcoeff+GS_SAVECOEFF+2] *
	        yrange / 2.d0
	}
	if (gsgeti (sy1, GSTYPE) != GS_POLYNOMIAL) {
	    yrange = gsgetr (sy1, GSYMAX) - gsgetr (sy1, GSYMIN)
	    Memr[ycoeff+GS_SAVECOEFF+1] = Memr[ycoeff+GS_SAVECOEFF+1] *
	        xrange / 2.d0
	    Memr[ycoeff+GS_SAVECOEFF+2] = Memr[ycoeff+GS_SAVECOEFF+2] *
	        yrange / 2.d0
	}

	# Free the original fit.
	call gsfree (sx1)
	call gsfree (sy1)

	# Restore the edited fit.
	call gsrestore (sx1, Memr[xcoeff])
	call gsrestore (sy1, Memr[ycoeff])

	call sfree (sp)
end


# GEO_XYSHIFT -- Shift the linear part of the fit in x and y.

procedure geo_xyshiftr (sx1, sy1, xshift, yshift)

pointer	sx1, sy1		#I pointers to linear x and y surfaces
real	xshift, yshift		#I the input x and y shifts

int	ncoeff
pointer	sp, xcoeff, ycoeff
int	gsgeti()

begin
	call smark (sp)

	# Allocate working space.
	ncoeff = max (gsgeti (sx1, GSNSAVE), gsgeti (sy1, GSNSAVE))
	call salloc (xcoeff, ncoeff, TY_REAL)
	call salloc (ycoeff, ncoeff, TY_REAL)

	# Get coefficients.
	call gssave (sx1, Memr[xcoeff])
	call gssave (sy1, Memr[ycoeff])

	# Shift the coefficients.
	Memr[xcoeff+GS_SAVECOEFF] = Memr[xcoeff+GS_SAVECOEFF] + xshift
	Memr[ycoeff+GS_SAVECOEFF] = Memr[ycoeff+GS_SAVECOEFF] + yshift

	# Free original fit.
	call gsfree (sx1)
	call gsfree (sy1)

	# Restore fit.
	call gsrestore (sx1, Memr[xcoeff])
	call gsrestore (sy1, Memr[ycoeff])

	call sfree (sp)
end




# GEO_DROTMAG -- Adjust the coefficients  of the fit using the database file.

procedure geo_drotmagd (dt, rec, sx1, sy1, xmag, ymag, xrot, yrot)

pointer	dt		#I pointer to the text database file
int	rec		#I record number
pointer	sx1, sy1	#I/O pointers to the x and y linear surfaces
double	xmag, ymag	#I/O the x and y magnification
double	xrot, yrot	#I/O the x and y axis rotation

real	dtgetr()

begin
	if (IS_INDEFD(xmag))
	    xmag = double (dtgetr (dt, rec, "xmag"))
	if (IS_INDEFD(ymag))
	    ymag = double (dtgetr (dt, rec, "ymag"))
	if (IS_INDEFD(xrot))
	    xrot = DEGTORAD (double(dtgetr (dt, rec, "xrotation")))
	else
	    xrot = DEGTORAD(xrot)
	if (IS_INDEFD(yrot))
	    yrot = DEGTORAD (double (dtgetr (dt, rec, "yrotation")))
	else
	    yrot = DEGTORAD(yrot)
	call geo_rotmagd (sx1, sy1, xmag, ymag, xrot, yrot)
end


# GEO_DXYSHIFT -- Adjust the fitted xy shift using the database file.

procedure geo_dxyshiftd (dt, rec, sx1, sy1, xout, yout, xref, yref,
	xshift, yshift)

pointer	dt		#I pointer to the text file database
int	rec		#I the database record
pointer	sx1, sy1	#I/O pointers to the x and y linear surfaces
double	xout, yout	#I the input coordinate system origin
double	xref, yref	#I the reference coordinate system origin
double	xshift, yshift	#I the origin shift in input coordinates

double	dgsgetd(), dgseval()

begin
	if (IS_INDEFD(xref))
	    xref = (dgsgetd (sx1, GSXMIN) + dgsgetd (sx1, GSXMAX)) / 2.0d0
	if (IS_INDEFD(yref))
	    yref = (dgsgetd (sy1, GSYMIN) + dgsgetd (sy1, GSYMAX)) / 2.0d0

	if (IS_INDEFD(xout))
	    xout = dgseval (sx1, xref, yref)
	if (IS_INDEFD(yout))
	    yout = dgseval (sy1, xref, yref)

	if (IS_INDEFD(xshift))
	    xshift = xout - dgseval (sx1, xref, yref)
	if (IS_INDEFD(yshift))
	    yshift = yout - dgseval (sy1, xref, yref)

	call geo_xyshiftd (sx1, sy1, xshift, yshift)
end


# GEO_ROTMAG -- Edit the coefficients of the linear surface which determine
# magnification and rotation.

procedure geo_rotmagd (sx1, sy1, xscale, yscale, xrotation, yrotation)

pointer	sx1, sy1		#I/O pointers to the linear x and y surfaces
double	xscale, yscale		#I the x and y scales
double	xrotation,yrotation	#I the x and y axis  rotation angles in radians

double	cosx, sinx, cosy, siny, xrange, yrange
int	ncoeff
pointer	sp, xcoeff, ycoeff
double	dgsgetd()
int	dgsgeti()

begin
	# Get the current solution.
	call smark (sp)
	ncoeff = max (dgsgeti (sx1, GSNSAVE), dgsgeti (sy1, GSNSAVE))
	call salloc (xcoeff, ncoeff, TY_DOUBLE)
	call salloc (ycoeff, ncoeff, TY_DOUBLE)
	call dgssave (sx1, Memd[xcoeff])
	call dgssave (sy1, Memd[ycoeff])

	# Define the scaling parameters.
	cosx = cos (xrotation)
	sinx = sin (xrotation)
	cosy = cos (yrotation)
	siny = sin (yrotation)

	# Calculate coefficients.
	Memd[xcoeff+GS_SAVECOEFF+1] =  xscale * cosx
	Memd[xcoeff+GS_SAVECOEFF+2] =  yscale * siny
	Memd[ycoeff+GS_SAVECOEFF+1] = -xscale * sinx
	Memd[ycoeff+GS_SAVECOEFF+2] =  yscale * cosy

	# Normalize coefficients for-non polynomial functions.
	if (dgsgeti (sx1, GSTYPE) != GS_POLYNOMIAL) {
	    xrange = dgsgetd (sx1, GSXMAX) - dgsgetd (sx1, GSXMIN)
	    Memd[xcoeff+GS_SAVECOEFF+1] = Memd[xcoeff+GS_SAVECOEFF+1] *
	        xrange / 2.d0
	    Memd[xcoeff+GS_SAVECOEFF+2] = Memd[xcoeff+GS_SAVECOEFF+2] *
	        yrange / 2.d0
	}
	if (dgsgeti (sy1, GSTYPE) != GS_POLYNOMIAL) {
	    yrange = dgsgetd (sy1, GSYMAX) - dgsgetd (sy1, GSYMIN)
	    Memd[ycoeff+GS_SAVECOEFF+1] = Memd[ycoeff+GS_SAVECOEFF+1] *
	        xrange / 2.d0
	    Memd[ycoeff+GS_SAVECOEFF+2] = Memd[ycoeff+GS_SAVECOEFF+2] *
	        yrange / 2.d0
	}

	# Free the original fit.
	call dgsfree (sx1)
	call dgsfree (sy1)

	# Restore the edited fit.
	call dgsrestore (sx1, Memd[xcoeff])
	call dgsrestore (sy1, Memd[ycoeff])

	call sfree (sp)
end


# GEO_XYSHIFT -- Shift the linear part of the fit in x and y.

procedure geo_xyshiftd (sx1, sy1, xshift, yshift)

pointer	sx1, sy1		#I pointers to linear x and y surfaces
double	xshift, yshift		#I the input x and y shifts

int	ncoeff
pointer	sp, xcoeff, ycoeff
int	dgsgeti()

begin
	call smark (sp)

	# Allocate working space.
	ncoeff = max (dgsgeti (sx1, GSNSAVE), dgsgeti (sy1, GSNSAVE))
	call salloc (xcoeff, ncoeff, TY_DOUBLE)
	call salloc (ycoeff, ncoeff, TY_DOUBLE)

	# Get coefficients.
	call dgssave (sx1, Memd[xcoeff])
	call dgssave (sy1, Memd[ycoeff])

	# Shift the coefficients.
	Memd[xcoeff+GS_SAVECOEFF] = Memd[xcoeff+GS_SAVECOEFF] + xshift
	Memd[ycoeff+GS_SAVECOEFF] = Memd[ycoeff+GS_SAVECOEFF] + yshift

	# Free original fit.
	call dgsfree (sx1)
	call dgsfree (sy1)

	# Restore fit.
	call dgsrestore (sx1, Memd[xcoeff])
	call dgsrestore (sy1, Memd[ycoeff])

	call sfree (sp)
end



