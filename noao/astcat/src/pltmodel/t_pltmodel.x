include <math.h>

task pltmodel = t_pltmodel

procedure t_pltmodel()

double	x_zero, y_zero, xi_zero, eta_zero, ra_tan, dec_tan, scale, ratio
double	xrot, yrot, dra_tan, ddec_tan, x, y, xstep, ystep, tra, tdec
double	xpix[1000], ypix[1000], xi[1000], eta[1000], dxi[1000], deta[1000]
double	cosd, sind, dra, ddec, c1, f1, b1, ddxi, ddeta, q1, q2, q3
double	rpix, theta, rstd, tstd
int	i, j, ncols, nlines, ncgrid, nlgrid, npts
double	clgetd()
int	clgeti()

begin
	# Get the image size.
	ncols = clgeti ("ncols")
	nlines = clgeti ("nlines")
	ncgrid = clgeti ("ncgrid")
	nlgrid = clgeti ("nlgrid")

	# Get the image zero point in pixels.
	x_zero = clgetd ("x_zero")
	if (IS_INDEFD(x_zero))
	    x_zero = (1.0d0 + ncols) / 2.0d0
	y_zero = clgetd ("y_zero")
	if (IS_INDEFD(y_zero))
	    y_zero = (1.0d0 + nlines) / 2.0d0
	xi_zero = clgetd ("xi_zero")
	if (IS_INDEFD(xi_zero))
	    xi_zero = 0.0d0
	eta_zero = clgetd ("eta_zero")
	if (IS_INDEFD(eta_zero))
	    eta_zero = 0.0d0

	# Get the image scale in " / pixel and the ratio of x to y scales.
	scale = clgetd ("scale")
	if (IS_INDEFD(scale))
	    scale = 1.0d0
	scale = DEGTORAD (scale / 3600.0d0)
	ratio = clgetd ("ratio")
	if (IS_INDEFD(ratio))
	    ratio = 1.0d0

	# Get the rotation and ske in degrees.
	xrot = clgetd ("xrot")
	if (IS_INDEFD(xrot))
	    xrot = 0.0d0
	yrot = clgetd ("yrot")
	if (IS_INDEFD(yrot))
	    yrot = 0.0d0

	# Get the assumed image tangent point in hours and degrees.
	ra_tan = clgetd ("ra_tan")
	if (IS_INDEFD(ra_tan))
	    ra_tan = 0.0d0
	dec_tan = clgetd ("dec_tan")
	if (IS_INDEFD(dec_tan))
	    dec_tan = 0.0d0
	cosd = cos (DEGTORAD(dec_tan))
	sind = sin (DEGTORAD(dec_tan))

	# Get the tangent point error.
	dra_tan = clgetd ("dra_tan")
	if (IS_INDEFD(dra_tan))
	    dra_tan = 0.0d0
	ddec_tan = clgetd ("ddec_tan")
	if (IS_INDEFD(ddec_tan))
	    ddec_tan = 0.0d0

	# Get the tilt error.
	tra = clgetd ("tra")
	if (IS_INDEFD(tra))
	    tra = 0.0d0
	tdec = clgetd ("tdec")
	if (IS_INDEFD(tdec))
	    tdec = 0.0d0

	# Get the cubic distortion term
	q1 = clgetd ("q3ra")
	if (IS_INDEFD(q1))
	    q1 = 0.0d0
	q2 = clgetd ("q3dec")
	if (IS_INDEFD(q2))
	    q2 = 0.0d0
	q3 = clgetd ("q3")
	if (IS_INDEFD(q3))
	    q3 = 0.0d0

	# Compute the x and y grid.
	xstep = (ncols - 1.0d0) / (ncgrid - 1.0d0)
	ystep = (nlines - 1.0d0) / (nlgrid - 1.0d0)
	npts = 0
	y = 1.0d0
	do j = 1, nlgrid {
	    x = 1.0d0
	    do i = 1, ncgrid {
		npts = npts + 1
		xpix[npts] = x
		ypix[npts] = y
		dxi[npts] = 0.0d0
		deta[npts] = 0.0d0
		x = x + xstep
	    }
	    y = y + ystep
	}

	# Compute the linear part of the plate solution.
	do i = 1, npts {
	    xi[i] = xi_zero + scale * (xpix[i] - x_zero) *
	        cos (DEGTORAD(xrot)) - scale * ratio * (ypix[i] - y_zero) *
		sin(DEGTORAD(yrot)) 
	    eta[i] = eta_zero +  scale * (xpix[i] - x_zero) *
	        sin(DEGTORAD(xrot)) + scale * ratio * (ypix[i] - y_zero) *
		cos(DEGTORAD(yrot)) 
	}

	# Estimate the tilt terms.
	dra = DEGTORAD(tra / 60.0d0)
	ddec = DEGTORAD(tdec / 60.0d0)
	c1 = cosd * dra
	f1 = ddec
	do i = 1, npts {
	    ddxi = c1 * xi[i] ** 2 + f1 * xi[i] * eta[i]
	    ddeta = f1 * xi[i] * eta[i] + c1 * eta[i] ** 2
	    dxi[i] = dxi[i] + ddxi
	    deta[i] = deta[i] + ddeta
	}

	# Compute the components of the centering error.
	dra = DEGTORAD(dra_tan / 60.0d0)
	ddec = DEGTORAD(ddec_tan / 60.0d0)
	c1 = cosd * dra
	b1 = sind * dra
	f1 = ddec
	do i = 1, npts {
	    ddxi = c1 - b1 * eta[i] + c1 * xi[i] ** 2 + f1 * xi[i] *
	        eta[i]
	    ddeta = f1 + b1 * xi[i] + f1 * eta[i] ** 2 + c1 * xi[i] *
	        eta[i]
	    dxi[i] = dxi[i] + ddxi
	    deta[i] = deta[i] + ddeta
	}

	# Compute the radial distortion terms
	dra = DEGTORAD(q1 / 60.0d0)
	ddec = DEGTORAD(q2 / 60.0d0)
	c1 = -cosd * dra * q3
	f1 = -ddec * q3
	do i = 1, npts {
	    ddxi = c1 * (3.0d0 * xi[i] ** 2 + eta[i] ** 2) + 2.0d0 * f1 *
	        xi[i] * eta[i] + q3 * xi[i] * (xi[i] ** 2 + eta[i] ** 2)
	    ddeta = 2.0d0 * c1 * xi[i] * eta[i] + f1 * (xi[i] ** 2 + 3.0d0 *
	        eta[i] ** 2) + q3 * eta[i] * (xi[i] ** 2 + eta[i] ** 2)
	    dxi[i] = dxi[i] - ddxi
	    deta[i] = deta[i] - ddeta
	}


	# Estimate the refraction and aberration terms.

	# Compute the cubic distortion correction.
	# Do the correction
	do i = 1, npts {
	    xi[i] = xi[i] + dxi[i]
	    eta[i] = eta[i] + deta[i]
	}

	# Print the results.
	do i = 1, npts {
	    rpix = sqrt ((xpix[i] - x_zero) ** 2 + (ypix[i] - y_zero) ** 2)
	    if (ypix[i] == y_zero && xpix[i] == x_zero)
		theta = 0.0d0
	    else
	        theta = RADTODEG(atan2 (ypix[i] - y_zero, xpix[i] - x_zero))
	    #if (theta < 0.0d0)
		#theta = theta + 360.0d0
	    rstd = sqrt ((xi[i] - xi_zero) ** 2 + (eta[i] - eta_zero) ** 2)
	    if (eta[i] == eta_zero && xi[i] == xi_zero)
		tstd = 0.0d0
	    else
	        tstd = RADTODEG(atan2 (eta[i] - eta_zero, xi[i] - xi_zero))
	    #if (tstd < 0.0d0)
		#tstd = tstd + 360.0d0
	    call printf ("%12g %12g  %12g %12g  %12g %12g  %12g %12g\n")
		call pargd (xpix[i])
		call pargd (ypix[i])
		call pargd (RADTODEG(xi[i]) * 3600.0d0)
		call pargd (RADTODEG(eta[i]) * 3600.0d0)
		call pargd (rpix)
		call pargd (theta)
		call pargd (RADTODEG(rstd) * 3600.0d0)
		call pargd (tstd)
	}
end
