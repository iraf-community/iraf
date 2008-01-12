# The code here is taken from t_transform.x in the longslit package.  The
# changes are to use a sum instead of an average when multiple surfaces
# are given and not to use the xgs interface.  Also the convergence
# tolerance is user specified since in this application the units might
# not be pixels.


define	MAX_ITERATE	20
define	ERROR		0.05
define	FUDGE		0.5

# TR_INVERT -- Given user coordinate surfaces U(X,Y) and V(X,Y)
# (if none use one-to-one mapping and if more than one sum)
# corresponding to a given U and V and also the various partial
# derivatives.  This is done using a gradient following interative
# method based on evaluating the partial derivative at each point
# and solving the linear Taylor expansions simultaneously.  The last
# point sampled is used as the starting point.  Thus, if the
# input U and V progress smoothly then the number of iterations
# can be small.  The output is returned in x and y and in the derivative array
# DER.  A point outside of the surfaces is returned as the nearest
# point at the edge of the surfaces in the DER array.

procedure tr_invert (usf, nusf, vsf, nvsf, u, v, x, y, der,
	xmin, xmax, ymin, ymax, tol)

pointer	usf[ARB], vsf[ARB]	# User coordinate surfaces U(X,Y) and V(X,Y)
int	nusf, nvsf		# Number of surfaces for each coordinate
double	u, v			# Input U and V to determine X and Y
double	x, y			# Output X and Y
double	der[8]			# Last result as input, new result as output 
				# 1=X, 2=Y, 3=U, 4=DUDX, 5=DUDY, 6=V,
				# 7=DVDX, 8=DVDY
double	xmin, xmax, ymin, ymax	# Limits of coordinate surfaces.
double	tol			# Tolerance

int	i, j, nedge
double	fudge, du, dv, dx, dy, tmp[3]

begin
	# Use the last result as the starting point for the next position.
	# If this is near the desired value then the interation will converge
	# quickly.  Allow a iteration to go off the surface twice.
	# Quit when DX and DY are within tol.

	nedge = 0
	do i = 1, MAX_ITERATE {
	    du = u - der[3]
	    dv = v - der[6]
	    dx = (der[8] * du - der[5] * dv) /
		(der[8] * der[4] - der[5] * der[7])
	    dy = (dv - der[7] * dx) / der[8]
	    fudge = 1 - FUDGE / i
	    x = der[1] + fudge * dx
	    y = der[2] + fudge * dy
	    der[1] = max (xmin, min (xmax, x))
	    der[2] = max (ymin, min (ymax, y))
	    if ((abs (dx) < tol) && (abs (dy) < tol))
	        break

	    if (nusf == 0)
		der[3] = der[1]
	    else if (nusf == 1) {
	        call dgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	        call dgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	        call dgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	    } else {
	        call dgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	        call dgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	        call dgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
		do j = 2, nusf {
	            call dgsder (usf[j], der[1], der[2], tmp[1], 1, 0, 0)
	            call dgsder (usf[j], der[1], der[2], tmp[2], 1, 1, 0)
	            call dgsder (usf[j], der[1], der[2], tmp[3], 1, 0, 1)
		    der[3] = der[3] + tmp[1]
		    der[4] = der[4] + tmp[2]
		    der[5] = der[5] + tmp[3]
		}
	    }

	    if (nvsf == 0)
		der[6] = der[2]
	    else if (nvsf == 1) {
	        call dgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	        call dgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	        call dgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	    } else {
	        call dgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	        call dgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	        call dgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
		do j = 2, nvsf {
	            call dgsder (vsf[j], der[1], der[2], tmp[1], 1, 0, 0)
	            call dgsder (vsf[j], der[1], der[2], tmp[2], 1, 1, 0)
	            call dgsder (vsf[j], der[1], der[2], tmp[3], 1, 0, 1)
		    der[6] = der[6] + tmp[1]
		    der[7] = der[7] + tmp[2]
		    der[8] = der[8] + tmp[3]
		}
	    }
	}
end


# TR_INIT -- Since the inversion iteration always begins from the last
# point we need to initialize before the first call to TR_INVERT.

procedure tr_init (usf, nusf, vsf, nvsf, x, y, der)

pointer	usf[ARB], vsf[ARB]	# User coordinate surfaces
int	nusf, nvsf		# Number of surfaces for each coordinate
double	x, y			# Starting X and Y
double	der[8]			# Inversion data

int	j
double	tmp[3]

begin
	der[1] = x
	der[2] = y
	if (nusf == 0) {
	    der[3] = der[1]
	    der[4] = 1.
	    der[5] = 0.
	} else if (nusf == 1) {
	    call dgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	    call dgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	    call dgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	} else {
	    call dgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	    call dgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	    call dgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	    do j = 2, nusf {
	        call dgsder (usf[j], der[1], der[2], tmp[1], 1, 0, 0)
	        call dgsder (usf[j], der[1], der[2], tmp[2], 1, 1, 0)
	        call dgsder (usf[j], der[1], der[2], tmp[3], 1, 0, 1)
		der[3] = der[3] + tmp[1]
		der[4] = der[4] + tmp[2]
		der[5] = der[5] + tmp[3]
	    }
	}

	if (nvsf == 0) {
	    der[6] = der[2]
	    der[7] = 0.
	    der[8] = 1.
	} else if (nvsf == 1) {
	    call dgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	    call dgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	    call dgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	} else {
	    call dgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	    call dgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	    call dgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	    do j = 2, nvsf {
	        call dgsder (vsf[j], der[1], der[2], tmp[1], 1, 0, 0)
	        call dgsder (vsf[j], der[1], der[2], tmp[2], 1, 1, 0)
	        call dgsder (vsf[j], der[1], der[2], tmp[3], 1, 0, 1)
		der[6] = der[6] + tmp[1]
		der[7] = der[7] + tmp[2]
		der[8] = der[8] + tmp[3]
	    }
	}
end
