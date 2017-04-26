include	<math.h>
include	<math/gsurfit.h>
include	<math/iminterp.h>

# Wrapper for MWCS CT pointer to include the image pixel range.

define	CT_LW		Memi[$1]		# MWCS CT (logical -> world)
define	CT_WL		Memi[$1+1]		# MWCS CT (world -> logical)
define	CT_NX		Memi[$1+2]		# Number of pixels in X
define	CT_NY		Memi[$1+3]		# Number of pixels Y


# TR_GSF -- Get coordinate surface fits from the database.

procedure tr_gsf (database, sflist, un, usf, nusf, vsf, nvsf)

char	database		#I Database containing coordinate surfaces
int	sflist			#I List of user coordinate surfaces
pointer	un[2]			#O Units pointers
pointer	usf			#O Pointer to array of U surface fits
int	nusf			#O Number of U surface fits
pointer	vsf			#O Pointer to array of V surface fits
int	nvsf			#O Number of U surface fits

int	i, nsf
pointer	sp, sfname, un1, sf

bool	un_compare()
int	clgfil(), clplen()

begin
	# Get the user coordinate surfaces and separate them into U and V.
	# Check that all surfaces have the same range of X and Y and determine
	# the range of U and V.

	call smark (sp)
	call salloc (sfname, SZ_FNAME, TY_CHAR)

	nsf = max (1, clplen (sflist))
	call malloc (usf, nsf, TY_INT)
	call malloc (vsf, nsf, TY_INT)

	un[1] = NULL
	un[2] = NULL
	Memi[usf] = NULL
	Memi[vsf] = NULL
	nusf = 0
	nvsf = 0
	while (clgfil (sflist, Memc[sfname], SZ_FNAME) != EOF) {
	    call lm_dbread (database, Memc[sfname], i, un1, sf)
	    if (un1 != NULL) {
		if (un[i] == NULL)
		    un[i] = un1
		else if (un_compare (un1, un[i]))
		    call un_close (un1)
		else {
		    call un_close (un1)
		    call un_close (un[i])
		    call sfree (sp)
		    call error (1, "Input units disagree")
		}
	    }

	    if (sf != NULL) {
		if (i == 1) {
		    nusf = nusf+1
		    Memi[usf+nusf-1] = sf
		} else if (i == 2) {
		    nvsf = nvsf+1
		    Memi[vsf+nvsf-1] = sf
		}
	    }
	}
	call clprew (sflist)

	if (nusf + nvsf == 0)
	    call error (0, "No user coordinates")

	call sfree (sp)
end


# TR_GWCS -- Get WCS.

procedure tr_gwcs (mw, un, nx, ny, ct, usf, nusf, vsf, nvsf)

pointer	mw			#I MWCS pointer
pointer	un[2]			#O Units pointers
int	nx, ny			#I Image size

pointer	ct			#O CT pointer
pointer	usf			#O Pointer to array of U surface fits
int	nusf			#O Number of U surface fits
pointer	vsf			#O Pointer to array of V surface fits
int	nvsf			#O Number of U surface fits

int	i
pointer	sp, units, un_open(), mw_sctran()
errchk	un_open

begin
	call smark (sp)
	call salloc (units, SZ_FNAME, TY_CHAR)

	call malloc (ct, 4, TY_STRUCT)
	nusf = 1
	call calloc (usf, nusf, TY_INT) 
	nvsf = 1
	call calloc (vsf, nvsf, TY_INT) 

	CT_LW(ct) =  mw_sctran (mw, "logical", "world", 3)
	CT_WL(ct) =  mw_sctran (mw, "world", "logical", 3)
	CT_NX(ct) = nx
	CT_NY(ct) = ny

	do i = 1, 2 {
	    ifnoerr (call mw_gwattrs (mw, i, "units", Memc[units], SZ_FNAME))
	        un[i] = un_open (Memc[units])
	    else
		un[i] = NULL
	}
end


# TR_SETUP -- Setup the transformation interpolation.
# 
# At each point (U,V) in the output image we need to know the coordinate
# (X,Y) of the input images to be interpolated.  This means we need
# to determine X(U,V) and Y(U,V).  The input user coordinate surfaces,
# however, are U(X,Y) and V(X,Y) (a missing surface implies a one to one
# mapping of U=X or V=Y).  This requires simultaneously inverting the user
# coordinate surfaces.  This is a slow process using a gradient following
# iterative technique.
# 
# Note that when an WCS is used, the MWCS routines already provide the
# inverse mapping.  But even in this case it may be slow and so we use the
# same sampling and surface fitting technique for setting up the inversion
# mapping.
# 
# The inverted coordinates are determined on a evenly subsampled grid of
# linear output coordinates.  A linear interpolation surface can then be fit
# to this grid which is much faster to evaluate at each output coordinate.
# These interpolation surfaces are returned.  If flux is to be conserved a
# similar interpolation surface for the Jacobian, J(U,V) is also returned.
# There may also be a mapping of the output image into logrithmic intervals
# which maps to the linearly sampled interpolation surfaces.  The mappings
# of the output U and V intervals to the subsampled interpolation coordinates
# are also returned.
# 
# 1. Set the output coordinate system based on the ranges of X, Y, U, and V.
# 2. Determine X(U,V), Y(U,V), and J(U,V) on a evenly subsampled grid of
#    U and V.
# 3. Fit linear interpolation surfaces to these data.
# 4. Compute the mapping between output coordinates along each axis, which
#    may be logrithmic, into the subsampling interpolation coordinates.

procedure tr_setup (ct, usf, nusf, vsf, nvsf, un, xmsi, ymsi, jmsi,
	uout, vout, duout, dvout)

pointer	ct			#I CT pointer
pointer	usf			#U Pointers to U surface fits: freed upon return
int	nusf			#I Number of U surface fits
pointer	vsf			#U Pointers to V surface fits: freed upon return
int	nvsf			#I Number of V surface fits
pointer	un[2]			#O Units pointers
pointer	xmsi, ymsi, jmsi	#O Surface interpolators for X, Y and Jacobian
pointer	uout, vout		#O Output coordinates relative to interpolator 
pointer	duout, dvout		#O Output coordinate intervals

int	i, j, step, nu1, nv1
real	xmin, xmax, ymin, ymax, umin, umax, vmin, vmax
real	u, v, x, y, du1, dv1, der[8]
double	dval
pointer	xgrid, ygrid, zgrid, ptr1, ptr2, ptr3

real	tr_getr(), tr_eval()

include	"transform.com"

begin
	#step = clgeti ("step")
	step = 10

	xmin = INDEF
	xmax = INDEF
	ymin = INDEF
	ymax = INDEF
	umin = INDEF
	umax = INDEF
	vmin = INDEF
	vmax = INDEF
	do i = 1, nusf {
	    if (IS_INDEF (xmin)) {
		xmin = tr_getr (ct, Memi[usf+i-1], GSXMIN)
		xmax = tr_getr (ct, Memi[usf+i-1], GSXMAX)
		ymin = tr_getr (ct, Memi[usf+i-1], GSYMIN)
		ymax = tr_getr (ct, Memi[usf+i-1], GSYMAX)
	    } else {
		if ((xmin != tr_getr (ct, Memi[usf+i-1], GSXMIN)) ||
		    (xmax != tr_getr (ct, Memi[usf+i-1], GSXMAX)) ||
		    (ymin != tr_getr (ct, Memi[usf+i-1], GSYMIN)) ||
		    (ymax != tr_getr (ct, Memi[usf+i-1], GSYMAX)))
		    call error (0, "tr_setup: Inconsistent coordinate fits")
	    }

	    if (IS_INDEF (umin)) {
	        umin = tr_eval (ct, Memi[usf+i-1], 1, xmin, ymin)
	        umax = umin
	    }
	    u = tr_eval (ct, Memi[usf+i-1], 1, xmin, ymin)
	    umin = min (u, umin)
	    umax = max (u, umax)
	    u = tr_eval (ct, Memi[usf+i-1], 1, xmax, ymin)
	    umin = min (u, umin)
	    umax = max (u, umax)
	    u = tr_eval (ct, Memi[usf+i-1], 1, xmin, ymax)
	    umin = min (u, umin)
	    umax = max (u, umax)
	    u = tr_eval (ct, Memi[usf+i-1], 1, xmax, ymax)
	    umin = min (u, umin)
	    umax = max (u, umax)
	}
	do i = 1, nvsf {
	    if (IS_INDEF (xmin)) {
		xmin = tr_getr (ct, Memi[vsf+i-1], GSXMIN)
		xmax = tr_getr (ct, Memi[vsf+i-1], GSXMAX)
		ymin = tr_getr (ct, Memi[vsf+i-1], GSYMIN)
		ymax = tr_getr (ct, Memi[vsf+i-1], GSYMAX)
	    } else {
		if ((xmin != tr_getr (ct, Memi[vsf+i-1], GSXMIN)) ||
		    (xmax != tr_getr (ct, Memi[vsf+i-1], GSXMAX)) ||
		    (ymin != tr_getr (ct, Memi[vsf+i-1], GSYMIN)) ||
		    (ymax != tr_getr (ct, Memi[vsf+i-1], GSYMAX)))
		    call error (0, "tr_setup: Inconsistent coordinate fits")
	    }

	    if (IS_INDEF (vmin)) {
	        vmin = tr_eval (ct, Memi[vsf+i-1], 2, xmin, ymin)
	        vmax = vmin
	    }
	    v = tr_eval (ct, Memi[vsf+i-1], 2, xmin, ymin)
	    vmin = min (v, vmin)
	    vmax = max (v, vmax)
	    v = tr_eval (ct, Memi[vsf+i-1], 2, xmax, ymin)
	    vmin = min (v, vmin)
	    vmax = max (v, vmax)
	    v = tr_eval (ct, Memi[vsf+i-1], 2, xmin, ymax)
	    vmin = min (v, vmin)
	    vmax = max (v, vmax)
	    v = tr_eval (ct, Memi[vsf+i-1], 2, xmax, ymax)
	    vmin = min (v, vmin)
	    vmax = max (v, vmax)
	}
	if (IS_INDEF (umin)) {
	    umin = xmin
	    umax = xmax
	}
	if (IS_INDEF (vmin)) {
	    vmin = ymin
	    vmax = ymax
	}

	# Set the output coordinate system which is in a common block.
	call tr_setoutput (xmin, xmax, ymin, ymax, umin, umax, vmin, vmax)

	# Subsample the inverted coordinates and fit an interpolation
	# surface.  The grid is evaluated in a back and forth pattern to
	# use the last point evaluated and the starting point for the next
	# point.  This allows the interative inversion routine to work most
	# efficiently with typically only two evaluations per step.

	nu1 = max (2, nu / step)
	nv1 = max (2, nv / step)
	du1 = (u2 - u1) / (nu1 - 1)
	dv1 = (v2 - v1) / (nv1 - 1)

	call malloc (xgrid, nu1 * nv1, TY_REAL)
	call malloc (ygrid, nu1 * nv1, TY_REAL)
	call malloc (zgrid, nu1 * nv1, TY_REAL)

	call tr_init (ct, Memi[usf], nusf, Memi[vsf], nvsf, xmin, ymin, der)
	do i = 1, nv1, 2 {
	    # Do this line from left to right.
	    ptr1 = xgrid + (i - 1) * nu1 - 1
	    ptr2 = ygrid + (i - 1) * nu1 - 1
	    ptr3 = zgrid + (i - 1) * nu1 - 1
	    v = v1 + (i - 1) * dv1
	    do j = 1, nu1 {
		u = u1 + (j - 1) * du1
		call tr_invert (ct, Memi[usf], nusf, Memi[vsf], nvsf, u, v,
		    x, y, der, xmin, xmax, ymin, ymax)
		# V2.10.2
		#Memr[ptr1+j] = der[1]
		#Memr[ptr2+j] = der[2]
		# After V2.10.3
		Memr[ptr1+j] = x
		Memr[ptr2+j] = y

		Memr[ptr3+j] = 1. / abs (der[4] * der[8] - der[5] * der[7])
	    }
	    if (i == nv1)
		break

	    # Do the next line from right to left.
	    ptr1 = xgrid + i * nu1 - 1
	    ptr2 = ygrid + i * nu1 - 1
	    ptr3 = zgrid + i * nu1 - 1
	    v = v1 + i * dv1
	    do j = nu1, 1, -1 {
		u = u1 + (j - 1) * du1
		call tr_invert (ct, Memi[usf], nusf, Memi[vsf], nvsf, u, v,
		    x, y, der, xmin, xmax, ymin, ymax)
		# V2.10.2
		#Memr[ptr1+j] = der[1]
		#Memr[ptr2+j] = der[2]
		# V2.10.3
		Memr[ptr1+j] = x
		Memr[ptr2+j] = y
		Memr[ptr3+j] = 1. / abs (der[4] * der[8] - der[5] * der[7])
	    }
	}

	# Free the surfaces since we are now done with them.
	if (ct != NULL)
	    call mfree (ct, TY_STRUCT)
	for (i=1; i<=nusf; i=i+1)
	    if (Memi[usf+i-1] != NULL)
		call xgsfree (Memi[usf+i-1])
	call mfree (usf, TY_POINTER)
	for (i=1; i<=nvsf; i=i+1)
	    if (Memi[vsf+i-1] != NULL)
		call xgsfree (Memi[vsf+i-1])
	call mfree (vsf, TY_POINTER)

	# Fit a linear interpolator to the subsampled grids of X(U,V), Y(U,V),
	# and J(U,V) to avoid having to evaluate the inverse at each point in
	# the output image.  The inversion is slow because of the many
	# evaluations of the surfaces coordinates.  Also compute an return
	# arrays mapping the output coordinates to the subsampled coordinates.
	# This may include a transformation to logrithmic intervals.

	call msiinit (xmsi, II_BILINEAR)
	call msifit (xmsi, Memr[xgrid], nu1, nv1, nu1)
	call mfree (xgrid, TY_REAL)

	call msiinit (ymsi, II_BILINEAR)
	call msifit (ymsi, Memr[ygrid], nu1, nv1, nu1)
	call mfree (ygrid, TY_REAL)

	if (flux) {
	    call msiinit (jmsi, II_BILINEAR)
	    call msifit (jmsi, Memr[zgrid], nu1, nv1, nu1)
	}
	call mfree (zgrid, TY_REAL)

	# Compute the mapping between output coordinates and the subsampled
	# interpolation surface.  Also compute the intervals used to define
	# the pixel areas for conserving flux.

	call malloc (uout, nu, TY_REAL)
	call malloc (duout, nu, TY_REAL)
	if (ulog) {
	    dval = log10 (double(u1))
	    do i = 0, nu - 1
		Memr[uout+i] = 10.**(dval+i*du)
	    call amulkr (Memr[uout], du * LN_10, Memr[duout], nu)
	} else {
	    do i = 0, nu - 1
		Memr[uout+i] = u1 + i * du
	    call amovkr (du, Memr[duout], nu)
	}
	u2 = Memr[uout+nu-1]

	call malloc (vout, nv, TY_REAL)
	call malloc (dvout, nv, TY_REAL)
	if (vlog) {
	    dval = log10 (double(v1))
	    do i = 0, nv - 1
		Memr[vout+i] = 10.**(dval+i*dv)
	    call amulkr (Memr[vout], dv * LN_10, Memr[dvout], nv)
	} else {
	    do i = 0, nv - 1
		Memr[vout+i] = v1 + i * dv
	    call amovkr (dv, Memr[dvout], nv)
	}
	v2 = Memr[vout+nv-1]

	# Convert to interpolation coordinates.
	umin = 1.; umax = nu
	do i = 0, nu - 1
	    Memr[uout+i] = max (umin, min (umax, (Memr[uout+i]-u1)/du1+1))
	vmin = 1.; vmax = nv
	do i = 0, nv - 1
	    Memr[vout+i] = max (vmin, min (vmax, (Memr[vout+i]-v1)/dv1+1))
end


define	MAX_ITERATE	10
define	ERROR		0.05
define	FUDGE		0.5

# TR_INVERT -- Given user coordinate surfaces U(X,Y) and V(X,Y)
# (if none use one-to-one mapping and if more than one average)
# corresponding to a given U and V and also the various partial
# derivatives.  This is done using a gradient following interative
# method based on evaluating the partial derivative at each point
# and solving the linear Taylor expansions simultaneously.  The last
# point sampled is used as the starting point.  Thus, if the
# input U and V progress smoothly then the number of iterations
# can be small.  The output is returned in x and y and in the derivative array
# DER.  A point outside of the surfaces is returned as the nearest
# point at the edge of the surfaces in the DER array.
#
# If a WCS is used then we let MWCS do the inversion and compute the
# derivatives numerically.

procedure tr_invert (ct, usf, nusf, vsf, nvsf, u, v, x, y, der,
	xmin, xmax, ymin, ymax)

pointer	ct			#I CT pointer
pointer	usf[ARB], vsf[ARB]	#I User coordinate surfaces U(X,Y) and V(X,Y)
int	nusf, nvsf		#I Number of surfaces for each coordinate
real	u, v			#I Input U and V to determine X and Y
real	x, y			#O Output X and Y
real	der[8]			#U Last result as input, new result as output 
				#  1=X, 2=Y, 3=U, 4=DUDX, 5=DUDY, 6=V,
				#  7=DVDX, 8=DVDY
real	xmin, xmax, ymin, ymax	#I Limits of coordinate surfaces.

int	i, j, nedge
real	fudge, du, dv, dx, dy, a, b, tmp[4]

begin
	# If using a WCS we let MWCS do the inversion.
	if (ct != NULL) {
	    call mw_c2tranr (CT_WL(ct), u, v, x, y)
	    call mw_c2tranr (CT_LW(ct), x-0.5, y, tmp[1], tmp[3])
	    call mw_c2tranr (CT_LW(ct), x+0.5, y, tmp[2], tmp[4])
	    der[4] = tmp[2] - tmp[1]
	    der[7] = tmp[4] - tmp[3]
	    call mw_c2tranr (CT_LW(ct), x, y-0.5, tmp[1], tmp[3])
	    call mw_c2tranr (CT_LW(ct), x, y+0.5, tmp[2], tmp[4])
	    der[5] = tmp[2] - tmp[1]
	    der[8] = tmp[4] - tmp[3]
	    return
	}

	# Use the last result as the starting point for the next position.
	# If this is near the desired value then the interation will converge
	# quickly.  Allow a iteration to go off the surface twice.
	# Quit when DX and DY are within ERROR.

	nedge = 0
	do i = 1, MAX_ITERATE {
	    du = u - der[3]
	    dv = v - der[6]
	    a = der[8] * du - der[5] * dv
	    b = der[8] * der[4] - der[5] * der[7]
	    if (b == 0.) {
		if (a < 0.)
		    dx = -2.
		else
		    dx = 2.
	    } else
	        dx = a / b
	    a = dv - der[7] * dx
	    b = der[8]
	    if (b == 0.) {
		if (a < 0.)
		    dy = -2.
		else
		    dy = 2.
	    } else
	        dy = a / b
	    fudge = 1 - FUDGE / i
	    x = der[1] + fudge * dx
	    y = der[2] + fudge * dy
	    der[1] = max (xmin, min (xmax, x))
	    der[2] = max (ymin, min (ymax, y))
#	    if (x < xmin || x > xmax)
#	        nedge = nedge + 1
#	    if (y < ymin || y > ymax)
#	        nedge = nedge + 1
#	    if (nedge > 2)
#	        break
	    if ((abs (dx) < ERROR) && (abs (dy) < ERROR))
	        break

	    if (nusf == 0)
		der[3] = der[1]
	    else if (nusf == 1) {
	        call xgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	        call xgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	        call xgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	    } else {
	        call xgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	        call xgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	        call xgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
		do j = 2, nusf {
	            call xgsder (usf[j], der[1], der[2], tmp[1], 1, 0, 0)
	            call xgsder (usf[j], der[1], der[2], tmp[2], 1, 1, 0)
	            call xgsder (usf[j], der[1], der[2], tmp[3], 1, 0, 1)
		    der[3] = der[3] + tmp[1]
		    der[4] = der[4] + tmp[2]
		    der[5] = der[5] + tmp[3]
		}
		der[3] = der[3] / nusf
		der[4] = der[4] / nusf
		der[5] = der[5] / nusf
	    }

	    if (nvsf == 0)
		der[6] = der[2]
	    else if (nvsf == 1) {
	        call xgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	        call xgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	        call xgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	    } else {
	        call xgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	        call xgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	        call xgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
		do j = 2, nvsf {
	            call xgsder (vsf[j], der[1], der[2], tmp[1], 1, 0, 0)
	            call xgsder (vsf[j], der[1], der[2], tmp[2], 1, 1, 0)
	            call xgsder (vsf[j], der[1], der[2], tmp[3], 1, 0, 1)
		    der[6] = der[6] + tmp[1]
		    der[7] = der[7] + tmp[2]
		    der[8] = der[8] + tmp[3]
		}
		der[6] = der[6] / nvsf
		der[7] = der[7] / nvsf
		der[8] = der[8] / nvsf
	    }
	}
end


# TR_INIT -- Since the inversion iteration always begins from the last
# point we need to initialize before the first call to TR_INVERT.
# When using a WCS this simply returns.

procedure tr_init (ct, usf, nusf, vsf, nvsf, x, y, der)

pointer	ct			#I CT pointer
pointer	usf[ARB], vsf[ARB]	#I User coordinate surfaces
int	nusf, nvsf		#I Number of surfaces for each coordinate
real	x, y			#I Starting X and Y
real	der[8]			#O Inversion data

int	j
real	tmp[3]

begin
	if (ct != NULL)
	    return

	der[1] = x
	der[2] = y
	if (nusf == 0) {
	    der[3] = der[1]
	    der[4] = 1.
	    der[5] = 0.
	} else if (nusf == 1) {
	    call xgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	    call xgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	    call xgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	} else {
	    call xgsder (usf[1], der[1], der[2], der[3], 1, 0, 0)
	    call xgsder (usf[1], der[1], der[2], der[4], 1, 1, 0)
	    call xgsder (usf[1], der[1], der[2], der[5], 1, 0, 1)
	    do j = 2, nusf {
	        call xgsder (usf[j], der[1], der[2], tmp[1], 1, 0, 0)
	        call xgsder (usf[j], der[1], der[2], tmp[2], 1, 1, 0)
	        call xgsder (usf[j], der[1], der[2], tmp[3], 1, 0, 1)
		der[3] = der[3] + tmp[1]
		der[4] = der[4] + tmp[2]
		der[5] = der[5] + tmp[3]
	    }
	    der[3] = der[3] / nusf
	    der[4] = der[4] / nusf
	    der[5] = der[5] / nusf
	}

	if (nvsf == 0) {
	    der[6] = der[2]
	    der[7] = 0.
	    der[8] = 1.
	} else if (nvsf == 1) {
	    call xgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	    call xgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	    call xgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	} else {
	    call xgsder (vsf[1], der[1], der[2], der[6], 1, 0, 0)
	    call xgsder (vsf[1], der[1], der[2], der[7], 1, 1, 0)
	    call xgsder (vsf[1], der[1], der[2], der[8], 1, 0, 1)
	    do j = 2, nvsf {
	        call xgsder (vsf[j], der[1], der[2], tmp[1], 1, 0, 0)
	        call xgsder (vsf[j], der[1], der[2], tmp[2], 1, 1, 0)
	        call xgsder (vsf[j], der[1], der[2], tmp[3], 1, 0, 1)
		der[6] = der[6] + tmp[1]
		der[7] = der[7] + tmp[2]
		der[8] = der[8] + tmp[3]
	    }
	    der[6] = der[6] / nvsf
	    der[7] = der[7] / nvsf
	    der[8] = der[8] / nvsf
	}
end


# TR_EVAL -- Evalute coordinate function.
#
# This is an interface routine to allow using either an MWCS CT (coordinate
# transform) pointer or a GSURFIT SF (2D surface function) pointer.  The
# surface method is used with a FITCOORDS database.  The MWCS method is
# used to retransform an image with a WCS.

real procedure tr_eval (ct, sf, axis, x, y)

pointer	ct			#I CT pointer
pointer	sf			#I SF pointer
int	axis			#I World coordinate axis to return
real	x, y			#I Pixel coordinate to transform

real	w[2], xgseval()

begin
	if (sf != NULL)
	    return (xgseval (sf, x, y))
	
	call mw_c2tranr (CT_LW(ct), x, y, w[1], w[2])
	return (w[axis])
end


# TR_GETR -- Get real valued parameter.
#
# This is an interface routine to allow using either an MWCS CT (coordinate
# transform) pointer or a GSURFIT SF (2D surface function) pointer.  The
# surface method is used with a FITCOORDS database.  The MWCS method is
# used to retransform an image with a WCS.

real procedure tr_getr (ct, sf, param)

pointer	ct			#I CT pointer
pointer	sf			#I SF pointer
int	param			#I Parameter code

real	xgsgetr()

begin
	if (sf != NULL)
	    return (xgsgetr (sf, param))

	switch (param) {
	case GSXMIN, GSYMIN: 
	    return (real (1))
	case GSXMAX:
	    return (real (CT_NX(ct)))
	case GSYMAX:
	    return (real (CT_NY(ct)))
	}
end
