# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"mwcs.h"

.help WFMSPEC
.nf -------------------------------------------------------------------------
WFMSPEC -- WCS function driver for MULTISPEC spectral format.

The dispersion coordinate is along image lines and each line has independent
linear or nonlinear dispersion coordinates.  The dispersion coordinates are
defined by specn attributes where n is the physical line number.  The format
of the attributes is:

	ap beam dtype w1 dw nw z aplow aphigh coeffs...

where ap is the aperture number (unique within an image), beam is a beam
number (not used by the driver), dtype is the dispersion type with values

	0 = linear dispersion
	1 = log linear dispersion
	2 = nonlinear dispersion

w1 is the wavelength of the first physical pixel, dw is the average
increment per pixel between the first and last pixel, nw is the number of
pixels, z is a redshift factor to be applied to the dispersion coordinates,
aplow and aphigh are aperture limits defining the origin of the spectra (not
used by the driver), and coeffs are the nonlinear dispersion coefficients.

The nonlinear dispersion function coefficients may describe several function
types; chebyshev polynomial, legendre polynomial, linear spline, cubic
spline, linear interpolation in a pixel coordinate array, and linear
interpolation in a sampled array.

The axes and dispersion parameters are in terms of the physical image.  The
aperture number is used for the world coordinate of the line coordinate.
Coordinates outside the valid range are mapped to nearest valid world
coordinate.  In application this would give a correct world coordinate graph
for a general WCS blind graphics task (especially if all invalid pixels have
the same value as the last valid pixel).

Driver routines:

	FN_INIT		    wf_msp_init (fc, dir)
	FN_DESTROY	 wf_msp_destroy (fc)
	FN_FWD		     wf_msp_fwd (fc, v1, v2)
	FN_INV		     wf_msp_inv (fc, v1, v2)

In addition the nonlinear dispersion functions use the following routines:

	wf_msp_coeff	Convert the attribute string to a coefficient array
	wf_msp_eval	Evaluate the function (P->W)
	wf_msp_evali	Evaluate the inverse function (W->P)

.endhelp --------------------------------------------------------------------

# Driver specific fields of function call (FC) descriptor.
define	FC_NAPS		Memi[$1+FCU]		# number of apertures
define	FC_APS		Memi[$1+FCU+1]		# pointer to indep coords
define	FC_DTYPE	Memi[$1+FCU+2]		# pointer to dispersion type
define	FC_CRVAL	Memi[$1+FCU+3]		# pointer to linear origins
define	FC_CDELT	Memi[$1+FCU+4]		# pointer to linear intervals
define	FC_NPTS		Memi[$1+FCU+5]		# pointer to number of points
define	FC_Z		Memi[$1+FCU+6]		# pointer to doppler corrections
define	FC_COEFF	Memi[$1+FCU+7]		# pointer to nonlinear coeffs
define	FC_X		Memi[$1+FCU+8]		# pointer to last phys. coord.
define	FC_DYDX		Memi[$1+FCU+9]		# pointer to last deriv.
define	FC_DIR		Memi[$1+FCU+10]		# direction of transform

# Function types.
define	CHEBYSHEV	1	# CURFIT Chebyshev polynomial
define	LEGENDRE	2	# CURFIT Legendre polynomial
define	SPLINE3		3	# CURFIT cubic spline
define	SPLINE1		4	# CURFIT linear spline
define	PIXEL		5	# pixel coordinate array
define	SAMPLE		6	# sampled coordinates

# Dispersion types.
define	LINEAR		0	# linear
define	LOG		1	# log linear
define	NONLINEAR	2	# nonlinear

# Iterative inversion parameters.
define	NALLOC		10	# size of allocation increments
define  NIT		10	# max interations in determining inverse
define  DX		0.0001  # accuracy limit in pixels for inverse

# Size limiting definitions.
define	DEF_SZATVAL	2048	# dynamically resized if overflow


# WF_MSP_INIT -- Initialize the function call descriptor for the indicated
# type of transform (forward or inverse).

procedure wf_msp_init (fc, dir)

pointer	fc			#I pointer to FC descriptor
int	dir			#I type of transformation

pointer	ct, mw
int	sz_atval, naps, ip, i
pointer	sp, atkey, atval, aps, dtype, crval, cdelt, npts, z, coeff
int	strlen(), ctoi(), ctod()
double	x, dval, wf_msp_eval()
errchk	malloc, realloc

begin
	# Get pointers.
	ct = FC_CT(fc)
	mw = CT_MW(ct)

	# Check axes.
	if (FC_NAXES(fc) != 2 || CT_AXIS(ct,1) != 1 || CT_AXIS(ct,2) != 2)
	    call error (1, "WFMSPEC: Wrong axes")

	# Get spectrum information.
	call smark (sp)
	sz_atval = DEF_SZATVAL
	call malloc (atval, sz_atval, TY_CHAR)
	call salloc (atkey, SZ_ATNAME, TY_CHAR)

	for (naps=0;  ;  naps=naps+1) {
	    call sprintf (Memc[atkey], SZ_ATNAME, "spec%d")
		call pargi (naps+1)
	    iferr (call mw_gwattrs (mw, 2, Memc[atkey], Memc[atval], sz_atval))
		break

	    while (strlen (Memc[atval]) == sz_atval) {
		sz_atval = 2 * sz_atval
		call realloc (atval, sz_atval, TY_CHAR)
		call mw_gwattrs (mw, 2, Memc[atkey], Memc[atval], sz_atval)
	    }

	    if (naps == 0) {
		call malloc (aps, NALLOC, TY_INT) 
		call malloc (dtype, NALLOC, TY_INT) 
		call malloc (crval, NALLOC, TY_DOUBLE) 
		call malloc (cdelt, NALLOC, TY_DOUBLE) 
		call malloc (npts, NALLOC, TY_INT) 
		call malloc (z, NALLOC, TY_DOUBLE) 
		call malloc (coeff, NALLOC, TY_POINTER)
	    } else if (mod (naps, NALLOC) == 0) {
		call realloc (aps, naps+NALLOC, TY_INT) 
		call realloc (dtype, naps+NALLOC, TY_INT) 
		call realloc (crval, naps+NALLOC, TY_DOUBLE)
		call realloc (cdelt, naps+NALLOC, TY_DOUBLE) 
		call realloc (npts, naps+NALLOC, TY_INT) 
		call realloc (z, naps+NALLOC, TY_DOUBLE) 
		call realloc (coeff, naps+NALLOC, TY_POINTER) 
	    }

	    # Linear dispersion function.
	    ip = 1
	    if (ctoi (Memc[atval], ip, Memi[aps+naps]) <= 0)
		next
	    if (ctoi (Memc[atval], ip, Memi[dtype+naps]) <= 0)
		next
	    if (ctoi (Memc[atval], ip, Memi[dtype+naps]) <= 0)
		next
	    if (ctod (Memc[atval], ip, Memd[crval+naps]) <= 0)
		next
	    if (ctod (Memc[atval], ip, Memd[cdelt+naps]) <= 0)
		next
	    if (ctoi (Memc[atval], ip, Memi[npts+naps]) <= 0)
		next
	    if (ctod (Memc[atval], ip, Memd[z+naps]) <= 0)
		next
	    if (ctod (Memc[atval], ip, dval) <= 0)
		next
	    if (ctod (Memc[atval], ip, dval) <= 0)
		next
	    Memd[z+naps] = Memd[z+naps] + 1

	    # Set nonlinear dispersion function.
	    if (Memi[dtype+naps] == NONLINEAR)
		call wf_msp_coeff (Memc[atval+ip], Memi[coeff+naps],
		    double (0.5), double (Memi[npts+naps]+0.5))
	}

	if (naps <= 0)
	    call error (2, "WFMSPEC: No aperture information")

	call realloc (aps, naps, TY_INT) 
	call realloc (dtype, naps, TY_INT) 
	call realloc (crval, naps, TY_DOUBLE) 
	call realloc (cdelt, naps, TY_DOUBLE) 
	call realloc (npts, naps, TY_INT) 
	call realloc (z, naps, TY_DOUBLE) 
	call realloc (coeff, naps, TY_POINTER) 

	FC_NAPS(fc) = naps
	FC_APS(fc) = aps
	FC_DTYPE(fc) = dtype
	FC_CRVAL(fc) = crval
	FC_CDELT(fc) = cdelt
	FC_NPTS(fc) = npts
	FC_Z(fc) = z
	FC_COEFF(fc) = coeff
	FC_DIR(fc) = dir

	# Setup inverse parameters if needed.
	# The parameters make the interative inversion more efficient
	# when the inverse transformation is evaluated sequentially.

	if (dir == INVERSE) {
	   call malloc (crval, naps, TY_DOUBLE)
	   call malloc (cdelt, naps, TY_DOUBLE)
	   do i = 0, naps-1 {
		if (Memi[FC_NPTS(fc)+i] == 0)
		    next
		if (Memi[FC_DTYPE(fc)+i] == NONLINEAR) {
		    coeff = Memi[FC_COEFF(fc)+i]
		    x = Memi[FC_NPTS(fc)+i]
		    Memd[crval+i] = x
		    Memd[cdelt+i] = wf_msp_eval (Memd[coeff], x) -
			wf_msp_eval (Memd[coeff], x - 1)
		}
	    }
	    FC_X(fc) = crval
	    FC_DYDX(fc) = cdelt
	} else {
	    FC_X(fc) = NULL
	    FC_DYDX(fc) = NULL
	}

	call mfree (atval, TY_CHAR)
	call sfree (sp)
end


# WF_MSP_DESTROY -- Free function driver descriptor.

procedure wf_msp_destroy (fc)

pointer	fc			#I pointer to FC descriptor
int	i

begin
	do i = 1, FC_NAPS(fc)
	    if (Memi[FC_DTYPE(fc)+i-1] == NONLINEAR)
		call mfree (Memi[FC_COEFF(fc)+i-1], TY_DOUBLE)

	call mfree (FC_APS(fc), TY_INT)
	call mfree (FC_DTYPE(fc), TY_INT)
	call mfree (FC_CRVAL(fc), TY_DOUBLE)
	call mfree (FC_CDELT(fc), TY_DOUBLE)
	call mfree (FC_NPTS(fc), TY_INT)
	call mfree (FC_Z(fc), TY_DOUBLE)
	call mfree (FC_COEFF(fc), TY_POINTER)
	call mfree (FC_X(fc), TY_DOUBLE)
	call mfree (FC_DYDX(fc), TY_DOUBLE)
end


# WF_MSP_FWD -- Evaluate P -> W (physical to world transformation).

procedure wf_msp_fwd (fc, in, out)

pointer	fc			#I pointer to FC descriptor
double	in[2]			#I point to sample WCS at
double	out[2]			#O value of WCS at that point

int	i
pointer	coeff
double	din, wf_msp_eval()

begin
	i = nint (in[2]) - 1
	if (i < 0 || i >= FC_NAPS(fc))
	    call error (3, "WFMSPEC: Coordinate out of bounds")
	if (Memi[FC_NPTS(fc)+i] == 0)
	    call error (4, "WFMSPEC: No dispersion function")

	if (Memi[FC_DTYPE(fc)+i] == NONLINEAR) {
	    coeff = Memi[FC_COEFF(fc)+i]
	    out[2] = Memi[FC_APS(fc)+i]
	    out[1] = wf_msp_eval (Memd[coeff], in[1])
	} else {
	    din = max (0.5D0, min (double (Memi[FC_NPTS(fc)+i]+0.5), in[1]))
	    out[2] = Memi[FC_APS(fc)+i]
	    out[1] = Memd[FC_CRVAL(fc)+i] + Memd[FC_CDELT(fc)+i] * (din - 1)
	    if (Memi[FC_DTYPE(fc)+i] == LOG)
		out[1] = 10. ** out[1]
	}

	out[1] = out[1] / Memd[FC_Z(fc)+i]
end


# WF_MSP_INV -- Evaluate W -> P (world to physical transformation).

procedure wf_msp_inv (fc, in, out)

pointer	fc			#I pointer to FC descriptor
double	in[2]			#I point to sample WCS at
double	out[2]			#O value of WCS at that point

int	i
pointer	coeff
double	din, dinmin
double	wf_msp_evali()

begin
	out[2] = 1
	dinmin = abs (in[2] - Memi[FC_APS(fc)])
	do i = 1, FC_NAPS(fc)-1 {
	    din = abs (in[2] - Memi[FC_APS(fc)+i])
	    if (din < dinmin) {
		out[2] = i + 1
		dinmin = din
	    }
	}

	i = nint (out[2]) - 1
	if (i < 0 || i >= FC_NAPS(fc))
	    call error (5, "WFMSPEC: Coordinate out of bounds")
	if (Memi[FC_NPTS(fc)+i] == 0)
	    call error (6, "WFMSPEC: No dispersion function")

	din = in[1] * Memd[FC_Z(fc)+i]
	if (Memi[FC_DTYPE(fc)+i] == NONLINEAR) {
	    coeff = Memi[FC_COEFF(fc)+i]
	    out[1] = wf_msp_evali (Memd[coeff], din, Memd[FC_X(fc)+i],
		Memd[FC_DYDX(fc)+i])
	} else {
	    if (Memi[FC_DTYPE(fc)+i] == LOG)
		din = log10 (din)
	    out[1] = (din-Memd[FC_CRVAL(fc)+i]) / Memd[FC_CDELT(fc)+i] + 1
	    out[1] = max (0.5D0, min (double(Memi[FC_NPTS(fc)+i]+0.5), out[1]))
	}
end


# WF_MSP_COEFF -- Initialize nonlinear coefficient array.

procedure wf_msp_coeff (atval, coeff, xmin, xmax)

char	atval[ARB]		#I attribute string
pointer	coeff			#O coefficient array
double	xmin, xmax		#I x limits

double	dval, temp
int	ncoeff, type, order, ip, i
errchk	malloc, realloc
double	wf_msp_eval()
int	ctod()

begin
	coeff = NULL
	ncoeff = 5

	ip = 1
	while (ctod (atval, ip, dval) > 0) {
	    if (coeff == NULL)
		call malloc (coeff, NALLOC, TY_DOUBLE)
	    else if (mod (ncoeff, NALLOC) == 0)
		call realloc (coeff, ncoeff+NALLOC, TY_DOUBLE)
	    Memd[coeff+ncoeff] = dval
	    ncoeff = ncoeff + 1
	}
	if (coeff == NULL)
	    return

	# Convert range elements to a more efficient form.
	call realloc (coeff, ncoeff, TY_DOUBLE)
	Memd[coeff] = ncoeff
	i = 6
	while (i < ncoeff) {
	    type = nint (Memd[coeff+i+1])
	    order = nint (Memd[coeff+i+2])
	    switch (type) {
	    case CHEBYSHEV, LEGENDRE:
		dval = 2 / (Memd[coeff+i+4] - Memd[coeff+i+3])
		Memd[coeff+i+3] = (Memd[coeff+i+4] + Memd[coeff+i+3]) / 2
		Memd[coeff+i+4] = dval
		i = i + 6 + order
	    case SPLINE3:
		Memd[coeff+i+4] = nint (Memd[coeff+i+2]) /
		    (Memd[coeff+i+4] - Memd[coeff+i+3])
		i = i + 9 + order
	    case SPLINE1:
		Memd[coeff+i+4] = nint (Memd[coeff+i+2]) /
		    (Memd[coeff+i+4] - Memd[coeff+i+3])
		i = i + 7 + order
	    case PIXEL:
		i = i + 4 + order
	    case SAMPLE:
		Memd[coeff+i+3] = i + 5
		i = i + 5 + order
	    }
	}

	# Set function limits.
	Memd[coeff+1] = xmin
	Memd[coeff+2] = xmax
	dval = wf_msp_eval (Memd[coeff], xmin)
	temp = wf_msp_eval (Memd[coeff], xmax)
	Memd[coeff+3] = min (dval, temp)
	Memd[coeff+4] = max (dval, temp)
end


# WF_MSP_EVAL -- Evaluate nonlinear function.

double procedure wf_msp_eval (coeff, xin)

double	coeff[ARB]			#I coefficients
double	xin				#I physical coordinate for evaluation

int	i, j, k, ncoeff, type, order
double	xval, x, y, w, ysum, wsum, a, b, c	

begin
	ncoeff = nint (coeff[1])
	xval = max (coeff[2], min (coeff[3], xin))
	ysum = 0.
	wsum = 0.
	j = 6
	while (j < ncoeff) {
	    type = nint (coeff[j+2])
	    order = nint (coeff[j+3])
	    y = coeff[j+1]
	    w = coeff[j]
	    switch (type) {
	    case CHEBYSHEV:
		x = (xval - coeff[j+4]) * coeff[j+5]
		y = y + coeff[j+6]
		if (order > 1)
		    y = y + coeff[j+7] * x
		if (order > 2) {
		    k = j + 8
		    a = 1
		    b = x
		    do i = 3, order {
			c = 2 * x * b - a
			y = y + coeff[k] * c
			a = b
			b = c
			k = k + 1
		    }
		}
		j = j + 6 + order
	    case LEGENDRE:
		x = (xval - coeff[j+4]) * coeff[j+5]
		y = y + coeff[j+6]
		if (order > 1)
		    y = y + coeff[j+7] * x
		if (order > 2) {
		    k = j + 8
		    a = 1
		    b = x
		    do i = 3, order {
			c = ((2 * i - 3) * x * b - (i - 2) * a) / (i - 1)
			y = y + coeff[k] * c
			a = b
			b = c
			k = k + 1
		    }
		}
		j = j + 6 + order
	    case SPLINE3:
		x = (xval - coeff[j+4]) * coeff[j+5]
		i = max (0, min (int (x), order-1))
		k = j + 6 + i
		b = x - i
		a = 1 - b
		c = a * a * a
		y = y + c * coeff[k]
		c = 1 + 3 * a * (1 + a * b)
		y = y + c * coeff[k+1]
		c = 1 + 3 * b * (1 + a * b)
		y = y + c * coeff[k+2]
		c = b * b * b
		y = y + c * coeff[k+3]
		j = j + 9 + order
	    case SPLINE1:
		x = (xval - coeff[j+4]) * coeff[j+5]
		i = max (0, min (int (x), order-1))
		k = j + 6 + i
		b = x - i
		a = 1 - b
		y = y + a * coeff[k] + b * coeff[k+1]
		j = j + 7 + order
	    case PIXEL:
		i = max (1, min (int (xval), order-1))
		x = xval - i
		y = y + (1 - x) * coeff[j+3+i] + x * coeff[j+4+i]
		j = j + 4 + order
	    case SAMPLE:
		i = nint (coeff[j+4])
		for (k=j+2+order; i < k && xval > coeff[i+2]; i=i+2)
		    ;
		for (k=j+5; i > k && xval < coeff[i-2]; i=i-2)
		    ;
		coeff[j+4] = i
		x = (xval - coeff[i]) / (coeff[i+2] - coeff[i])
		y = y + (1 - x) * coeff[i+1] + x * coeff[i+3]
		j = j + 5 + order
	    }
	    ysum = ysum + w * y
	    wsum = wsum + w
	}
	ysum = ysum / wsum

	return (ysum)
end


# WF_MSP_EVALI -- Evaluate inverse of nonlinear function.

double procedure wf_msp_evali (coeff, y, x, dydx)

double	coeff[ARB]		#I function coefficients
double	y			#I world coord to invert
double	x			#U last physical coordinate
double	dydx			#U last coordinate derivative

int     i
double  xval, yval, y1, dx, dy
double  wf_msp_eval()
bool	fp_equald()

begin
	yval = max (coeff[4], min (coeff[5], y))

        dx = 0.
	dy = 0.
        do i = 1, NIT {
            y1 = wf_msp_eval (coeff, x)
            if (dx > 1.) {
		if (x + 1 < coeff[3])
		    dy = wf_msp_eval (coeff, x+1.) - y1
		else
		    dy = y1 - wf_msp_eval (coeff, x-1.)
            } else if (dx < -1.) {
		if (x - 1 > coeff[2])
		    dy = y1 - wf_msp_eval (coeff, x-1.)
		else
		    dy = wf_msp_eval (coeff, x+1.) - y1
	    }
	    if (!fp_equald (dy, 0.0D0))
		dydx = dy
            dx = (yval - y1) / dydx
            x = x + dx
	    x = max (coeff[2], min (coeff[3], x))
            if (abs (dx) < DX)
                break
        }

	if (i > NIT) {
	    xval = (coeff[2] + coeff[3]) / 2.
	    yval = abs (wf_msp_eval (coeff, xval) - y)
	    dx = (coeff[3] - coeff[2]) / 18.
	    while (dx > DX) {
		for (x=max (coeff[2],xval-9*dx); x<=min (coeff[3],xval+9*dx);
		    x=x+dx) {
		    dy = abs (wf_msp_eval (coeff, x) - y)
		    if (dy < yval) {
			xval = x
			yval = dy
		    }
		}
		dx = dx / 10.
	    }
	    x = xval
	    if (x + 1 < coeff[3])
		dy = wf_msp_eval (coeff, x+1.) - wf_msp_eval (coeff, x)
	    else
		dy = wf_msp_eval (coeff, x) - wf_msp_eval (coeff, x-1.)
	    if (!fp_equald (dy, 0.0D0))
		dydx = dy
	}
		
	yval = int (x)
	x = yval + nint ((x-yval) / DX) * DX

	return (x)
end
