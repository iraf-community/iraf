# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# WFGSURFIT.X -- Surface fitting package used by WCS function drivers.
#
# The following routines are used by the experimental function drivers tnx
# and zpx to decode polynomial fits stored in the image header in the form
# of a list of parameters and coefficients into surface descriptors in
# ra / dec or longitude latitude. The polynomial surfaces so encoded consist
# of corrections to function drivers tan and zpn. The package routines are
# modelled after the equivalent gsurfit routines and are consistent with them.
# The routines are:
#
#                 sf = wf_gsopen (wattstr)
#                     wf_gsclose (sf)
#
#                  z = wf_gseval (sf, x, y)
#                     wf_gscoeff (sf, coeff, ncoeff)
#                zder = wf_gsder (sf, x, y, nxder, nyder)
#
# WF_GSOPEN is used to open a surface fit encoded in a WCS attribute, returning
# the SF surface fitting descriptor.  wf_gsclose should be called later to free
# the descriptor.  WF_GSEVAL is called to evaluate the surface at a point.


define  SZ_GSCOEFFBUF     20

# Define the surface descriptor.
define  LEN_WFGSSTRUCT    20

define  WF_XRANGE       Memd[P2D($1)]   # 2. / (xmax - xmin), polynomials
define  WF_XMAXMIN      Memd[P2D($1+2)] # - (xmax + xmin) / 2., polynomials
define  WF_YRANGE       Memd[P2D($1+4)] # 2. / (ymax - ymin), polynomials
define  WF_YMAXMIN      Memd[P2D($1+6)] # - (ymax + ymin) / 2., polynomials
define  WF_TYPE         Memi[$1+8]      # Type of curve to be fitted
define  WF_XORDER       Memi[$1+9]      # Order of the fit in x
define  WF_YORDER       Memi[$1+10]     # Order of the fit in y
define  WF_XTERMS       Memi[$1+11]     # Cross terms for polynomials
define  WF_NCOEFF       Memi[$1+12]     # Total number of coefficients
define  WF_COEFF        Memi[$1+13]     # Pointer to coefficient vector
define  WF_XBASIS       Memi[$1+14]     # Pointer to basis functions (all x)
define  WF_YBASIS       Memi[$1+15]     # Pointer to basis functions (all y)

# Define the structure elements for the wf_gsrestore task.
define  WF_SAVETYPE     $1[1]
define  WF_SAVEXORDER   $1[2]
define  WF_SAVEYORDER   $1[3]
define  WF_SAVEXTERMS   $1[4]
define  WF_SAVEXMIN     $1[5]
define  WF_SAVEXMAX     $1[6]
define  WF_SAVEYMIN     $1[7]
define  WF_SAVEYMAX     $1[8]

# Define the permitted types of surfaces.
define  WF_CHEBYSHEV    1
define  WF_LEGENDRE     2
define  WF_POLYNOMIAL   3

# Define the cross-terms flags.
define  WF_XNONE        0       # no x-terms (old NO)
define  WF_XFULL        1       # full x-terms (new YES)
define  WF_XHALF        2       # half x-terms (new)

define	WF_SAVECOEFF	8


# WF_GSOPEN -- Decode the longitude / latitude or ra / dec mwcs attribute
# and return a gsurfit compatible surface descriptor.

pointer procedure wf_gsopen (atstr)

char    atstr[ARB]              #I the input mwcs attribute string

double  dval
int     ip, npar, szcoeff
pointer gs, sp, par, coeff
int     nscan(), ctod()
errchk  wf_gsrestore()

begin
        if (atstr[1] == EOS)
            return (NULL)

        call smark (sp)
        call salloc (par, SZ_LINE, TY_CHAR)

        gs = NULL
        npar = 0
        szcoeff = SZ_GSCOEFFBUF
        call malloc (coeff, szcoeff, TY_DOUBLE)

        call sscan (atstr)
        repeat {
            call gargwrd (Memc[par], SZ_LINE)
            if (nscan() == npar)
                break
            if (Memc[par] == EOS)
                break
            ip = 1
            if (ctod (Memc[par], ip, dval) <= 0)
                break
            if (npar >= szcoeff) {
                szcoeff =szcoeff + SZ_GSCOEFFBUF
                call realloc (coeff, szcoeff, TY_DOUBLE)
            }
            Memd[coeff+npar] = dval
            npar = npar + 1
        }

        iferr (call wf_gsrestore (gs, Memd[coeff]))
            gs = NULL

        call sfree (sp)
        call mfree (coeff, TY_DOUBLE)

        if (npar == 0)
            return (NULL)
        else
            return (gs)
end


# WF_GSCLOSE -- Procedure to free the surface descriptor.

procedure wf_gsclose (sf)

pointer sf      	#U the surface descriptor
errchk  mfree

begin
        if (sf == NULL)
            return

        if (WF_XBASIS(sf) != NULL)
            call mfree (WF_XBASIS(sf), TY_DOUBLE)
        if (WF_YBASIS(sf) != NULL)
            call mfree (WF_YBASIS(sf), TY_DOUBLE)
        if (WF_COEFF(sf) != NULL)
            call mfree (WF_COEFF(sf), TY_DOUBLE)

        if (sf != NULL)
            call mfree (sf, TY_STRUCT)
end


# WF_GSEVAL -- Procedure to evaluate the fitted surface at a single point.
# The WF_NCOEFF(sf) coefficients are stored in the vector pointed to by
# WF_COEFF(sf).

double procedure wf_gseval (sf, x, y)

pointer sf              #I pointer to surface descriptor structure
double  x               #I x value
double  y               #I y value

double  sum, accum
int     i, ii, k, maxorder, xorder

begin
        # Calculate the basis functions.
        switch (WF_TYPE(sf)) {
        case WF_CHEBYSHEV:
            call wf_gsb1cheb (x, WF_XORDER(sf), WF_XMAXMIN(sf), WF_XRANGE(sf),
                Memd[WF_XBASIS(sf)])
            call wf_gsb1cheb (y, WF_YORDER(sf), WF_YMAXMIN(sf), WF_YRANGE(sf),
                Memd[WF_YBASIS(sf)])
        case WF_LEGENDRE:
            call wf_gsb1leg (x, WF_XORDER(sf), WF_XMAXMIN(sf), WF_XRANGE(sf),
                Memd[WF_XBASIS(sf)])
            call wf_gsb1leg (y, WF_YORDER(sf), WF_YMAXMIN(sf), WF_YRANGE(sf),
                Memd[WF_YBASIS(sf)])
        case WF_POLYNOMIAL:
            call wf_gsb1pol (x, WF_XORDER(sf), WF_XMAXMIN(sf), WF_XRANGE(sf),
                Memd[WF_XBASIS(sf)])
            call wf_gsb1pol (y, WF_YORDER(sf), WF_YMAXMIN(sf), WF_YRANGE(sf),
                Memd[WF_YBASIS(sf)])
        default:
            call error (0, "WF_GSEVAL: Unknown surface type.")
        }

        # Initialize accumulator basis functions.
        sum = 0.

        # Loop over y basis functions.
        maxorder = max (WF_XORDER(sf) + 1, WF_YORDER(sf) + 1)
        xorder = WF_XORDER(sf)
        ii = 1

        do i = 1, WF_YORDER(sf) {
            # Loop over the x basis functions.
            accum = 0.
            do k = 1, xorder {
                accum = accum + Memd[WF_COEFF(sf)+ii-1] *
		    Memd[WF_XBASIS(sf)+k-1) 
                ii = ii + 1
            }
            accum = accum * Memd[WF_YBASIS(sf)+i-1]
            sum = sum + accum

            # Elements of the coefficient vector where neither k = 1 or i = 1
            # are not calculated if WF_XTERMS(sf) = NO.

            switch (WF_XTERMS(sf)) {
            case WF_XNONE:
                xorder = 1
            case WF_XHALF:
                if ((i + WF_XORDER(sf) + 1) > maxorder)
                    xorder = xorder - 1
            default:
                ;
            }
        }

        return (sum)
end


# WF_GSCOEFF -- Procedure to fetch the number and magnitude of the coefficients.
# If the WF_XTERMS(sf) = WF_XBI (YES) then the number of coefficients will be
# (WF_XORDER(sf) * WF_YORDER(sf)); if WF_XTERMS is WF_XTRI then the number
# of coefficients will be (WF_XORDER(sf) *  WF_YORDER(sf) - order *
# (order - 1) / 2) where order is the minimum of the x and yorders;  if
# WF_XTERMS(sf) = WF_XNONE then the number of coefficients will be
# (WF_XORDER(sf) + WF_YORDER(sf) - 1).

procedure wf_gscoeff (sf, coeff, ncoeff)

pointer	sf		#I pointer to the surface fitting descriptor
double	coeff[ARB]	#O the coefficients of the fit
int	ncoeff		#O the number of coefficients

begin
	# Calculate the number of coefficients.
	ncoeff = WF_NCOEFF(sf)
	call amovd (Memd[WF_COEFF(sf)], coeff, ncoeff)
end


# WF_GSDER -- Procedure to calculate a new surface which is a derivative of
# the input surface.

double procedure wf_gsder (sf1, x, y, nxd, nyd)

pointer	sf1		#I pointer to the previous surface
double	x		#I x values
double	y		#I y values
int	nxd, nyd	#I order of the derivatives in x and y

int	ncoeff, nxder, nyder, i, j, k
int	order, maxorder1, maxorder2, nmove1, nmove2
pointer	sf2, sp, coeff, ptr1, ptr2
double	zfit, norm
double	wf_gseval()

begin
	if (sf1 == NULL)
	    return (0)

	if (nxd < 0 || nyd < 0)
	    call error (0, "GSDER: Order of derivatives cannot be < 0")

	if (nxd == 0 && nyd == 0) {
	    zfit = wf_gseval (sf1, x, y)
	    return (zfit)
	}

	# Allocate space for new surface.
	call calloc (sf2, LEN_WFGSSTRUCT, TY_STRUCT)

	# check the order of the derivatives
	nxder = min (nxd, WF_XORDER(sf1) - 1)
	nyder = min (nyd, WF_YORDER(sf1) - 1)

	# Set up new surface.
	WF_TYPE(sf2) = WF_TYPE(sf1)

	# Set the derivative surface parameters.
	switch (WF_TYPE(sf2)) {
	case WF_LEGENDRE, WF_CHEBYSHEV, WF_POLYNOMIAL:

	    WF_XTERMS(sf2) = WF_XTERMS(sf1)

	    # Find the order of the new surface.
	    switch (WF_XTERMS(sf2)) {
	    case WF_XNONE: 
		if (nxder > 0 && nyder > 0) {
		    WF_XORDER(sf2) = 1
		    WF_YORDER(sf2) = 1
		    WF_NCOEFF(sf2) = 1
		} else if (nxder > 0) {
		    WF_XORDER(sf2) = max (1, WF_XORDER(sf1) - nxder)
		    WF_YORDER(sf2) = 1
		    WF_NCOEFF(sf2) = WF_XORDER(sf2)
		} else if (nyder > 0) {
		    WF_XORDER(sf2) = 1
		    WF_YORDER(sf2) = max (1, WF_YORDER(sf1) - nyder)
		    WF_NCOEFF(sf2) = WF_YORDER(sf2)
		}

	    case WF_XHALF:
		maxorder1 = max (WF_XORDER(sf1) + 1, WF_YORDER(sf1) + 1)
		order = max (1, min (maxorder1 - 1 - nyder - nxder,
		    WF_XORDER(sf1) - nxder))
	        WF_XORDER(sf2) = order
		order = max (1, min (maxorder1 - 1 - nyder - nxder,
		    WF_YORDER(sf1) - nyder))
	        WF_YORDER(sf2) = order
		order = min (WF_XORDER(sf2), WF_YORDER(sf2))
		WF_NCOEFF(sf2) = WF_XORDER(sf2) * WF_YORDER(sf2)  -
			order * (order - 1) / 2

	    default:
	        WF_XORDER(sf2) = max (1, WF_XORDER(sf1) - nxder)
	        WF_YORDER(sf2) = max (1, WF_YORDER(sf1) - nyder)
		WF_NCOEFF(sf2) = WF_XORDER(sf2) * WF_YORDER(sf2) 
	    }

	    # Define the data limits.
	    WF_XRANGE(sf2) = WF_XRANGE(sf1)
	    WF_XMAXMIN(sf2) = WF_XMAXMIN(sf1)
	    WF_YRANGE(sf2) = WF_YRANGE(sf1)
	    WF_YMAXMIN(sf2) = WF_YMAXMIN(sf1)

	default:
	    call error (0, "WF_GSDER: Unknown surface type.")
	}

	# Allocate space for coefficients and basis functions.
	call calloc (WF_COEFF(sf2), WF_NCOEFF(sf2), TY_DOUBLE)
	call calloc (WF_XBASIS(sf2), WF_XORDER(sf2), TY_DOUBLE)
	call calloc (WF_YBASIS(sf2), WF_YORDER(sf2), TY_DOUBLE)

	# Get coefficients.
	call smark (sp)
	call salloc (coeff, WF_NCOEFF(sf1), TY_DOUBLE)
	call wf_gscoeff (sf1, Memd[coeff], ncoeff)

	# Compute the new coefficients.
	switch (WF_XTERMS(sf2)) {
	case WF_XFULL:
	    ptr2 = WF_COEFF(sf2) + (WF_YORDER(sf2) - 1) * WF_XORDER(sf2)
	    ptr1 = coeff + (WF_YORDER(sf1) - 1) * WF_XORDER(sf1)
	    do i = WF_YORDER(sf1), nyder + 1, -1 {
		do j = i, i - nyder + 1, -1
		    call amulkd (Memd[ptr1+nxder], double (j - 1),
		        Memd[ptr1+nxder], WF_XORDER(sf2))
		do j = WF_XORDER(sf1), nxder + 1, - 1 {
		    do k = j , j - nxder + 1, - 1
			Memd[ptr1+j-1] = Memd[ptr1+j-1] * (k - 1)
		}
		call amovd (Memd[ptr1+nxder], Memd[ptr2], WF_XORDER(sf2))
		ptr2 = ptr2 - WF_XORDER(sf2)
		ptr1 = ptr1 - WF_XORDER(sf1)
	    }

	case WF_XHALF:
	    maxorder1 = max (WF_XORDER(sf1) + 1, WF_YORDER(sf1) + 1)
	    maxorder2 = max (WF_XORDER(sf2) + 1, WF_YORDER(sf2) + 1)
	    ptr2 = WF_COEFF(sf2) + WF_NCOEFF(sf2)
	    ptr1 = coeff + WF_NCOEFF(sf1)
	    do i = WF_YORDER(sf1), nyder + 1, -1 {
		nmove1 = max (0, min (maxorder1 - i, WF_XORDER(sf1)))
		nmove2 = max (0, min (maxorder2 - i + nyder, WF_XORDER(sf2)))
		ptr1 = ptr1 - nmove1
		ptr2 = ptr2 - nmove2
		do j = i, i - nyder + 1, -1
		    call amulkd (Memd[ptr1+nxder], double (j - 1),
		        Memd[ptr1+nxder], nmove2)
		do j = nmove1, nxder + 1, - 1 {
		    do k = j , j - nxder + 1, - 1
			Memd[ptr1+j-1] = Memd[ptr1+j-1] * (k - 1)
		}
		call amovd (Memd[ptr1+nxder], Memd[ptr2], nmove2)
	    }

	default:
	    if (nxder > 0 && nyder > 0) {
		Memd[WF_COEFF(sf2)] = 0.

	    } else if (nxder > 0) { 
		ptr1 = coeff
		ptr2 = WF_COEFF(sf2) + WF_NCOEFF(sf2) - 1
		do j = WF_XORDER(sf1), nxder + 1, -1 {
		    do k = j, j - nxder + 1, -1
			Memd[ptr1+j-1] = Memd[ptr1+j-1] * (k - 1)
		    Memd[ptr2] = Memd[ptr1+j-1]
		    ptr2 = ptr2 - 1
		}

	    } else if (nyder > 0) {
		ptr1 = coeff + WF_NCOEFF(sf1) - 1
		ptr2 = WF_COEFF(sf2)
		do i = WF_YORDER(sf1), nyder + 1, -1 {
		    do j = i, i - nyder + 1, - 1
			Memd[ptr1] = Memd[ptr1] * (j - 1)	
		    ptr1 = ptr1 - 1
		}
		call amovd (Memd[ptr1+1], Memd[ptr2], WF_NCOEFF(sf2))
	    }
	}

	# Evaluate the derivatives.
	zfit = wf_gseval (sf2, x, y)

	# Normalize.
	if (WF_TYPE(sf2) != WF_POLYNOMIAL) { 
	    norm = WF_XRANGE(sf2) ** nxder * WF_YRANGE(sf2) ** nyder
	    zfit = norm * zfit
	}

	# Free the space.
	call wf_gsclose (sf2)
	call sfree (sp)

	return (zfit)
end


# WF_GSRESTORE -- Procedure to restore the surface fit encoded in the
# image header as a list of double precision parameters and coefficients
# to the surface descriptor for use by the evaluating routines. The
# surface parameters, surface type, xorder (or number of polynomial
# terms in x), yorder (or number of polynomial terms in y), xterms,
# xmin, xmax and ymin and ymax, are stored in the first eight elements
# of the double array fit, followed by the WF_NCOEFF(sf) surface coefficients.

procedure wf_gsrestore (sf, fit)

pointer	sf		#O surface descriptor
double	fit[ARB]	#I array containing the surface parameters and
			#I coefficients

int	surface_type, xorder, yorder, order
double	xmin, xmax, ymin, ymax	

begin
	# Allocate space for the surface descriptor.
	call calloc (sf, LEN_WFGSSTRUCT, TY_STRUCT)

	xorder = nint (WF_SAVEXORDER(fit))
	if (xorder < 1)
	    call error (0, "WF_GSRESTORE: Illegal x order.")
	yorder = nint (WF_SAVEYORDER(fit))
	if (yorder < 1)
	    call error (0, "WF_GSRESTORE: Illegal y order.")

	xmin = WF_SAVEXMIN(fit)
	xmax = WF_SAVEXMAX(fit)
	if (xmax <= xmin)
	    call error (0, "WF_GSRESTORE: Illegal x range.")
	ymin = WF_SAVEYMIN(fit)
	ymax = WF_SAVEYMAX(fit)
	if (ymax <= ymin)
	    call error (0, "WF_GSRESTORE: Illegal y range.")

	# Set surface type dependent surface descriptor parameters.
	surface_type = nint (WF_SAVETYPE(fit))

	switch (surface_type) {
	case WF_LEGENDRE, WF_CHEBYSHEV, WF_POLYNOMIAL:
	    WF_XORDER(sf) = xorder
	    WF_XRANGE(sf) = double(2.0) / (xmax - xmin)
	    WF_XMAXMIN(sf) =  - (xmax + xmin) / double(2.0)
	    WF_YORDER(sf) = yorder
	    WF_YRANGE(sf) = double(2.0) / (ymax - ymin)
	    WF_YMAXMIN(sf) =  - (ymax + ymin) / double(2.0)
	    WF_XTERMS(sf) = WF_SAVEXTERMS(fit)
	    switch (WF_XTERMS(sf)) {
	    case WF_XNONE:
		WF_NCOEFF(sf) = WF_XORDER(sf) + WF_YORDER(sf) - 1
	    case WF_XHALF:
		order = min (xorder, yorder)
		WF_NCOEFF(sf) = WF_XORDER(sf) * WF_YORDER(sf) - order *
		    (order - 1) / 2
	    case WF_XFULL:
		WF_NCOEFF(sf) = WF_XORDER(sf) * WF_YORDER(sf)
	    }
	default:
	    call error (0, "WF_GSRESTORE: Unknown surface type.")
	}

	# Set remaining curve parameters.
	WF_TYPE(sf) = surface_type

	call malloc (WF_COEFF(sf), WF_NCOEFF(sf), TY_DOUBLE)
	call malloc (WF_XBASIS(sf), WF_XORDER(sf), TY_DOUBLE)
	call malloc (WF_YBASIS(sf), WF_YORDER(sf), TY_DOUBLE)

	# restore coefficient array
	call amovd (fit[WF_SAVECOEFF+1], Memd[WF_COEFF(sf)], WF_NCOEFF(sf))
end


# WF_GSB1POL -- Procedure to evaluate all the non-zero polynomial functions
# for a single point and given order.

procedure wf_gsb1pol (x, order, k1, k2, basis)

double  x               #I data point
int     order           #I order of polynomial, order = 1, constant
double  k1, k2          #I nomalizing constants, dummy in this case
double  basis[ARB]      #O basis functions

int     i

begin
        basis[1] = 1.
        if (order == 1)
            return

        basis[2] = x
        if (order == 2)
            return

        do i = 3, order
            basis[i] = x * basis[i-1]
end


# WF_GSB1LEG -- Procedure to evaluate all the non-zero Legendre functions for
# a single point and given order.

procedure wf_gsb1leg (x, order, k1, k2, basis)

double  x               #I data point
int     order           #I order of polynomial, order = 1, constant
double  k1, k2          #I normalizing constants
double  basis[ARB]      #O basis functions

int     i
double  ri, xnorm

begin
        basis[1] = 1.
        if (order == 1)
            return

        xnorm = (x + k1) * k2 
        basis[2] = xnorm
        if (order == 2)
            return

        do i = 3, order {
            ri = i
            basis[i] = ((2. * ri - 3.) * xnorm * basis[i-1] -
                       (ri - 2.) * basis[i-2]) / (ri - 1.)
        }
end


# WF_GSB1CHEB -- Procedure to evaluate all the non zero Chebyshev function
# for a given x and order.

procedure wf_gsb1cheb (x, order, k1, k2, basis)

double  x               #I number of data points
int     order           #I order of polynomial, 1 is a constant
double  k1, k2          #I normalizing constants
double  basis[ARB]      #O array of basis functions

int     i
double  xnorm

begin
        basis[1] = 1.
        if (order == 1)
            return

        xnorm = (x + k1) * k2
        basis[2] = xnorm
        if (order == 2)
            return

        do i = 3, order
            basis[i] = 2. * xnorm * basis[i-1] - basis[i-2]
end
