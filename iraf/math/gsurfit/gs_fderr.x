# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <math/gsurfit.h>

# GS_DERPOLY -- Evaluate the new polynomial derivative surface.

procedure rgs_derpoly (coeff, x, y, zfit, npts, xterms, xorder, yorder, nxder,
	nyder, k1x, k2x, k1y, k2y)

real	coeff[ARB]		# 1D array of coefficients
real	x[npts]			# x values of points to be evaluated
real	y[npts]
real	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
int	nxder,nyder		# order of the derivatives in x and y
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, xbptr, ybptr, accum

begin
	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_REAL)
	call salloc (yb, yorder * npts, TY_REAL)
	call salloc (accum, npts, TY_REAL)

	# calculate basis functions
	call rgs_dpol (x, npts, xorder, nxder, k1x, k2x, Memr[xb])
	call rgs_dpol (y, npts, yorder, nyder, k1y, k2y, Memr[yb])

	# accumulate the output vector
	cptr = 0
	call aclrr (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        call aclrr (Memr[accum], npts)
	        xbptr = xb
	        do k = 1, xincr {
		    call awsur (Memr[accum], Memr[xbptr], Memr[accum], npts,
		        1.0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpr (Memr[accum], Memr[ybptr], zfit, zfit, npts)
	        cptr = cptr + xincr
	        ybptr = ybptr + npts
		switch (xterms) {
		case GS_XHALF:
		    if ((i + xorder + 1) > maxorder)
			xincr = xincr - 1
		default:
		    ;
		}
	    }
	} else {
	    call amulr (Memr[xb], Memr[yb], zfit, npts)
	    call amulkr (zfit, coeff[1], zfit, npts)
	    xbptr = xb + npts
	    do k = 1, xorder - 1 {
		call awsur (zfit, Memr[xbptr], zfit, npts, 1.0, coeff[k+1])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsur (zfit, Memr[ybptr], zfit, npts, 1.0, coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}


	call sfree (sp)
end

# GS_DERCHEB -- Evaluate the new Chebyshev polynomial derivative surface.

procedure rgs_dercheb (coeff, x, y, zfit, npts, xterms, xorder, yorder,
	nxder, nyder, k1x, k2x, k1y, k2y)

real	coeff[ARB]		# 1D array of coefficients
real	x[npts]			# x values of points to be evaluated
real	y[npts]
real	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
int	nxder,nyder		# order of the derivatives in x and y
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, xbptr, ybptr, accum

begin
	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_REAL)
	call salloc (yb, yorder * npts, TY_REAL)
	call salloc (accum, npts, TY_REAL)

	# calculate basis functions
	call rgs_dcheb (x, npts, xorder, nxder, k1x, k2x, Memr[xb])
	call rgs_dcheb (y, npts, yorder, nyder, k1y, k2y, Memr[yb])

	# accumulate thr output vector
	cptr = 0
	call aclrr (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        call aclrr (Memr[accum], npts)
	        xbptr = xb
	        do k = 1, xincr {
		    call awsur (Memr[accum], Memr[xbptr], Memr[accum], npts,
		        1.0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpr (Memr[accum], Memr[ybptr], zfit, zfit, npts)
	        cptr = cptr + xincr
	        ybptr = ybptr + npts
		switch (xterms) {
		case GS_XHALF:
		    if ((i + xorder + 1) > maxorder)
			xincr = xincr - 1
		default:
		    ;
		}
	    }
	} else {
	    call amulr (Memr[xb], Memr[yb], zfit, npts)
	    call amulkr (zfit, coeff[1], zfit, npts)
	    xbptr = xb + npts
	    do k = 1, xorder - 1 {
		call awsur (zfit, Memr[xbptr], zfit, npts, 1.0, coeff[k+1])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsur (zfit, Memr[ybptr], zfit, npts, 1.0, coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}

	# free temporary space
	call sfree (sp)
end


# GS_DERLEG -- Evaluate the new Legendre polynomial derivative surface.

procedure rgs_derleg (coeff, x, y, zfit, npts, xterms, xorder, yorder,
	nxder, nyder, k1x, k2x, k1y, k2y)

real	coeff[ARB]		# 1D array of coefficients
real	x[npts]			# x values of points to be evaluated
real	y[npts]
real	zfit[npts]		# the fitted points
int	npts			# number of points to be evaluated
int	xterms			# cross terms ?
int	xorder,yorder		# order of the polynomials in x and y
int	nxder,nyder		# order of the derivatives in x and y
real	k1x, k2x		# normalizing constants
real	k1y, k2y

int	i, k, cptr, maxorder, xincr
pointer	sp, xb, yb, accum, xbptr, ybptr

begin
	# allocate temporary space for the basis functions
	call smark (sp)
	call salloc (xb, xorder * npts, TY_REAL)
	call salloc (yb, yorder * npts, TY_REAL)
	call salloc (accum, npts, TY_REAL)

	# calculate basis functions
	call rgs_dleg (x, npts, xorder, nxder, k1x, k2x, Memr[xb])
	call rgs_dleg (y, npts, yorder, nyder, k1y, k2y, Memr[yb])

	cptr = 0
	call aclrr (zfit, npts)
	if (xterms != GS_XNONE) {
	    maxorder = max (xorder + 1, yorder + 1)
	    xincr = xorder
	    ybptr = yb
	    do i = 1, yorder {
	        xbptr = xb
	        call aclrr (Memr[accum], npts)
	        do k = 1, xincr {
		    call awsur (Memr[accum], Memr[xbptr], Memr[accum], npts,
		        1.0, coeff[cptr+k])
		    xbptr = xbptr + npts
	        }
	        call gs_asumvpr (Memr[accum], Memr[ybptr], zfit, zfit, npts)
	        cptr = cptr + xincr
	        ybptr = ybptr + npts
		switch (xterms) {
		case GS_XHALF:
		    if ((i + xorder + 1) > maxorder)
			xincr = xincr - 1
		default:
		    ;
		}
	    }
	} else {
	    call amulr (Memr[xb], Memr[yb], zfit, npts)
	    call amulkr (zfit, coeff[1], zfit, npts)
	    xbptr = xb + npts
	    do k = 1, xorder - 1 {
		call awsur (zfit, Memr[xbptr], zfit, npts, 1.0, coeff[k+1])
		xbptr = xbptr + npts
	    }
	    ybptr = yb + npts
	    do k = 1, yorder - 1 {
		call awsur (zfit, Memr[ybptr], zfit, npts, 1.0, coeff[xorder+k])
		ybptr = ybptr + npts
	    }
	}

	# free temporary space
	call sfree (sp)
end
