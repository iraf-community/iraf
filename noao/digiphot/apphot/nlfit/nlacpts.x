include	"nlfitdef.h"
include	"../lib/nlfit.h"

# NLACPTS - Accumulate a series of data points.

real procedure nlacpts (nl, x, y, z, w, nx, ny, nz, wtflag)

pointer	nl		# pointer to nl fitting structure
real	x[nx]		# x ordinates
real	y[ny]		# y ordinates
real	z[nx,nz]	# data values
real	w[nx,nz]	# weights
int	nx, ny, nz	# number of points
int	wtflag		# type of weighting

int	i, j, nfree
real	sum, z0, dz

begin
	# Zero the accumulators.
	call aclrr (ALPHA(NL_ALPHA(nl)), NL_NFPARAMS(nl) ** 2)
	call aclrr (BETA(NL_BETA(nl)), NL_NFPARAMS(nl))

	# Accumulate the points into the fit.
	if (nz == 1 && nx == ny) {

	    NL_NPTS(nl) = nx
	    sum = 0.0
	    do i = 1, nx {
		call zcall7 (NL_DFUNC(nl), x[i], y[i], PARAM(NL_PARAM(nl)),
		    DPARAM(NL_DPARAM(nl)), NL_NPARAMS(nl), z0,
		    DERIV(NL_DERIV(nl)))
		dz = z[i,1] - z0
		call nl_accum (DERIV(NL_DERIV(nl)), PLIST(NL_PLIST(nl)), w[i,1],
		    dz, NL_NFPARAMS(nl), ALPHA(NL_ALPHA(nl)), 
		    BETA(NL_BETA(nl)))
		sum = sum + dz * dz * w[i,1]
	    }

	} else if (nz == ny) {

	    NL_NPTS(nl) = nx * ny
	    sum = 0.0
	    do j = 1 , ny {
		do i = 1, nx {
		    call zcall7 (NL_DFUNC(nl), x[i], y[j],
		        PARAM(NL_PARAM(nl)), DPARAM(NL_DPARAM(nl)),
		        NL_NPARAMS(nl), z0, DERIV(NL_DERIV(nl)))
		    dz = z[i,j] - z0
		    call nl_accum (DERIV(NL_DERIV(nl)), PLIST(NL_PLIST(nl)),
		        w[i,j], dz, NL_NFPARAMS(nl), ALPHA(NL_ALPHA(nl)), 
		        BETA(NL_BETA(nl)))
		    sum = sum + w[i,j] * dz * dz
	        }
	    }

	} else
	    call error ( 0, "NLACPTS: Illegal nx, ny, or nz values" )

	# Return the reduced chisqr.
	nfree = NL_NPTS(nl) - NL_NFPARAMS(nl)
	if (nfree <= 0)
	    return (0.0)
	else
	    return (sum / nfree)
end


# NLRESID -- Recompute the residuals

real procedure nlresid (nl, x, y, z, w, nx, ny, nz, wtflag)

pointer	nl		# pointer to nl fitting structure
real	x[nx]		# x ordinates
real	y[ny]		# y ordinates
real	z[nx,nz]	# data values
real	w[nx,nz]	# weights
int	nx, ny, nz	# number of points
int	wtflag		# type of weighting

int	i, j, nfree
real	sum, z0, dz

begin
	# Accumulate the residuals.
	if (nz == 1 && nx == ny) {

	    NL_NPTS(nl) = nx
	    sum = 0.0
	    do i = 1, nx {
		call zcall5 (NL_FUNC(nl), x[i], y[i], TRY(NL_TRY(nl)),
		     NL_NPARAMS(nl), z0)
		dz = z[i,1] - z0
		sum = sum + dz * dz * w[i,1]
	    }

	} else if (nz == ny) {

	    NL_NPTS(nl) = nx * ny 
	    sum = 0.0
	    do j = 1 , ny {
		do i = 1, nx {
		    call zcall5 (NL_FUNC(nl), x[i], y[j], TRY(NL_TRY(nl)),
		        NL_NPARAMS(nl), z0)
		    dz = z[i,j] - z0
		    sum = sum + w[i,j] * dz * dz
	        }
	    }

	} else
	    call error ( 0, "NLACPTS: Illegal nx, ny, or nz values" )

	# Compute the reduced chisqr.
	nfree = NL_NPTS(nl) - NL_NFPARAMS(nl)
	if (nfree <= 0)
	    return (0.0)
	else
	    return (sum / nfree)

end


# NL_ACCUM -- Accumulate a single point into the fit

procedure nl_accum (deriv, list, w, dz, nfit, alpha, beta)

real	deriv[ARB]	# derivatives
int	list[ARB]	# list of active parameters
real	w		# weight
real	dz		# difference between data and model
int	nfit		# number of fitted parameters
real	alpha[nfit,ARB]	# alpha matrix
real	beta[nfit]	# beta matrix

int	i, j, k
real	wt

begin
	do i = 1, nfit {
	    wt = deriv[list[i]] * w
	    k = 1
	    do j = i, nfit {
	        alpha[k,i] = alpha[k,i] + wt * deriv[list[j]]
		k = k + 1
	    }
	    beta[i] = beta[i] + dz * wt
	}
end
