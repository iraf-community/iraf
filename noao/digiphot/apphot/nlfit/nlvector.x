include	"nlfitdef.h"
include	"../lib/nlfit.h"

# NLVECTOR - Accumulate a series of data points. Surface data may be in
# either 1 or 2 D dimensional form

procedure nlvector (nl, x, y, zfit, nx, ny, nz)

pointer	nl		# pointer to nl fitting structure
real	x[nx]		# x ordinates
real	y[ny]		# y ordinates
real	zfit[nx,ARB]	# data values
int	nx, ny, nz	# number of points

int	i, j

begin
	# Compute the fitted function.
	if (nz == 1 && nx == ny) {
	    do i = 1, nx
		call zcall5 (NL_FUNC(nl), x[i], y[i],
		    PARAM(NL_PARAM(nl)), NL_NPARAMS(nl), zfit[i,1])
	} else if (nz == ny) {
	    do j = 1 , ny {
		do i = 1, nx
		    call zcall5 (NL_FUNC(nl), x[i], y[j],
			PARAM(NL_PARAM(nl)), NL_NPARAMS(nl), zfit[i,j])
	    }
	} else
	    call error ( 0, "NLVECTOR: Illegal nx, ny, or nz values" )
end
