include	<math/nlfit.h>
include "nlfitdefd.h"

define	VAR	(($1 - 1) * $2 + 1)

# NLVECTOR -- Evaluate the fit for a series of data points.

procedure nlvectord (nl, x, zfit, npts, nvars)

pointer	nl		# pointer to nl fitting structure
double	x[ARB]		# independent variables (npts * nvars)
double	zfit[ARB]	# function values (npts)
int	npts		# number of points
int	nvars		# number of independent variables

int	i

begin
	# Compute the fitted function.
	do i = 1, npts
	    call zcall5 (NL_FUNC(nl), x[VAR (i, nvars)], nvars,
		PARAM(NL_PARAM(nl)), NL_NPARAMS(nl), zfit[i])
end
