include "../lib/daophotdef.h"
include "../lib/nstardef.h"

# DP_MEMNSTAR -- Procedure to allocate sufficient memory for NSTAR.

procedure dp_memnstar (dao, max_star)

pointer	dao			# pointer to daophot structure
int	max_star		# maximum number of stars

pointer	nstar

begin
	nstar = DP_NSTAR(dao)

	if (DP_NPIX (nstar) != NULL)
	    call mfree (DP_NPIX (nstar), TY_INT)
	call malloc (DP_NPIX (nstar), max_star + 1, TY_INT)

	if (DP_NUMER (nstar) != NULL)
	    call mfree (DP_NUMER (nstar), TY_REAL)
	call malloc (DP_NUMER (nstar), max_star + 1, TY_REAL)

	if (DP_DENOM (nstar) != NULL)
	    call mfree (DP_DENOM (nstar), TY_REAL)
	call malloc (DP_DENOM (nstar), max_star + 1, TY_REAL)

	if (DP_RPIXSQ (nstar) != NULL)
	    call mfree (DP_RPIXSQ (nstar), TY_REAL)
	call malloc (DP_RPIXSQ (nstar), max_star + 1, TY_REAL)

	if (DP_SKIP (nstar) != NULL)
	    call mfree (DP_SKIP (nstar), TY_INT)
	call malloc (DP_SKIP (nstar), max_star + 1, TY_INT)

	if (DP_XCLAMP (nstar) != NULL)
	    call mfree (DP_XCLAMP (nstar), TY_REAL)
	call malloc (DP_XCLAMP (nstar), 3 * max_star + 1, TY_REAL)

	if (DP_XOLD (nstar) != NULL)
	    call mfree (DP_XOLD (nstar), TY_REAL)
	call malloc (DP_XOLD (nstar), 3 * max_star + 1, TY_REAL)

	if (DP_X (nstar) != NULL)
	    call mfree (DP_X (nstar), TY_REAL)
	call malloc (DP_X (nstar), 3 * max_star + 1, TY_REAL)

	if (DP_V (nstar) != NULL)
	    call mfree (DP_V (nstar), TY_REAL)
	call malloc (DP_V (nstar), 3 * max_star + 1, TY_REAL)

	if (DP_SUMWT (nstar) != NULL)
	    call mfree (DP_SUMWT (nstar), TY_REAL)
	call malloc (DP_SUMWT (nstar), max_star + 1, TY_REAL)

	if (DP_C (nstar) != NULL)
	    call mfree (DP_C (nstar), TY_REAL)
	call malloc (DP_C (nstar), (3 * max_star + 1) * (3 * max_star + 1),
	    TY_REAL)
end
