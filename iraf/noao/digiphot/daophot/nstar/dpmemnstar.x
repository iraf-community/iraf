include "../lib/daophotdef.h"
include "../lib/nstardef.h"

# DP_NSTARSETUP -- Procedure to set up the NSTAR parameters.

procedure dp_nstarsetup (dp)

pointer	dp		# pointer to daophot structure

size_t	sz_val
pointer	nstar

begin
	# Allocate Memory
	sz_val = LEN_NSTARSTRUCT
	call malloc (DP_NSTAR(dp), sz_val, TY_STRUCT)
	nstar = DP_NSTAR(dp)

	DP_NNPIX(nstar) = NULL
	DP_NNUMER(nstar) = NULL
	DP_NDENOM(nstar) = NULL
	DP_NRPIXSQ(nstar) = NULL
	DP_NSKIP(nstar) = NULL
	DP_NXCLAMP(nstar) = NULL
	DP_NXOLD(nstar) = NULL
	DP_NX(nstar) = NULL
	DP_NV(nstar) = NULL
	DP_NSUMWT(nstar) = NULL
	DP_NC(nstar) = NULL
	DP_NIER(nstar) = NULL
end


# DP_MEMNSTAR -- Procedure to allocate sufficient memory for NSTAR.

procedure dp_memnstar (dao, max_star)

pointer	dao			# pointer to daophot structure
int	max_star		# maximum number of stars

size_t	sz_maxstar
pointer	nstar

begin
	nstar = DP_NSTAR(dao)
	sz_maxstar = max_star

	if (DP_NNPIX (nstar) != NULL)
	    call mfree (DP_NNPIX (nstar), TY_LONG)
	call malloc (DP_NNPIX (nstar), sz_maxstar + 1, TY_LONG)

	if (DP_NNUMER (nstar) != NULL)
	    call mfree (DP_NNUMER (nstar), TY_REAL)
	call malloc (DP_NNUMER (nstar), sz_maxstar + 1, TY_REAL)

	if (DP_NDENOM (nstar) != NULL)
	    call mfree (DP_NDENOM (nstar), TY_REAL)
	call malloc (DP_NDENOM (nstar), sz_maxstar + 1, TY_REAL)

	if (DP_NRPIXSQ (nstar) != NULL)
	    call mfree (DP_NRPIXSQ (nstar), TY_REAL)
	call malloc (DP_NRPIXSQ (nstar), sz_maxstar + 1, TY_REAL)

	if (DP_NSKIP (nstar) != NULL)
	    call mfree (DP_NSKIP (nstar), TY_INT)
	call malloc (DP_NSKIP (nstar), sz_maxstar + 1, TY_INT)

	if (DP_NIER (nstar) != NULL)
	    call mfree (DP_NIER (nstar), TY_INT)
	call malloc (DP_NIER(nstar), sz_maxstar + 1, TY_INT)

	if (DP_NSUMWT (nstar) != NULL)
	    call mfree (DP_NSUMWT (nstar), TY_REAL)
	call malloc (DP_NSUMWT (nstar), sz_maxstar + 1, TY_REAL)

	if (DP_RECENTER(dao) == YES) {

	    if (DP_NXCLAMP (nstar) != NULL)
	        call mfree (DP_NXCLAMP (nstar), TY_REAL)
	    call malloc (DP_NXCLAMP (nstar), 3 * sz_maxstar + 1, TY_REAL)

	    if (DP_NXOLD (nstar) != NULL)
	        call mfree (DP_NXOLD (nstar), TY_REAL)
	    call malloc (DP_NXOLD (nstar), 3 * sz_maxstar + 1, TY_REAL)

	    if (DP_NX (nstar) != NULL)
	        call mfree (DP_NX (nstar), TY_REAL)
	    call malloc (DP_NX (nstar), 3 * sz_maxstar + 1, TY_REAL)

	    if (DP_NC (nstar) != NULL)
	        call mfree (DP_NC (nstar), TY_REAL)
	    call malloc (DP_NC (nstar), (3 * sz_maxstar + 1) * (3 * sz_maxstar + 1),
	        TY_REAL)

	    if (DP_NV (nstar) != NULL)
	        call mfree (DP_NV (nstar), TY_REAL)
	    call malloc (DP_NV (nstar), 3 * sz_maxstar + 1, TY_REAL)

	} else {

	    if (DP_NXCLAMP (nstar) != NULL)
	        call mfree (DP_NXCLAMP (nstar), TY_REAL)
	    call malloc (DP_NXCLAMP (nstar), sz_maxstar + 1, TY_REAL)

	    if (DP_NXOLD (nstar) != NULL)
	        call mfree (DP_NXOLD (nstar), TY_REAL)
	    call malloc (DP_NXOLD (nstar), sz_maxstar + 1, TY_REAL)

	    if (DP_NX (nstar) != NULL)
	        call mfree (DP_NX (nstar), TY_REAL)
	    call malloc (DP_NX (nstar), sz_maxstar + 1, TY_REAL)

	    if (DP_NC (nstar) != NULL)
	        call mfree (DP_NC (nstar), TY_REAL)
	    call malloc (DP_NC (nstar), (sz_maxstar + 1) * (sz_maxstar + 1),
	        TY_REAL)

	    if (DP_NV (nstar) != NULL)
	        call mfree (DP_NV (nstar), TY_REAL)
	    call malloc (DP_NV (nstar), sz_maxstar + 1, TY_REAL)
	}
end


# DP_NSCLOSE -- Procedure to close up the NSTAR parameters.

procedure dp_nsclose (dp)

pointer	dp		# pointer to daophot structure

pointer	nstar

begin
	nstar = DP_NSTAR(dp)

	if (DP_NNPIX (nstar) != NULL)
	    call mfree (DP_NNPIX (nstar), TY_LONG)
	if (DP_NNUMER (nstar) != NULL)
	    call mfree (DP_NNUMER (nstar), TY_REAL)
	if (DP_NDENOM (nstar) != NULL)
	    call mfree (DP_NDENOM (nstar), TY_REAL)
	if (DP_NRPIXSQ (nstar) != NULL)
	    call mfree (DP_NRPIXSQ (nstar), TY_REAL)
	if (DP_NSKIP (nstar) != NULL)
	    call mfree (DP_NSKIP (nstar), TY_INT)
	if (DP_NXCLAMP (nstar) != NULL)
	    call mfree (DP_NXCLAMP (nstar), TY_REAL)
	if (DP_NXOLD (nstar) != NULL)
	    call mfree (DP_NXOLD (nstar), TY_REAL)
	if (DP_NX (nstar) != NULL)
	    call mfree (DP_NX (nstar), TY_REAL)
	if (DP_NV (nstar) != NULL)
	    call mfree (DP_NV (nstar), TY_REAL)
	if (DP_NSUMWT (nstar) != NULL)
	    call mfree (DP_NSUMWT (nstar), TY_REAL)
	if (DP_NC (nstar) != NULL)
	    call mfree (DP_NC (nstar), TY_REAL)
	if (DP_NIER (nstar) != NULL)
	    call mfree (DP_NIER (nstar), TY_INT)

	call mfree (nstar, TY_STRUCT)
end
