include "../lib/daophotdef.h"
include "../lib/allstardef.h"

# DP_ALMEMSTAR -- Procedure to allocate sufficient memory for ALLSTAR.

procedure dp_almemstar (dao, max_star, max_group)

pointer	dao			# pointer to daophot structure
int	max_star		# maximum number of stars
int	max_group		# maximum group size

pointer	allstar

begin
	allstar = DP_ALLSTAR(dao)

	if (DP_ANUMER1 (allstar) != NULL)
	    call mfree (DP_ANUMER1 (allstar), TY_REAL)
	call malloc (DP_ANUMER1 (allstar), max_star, TY_REAL)

	if (DP_ANUMER2 (allstar) != NULL)
	    call mfree (DP_ANUMER2 (allstar), TY_REAL)
	call malloc (DP_ANUMER2 (allstar), max_star, TY_REAL)

	if (DP_ADENOM1 (allstar) != NULL)
	    call mfree (DP_ADENOM1 (allstar), TY_REAL)
	call malloc (DP_ADENOM1 (allstar), max_star, TY_REAL)

	if (DP_ADENOM2 (allstar) != NULL)
	    call mfree (DP_ADENOM2 (allstar), TY_REAL)
	call malloc (DP_ADENOM2 (allstar), max_star, TY_REAL)

	if (DP_ARPIXSQ (allstar) != NULL)
	    call mfree (DP_ARPIXSQ (allstar), TY_REAL)
	call malloc (DP_ARPIXSQ (allstar), max_star, TY_REAL)

	if (DP_ASUMWT (allstar) != NULL)
	    call mfree (DP_ASUMWT (allstar), TY_REAL)
	call malloc (DP_ASUMWT (allstar), max_star, TY_REAL)

	if (DP_ANPIX (allstar) != NULL)
	    call mfree (DP_ANPIX (allstar), TY_INT)
	call malloc (DP_ANPIX (allstar), max_star, TY_INT)

	if (DP_ASKIP (allstar) != NULL)
	    call mfree (DP_ASKIP (allstar), TY_INT)
	call malloc (DP_ASKIP (allstar), max_star, TY_INT)

	if (DP_ALAST (allstar) != NULL)
	    call mfree (DP_ALAST (allstar), TY_INT)
	call malloc (DP_ALAST (allstar), max_star, TY_INT)

	if (DP_AXCLAMP (allstar) != NULL)
	    call mfree (DP_AXCLAMP (allstar), TY_REAL)
	call malloc (DP_AXCLAMP (allstar), max_star, TY_REAL)

	if (DP_AYCLAMP (allstar) != NULL)
	    call mfree (DP_AYCLAMP (allstar), TY_REAL)
	call malloc (DP_AYCLAMP (allstar), max_star, TY_REAL)

	if (DP_AXOLD (allstar) != NULL)
	    call mfree (DP_AXOLD (allstar), TY_REAL)
	call malloc (DP_AXOLD (allstar), max_star, TY_REAL)

	if (DP_AYOLD (allstar) != NULL)
	    call mfree (DP_AYOLD (allstar), TY_REAL)
	call malloc (DP_AYOLD (allstar), max_star, TY_REAL)

	if (DP_AX (allstar) != NULL)
	    call mfree (DP_AX (allstar), TY_REAL)
	call malloc (DP_AX (allstar), 3 * max_group + 1, TY_REAL)

	if (DP_AV (allstar) != NULL)
	    call mfree (DP_AV (allstar), TY_REAL)
	call malloc (DP_AV (allstar), 3 * max_group + 1, TY_REAL)

	if (DP_AC (allstar) != NULL)
	    call mfree (DP_AC (allstar), TY_REAL)
	call malloc (DP_AC (allstar), (3 * max_group + 1) *
	    (3 * max_group + 1), TY_REAL)
end
