include "../lib/daophotdef.h"
include "../lib/allstardef.h"

# DP_ALLSTARSETUP -- Procedure to set up the ALLSTAR parameters.

procedure dp_allstarsetup (dp)

pointer	dp		# pointer to daophot structure

pointer	allstar

begin
	# Allocate memory.
	call malloc (DP_ALLSTAR(dp), LEN_ALLSTARSTRUCT, TY_STRUCT)
	allstar = DP_ALLSTAR(dp)

	DP_DATA(allstar) = NULL
	DP_SUBT(allstar) = NULL
	DP_WEIGHTS(allstar) = NULL
	DP_DBUF(allstar) = NULL
	DP_SBUF(allstar) = NULL
	DP_WBUF(allstar) = NULL

	DP_ANUMER(allstar) = NULL
	DP_ADENOM(allstar) = NULL
	DP_ASUMWT(allstar) = NULL
	DP_ARPIXSQ(allstar) = NULL
	DP_ASKIP(allstar) = NULL
	DP_AXCLAMP(allstar) = NULL
	DP_AYCLAMP(allstar) = NULL
	DP_AXOLD(allstar) = NULL
	DP_AYOLD(allstar) = NULL
	DP_AX(allstar) = NULL
	DP_AV(allstar) = NULL
	DP_AC(allstar) = NULL
	DP_ALAST(allstar) = NULL
	DP_ANPIX(allstar) = NULL
	DP_AIER(allstar) = NULL
end


# DP_ALMEMSTAR -- Procedure to allocate sufficient memory for ALLSTAR.

procedure dp_almemstar (dao, max_star, max_group)

pointer	dao			# pointer to daophot structure
int	max_star		# maximum number of stars
int	max_group		# maximum group size

pointer	allstar

begin
	allstar = DP_ALLSTAR(dao)

	if (DP_ANUMER (allstar) != NULL)
	    call mfree (DP_ANUMER (allstar), TY_REAL)
	call malloc (DP_ANUMER (allstar), max_star, TY_REAL)

	if (DP_ADENOM (allstar) != NULL)
	    call mfree (DP_ADENOM (allstar), TY_REAL)
	call malloc (DP_ADENOM (allstar), max_star, TY_REAL)

	if (DP_ARPIXSQ (allstar) != NULL)
	    call mfree (DP_ARPIXSQ (allstar), TY_REAL)
	call malloc (DP_ARPIXSQ (allstar), max_star, TY_REAL)

	if (DP_ASUMWT (allstar) != NULL)
	    call mfree (DP_ASUMWT (allstar), TY_REAL)
	call malloc (DP_ASUMWT (allstar), max_star, TY_REAL)

	if (DP_ANPIX (allstar) != NULL)
	    call mfree (DP_ANPIX (allstar), TY_INT)
	call malloc (DP_ANPIX (allstar), max_star, TY_INT)

	if (DP_AIER (allstar) != NULL)
	    call mfree (DP_AIER (allstar), TY_INT)
	call malloc (DP_AIER (allstar), max_star, TY_INT)

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

        # Allocate space for the fitting matrices. Note that nine
        # times less space is required if recentering is turned
        # off.
        if (DP_RECENTER(dao) == YES) {

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

        } else {

            if (DP_AX (allstar) != NULL)
                call mfree (DP_AX (allstar), TY_REAL)
            call malloc (DP_AX (allstar), max_group + 1, TY_REAL)

            if (DP_AV (allstar) != NULL)
                call mfree (DP_AV (allstar), TY_REAL)
            call malloc (DP_AV (allstar), max_group + 1, TY_REAL)

            if (DP_AC (allstar) != NULL)
                call mfree (DP_AC (allstar), TY_REAL)
            call malloc (DP_AC (allstar), (max_group + 1) * (max_group + 1),
                TY_REAL)
        }
end


# DP_ALCLOSE -- Procedure to close up the ALLSTAR parameters.

procedure dp_alclose (dp)

pointer	dp		# pointer to daophot structure

pointer	allstar

begin
	allstar = DP_ALLSTAR(dp)

	if (DP_ANUMER (allstar) != NULL)
	    call mfree (DP_ANUMER (allstar), TY_REAL)
	if (DP_ADENOM (allstar) != NULL)
	    call mfree (DP_ADENOM (allstar), TY_REAL)
	if (DP_ARPIXSQ (allstar) != NULL)
	    call mfree (DP_ARPIXSQ (allstar), TY_REAL)
	if (DP_ASUMWT (allstar) != NULL)
	    call mfree (DP_ASUMWT (allstar), TY_REAL)
	if (DP_ASKIP (allstar) != NULL)
	    call mfree (DP_ASKIP (allstar), TY_INT)
	if (DP_ALAST (allstar) != NULL)
	    call mfree (DP_ALAST (allstar), TY_INT)
	if (DP_AXCLAMP (allstar) != NULL)
	    call mfree (DP_AXCLAMP (allstar), TY_REAL)
	if (DP_AYCLAMP (allstar) != NULL)
	    call mfree (DP_AYCLAMP (allstar), TY_REAL)
	if (DP_AXOLD (allstar) != NULL)
	    call mfree (DP_AXOLD (allstar), TY_REAL)
	if (DP_AYOLD (allstar) != NULL)
	    call mfree (DP_AYOLD (allstar), TY_REAL)
	if (DP_AX (allstar) != NULL)
	    call mfree (DP_AX (allstar), TY_REAL)
	if (DP_AV (allstar) != NULL)
	    call mfree (DP_AV (allstar), TY_REAL)
	if (DP_AC (allstar) != NULL)
	    call mfree (DP_AC (allstar), TY_REAL)
	if (DP_ANPIX (allstar) != NULL)
	    call mfree (DP_ANPIX (allstar), TY_INT)
	if (DP_AIER (allstar) != NULL)
	    call mfree (DP_AIER (allstar), TY_INT)

	call mfree (allstar, TY_STRUCT)
end
