include "../lib/daophotdef.h"
include "../lib/peakdef.h"


# DP_PKSETUP -- Initialize the PEAK fitting structure.

procedure dp_pksetup (dao)

pointer	dao			# pointer to the daophot structure

pointer	peak

begin
	call malloc (DP_PEAK(dao), LEN_PKSTRUCT, TY_STRUCT)
	peak = DP_PEAK(dao)

	DP_PKNTERM(peak) = 0
	DP_PKCLAMP(peak) = NULL
	DP_PKNORMAL(peak) = NULL
	DP_PKRESID(peak) = NULL
	DP_PKDERIV(peak) = NULL
	DP_PKRESULT(peak) = NULL
	DP_PKOLDRESULT(peak) = NULL
end


# DP_MEMPK -- Allocate memory for the PEAK fitting arrays.

procedure dp_mempk (dao, nterm)

pointer	dao			# pointer to the daophot structure
int	nterm			# the number of terms to be fit

pointer	peak

begin
	peak = DP_PEAK(dao)

	call malloc (DP_PKCLAMP(peak), nterm, TY_REAL)
	call malloc (DP_PKNORMAL(peak), nterm * nterm, TY_REAL)
	call malloc (DP_PKRESID(peak), nterm * nterm, TY_REAL)
	call malloc (DP_PKDERIV(peak), nterm * nterm, TY_REAL)
	call malloc (DP_PKRESULT(peak), nterm * nterm, TY_REAL)
	call malloc (DP_PKOLDRESULT(peak), nterm * nterm, TY_REAL)
	DP_PKNTERM(peak) = nterm
end


# DP_PKCLOSE -- Free the PEAK fitting structure.

procedure dp_pkclose (dao)

pointer	dao			# pointer to the daophot structure

pointer	peak

begin
	peak = DP_PEAK(dao)
	if (DP_PKCLAMP(peak) != NULL)
	    call mfree (DP_PKCLAMP(peak), TY_REAL)
	if (DP_PKNORMAL(peak) != NULL)
	    call mfree (DP_PKNORMAL(peak), TY_REAL)
	if (DP_PKRESID(peak) != NULL)
	    call mfree (DP_PKRESID(peak), TY_REAL)
	if (DP_PKDERIV(peak) != NULL)
	    call mfree (DP_PKDERIV(peak), TY_REAL)
	if (DP_PKRESULT(peak) != NULL)
	    call mfree (DP_PKRESULT(peak), TY_REAL)
	if (DP_PKOLDRESULT(peak) != NULL)
	    call mfree (DP_PKOLDRESULT(peak), TY_REAL)
	call mfree (peak, TY_STRUCT)
end
