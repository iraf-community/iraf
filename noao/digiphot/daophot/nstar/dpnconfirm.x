include "../lib/daophotdef.h"

# DP_NCONFIRM -- Procedure to confirm the critical nstar parameters.

procedure dp_nconfirm (dao)

pointer	dao		# pointer to the group structure

int	dp_stati()

begin
	call printf ("\n")

	# Confirm recentering and sky fitting.
	call dp_vrecenter (dao)
	call dp_vfitsky (dao)
	if (dp_stati (dao, FITSKY) == NO)
	    call dp_vgroupsky (dao)

	# Confirm the psf radius.
	call dp_vpsfrad (dao)

	# Confirm the fitting radius.
	call dp_vfitrad (dao)

	# Confirm the maximum group size.
	call dp_vmaxgroup (dao)

	# Confirm the minimum and maximum good data values.
	call dp_vdatamin (dao)
	call dp_vdatamax (dao)

	call printf ("\n")
end
