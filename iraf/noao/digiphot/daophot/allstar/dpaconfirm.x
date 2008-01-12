include "../lib/daophotdef.h"

# DP_ACONFIRM -- Procedure to confirm the critical allstar parameters.

procedure dp_aconfirm (dao)

pointer	dao		# pointer to the group structure

int	dp_stati()

begin
	call printf ("\n")

	# Confirm the recentering and sky fitting parameters.
	call dp_vrecenter (dao)
	call dp_vgroupsky (dao)
	call dp_vfitsky (dao)
	if (dp_stati (dao, FITSKY) == YES) {
	    call dp_vsannulus (dao)
	    call dp_vwsannulus (dao)
	}

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
