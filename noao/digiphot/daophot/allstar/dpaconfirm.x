include "../lib/daophot.h"

# DP_ACONFIRM -- Procedure to confirm the critical allstar parameters.

procedure dp_aconfirm (dao)

pointer	dao		# pointer to the group structure

begin
	call printf ("\n")

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
