include "../lib/daophot.h"

# DP_NCONFIRM -- Procedure to confirm the critical nstar parameters.

procedure dp_nconfirm (dao)

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
