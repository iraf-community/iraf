include "../lib/daophot.h"

# DP_PCONFIRM -- Procedure to confirm the critical psf parameters.

procedure dp_pconfirm (dao)

pointer	dao		# pointer to the daophot structure

begin
	call printf ("\n")

	# Confirm that the psf is variable.
	call dp_vvarpsf (dao)

	# Confirm the psf radius.
	call dp_vpsfrad (dao)

	# Confirm the fitting radius.
	call dp_vfitrad (dao)

	# Confirm the matching radius.
	call dp_vmatchrad (dao)

	# Confirm the minimum and maximum good data values.
	call dp_vdatamin (dao)
	call dp_vdatamax (dao)

	call printf ("\n")
end
