include "../lib/daophot.h"

# DP_GCONFIRM -- Confirm the critical GROUP task parameters.

procedure dp_gconfirm (dao)

pointer	dao		# pointer to the group structure

begin
	call printf ("\n")

	# Confirm the psf radius.
	call dp_vpsfrad (dao)

	# Confirm the fitting radius.
	call dp_vfitrad (dao)

	# Confirm the critical overlap.
	call dp_vcritovlap (dao)

	# Confirm the minimum and maximum good data values.
	call dp_vdatamin (dao)
	call dp_vdatamax (dao)

	call printf ("\n")
end
