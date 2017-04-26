# DP_ADCONFIRM -- Confirm the critical ADDSTAR parameters.

procedure dp_adconfirm (dao)

pointer	dao		# pointer to the daophot structure

begin
	call printf ("\n")

	# Confirm the psf radius.
	call dp_vpsfrad (dao)

	call printf ("\n")
end
