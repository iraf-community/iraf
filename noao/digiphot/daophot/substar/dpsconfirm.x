# DP_SCONFIRM -- Procedure to confirm the critical substar parameters.

procedure dp_sconfirm (dao)

pointer	dao		# pointer to the daophot structure

begin
	call printf ("\n")

	# Confirm the psf radius.
	call dp_vpsfrad (dao)

	# Confirm the minimum and maximum good data values.
	call dp_vdatamin (dao)
	call dp_vdatamax (dao)

	call printf ("\n")
end
