# DP_PKCONFIRM -- Procedure to confirm the critical peak parameters.

procedure dp_pkconfirm (dao)

pointer	dao		# pointer to the daophot structure

begin
	call printf ("\n")

	# Confirm recentering and sky fitting.
	call dp_vrecenter (dao)
	call dp_vfitsky (dao)

	# Confirm the psf radius.
	call dp_vpsfrad (dao)

	# Confirm the fitting radius.
	call dp_vfitrad (dao)

	# Confirm the minimum and maximum good data values.
	call dp_vdatamin (dao)
	call dp_vdatamax (dao)

	call printf ("\n")
end
