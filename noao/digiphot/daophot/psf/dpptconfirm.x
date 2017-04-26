# DP_PTCONFIRM -- Confirm the critical PSTSELECT parameters.

procedure dp_ptconfirm (dao)

pointer	dao		# pointer to the daophot structure

begin
	call printf ("\n")

	# Confirm the psf radius.
	call dp_vpsfrad (dao)

	# Confirm the fitting radius.
	call dp_vfitrad (dao)

	# Confirm the data minimum and maximum values.
	call dp_vdatamin (dao)
	call dp_vdatamax (dao)

	call printf ("\n")
end
