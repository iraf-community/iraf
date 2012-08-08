# DP_PCONFIRM -- Procedure to confirm the critical psf parameters.

procedure dp_pconfirm (dao)

pointer	dao		# pointer to the daophot structure

begin
	call printf ("\n")

	# Verify the functional form of the psf.
	call dp_vfunction(dao)
	call dp_vvarorder (dao)
	#call dp_vfexpand (dao)

	# Confirm the psf radius.
	call dp_vpsfrad (dao)

	# Confirm the fitting radius.
	call dp_vfitrad (dao)

	# Confirm the minimum and maximum good data values.
	call dp_vdatamin (dao)
	call dp_vdatamax (dao)

	call printf ("\n")
end
