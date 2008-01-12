# AP_PYPARS -- Procedure to write the current polyphot parameters to the
# output file.

procedure ap_pypars (ap)

pointer	ap		# pointer to apphot structure

begin
	# Write the data dependent parameters.
	call ap_dapars (ap)

	# Write the centering parameters.
	call ap_cepars (ap)

	# Write the sky fitting parameters.
	call ap_sapars (ap)

	# Set the photometry parameters.
	call ap_popars (ap)
end
