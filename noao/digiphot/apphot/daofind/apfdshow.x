# AP_FDSHOW -- Display the current find parameters.

procedure ap_fdshow (ap)

pointer	ap	# pointer to the apphot strucuture

begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_fshow (ap)
end
