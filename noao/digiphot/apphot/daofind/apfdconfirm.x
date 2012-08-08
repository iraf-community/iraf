# AP_FDCONFIRM -- Procedure to confirm the critical daofind parameters.

procedure ap_fdconfirm (ap)

pointer	ap		# pointer to the apphot structure

real	rval
real	ap_vfwhmpsf(), ap_vsigma(), ap_vthreshold()
real	ap_vdatamin(), ap_vdatamax()

begin
	call printf ("\n")

	# Verify the critical parameters.
	rval = ap_vfwhmpsf (ap)
	rval = ap_vsigma (ap)
	rval = ap_vthreshold (ap)
	rval = ap_vdatamin (ap)
	rval = ap_vdatamax (ap)

	call printf ("\n")
end
