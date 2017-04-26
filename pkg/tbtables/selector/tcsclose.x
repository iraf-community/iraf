# TCS_CLOSE -- Free memory associated with column selectors

procedure tcs_close (descrip, ndescrip)

pointer	descrip[ARB]	# i: column selectors
int	ndescrip	# i: number of descriptors
#--
int	id

begin
	do id = 1, ndescrip
	    call mfree (descrip[id], TY_INT)

end
