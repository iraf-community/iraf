# DP_RPARAM -- Encode a daophot real parameter.

procedure dp_rparam (out, keyword, value, units, comments)

int	out		# output file descriptor
char	keyword[ARB]	# keyword string
real	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-23.7g%41t%-10.10s%52t%-10s\n")
	    call pargstr (keyword)
	    call pargr (value)
	    call pargstr (units)
	    call pargstr ("%-23.7g")
	    call pargstr (comments)
end


# DP_IPARAM -- Encode a daophot integer parameter.

procedure dp_iparam (out, keyword, value, units, comments)

int	out		# output file descriptor
char	keyword[ARB]	# keyword string
int	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-23d%41t%-10.10s%52t%-10s\n")
	    call pargstr (keyword)
	    call pargi (value)
	    call pargstr (units)
	    call pargstr ("%-23d")
	    call pargstr (comments)
end


# DP_BPARAM -- Encode a daophot boolean parameter.

procedure dp_bparam (out, keyword, value, units, comments)

int	out		# output file descriptor
char	keyword[ARB]	# keyword string
bool	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-23b%41t%-10.10s%52t%-10s\n")
	    call pargstr (keyword)
	    call pargb (value)
	    call pargstr (units)
	    call pargstr ("%-23b")
	    call pargstr (comments)
end


# DP_SPARAM -- Encode a daophot string parameter.

procedure dp_sparam (out, keyword, value, units, comments)

int	out		# output file descriptor
char	keyword[ARB]	# keyword string
char	value[ARB]	# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-23.23s%41t%-10.10s%52t%-10s\n")
	    call pargstr (keyword)
	    call pargstr (value)
	    call pargstr (units)
	    call pargstr ("%-23s")
	    call pargstr (comments)
end
