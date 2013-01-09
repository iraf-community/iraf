# TCS_ADDCOL -- Add a single column to the list of descriptors

procedure tcs_addcol (tp, cp, descrip, ndescrip, maxdescrip)

pointer	tp		# i: table descriptor
pointer	cp		# i: column descriptor
pointer	descrip[ARB]	# u: list of column array selectors
int	ndescrip	# u: number of column array selectors
int	maxdescrip	# i: length of descrip array
#--
string	toomany  "Overflow in descriptor array"

begin
	# Check for descriptor array overflow

	if (ndescrip >= maxdescrip)
	    call error (1, toomany)

	# Convert the column pointer to a table column descriptor
	# Function tcs_fillstruct can be found in tcs_open

	ndescrip = ndescrip + 1
	call tcs_fillstruct (tp, cp, "", descrip[ndescrip])

end

