# PT_KID -- Decode a column specification into a name and an element.

procedure pt_kid (column, name, element)

char	column[ARB]	# column name
char	name[ARB]	# column name
int	element		# column element

char	left_bracket
int	index
int	stridx(), ctoi()
data	left_bracket /'['/

begin
	# Get the proper name in upper case and strip off and subscript.
	call strcpy (column, name, SZ_FNAME)
	call strupr (name)
	index = stridx (left_bracket, name)
	if (index > 0) {
	    name[index] = EOS
	    index = index + 1
	    if (ctoi (column, index, element) < 0)
		element = 1
	} else
	    element = 1
end
