# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# LOCVA -- Return the address (in CHAR units) of a variable.

pointer procedure locva (variable)

int	variable		# data object to be addressed
pointer	address

begin
	call zlocva (variable, address)
	return (address)
end
