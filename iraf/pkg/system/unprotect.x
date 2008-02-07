# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<protect.h>
include	<error.h>

# UNPROTECT -- Remove protection from a list of files.

procedure t_unprotect()

char	fname[SZ_FNAME]
int	status
pointer	list

int	clgfil(), protect()
pointer	clpopns()

begin
	list = clpopns ("files")

	while (clgfil (list, fname, SZ_FNAME) != EOF)
	    iferr (status = protect (fname, REMOVE_PROTECTION))
		call erract (EA_WARN)

	call clpcls (list)
end
