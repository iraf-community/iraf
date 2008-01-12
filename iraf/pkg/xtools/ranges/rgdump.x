# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


include	<pkg/rg.h>

# RG_DUMP -- Dump the contents of a range structure.

procedure rg_dump (rg)

pointer	rg		# Ranges

int	i

begin
	if (rg == NULL)
	    call printf ("RG_DUMP: The range pointer is NULL\n")
	else {
	    call printf ("RG_DUMP: NPTS = %d, NRGS = %d\n")
		call pargi (RG_NPTS(rg))
		call pargi (RG_NRGS(rg))
	    do i = 1, RG_NRGS(rg) {
		call printf ("  %4d - %4d\n")
		    call pargi (RG_X1(rg, i))
		    call pargi (RG_X2(rg, i))
	    }
	}
	call flush (STDOUT)
end
