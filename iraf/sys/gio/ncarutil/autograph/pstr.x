# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PSTR -- Print a character string from a fortran program.  The string is
# passed as an unpacked spp string, the result of f77upk in the calling
# program.  PSTR is called by agppid.f in the autograph package.

procedure pstr (spp_string)

char 	spp_string[ARB]

begin
	call eprintf ("%s\n")
	    call pargstr (spp_string)
end
