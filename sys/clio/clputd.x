# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLPUTD -- Put a double precision parameter to the CL.

procedure clputd (param, dval)

char	param[ARB]
double	dval

pointer	sp, value, pname
pointer	clc_find()

begin
	call smark (sp)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (pname, SZ_FNAME, TY_CHAR)

	# Update the parameter in the CL.
	call sprintf (Memc[value], SZ_FNAME, "%g")
	    call pargd (dval)
	call fprintf (CLOUT, "%s = %s\n")
	    call pargstr (param)
	    call pargstr (Memc[value])

	# If the parameter is in the cache, update the cached value as well.
	if (clc_find (param, Memc[pname], SZ_FNAME) != NULL)
	    call clc_enter (Memc[pname], Memc[value])

	call sfree (sp)
end
