# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLPSTR -- Put a string type parameter to the CL.

procedure clpstr (param, value)

char	param[ARB]		# param name
char	value[ARB]		# new value

size_t	sz_val
pointer	sp, pname
pointer	clc_find()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (pname, sz_val, TY_CHAR)

	call fprintf (CLOUT, "%s=\"%s\"\n")
	    call pargstr (param)
	    call pargstr (value)

	# If the parameter is in the cache, update the cached value as well.
	if (clc_find (param, Memc[pname], SZ_FNAME) != NULL)
	    call clc_enter (Memc[pname], value)

	call sfree (sp)
end
