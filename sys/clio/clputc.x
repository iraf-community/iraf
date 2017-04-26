# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# CLPUTC -- Put a character constant type parameter to the CL.

procedure clputc (param, cval)

char	param[ARB]
char	cval

pointer	sp, value, pname
pointer	clc_find()

begin
	call smark (sp)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (pname, SZ_FNAME, TY_CHAR)

	# Character constants are stored as strings in the CL.  Add single
	# quotes about the character value to deimit the string.  The
	# character may be represented as a single character or as an escape
	# sequence.

	call sprintf (Memc[value], SZ_FNAME, "'%c'")
	    call pargc (cval)
	call fprintf (CLOUT, "%s = %s\n")
	    call pargstr (param)
	    call pargstr (Memc[value])

	# If the parameter is in the cache, update the cached value as well.
	if (clc_find (param, Memc[pname], SZ_FNAME) != NULL)
	    call clc_enter (Memc[pname], Memc[value])

	call sfree (sp)
end
