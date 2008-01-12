# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLPUTI -- Set a CL parameter of type integer.

procedure clputi (param, value)

char	param[ARB]		# parameter to be set
int	value			# new value
long	lval

begin
	lval = value
	if (IS_INDEFI (value))
	    lval = INDEFL

	call clputl (param, lval)
end


# CLPUTS -- Set a CL parameter of type short.

procedure clputs (param, value)

char	param[ARB]		# parameter to be set
short	value			# new value
long	lval

begin
	lval = value
	if (IS_INDEFS (value))
	    lval = INDEFL

	call clputl (param, lval)
end


# CLPUTL -- Put a long integer parameter to the CL.

procedure clputl (param, lval)

char	param[ARB]
long	lval

pointer	sp, value, pname
pointer	clc_find()

begin
	call smark (sp)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (pname, SZ_FNAME, TY_CHAR)

	# Update the parameter in the CL.
	call sprintf (Memc[value], SZ_FNAME, "%d")
	    call pargl (lval)
	call fprintf (CLOUT, "%s = %s\n")
	    call pargstr (param)
	    call pargstr (Memc[value])

	# If the parameter is in the cache, update the cached value as well.
	if (clc_find (param, Memc[pname], SZ_FNAME) != NULL)
	    call clc_enter (Memc[pname], Memc[value])

	call sfree (sp)
end
