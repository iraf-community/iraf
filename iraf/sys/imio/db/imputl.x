# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTL -- Put an image header parameter of type long integer.

procedure imputl (im, key, lval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
long	lval			# parameter value
pointer	sp, sval

begin
	call smark (sp)
	call salloc (sval, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[sval], SZ_FNAME, "%d")
	    call pargl (lval)
	call impstr (im, key, Memc[sval])

	call sfree (sp)
end
