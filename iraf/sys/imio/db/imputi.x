# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTI -- Put an image header parameter of type integer.

procedure imputi (im, key, ival)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
int	ival			# parameter value
pointer	sp, sval

begin
	call smark (sp)
	call salloc (sval, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[sval], SZ_FNAME, "%d")
	    call pargi (ival)
	call impstr (im, key, Memc[sval])

	call sfree (sp)
end
