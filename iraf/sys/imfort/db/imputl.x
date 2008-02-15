# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTL -- Put an image header parameter of type long integer.

procedure imputl (im, key, lval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
long	lval			# parameter value

int	junk
pointer	sp, sval
int	ltoc()

begin
	call smark (sp)
	call salloc (sval, SZ_FNAME, TY_CHAR)

	junk = ltoc (lval, Memc[sval], SZ_FNAME)
	call impstr (im, key, Memc[sval])

	call sfree (sp)
end
