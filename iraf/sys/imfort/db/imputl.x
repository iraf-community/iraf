# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTL -- Put an image header parameter of type long integer.

procedure imputl (im, key, lval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
long	lval			# parameter value

size_t	sz_val
int	junk
pointer	sp, sval
int	ltoc()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (sval, sz_val, TY_CHAR)

	junk = ltoc (lval, Memc[sval], SZ_FNAME)
	call impstr (im, key, Memc[sval])

	call sfree (sp)
end
