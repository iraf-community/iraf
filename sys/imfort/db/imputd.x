# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPUTD -- Put an image header parameter of type double.

procedure imputd (im, key, dval)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
double	dval			# double precision value

int	junk
pointer	sp, sval
int	dtoc()

begin
	call smark (sp)
	call salloc (sval, SZ_FNAME, TY_CHAR)

	junk = dtoc (dval, Memc[sval], SZ_FNAME, 15, 'g', SZ_FNAME)
	call impstr (im, key, Memc[sval])

	call sfree (sp)
end
