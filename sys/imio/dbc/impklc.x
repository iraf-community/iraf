# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPKLC -- Put an image header parameter of type long integer.

procedure impklc (im, key, lval, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
long	lval			# parameter value
char	comment[ARB]		# 
pointer	sp, sval

begin
	call smark (sp)
	call salloc (sval, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[sval], SZ_FNAME, "%d")
	    call pargl (lval)
	call impstrc (im, key, Memc[sval], comment)

	call sfree (sp)
end
