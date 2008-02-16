# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPKIC -- Put an image header parameter of type integer.

procedure impkic (im, key, ival, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
int	ival			# parameter value
char	comment[ARB]		# 
pointer	sp, sval

begin
	call smark (sp)
	call salloc (sval, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[sval], SZ_FNAME, "%d")
	    call pargi (ival)
	call impstrc (im, key, Memc[sval], comment)

	call sfree (sp)
end
