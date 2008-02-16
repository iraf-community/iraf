# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPKSC -- Put an image header parameter of type short integer.

procedure impksc (im, key, value, comment)

pointer	im			# image descriptor
char	key[ARB]		# parameter to be set
short	value			# parameter value
char	comment[ARB]		# 
size_t	sz_val
pointer	sp, sval

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (sval, sz_val, TY_CHAR)

	call sprintf (Memc[sval], SZ_FNAME, "%d")
	    call pargs (value)
	call impstrc (im, key, Memc[sval], comment)

	call sfree (sp)
end
