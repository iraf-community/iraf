# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<ctype.h>

# IMGETS -- Get the value of an image header parameter as a character string.
# The value is returned as a CL parameter of type string; the type coercion
# facilities of the CL may be used to convert to a different datatype if
# desired.

procedure t_imgets()

pointer	sp, im
pointer	image, param, value
pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (param, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_LINE,  TY_CHAR)

	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("param", Memc[param], SZ_FNAME)

	im = immap (Memc[image], READ_ONLY, 0)

	iferr (call imgstr (im, Memc[param], Memc[value], SZ_LINE)) {
	    call erract (EA_WARN)
	    call clpstr ("value", "0")
	} else
	    call clpstr ("value", Memc[value])

	call imunmap (im)
	call sfree (sp)
end
