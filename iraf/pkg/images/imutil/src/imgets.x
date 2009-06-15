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
size_t	sz_val
pointer	immap()
pointer	ip, op
int	stridxs()
include	<nullptr.inc>

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (param, sz_val, TY_CHAR)
	call salloc (value, sz_val,  TY_CHAR)

	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("param", Memc[param], SZ_LINE)

	im = immap (Memc[image], READ_ONLY, NULLPTR)

	iferr (call imgstr (im, Memc[param], Memc[value], SZ_LINE)) {
	    call erract (EA_WARN)
	    call clpstr ("value", "0")
	} else {
	    # Check for special case of string with double quotes.
	    if (stridxs ("\"", Memc[value]) != 0) {
		op = param
		for (ip=value; Memc[ip]!=EOS; ip=ip+1) {
		    if (Memc[ip] == '"') {
		        Memc[op] = '\\'
			op = op + 1
		    }
		    Memc[op] = Memc[ip]
		    op = op + 1
		}
		Memc[op] = EOS
	        call clpstr ("value", Memc[param])
	    } else
	        call clpstr ("value", Memc[value])
	}

	call imunmap (im)
	call sfree (sp)
end
