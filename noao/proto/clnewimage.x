include	<imhdr.h>

# CL_NEW_IMAGE -- Get a new image title and pixel type.
#
# The strings 'default' or '*' are recognized as defaulting to the original
# title or pixel datatype.

define	NTYPES	5

procedure cl_new_image (im)

pointer	im				# Image descriptor
char	line[SZ_LINE]
int	i, type_codes[NTYPES]
int	stridx()
bool	strne()
string	types "silrd"			# supported image data types
data	type_codes /TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE/

begin
	call clgstr ("title", line, SZ_LINE)
	if (strne (line, "default") && strne (line, "*"))
	    call strcpy (line, IM_TITLE(im), SZ_IMTITLE) 

	call clgstr ("pixtype", line, SZ_LINE)
	if (strne (line, "default") && strne (line, "*")) {
	    i = stridx (line[1], types)
	    if (i != 0)
	        IM_PIXTYPE(im) = type_codes[stridx (line[1], types)]
	}
end
