include	<error.h>
include	"ccdtypes.h"

# SET_INPUT -- Set the input image and image type.
#
# 1.  Open the input image.  Return warning and NULL pointer for an error.
# 2.  Get the requested CCD image type.
#	a. If no type is requested then accept the image.
#	b. If a type is requested then match against the image type.
#	   Unmap the image if no match.
# 3.  If the image is acceptable then get the CCD type code.

procedure set_input (image, im, ccdtype)

char	image[ARB]	# Input image name
pointer	im		# IMIO pointer (returned)
int	ccdtype		# CCD image type

bool	strne()
int	ccdtypei()
pointer	sp, str1, str2, immap()

begin
	# Open the image.  Return a warning and NULL pointer for an error.
	iferr (im = immap (image, READ_ONLY, 0)) {
	    call erract (EA_WARN)
	    im = NULL
	    return
	}

	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Get the requested CCD type.
	call clgstr ("ccdtype", Memc[str1], SZ_LINE)
	call xt_stripwhite (Memc[str1])
	if (Memc[str1] != EOS) {
	    call ccdtypes (im, Memc[str2], SZ_LINE)
	    if (strne (Memc[str1], Memc[str2]))
		call imunmap (im)
	}

	if (im != NULL)
	    ccdtype = ccdtypei (im)

	call sfree (sp)
end
