include	"ccdtypes.h"

# CCDTYPES -- Return the CCD type name string.
# CCDTYPEI -- Return the CCD type code.


# CCDTYPES -- Return the CCD type name string.

procedure ccdtypes (im, name, sz_name)

pointer	im			# Image
char	name[sz_name]		# CCD type name
int	sz_name			# Size of name string

int	strdic()
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the image type string.  If none then return "none".
	# Otherwise get the corresponding package image type string.
	# If the image type is unknown return "unknown" otherwise return
	# the package name.

	call hdmgstr (im, "imagetyp", Memc[str], SZ_LINE)
	if (Memc[str] == EOS) {
	    call strcpy ("none", name, sz_name)
	} else {
	    call hdmname (Memc[str], name, sz_name)
	    if (name[1] == EOS)
		call strcpy (Memc[str], name, sz_name)
	    if (strdic (name, name, sz_name, CCDTYPES) == UNKNOWN)
	    	call strcpy ("unknown", name, sz_name)
	}

	call sfree (sp)
end


# CCDTYPEI -- Return the CCD type code.

int procedure ccdtypei (im)

pointer	im			# Image
int	ccdtype			# CCD type (returned)

pointer	sp, str1, str2
int	strdic()

begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Get the image type and if there is none then return the NONE code.
	call hdmgstr (im, "imagetyp", Memc[str1], SZ_LINE)
	if (Memc[str1] == EOS) {
	    ccdtype = NONE

	# Otherwise get the package type and convert to an image type code.
	} else {
	    call hdmname (Memc[str1], Memc[str2], SZ_LINE)
	    if (Memc[str2] == EOS)
		call strcpy (Memc[str1], Memc[str2], SZ_LINE)
	    ccdtype = strdic (Memc[str2], Memc[str2], SZ_LINE, CCDTYPES)
	}

	call sfree (sp)
	return (ccdtype)
end
