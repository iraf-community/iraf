# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<imhdr.h>

# NEW_TITLE -- Get a new image title.
# The null string defaults to the original title.

procedure new_title (param, im)

char	param[ARB]			# Parameter
pointer	im				# Image descriptor
char	title[SZ_LINE]
int	strlen()

begin
	call clgstr (param, title, SZ_LINE)
	if (strlen (title) > 0)
	    call strcpy (title, IM_TITLE(im), SZ_IMTITLE) 
end


# NEW_PIXTYPE -- Get a new pixel datatype.
# The null string defaults to the original pixel datatype.

procedure new_pixtype (param, im)

char	param[ARB]			# Parameter
pointer	im				# Image descriptor

char	pixtype[7]
int	type_codes[6], i
data	type_codes /TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE, TY_COMPLEX/
int	strdic()

begin
	call clgstr ("pixtype", pixtype, 7)
	i = strdic (pixtype, pixtype, 7, "|short|int|long|real|double|complex|")
	if (i > 0)
	    IM_PIXTYPE(im) = type_codes[i]
end


# GET_ROOT -- Get the root name from an image.

procedure get_root (image, root, maxch)

char	image[ARB]			# Image name with possible section
char	root[ARB]			# Root image name
int	maxch				# Maximum length of root image name

begin
	call imgimage (image, root, maxch)
end


# GET_SECTION -- Get the image section from an image.

procedure get_section (image, section, maxch)

char	image[ARB]			# Image name with possible section
char	section[ARB]			# Section
int	maxch				# Maximum length of section

begin
	call imgsection (image, section, maxch)
end


# XT_MKIMTEMP -- Return the temporary output image name to be used.
# XT_DELIMTEMP -- Delete the temporary image.
#
# In order to have an output image be the same as the input
# image a temporary image is used.  When the temporary image has been
# created it replaces the desired output image name.  Only root names
# are considered.

procedure xt_mkimtemp (input, output, original, sz_fname)

char	input[ARB]			#I Input image
char	output[ARB]			#U Output image to use
char	original[ARB]			#O Root of original output image
int	sz_fname			#I Maximum size of image names

pointer	sp, inname, outname, extn
int	i, j, k, gstrmatch(), strncmp(), fnextn()
bool	xt_imnameeq()

begin
	call smark (sp)
	call salloc (inname, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	# Strip image sections leaving only the path and root image name
	# (with group and image kernel parameters).  To change to
	# remove group and image kernel stuff use imgcluster instead of
	# imgimage.

	call imgimage (input, Memc[inname], SZ_FNAME)
	if (gstrmatch (input, Memc[inname], i, k) > 0)
	    call strcpy (input, Memc[inname], k)

	call imgimage (output, Memc[outname], SZ_FNAME)
	if (gstrmatch (output, Memc[outname], j, k) > 0)
	    call strcpy (output, Memc[outname], k)

	call strcpy (Memc[outname], output, sz_fname)
	call strcpy (Memc[outname], original, sz_fname)

	# Check if the input and output images are the same.
	# First check if the path names are the same and then if
	# the image names are the same.  If they are return a temporary
	# image name with the same extension as the output image.

	if (i == j && strncmp (Memc[inname], Memc[outname], i-1) == 0) {
	    if (xt_imnameeq (Memc[inname], Memc[outname])) {
		call mktemp ("tmp", output, sz_fname)
		if (fnextn (original, Memc[extn], SZ_FNAME) > 0) {
		    call strcat (".", output, sz_fname)
		    call strcat (Memc[extn], output, sz_fname)
		}
	    }
	}

	call sfree (sp)
end


procedure xt_delimtemp (output, original)

char	output[ARB]		# Output image
char	original[ARB]		# Temporary output image name

bool	strne()
errchk	imdelete

begin
	# If the output image is not the same as the original output image name
	# replace the original output image with the new image.

	if (strne (output, original)) {
	    iferr (call imdelete (original))
		;
	    call imrename (output, original)
	}
end
