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

char	input[ARB]			# Input image
char	output[ARB]			# Output image to use
char	original[ARB]			# Root of original output image
int	sz_fname			# Maximum size of image names

char	root_in[SZ_FNAME]		# Root of input image
bool	streq()

begin
	# Check if the input and output images are the same.
	# If they are return a temporary image name.  If not
	# return the root of the output image.

	call get_root (input, root_in, SZ_FNAME)
	call get_root (output, original, SZ_FNAME)

	if (streq (root_in, original))
	    call mktemp ("tmp", output, sz_fname)
	else
	    call strcpy (original, output, sz_fname)
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
	    call imdelete (original)
	    call imrename (output, original)
	}
end
