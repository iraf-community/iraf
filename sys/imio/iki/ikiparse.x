# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "iki.h"

# IKI_PARSE -- Parse an image name into the root pathname and filename
# extension, if any.  Only the known image type extensions are recognized
# as extensions, hence this routine cannot be used to parse general filenames.

procedure iki_parse (image, root, extn)

char	image[ARB]			#I input image name
char	root[SZ_PATHNAME]		#U output root pathname
char	extn[MAX_LENEXTN]		#O output extension

pointer	sp, imname
int	ip, op, dot
int	strlen(), iki_validextn()
bool	streq()

begin
	call smark (sp)
	call salloc (imname, SZ_PATHNAME, TY_CHAR)

	dot = 0
	op  = 1

	# The following is a backwards-compatibility kludge.  If the image
	# name we are given is the canonical standard test image STD_TESTIMAGE
	# ("dev$pix") replace the name with the fully qualified name
	# DEF_TESTIMAGE.  This is necessary to avoid ambiguous image name
	# errors due to pix.imh and pix.hhh being in the same directory; these
	# are well known names, neither of which can easily be changed.

	if (streq (image, STD_TESTIMAGE))
	    call strcpy (DEF_TESTIMAGE, Memc[imname], SZ_PATHNAME)
	else
	    call strcpy (image, Memc[imname], SZ_PATHNAME)

	# Copy image name to root and mark the position of the last dot.
	for (ip=1;  Memc[imname+ip-1] != EOS;  ip=ip+1) {
	    root[op] = Memc[imname+ip-1]
	    if (root[op] == '.')
		dot = op
	    op = op + 1
	}

	root[op] = EOS
	extn[1]  = EOS

	# Reject . delimited fields longer than the maximum extension length.
	if (op - dot - 1 > MAX_LENEXTN)
	    dot = NULL

	# If found extension, chop the root and fill in the extn field.
	# If no extension found, we are all done.

	if (dot == NULL) {
	    call sfree (sp)
	    return
	} else {
	    root[dot] = EOS
	    call strcpy (root[dot+1], extn, MAX_LENEXTN)
	}

	# Search the list of legal imagefile extensions.  If the extension
	# given is not found in the list, tack it back onto the root and
	# return a null extension.  This is necessary if we are to allow
	# dot delimited fields within image names without requiring the
	# user to supply the image type extension.  For example, "im.c"
	# and "im.c.imh" must refer to the same image - ".c" is part of
	# the image name, not an image type extension.

	if (strlen(extn) >= MIN_LENEXTN)
	    if (iki_validextn (0, extn) > 0) {
		call sfree (sp)
		return
	    }

	# Not a legal image header extension.  Restore the extn field to the
	# root and null the extn.

	root[dot] = '.'
	extn[1] = EOS
	call sfree (sp)
end
