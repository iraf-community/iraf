# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"oif.h"

# IMF_PARSE -- Parse an image name into the root pathname and filename
# extension, if any.  Only the known image type extensions are recognized
# as extensions, hence this routine cannot be used to parse general filenames.

procedure imf_parse (image, root, extn)

char	image[ARB]		# input image name
char	root[SZ_PATHNAME]	# output root pathname
char	extn[MAX_LENEXTN]	# output extension

int	ip, op
int	dot, delim
pointer	sp, pattern
string	ex HDR_EXTENSIONS
int	strmatch(), strlen()

begin
	call smark (sp)
	call salloc (pattern, SZ_FNAME, TY_CHAR)

	dot = 0
	op  = 1

	# Copy image name to root and mark the position of the last dot.
	for (ip=1;  image[ip] != EOS;  ip=ip+1) {
	    root[op] = image[ip]
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
	#
	# Note - EX is a string of the form "|imh|hhh|...|". (iki.h).

	if (strlen (extn) == LEN_EXTN) {
	    delim = ex[1]
	    for (ip=2;  ex[ip] != EOS;  ip=ip+1) {
		op = pattern
		while (ex[ip] != delim && ex[ip+1] != EOS) {
		    Memc[op] = ex[ip]
		    op = op + 1
		    ip = ip + 1
		}
		Memc[op] = EOS
		if (strmatch (extn, Memc[pattern]) > 0) {
		    call sfree (sp)
		    return
		}
	    }
	}

	# Not a legal image header extension.  Restore the extn field to the
	# root and null the extn.

	root[dot] = '.'
	extn[1] = EOS
	call sfree (sp)
end
