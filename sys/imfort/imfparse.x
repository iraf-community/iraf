# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"oif.h"

# IMF_PARSE -- Parse an image name into the root pathname and filename
# extension, if any.  Only the known image type extensions are recognized
# as extensions, hence this routine cannot be used to parse general filenames.

procedure imf_parse (image, root, extn)

char	image[ARB]		# input image name
char	root[SZ_PATHNAME]	# output root pathname
char	extn[MAX_LENEXTN]	# output extension

int	delim, ip, op
pointer	sp, pattern, osfn
int	strmatch(), strlen()
string	ex HDR_EXTENSIONS

begin
	call smark (sp)
	call salloc (pattern, SZ_FNAME, TY_CHAR)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	# Parse the image name into the root and extn fields.  The portion
	# of the filename excluding any directory specification is also
	# escape sequence encoded.

	call imf_trans (image, root, extn)

	# Search the list of legal imagefile extensions.  If the extension
	# given is not found in the list, tack it back onto the root and
	# return a null extension.  This is necessary if we are to allow
	# dot delimited fields within image names without requiring the
	# user to supply the image type extension.  For example, "im.c"
	# and "im.c.imh" must refer to the same image - ".c" is part of
	# the image name, not an image type extension.
	#
	# Note - EX is a string of the form "|imh|hhh|...|". (iki.h).

	if (strlen(extn) == LEN_EXTN) {
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
	# root and null the extn.  Tacking on the dummy extension .foo and
	# later discarding it ensures that the root name is properly encoded
	# for the local host.

	if (strlen(extn) > 0) {
	    call strcpy (image, Memc[osfn], SZ_PATHNAME)
	    call strcat (".foo", Memc[osfn], SZ_PATHNAME)
	    call imf_trans (Memc[osfn], root, extn)
	    extn[1] = EOS
	}

	call sfree (sp)
end
