# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stf.h"

# STF_ACCESS -- Test the accessibility or existence of an existing image, or the
# legality of the name of a new image.

procedure stf_access (root, extn, acmode, status)

char	root[ARB]		# root filename
char	extn[ARB]		# extension (SET on output if none specified)
int	acmode			# access mode (0 to test only existence)
int	status

int	nchars
pointer	sp, fname, stf_extn
int	access(), strmatch(), strlen()
string	stf_pattern STF_HDRPATTERN

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (stf_extn, MAX_LENEXTN, TY_CHAR)

	nchars = strlen (extn)

	# If new image, test only the legality of the given extension.
	# This is used to select a kernel given the imagefile extension.

	if (acmode == NEW_IMAGE || acmode == NEW_COPY) {
	    if ((strmatch (extn, stf_pattern) > 0) && nchars == MAX_LENEXTN)
		status = YES
	    else
		status = NO
	    call sfree (sp)
	    return
	}

	# If no extension was given, look for a file with the default
	# extension, and return the actual extension if an image with the
	# default extension is found.

	status = YES
	if (extn[1] == EOS) {
	    call stf_gethdrextn (Memc[stf_extn], MAX_LENEXTN)
	    call iki_mkfname (root, Memc[stf_extn], Memc[fname], SZ_PATHNAME)
	    if (access (Memc[fname], acmode, 0) == YES) {
		call strcpy (Memc[stf_extn], extn, MAX_LENEXTN)
		call sfree (sp)
		return
	    }

	} else if ((strmatch (extn,stf_pattern) > 0) && nchars == MAX_LENEXTN) {
	    call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)
	    if (access (Memc[fname], acmode, 0) == YES) {
		call sfree (sp)
		return
	    }
	}

	status = NO
	call sfree (sp)
end
