# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"oif.h"

# OIF_ACCESS -- Test the accessibility or existence of an existing image, or the
# legality of the name of a new image.

procedure oif_access (root, extn, acmode, status)

char	root[ARB]		# root filename
char	extn[ARB]		# extension (SET on output if none specified)
int	acmode			# access mode (0 to test only existence)
int	status

pointer	sp, fname
bool	streq()
int	btoi(), access()
string	oif_extn OIF_HDREXTN

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# If new image, test only the legality of the given extension.
	# This is used to select a kernel given the imagefile extension.

	if (acmode == NEW_IMAGE || acmode == NEW_COPY) {
	    status = btoi (streq (extn, oif_extn))
	    call sfree (sp)
	    return
	}

	# The OIF format permits image header filenames with no extensions.
	# If no extension was given, look for a file both with the standard
	# extension, and without any extension.  If such an image is found
	# return the actual extension.

	status = YES
	if (extn[1] == EOS) {
	    call iki_mkfname (root, oif_extn, Memc[fname], SZ_PATHNAME)
	    if (access (Memc[fname], acmode, 0) == YES) {
		call strcpy (oif_extn, extn, MAX_LENEXTN)
		call sfree (sp)
		return
	    } else if (access (root, acmode, 0) == YES) {
		call sfree (sp)
		return
	    }
	} else if (streq (extn, oif_extn)) {
	    call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)
	    if (access (Memc[fname], acmode, 0) == YES) {
		call sfree (sp)
		return
	    }
	}

	status = NO
	call sfree (sp)
end
