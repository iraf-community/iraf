# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"oif.h"

# OIF_ACCESS -- Test the accessibility or existence of an existing image, or
# the legality of the name of a new image.

procedure oif_access (kernel, root, extn, acmode, status)

int	kernel			#I IKI kernel
char	root[ARB]		#I root filename
char	extn[ARB]		#U extension (SET on output if none specified)
int	acmode			#I access mode (0 to test only existence)
int	status			#O status

pointer	sp, fname
int	btoi(), access(), iki_validextn()
string	oif_extn OIF_HDREXTN
bool	strne()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# If new image, test only the legality of the given extension.
	# This is used to select a kernel given the imagefile extension.

	if (acmode == NEW_IMAGE || acmode == NEW_COPY) {
	    status = btoi (iki_validextn (kernel, extn) > 0)
	    call sfree (sp)
	    return
	}

	# Reject image if an invalid extension is given.
	if (extn[1] != EOS && strne (extn, oif_extn)) {
	    status = NO
	    call sfree (sp)
	    return
	}

	# Check for the imagefile.
	call iki_mkfname (root, oif_extn, Memc[fname], SZ_PATHNAME)
	if (access (Memc[fname], acmode, 0) == YES) {
	    if (extn[1] == EOS)
		call strcpy (oif_extn, extn, MAX_LENEXTN)
	    status = YES
	} else
	    status = NO

	call sfree (sp)
end
