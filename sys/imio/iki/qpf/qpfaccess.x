# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpf.h"

# QPF_ACCESS -- Test the accessibility or existence of an existing image,
# or the legality of the name of a new image.

procedure qpf_access (root, extn, acmode, status)

char	root[ARB]		#I root filename
char	extn[ARB]		#U extension (SET on output if none specified)
int	acmode			#I access mode (0 to test only existence)
int	status			#O ok or err

pointer	sp, fname

bool	streq()
int	btoi(), qp_access()
string	qpf_extn QPF_EXTN

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# If new image, test only the legality of the given extension.
	# This is used to select a kernel given the imagefile extension.

	if (acmode == NEW_IMAGE || acmode == NEW_COPY) {
	    status = btoi (streq (extn, qpf_extn))
	} else if (extn[1] == EOS) {
	    call iki_mkfname (root, qpf_extn, Memc[fname], SZ_PATHNAME)
	    status = qp_access (Memc[fname], acmode)
	    if (status != NO)
		call strcpy (qpf_extn, extn, MAX_LENEXTN)
	} else {
	    call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)
	    status = qp_access (Memc[fname], acmode)
	}

	call sfree (sp)
end
