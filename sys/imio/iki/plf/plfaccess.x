# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"plf.h"

# PLF_ACCESS -- Test the accessibility or existence of an existing image,
# or the legality of the name of a new image.

procedure plf_access (root, extn, acmode, status)

char	root[ARB]		#I root filename
char	extn[ARB]		#U extension (SET on output if none specified)
int	acmode			#I access mode (0 to test only existence)
int	status			#O ok or err

pointer	sp, fname

bool	streq()
int	btoi(), access()
string	plf_extn PLF_EXTN

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# If new image, test only the legality of the given extension.
	# This is used to select a kernel given the imagefile extension.

	if (acmode == NEW_IMAGE || acmode == NEW_COPY) {
	    status = btoi (streq (extn, plf_extn))
	} else if (extn[1] == EOS) {
	    call iki_mkfname (root, plf_extn, Memc[fname], SZ_PATHNAME)
	    status = access (Memc[fname], acmode, 0)
	    if (status != NO)
		call strcpy (plf_extn, extn, MAX_LENEXTN)
	} else {
	    call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)
	    status = access (Memc[fname], acmode, 0)
	}

	call sfree (sp)
end
