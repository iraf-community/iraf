# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"stf.h"

# STF_GETHDREXTN -- Get the default header file extension.

procedure stf_gethdrextn (outstr, maxch)

char	outstr[maxch]		# receives header extension
int	maxch

pointer	sp, stf_extn
int	envfind(), strmatch()
bool	strne()

begin
	call smark (sp)
	call salloc (stf_extn, MAX_LENEXTN, TY_CHAR)

	# The default extension if no extension was given is the value
	# of the IMTYPE environment variable, else the default STF extn.
	#
	# Note - do not use the value of imtype if set to "imh", or the
	# new STF image will be inaccessible (IMIO will think it is an
	# OIF format image).  There should be a general way to do this
	# but for now exclude imh as a special case.

	call strcpy (STF_DEFHDREXTN, outstr, maxch)
	if (envfind (ENV_DEFIMTYPE, Memc[stf_extn], MAX_LENEXTN) >= 0)
	    if (strne (Memc[stf_extn], "imh"))
		if (strmatch (Memc[stf_extn], STF_HDRPATTERN) > 0)
		    call strcpy (Memc[stf_extn], outstr, maxch)

	call sfree (sp)
end
