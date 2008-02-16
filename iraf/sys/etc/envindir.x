# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# ENVINDIR -- Return the name of an environment variable which may be given
# by the value of another environment variable as "@envvar".

procedure envindir (envvar, outstr, maxch)

char	envvar[ARB]		# possibly indirect env. variable name
char	outstr[ARB]		# receives value of variable
int	maxch

size_t	sz_val
pointer	sp, envname
int	envfind()
errchk	syserrs

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (envname, sz_val, TY_CHAR)

	call strcpy (envvar, outstr, maxch)

	while (outstr[1] == '@') {
	    call strcpy (outstr[2], Memc[envname], SZ_FNAME)
	    if (envfind (Memc[envname], outstr, maxch) <= 0)
		call syserrs (SYS_ENVNF, Memc[envname])
	}

	call sfree (sp)
end
