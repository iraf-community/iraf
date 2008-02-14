# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pmset.h>


# IC_PMMAP -- Map pixel mask.

pointer procedure ic_pmmap (fname, mode, refim)

char	fname[ARB]		# Mask name
int	mode			# Image mode
pointer	refim			# Reference image
pointer	pm			# IMIO pointer (returned)

int	i, fnextn()
pointer	sp, extn, immap()
bool	streq()

begin
	call smark (sp)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	i = fnextn (fname, Memc[extn], SZ_FNAME)
	if (streq (Memc[extn], "pl"))
	    pm = immap (fname, mode, refim)
	else {
	    call strcpy (fname, Memc[extn], SZ_FNAME)
	    call strcat (".pl", Memc[extn], SZ_FNAME)
	    pm = immap (Memc[extn], mode, refim)
	}

	call sfree (sp)
	return (pm)
end
