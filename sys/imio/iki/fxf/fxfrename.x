# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<error.h>
include "fxf.h"


# FIT_RENAME -- Rename a fits file. NOTE: There is no prevision at this
# time to rename an extension.

procedure fxf_rename (kernel, oroot, oextn, nroot, nextn, status)

int	kernel			#I IKI kernel
char	oroot[ARB]		#I old image root name
char	oextn[ARB]		#I old image extn
char	nroot[ARB]		#I new image root name
char	nextn[ARB]		#I old image extn
int	status			#O status value

pointer	sp
int	cindx
pointer	ohdr_fname, nhdr_fname
bool	streq()

include "fxfcache.com"

begin
	call smark (sp)
	call salloc (ohdr_fname, SZ_PATHNAME, TY_CHAR)
	call salloc (nhdr_fname, SZ_PATHNAME, TY_CHAR)

	call fxf_init()

	# Generate filenames.
	call iki_mkfname (oroot, oextn, Memc[ohdr_fname], SZ_PATHNAME)
	call iki_mkfname (nroot, nextn, Memc[nhdr_fname], SZ_PATHNAME)

	if (!streq (Memc[ohdr_fname], Memc[nhdr_fname])) {
	    iferr (call rename (Memc[ohdr_fname], Memc[nhdr_fname]))
		call erract (EA_WARN)
	 
	    # Update the cache with the new name.
	    do cindx=1, rf_cachesize {
	        if (rf_fit[cindx] == NULL)
		    next
	        # Rename the cached entry.
	        if (streq (Memc[ohdr_fname], rf_fname[1,cindx]))
		    call strcpy (Memc[nhdr_fname], rf_fname[1,cindx], SZ_FNAME)
	    }
	}

	status = OK
	call sfree (sp)
end
