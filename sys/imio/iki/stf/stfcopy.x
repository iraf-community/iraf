# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"stf.h"

# STF_COPY -- Copy an image.  A special operator is provided for fast, blind
# copies of entire images.

procedure stf_copy (kernel, oroot, oextn, nroot, nextn, status)

int	kernel			#I IKI kernel
char	oroot[ARB]		# old image root name
char	oextn[ARB]		# old image extn
char	nroot[ARB]		# new image root name
char	nextn[ARB]		# old image extn
int	status

pointer	sp
pointer	ohdr_fname, opix_fname, nhdr_fname, npix_fname

begin
	call smark (sp)
	call salloc (ohdr_fname, SZ_PATHNAME, TY_CHAR)
	call salloc (opix_fname, SZ_PATHNAME, TY_CHAR)
	call salloc (nhdr_fname, SZ_PATHNAME, TY_CHAR)
	call salloc (npix_fname, SZ_PATHNAME, TY_CHAR)

	# Generate filenames.
	call iki_mkfname (oroot, oextn, Memc[ohdr_fname], SZ_PATHNAME)
	call iki_mkfname (nroot, nextn, Memc[nhdr_fname], SZ_PATHNAME)

	call stf_mkpixfname (oroot, oextn, Memc[opix_fname], SZ_PATHNAME)
	call stf_mkpixfname (nroot, nextn, Memc[npix_fname], SZ_PATHNAME)

	# If the header cannot be copied, leave the pixfile alone.
	iferr (call fcopy (Memc[ohdr_fname], Memc[nhdr_fname]))
	    call erract (EA_WARN)
	else iferr (call fcopy (Memc[opix_fname], Memc[npix_fname]))
	    call erract (EA_WARN)

	call sfree (sp)
	status = OK
end
