# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	"stf.h"

# STF_DELETE -- Delete an image.  A special operator is required since the
# image is stored as two files.

procedure stf_delete (kernel, root, extn, status)

int	kernel			#I IKI kernel
char	root[ARB]		#I root filename
char	extn[ARB]		#U header file extension
int	status			#O return value

pointer	sp
pointer	hdr_fname, pix_fname
int	access()

begin
	call smark (sp)
	call salloc (hdr_fname, SZ_PATHNAME, TY_CHAR)
	call salloc (pix_fname, SZ_PATHNAME, TY_CHAR)

	# Generate filename.
	call iki_mkfname (root, extn, Memc[hdr_fname], SZ_PATHNAME)
	call stf_mkpixfname (root, extn, Memc[pix_fname], SZ_PATHNAME)

	# If the header cannot be deleted, leave the pixfile alone.
	iferr (call delete (Memc[hdr_fname]))
	    call erract (EA_WARN)
	else if (access (Memc[pix_fname],0,0) == YES) {
	    iferr (call delete (Memc[pix_fname]))
		call erract (EA_WARN)
	}

	status = OK
	call sfree (sp)
end
