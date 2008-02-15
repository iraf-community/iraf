# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"oif.h"

# OIF_COPY -- Copy an image.  A special operator is provided for fast, blind
# copies of entire images.

procedure oif_copy (kernel, old_root, old_extn, new_root, new_extn, status)

int	kernel			#I IKI kernel
char	old_root[ARB]		# old image root name
char	old_extn[ARB]		# old image extn
char	new_root[ARB]		# new image root name
char	new_extn[ARB]		# new extn
int	status

pointer	sp
pointer	old_fname, new_fname

begin
	call smark (sp)
	call salloc (old_fname, SZ_PATHNAME, TY_CHAR)
	call salloc (new_fname, SZ_PATHNAME, TY_CHAR)

	# Get filename of old and new images.
	call iki_mkfname (old_root, old_extn, Memc[old_fname], SZ_PATHNAME)
	call iki_mkfname (new_root, OIF_HDREXTN, Memc[new_fname], SZ_PATHNAME)

	# For now, this is stubbed out.
	status = ERR
	call sfree (sp)
end
