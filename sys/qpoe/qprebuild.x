# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_REBUILD -- Rebuild a poefile to reclaim unused space, and render storage
# elements logically contiguous to improve file access efficiency.

procedure qp_rebuild (poefile)

char	poefile[ARB]		#I poefile name
pointer	sp, fname

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	call qp_mkfname (poefile, QPOE_EXTN, Memc[fname], SZ_PATHNAME)
	call fm_rebuild (Memc[fname])

	call sfree (sp)
end
