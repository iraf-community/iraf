# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_DELETE -- Delete a poefile.

procedure qp_delete (poefile)

char	poefile[ARB]		#I poefile name
pointer	sp, fname

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	call qp_mkfname (poefile, QPOE_EXTN, Memc[fname], SZ_PATHNAME)
	call delete (Memc[fname])

	call sfree (sp)
end
