# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_ACCESS -- Test if the named poefile exists and is accessible with the
# given mode (mode=0 merely tests for the existence of the poefile).

int procedure qp_access (poefile, mode)

char	poefile[ARB]		#I poefile name
int	mode			#I access mode

int	status
pointer	sp, fname
int	access()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	call qp_mkfname (poefile, QPOE_EXTN, Memc[fname], SZ_PATHNAME)
	status = access (Memc[fname], mode, 0)

	call sfree (sp)
	return (status)
end
