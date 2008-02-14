# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_RENAME -- Rename a poefile.

procedure qp_rename (o_poefile, n_poefile)

char	o_poefile[ARB]		#I old poefile name
char	n_poefile[ARB]		#I new poefile name

size_t	sz_val
pointer	sp, o_fname, n_fname
string	extn QPOE_EXTN

begin
	call smark (sp)
	sz_val = SZ_PATHNAME
	call salloc (o_fname, sz_val, TY_CHAR)
	call salloc (n_fname, sz_val, TY_CHAR)

	call qp_mkfname (o_poefile, extn, Memc[o_fname], SZ_PATHNAME)
	call qp_mkfname (n_poefile, extn, Memc[n_fname], SZ_PATHNAME)
	call fm_rename (Memc[o_fname], Memc[n_fname])

	call sfree (sp)
end
