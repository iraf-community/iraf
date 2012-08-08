# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"iki.h"

# IKI_LDDRIVER -- Load an IKI kernel into the kernel table, i.e., make a new
# kernel entry in the table containing the entry point address of each of the
# kernel procedures.

procedure iki_lddriver (kname, ex_open, ex_close, ex_opix, ex_updhdr,
	ex_access, ex_copy, ex_delete, ex_rename, flags)

char	kname[ARB]
extern	ex_open(), ex_close(), ex_opix(), ex_updhdr()
extern	ex_access(), ex_copy(), ex_delete(), ex_rename()
int	locpr()
int	flags

include	"iki.com"
errchk	syserr

begin
	if (k_nkernels + 1 > MAX_KERNELS)
	    call syserr (SYS_IKIKTBLOVFL)
	else
	    k_nkernels = k_nkernels + 1
	    
	call strcpy (kname, IKI_KNAME(k_nkernels), SZ_KNAME)
	IKI_OPEN(k_nkernels)	= locpr (ex_open)
	IKI_CLOSE(k_nkernels)	= locpr (ex_close)
	IKI_OPIX(k_nkernels)	= locpr (ex_opix)
	IKI_UPDHDR(k_nkernels)	= locpr (ex_updhdr)
	IKI_ACCESS(k_nkernels)	= locpr (ex_access)
	IKI_COPY(k_nkernels)	= locpr (ex_copy)
	IKI_DELETE(k_nkernels)	= locpr (ex_delete)
	IKI_RENAME(k_nkernels)	= locpr (ex_rename)
	IKI_FLAGS(k_nkernels)	= flags
end
