# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"iki.h"

# IKI_INIT -- Initialize the IKI kernel table, i.e., load all the standard
# kernels into the table.  Additional kernels may be dynamically added at
# run time for special applications.

procedure iki_init()

extern	oif_open(), oif_close(), oif_opix(), oif_updhdr(),
	oif_access(), oif_copy(), oif_delete(), oif_rename()
extern	stf_open(), stf_close(), stf_opix(), stf_updhdr(),
	stf_access(), stf_copy(), stf_delete(), stf_rname()
extern	qpf_open(), qpf_close(), qpf_opix(), qpf_updhdr(),
	qpf_access(), qpf_copy(), qpf_delete(), qpf_rename()

bool	first_time
data	first_time /true/
include	"iki.com"

begin
	if (!first_time)
	    return

	k_nkernels = 0

	# Load the old IRAF format (OIF) kernel.
	call iki_lddriver (oif_open, oif_close, oif_opix, oif_updhdr,
	    oif_access, oif_copy, oif_delete, oif_rename)

	# Load the SDAS GEIS format (STF) kernel.
	call iki_lddriver (stf_open, stf_close, stf_opix, stf_updhdr,
	    stf_access, stf_copy, stf_delete, stf_rname)

	# Load the QPOE photon image kernel (QPF).
	call iki_lddriver (qpf_open, qpf_close, qpf_opix, qpf_updhdr,
	    qpf_access, qpf_copy, qpf_delete, qpf_rename)

	first_time = false
end
