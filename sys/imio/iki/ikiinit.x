# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"iki.h"

# IKI_INIT -- Initialize the IKI kernel table, i.e., load all the standard
# kernels into the table.  Additional kernels may be dynamically added at
# run time for special applications.

procedure iki_init()

extern	oif_open(), oif_close(), oif_opix(), oif_updhdr(),
	oif_access(), oif_copy(), oif_delete(), oif_rename()
extern	fxf_open(), fxf_close(), fxf_opix(), fxf_updhdr(),
	fxf_access(), fxf_copy(), fxf_delete(), fxf_rename()
extern	plf_open(), plf_close(), plf_null(), plf_updhdr(),
	plf_access(), plf_copy(), plf_delete(), plf_rename()
extern	qpf_open(), qpf_close(), qpf_opix(), qpf_updhdr(),
	qpf_access(), qpf_copy(), qpf_delete(), qpf_rename()
extern	stf_open(), stf_close(), stf_opix(), stf_updhdr(),
	stf_access(), stf_copy(), stf_delete(), stf_rname()

bool	first_time
data	first_time /true/
int	iki_extninit()
include	"iki.com"

begin
	if (!first_time)
	    return

	k_nkernels = 0

	# Load the original IRAF format (OIF) kernel.
	call iki_lddriver ("oif", oif_open, oif_close, oif_opix, oif_updhdr,
	    oif_access, oif_copy, oif_delete, oif_rename, 0)

	# Load the FITS image kernel (FXF).
	call iki_lddriver ("fxf", fxf_open, fxf_close, fxf_opix, fxf_updhdr,
	    fxf_access, fxf_copy, fxf_delete, fxf_rename, 0)

	# Load the PLIO mask image mini-kernel (PLF - not a full kernel).
	call iki_lddriver ("plf", plf_open, plf_close, plf_null, plf_updhdr,
	    plf_access, plf_copy, plf_delete, plf_rename, 0)

	# Load the QPOE photon image kernel (QPF).
	call iki_lddriver ("qpf", qpf_open, qpf_close, qpf_opix, qpf_updhdr,
	    qpf_access, qpf_copy, qpf_delete, qpf_rename, IKF_NOCREATE)

	# Load the SDAS GEIS format (STF) kernel.
	call iki_lddriver ("stf", stf_open, stf_close, stf_opix, stf_updhdr,
	    stf_access, stf_copy, stf_delete, stf_rname, 0)

	# Initialize the extension-based image typing mechanism.
	if (iki_extninit (ENV_IMTYPE, DEF_IMTYPE, ENV_IMEXTN, DEF_IMEXTN) < 0)
	    ;

	first_time = false
end
