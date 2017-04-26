# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# WF_INIT -- Initialize the WCS function table.  Everything MWCS related
# having to do with a world function is contained either in this file or
# in the driver source file.  If the WCS must also be translated to/from
# a FITS image header representation, the image header translation routine
# iwewcs.x must also be modified.

procedure wf_init()

extern	wf_smp_init(), wf_smp_tran()
extern	wf_tan_init(), wf_tan_fwd(), wf_tan_inv()
extern	wf_arc_init(), wf_arc_fwd(), wf_arc_inv()
extern	wf_gls_init(), wf_gls_fwd(), wf_gls_inv()
extern	wf_sin_init(), wf_sin_fwd(), wf_sin_inv()
extern	wf_msp_init(), wf_msp_fwd(), wf_msp_inv(), wf_msp_destroy()

extern	wf_ait_init(), wf_ait_fwd(), wf_ait_inv()
extern	wf_car_init(), wf_car_fwd(), wf_car_inv()
extern	wf_csc_init(), wf_csc_fwd(), wf_csc_inv()
extern	wf_mer_init(), wf_mer_fwd(), wf_mer_inv()
extern	wf_mol_init(), wf_mol_fwd(), wf_mol_inv()
extern	wf_par_init(), wf_par_fwd(), wf_par_inv()
extern	wf_pco_init(), wf_pco_fwd(), wf_pco_inv()
extern	wf_qsc_init(), wf_qsc_fwd(), wf_qsc_inv()
extern	wf_stg_init(), wf_stg_fwd(), wf_stg_inv()
extern	wf_tsc_init(), wf_tsc_fwd(), wf_tsc_inv()
extern	wf_zea_init(), wf_zea_fwd(), wf_zea_inv()

extern	wf_zpx_init(), wf_zpx_fwd(), wf_zpx_inv(), wf_zpx_destroy()
extern	wf_zpn_init(), wf_zpn_fwd(), wf_zpn_inv(), wf_zpn_destroy()
extern	wf_tnx_init(), wf_tnx_fwd(), wf_tnx_inv(), wf_tnx_destroy()
extern	wf_tpv_init(), wf_tpv_fwd(), wf_tpv_inv(), wf_tpv_destroy()

bool	first_time
data	first_time /true/
errchk	wf_fnload
include	"mwcs.com"
int	locpr()

begin
	# Only do this once.
	if (!first_time)
	    return

	fn_nfn = 0
	first_time = false

	# Load the function drivers.
	call wf_fnload ("sampled", 0,
	    locpr(wf_smp_init), NULL, locpr(wf_smp_tran), locpr(wf_smp_tran))

	# For compatibility reasons (FN index codes) new functions should
	# be added at the end of the following list.

	call wf_fnload ("tan", F_RADEC,
	    locpr(wf_tan_init), NULL, locpr(wf_tan_fwd), locpr(wf_tan_inv))
	call wf_fnload ("arc", F_RADEC,
	    locpr(wf_arc_init), NULL, locpr(wf_arc_fwd), locpr(wf_arc_inv))
	call wf_fnload ("gls", F_RADEC,
	    locpr(wf_gls_init), NULL, locpr(wf_gls_fwd), locpr(wf_gls_inv))
	call wf_fnload ("sin", F_RADEC,
	    locpr(wf_sin_init), NULL, locpr(wf_sin_fwd), locpr(wf_sin_inv))

	# Custom IRAF WCS for images containing multiple spectra.
	call wf_fnload ("multispec", F_RADEC,
	    locpr(wf_msp_init), locpr(wf_msp_destroy), locpr(wf_msp_fwd),
	    locpr(wf_msp_inv))

	# Most of the following are from G&C (also GLS above).
	call wf_fnload ("ait", F_RADEC,
	    locpr(wf_ait_init), NULL, locpr(wf_ait_fwd), locpr(wf_ait_inv))
	call wf_fnload ("car", F_RADEC,
	    locpr(wf_car_init), NULL, locpr(wf_car_fwd), locpr(wf_car_inv))
	call wf_fnload ("csc", F_RADEC,
	    locpr(wf_csc_init), NULL, locpr(wf_csc_fwd), locpr(wf_csc_inv))
	call wf_fnload ("mer", F_RADEC,
	    locpr(wf_mer_init), NULL, locpr(wf_mer_fwd), locpr(wf_mer_inv))
	call wf_fnload ("mol", F_RADEC,
	    locpr(wf_mol_init), NULL, locpr(wf_mol_fwd), locpr(wf_mol_inv))
	call wf_fnload ("par", F_RADEC,
	    locpr(wf_par_init), NULL, locpr(wf_par_fwd), locpr(wf_par_inv))
	call wf_fnload ("pco", F_RADEC,
	    locpr(wf_pco_init), NULL, locpr(wf_pco_fwd), locpr(wf_pco_inv))
	call wf_fnload ("qsc", F_RADEC,
	    locpr(wf_qsc_init), NULL, locpr(wf_qsc_fwd), locpr(wf_qsc_inv))
	call wf_fnload ("stg", F_RADEC,
	    locpr(wf_stg_init), NULL, locpr(wf_stg_fwd), locpr(wf_stg_inv))
	call wf_fnload ("tsc", F_RADEC,
	    locpr(wf_tsc_init), NULL, locpr(wf_tsc_fwd), locpr(wf_tsc_inv))
	call wf_fnload ("zea", F_RADEC,
	    locpr(wf_zea_init), NULL, locpr(wf_zea_fwd), locpr(wf_zea_inv))

	# Experimental WCS for astrometric approximations.
	call wf_fnload ("zpx", F_RADEC,
	    locpr(wf_zpx_init), locpr(wf_zpx_destroy), locpr(wf_zpx_fwd),
	    locpr(wf_zpx_inv))
	call wf_fnload ("zpn", F_RADEC,
	    locpr(wf_zpn_init), locpr(wf_zpn_destroy), locpr(wf_zpn_fwd),
	    locpr(wf_zpn_inv))
	call wf_fnload ("tnx", F_RADEC,
	    locpr(wf_tnx_init), locpr(wf_tnx_destroy), locpr(wf_tnx_fwd),
	    locpr(wf_tnx_inv))
	call wf_fnload ("tpv", F_RADEC,
	    locpr(wf_tpv_init), locpr(wf_tpv_destroy), locpr(wf_tpv_fwd),
	    locpr(wf_tpv_inv))
end


# WF_FNLOAD -- Load a driver into the WCS function table.

procedure wf_fnload (name, flags, init, destroy, fwd, inv)

char	name[ARB]			#I function name
int	init				#I initialize procedure
int	flags				#I function type flags
int	destroy				#I destroy procedure
int	fwd				#I forward transform procedure
int	inv				#I inverse transform procedure

errchk	syserrs
include	"mwcs.com"

begin
	# Get a new driver slot.
	if (fn_nfn + 1 > MAX_FN)
	    call syserrs (SYS_MWFNOVFL, name)
	fn_nfn = fn_nfn + 1

	# Load the driver.
	FN_INIT(fn_nfn) = init
	FN_FLAGS(fn_nfn) = flags
	FN_DESTROY(fn_nfn) = destroy
	FN_FWD(fn_nfn) = fwd
	FN_INV(fn_nfn) = inv
	call strcpy (name, FN_NAME(fn_nfn), SZ_FNNAME)
end
