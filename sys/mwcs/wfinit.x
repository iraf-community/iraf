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

	call wf_fnload ("multispec", F_RADEC, locpr(wf_msp_init),
	    locpr(wf_msp_destroy), locpr(wf_msp_fwd), locpr(wf_msp_inv))
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
