# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# WF_INIT -- Initialize the WCS function table.  Everything having to do
# with a world function is contained either in this file or in the driver
# source file.

procedure wf_init()

extern	wf_smp_init(), wf_smp_tran()
extern	wf_tan_init(), wf_tan_fwd(), wf_tan_inv()

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
	call wf_fnload ("tan", F_RADEC,
	    locpr(wf_tan_init), NULL, locpr(wf_tan_fwd), locpr(wf_tan_inv))
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
