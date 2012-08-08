# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <gescape.h>
include "ccp.h"

# CCP_ESCAPE -- Pass a device dependent instruction on to the kernel.  
# used for passing exact scaling factors through gki metacode

procedure ccp_escape (fn, instruction, nwords)

int	fn			# function code
short	instruction[ARB]	# instruction data words
int	nwords			# length of instruction

int	ip
real	tempr
char	scale_str[SZ_LINE]
int	ctod ()

include "ccp.com"

string	warnx	"Warning: ccpkern unable to convert gki_escape xscale\n"
string	warny	"Warning: ccpkern unable to convert gki_escape yscale\n"

begin
	call achtsc (instruction, scale_str, nwords)
	scale_str[nwords+1] = EOS
	ip = 1

	switch (fn) {

	case GSC_X_GKITODEV:

	    # if kernel task scale params were not specified, set actual scale
	    # params to those passed from metacode if translatable, set to
	    # default scale from ccp_init/graphcap if untranslatable.  If 
	    # kernel task did specify scale, this is a no op.

	    if (IS_INDEF (g_xtask_scale)) {
		if (ctod (scale_str, ip, tempr) < 1) {
		    g_xndcto_p = g_xdefault_scale
		    call eprintf (warnx)
		    call eprintf ("scale string: %s\n")
			call pargstr (scale_str)
		    call eprintf ("new (graphcap-default) x scale: %f\n")
			call pargr (g_xndcto_p)
		} else 
		    g_xndcto_p = tempr
	    }

	case GSC_Y_GKITODEV:

	    if (IS_INDEF (g_ytask_scale)) {
		if (ctod (scale_str, ip, tempr) < 1) {
		    g_yndcto_p = g_ydefault_scale
		    call eprintf (warny)
		    call eprintf ("scale string: %s\n")
			call pargstr (scale_str)
		    call eprintf ("new (graphcap-default) y scale: %f\n")
			call pargr (g_yndcto_p)
		} else
		    g_yndcto_p = tempr
	    }
	}
end
