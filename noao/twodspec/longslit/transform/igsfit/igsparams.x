include	<pkg/gtools.h>

# IGS_PARAMS -- Set the GTOOLS parameter string.

procedure igs_params (gt)

pointer	gt		# GTOOLS pointer

pointer	params

include	"igsfit.com"

begin
	call malloc (params, SZ_LINE, TY_CHAR)
	call sprintf (Memc[params], SZ_LINE,
	    "Function = %s, xorder = %d, yorder = %d, rms = %.4g")
	    call pargstr (function)
	    call pargi (xorder)
	    call pargi (yorder)
	    call pargr (rms)
	call gt_sets (gt, GTPARAMS, Memc[params])
	call mfree (params, TY_CHAR)
end
