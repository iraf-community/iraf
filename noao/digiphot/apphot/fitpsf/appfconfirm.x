include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/fitpsf.h"

# AP_PFCONFIRM -- Procedure to confirm the critical fitpsf parameters.

procedure ap_pfconfirm (ap, out, stid)

pointer	ap		# pointer to the apphot structure
int	out		# the output file descriptor
int	stid		# the output file sequence number

pointer	sp, str
real	fwhmpsf, psfapert, datamin, datamax
int	apstati()
real	apstatr(), ap_vfwhmpsf(), ap_vpsfapert()
real	ap_vdatamin(), ap_vdatamax()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call printf ("\n")

	# Confirm the fitting function.
	call ap_vpfstring (ap, Memc[str], SZ_FNAME)

	# Confirm the fwhmpsf.
	if (apstati (ap, PSFUNCTION) != AP_MOMENTS)
	    fwhmpsf = ap_vfwhmpsf (ap)
	else
	    fwhmpsf = apstatr (ap, FWHMPSF)

	# Confirm the fitting box.
	psfapert = 2.0 * ap_vpsfapert (ap)

	# Confirm the good data minimum and maximum values.
	datamin = ap_vdatamin (ap)
	datamax = ap_vdatamax (ap)

	call printf ("\n")

	# Update the database file.
	if (out != NULL && stid > 1) {
	    call ap_sparam (out, KY_PSFSTRING, Memc[str], UN_PSFMODEL,
		"psf fitting function")
	    call ap_rparam (out, KY_FWHMPSF, fwhmpsf, UN_ASCALEUNIT,
	        "full width half maximum of the psf")
	    call ap_rparam (out, KY_PSFAPERT, psfapert, UN_PSFSCALEUNIT,
	        "width of fitting box")
	    call ap_rparam (out, KY_DATAMIN, datamin, UN_ACOUNTS,
	        "minimum good data value")
	    call ap_rparam (out, KY_DATAMAX, datamax, UN_ACOUNTS,
	        "maximum good data value")
	}

	call sfree (sp)
end
