include	<smw.h>

# SMW_DAXIS -- Set physical dispersion axis and summing factors.
# A default value of zero for the dispersion axis will cause the dispersion
# axis to be sought in the image header and, if not found, from the CL
# "dispaxis" parameter.  A default value of zero for the summing factors will
# cause them to be queried from the CL "nsum" parameter.  A default value of
# INDEFI in either parameter will leave the current default unchanged.
#
# When this procedure is called with an SMW and IMIO pointer the SMW
# pointer is updated to desired default dispersion axis and summing
# parameters.

procedure smw_daxis (smw, im, daxisp, nsum1, nsum2)

pointer	smw			#I SMW pointer
pointer	im			#I IMIO pointer
int	daxisp			#I Default dispersion axis
int	nsum1, nsum2		#I Default summing factors

int	i, da, ns[2], imgeti(), clgeti(), clscan(), nscan()
data	da/0/, ns/0,0/
errchk	clgeti

begin
	# Set defaults.
	# A value of 0 will use the image DISPAXIS or query the CL and
	# a value of INDEFI will leave the current default unchanged.

	if (!IS_INDEFI (daxisp))
	    da = daxisp
	if (!IS_INDEFI (nsum1))
	    ns[1] = nsum1
	if (!IS_INDEFI (nsum2))
	    ns[2] = nsum2

	if (smw == NULL)
	    return

	# This procedure is specific to the NDSPEC format.
	if (SMW_FORMAT(smw) != SMW_ND)
	    return

	# Set dispersion axis.
	if (da == 0) {
	    if (im == NULL)
		SMW_PAXIS(smw,1) = clgeti ("dispaxis")
	    else {
		iferr (SMW_PAXIS(smw,1) = imgeti (im, "DISPAXIS"))
		    SMW_PAXIS(smw,1) = clgeti ("dispaxis")
	    }
	} else
	    SMW_PAXIS(smw,1) = da

	# Set summing parameters.
	if (ns[1] == 0 || ns[2] == 0) {
	    if (clscan("nsum") == EOF)
		call error (1, "smw_daxis: Error in 'nsum' parameter") 
	    call gargi (i)
	    if (ns[1] == 0) {
		if (nscan() == 1)
		    SMW_NSUM(smw,1) = max (1, i)
		else
		    call error (1, "smw_daxis: Error in 'nsum' parameter")
	    } else
		SMW_NSUM(smw,1) = ns[1]
	    call gargi (i)
	    if (ns[2] == 0) {
		if (nscan() == 2)
		    SMW_NSUM(smw,2) = max (1, i)
		else
		    SMW_NSUM(smw,2) = SMW_NSUM(smw,1)
	    } else
		SMW_NSUM(smw,2) = ns[2]
	} else {
	    SMW_NSUM(smw,1) = ns[1]
	    SMW_NSUM(smw,2) = ns[2]
	}
end
