include	<smw.h>

define	CTYPES	"|LAMBDA|FREQ|WAVELENGTH|VELO|VELO-LSR|VELO-HEL|VELO-OBS|"

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

int	i, da, ns[2]
int	imgeti(), clgeti(), clscan(), nscan(), nowhite(), strdic()
pointer	sp, key, val
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
		iferr (SMW_PAXIS(smw,1) = imgeti (im, "DISPAXIS")) {
		    SMW_PAXIS(smw,1) = clgeti ("dispaxis")
		    call smark (sp)
		    call salloc (key, 8, TY_CHAR)
		    call salloc (val, SZ_FNAME, TY_CHAR)
		    do i = 1, 7 {
			call sprintf (Memc[key], 8, "CTYPE%d")
			    call pargi (i)
			iferr (call imgstr (im, Memc[key], Memc[val], SZ_FNAME))
			    break
			if (nowhite (Memc[val], Memc[val], SZ_FNAME) > 0) {
			    call strupr (Memc[val])
			    if (strdic(Memc[val],Memc[val],SZ_FNAME,CTYPES)>0) {
				SMW_PAXIS(smw,1) = i
				break
			    }
			}
		    }
		    call sfree (sp)
		}
		if (SMW_PAXIS(smw,1) < 1 || SMW_PAXIS(smw,1) > 7) {
		    i = SMW_PAXIS(smw,1)
		    SMW_PAXIS(smw,1) = clgeti ("dispaxis")
		    call eprintf (
	"WARNING: Image header dispersion axis %d invalid. Using axis %d.\n")
			call pargi (i)
			call pargi (SMW_PAXIS(smw,1))
		}
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
