include	<imhdr.h>
include	<smw.h>


# SMW_ND -- Setup the NDSPEC SMW.
# If there is only one spectrum convert it to EQUISPEC if possible.

procedure smw_nd (im, smw)

pointer	im			#I IMIO pointer
pointer	smw			#U MWCS pointer input SMW pointer output

errchk	smw_open, smw_daxis, smw_ndes

begin
	call smw_open (smw, NULL, im)
	if (SMW_NSPEC(smw) == 1)
	    call smw_ndes (im, smw)
end
