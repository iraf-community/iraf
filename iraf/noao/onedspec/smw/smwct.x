include	<smw.h>


# SMW_CT -- Get MCWS CT pointer for the specified physical line.

pointer procedure smw_ct (sct, line)

pointer	sct		#I SMW pointer
int	line		#I Physical line

begin
	if (SMW_NCT(sct) == 1)
	    return (SMW_CT(sct,0))

	if (line < 1 || line > SMW_NSPEC(SMW_SMW(sct)))
	    call error (1, "smw_ct: aperture not found")

	return (SMW_CT(sct,(line-1)/SMW_NSPLIT))
end
