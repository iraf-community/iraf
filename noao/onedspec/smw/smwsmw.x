include	<smw.h>


# SMW_SMW -- Set MCWS pointer

procedure smw_smw (smw, line, mw)

pointer	smw		#I SMW pointer
int	line		#I Physical line
pointer	mw		#I MWCS pointer

begin
	if (SMW_NMW(smw) == 1)
	    SMW_MW(smw,0) = mw

	else {
	    if (line < 1 || line > SMW_NSPEC(smw))
		call error (1, "smw_smw: aperture not found")
	    SMW_MW(smw,(line-1)/SMW_NSPLIT) = mw
	}
end
