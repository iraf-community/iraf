include	<smw.h>


# SMW_MW -- Get MWCS pointer and coordinates from spectrum line and band

procedure smw_mw (smw, line, band, mw, x, y)

pointer	smw		#I SMW pointer
int	line		#I Spectrum line
int	band		#I Spectrum band
pointer	mw		#O MWCS pointer
int	x, y		#O MWCS coordinates

real	mw_c1tranr()

begin
	if (line < 1 || line > SMW_NSPEC(smw))
	    call error (1, "smw_mw: spectrum not found")

	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    mw = SMW_MW(smw,0)
	    x = mod (line - 1, SMW_LLEN(smw,2)) + 1
	    y = (line - 1) / SMW_LLEN(smw,2) + band
	default:
	    if (SMW_NMW(smw) == 1) {
		mw = SMW_MW(smw,0)
		x = line
		y = band
		if (SMW_CTLP(smw) != NULL)
		    x = nint (mw_c1tranr (SMW_CTLP(smw), real(line)))
	    } else {
		mw = SMW_MW(smw,(line-1)/SMW_NSPLIT)
		x = mod (line - 1, SMW_NSPLIT) + 1
		y = band
	    }
	}
end
