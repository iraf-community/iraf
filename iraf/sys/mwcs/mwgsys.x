# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

# MW_GSYSTEM -- Return the name of the current default world system.

procedure mw_gsystem (mw, outstr, maxch)

pointer	mw			#I pointer to MWCS descriptor
char	outstr[ARB]		#O receives name of world system
int	maxch			#I max chars out

pointer	wp

begin
	wp = MI_WCS(mw)
	call strcpy (S(mw,WCS_SYSTEM(wp)), outstr, maxch)
end
