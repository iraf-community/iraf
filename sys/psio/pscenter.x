# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "psio.h"


# PS_CENTER -- Center the string on the page and break.

procedure ps_center (ps, str)

pointer	ps					#I PSIO descriptor
char	str[ARB]				#I text string

int	mtemp, ps_centerPos()
errchk	ps_centerpos, ps_output

begin
	mtemp = PS_CLMARGIN(ps)
	PS_CLMARGIN(ps) = ps_centerpos (ps, str)
	call ps_output (ps, str, NO)
	PS_CLMARGIN(ps) = mtemp
end


# PS_CENTERPOS -- Get the X position of the centered string.

int procedure ps_centerpos (ps, str)

pointer	ps					#I PSIO descriptor
char    str[ARB]                             	#I string to center

int	ps_textwidth()
errchk	ps_textwidth

begin
	return (((PS_PWIDTH(ps) * RESOLUTION)/2) - ps_textwidth (ps, str) / 2)
end
