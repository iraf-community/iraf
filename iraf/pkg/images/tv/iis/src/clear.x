# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	"../lib/ids.h"

# CLEAR -- clear certain frames in the display

procedure clear()

char	token[SZ_LINE]
int	tok
short	frames[IDS_MAXIMPL+1]

define	nexttok	10

include "cv.com"

begin
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)

	while ( (tok == TOK_IDENTIFIER) || (tok == TOK_NUMBER) ) {
	    if (tok == TOK_IDENTIFIER) {
		switch (token[1]) {
		    case 'a', 'g':
			# all colors
			call cvclearg (short(IDS_EOD), short (IDS_EOD))
			if (token[1] == 'g')
			    goto nexttok
			frames[1] = IDS_EOD

		    case 'f':
			call cv_frame (token[2], frames)
		}
	    } else
		call cv_frame (token[1], frames)

	    call cvcleari (frames)
	    if (token[1] == 'a')
		return

	    # get next token
nexttok
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	}
end
