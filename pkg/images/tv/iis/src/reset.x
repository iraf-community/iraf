# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	"../lib/ids.h"

# RESET -- reset the display

procedure reset()

char	token[SZ_LINE]
int	tok

include "cv.com"

begin
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (tok == TOK_IDENTIFIER) {
	    switch(token[1]) {
		case 'r':
		    call cvreset( IDS_R_SOFT)

		case 't':
		    call cvreset( IDS_R_MEDIUM)

		case 'i':
		    call cvreset( IDS_R_HARD)

		case 'a':
		    call cvreset( IDS_R_SOFT)
		    call cvreset( IDS_R_MEDIUM)
		    call cvreset( IDS_R_HARD)

	    }
	}
end
