# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../lib/ids.h"

# IDS_CLEAR -- Clear an image frame. 

procedure ids_clear (dummy)

int	dummy			# not used at present
include	"../lib/ids.com"

begin
	if (i_kt == NULL)
	    return
	call zclear(Mems[IDS_FRAME(i_kt)], Mems[IDS_BITPL(i_kt)], i_image)
end
