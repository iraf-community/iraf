# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../lib/ids.h"

# IDS_CLOSE -- Close the image display kernel.
# Free up storage.

procedure ids_close()

include	"../lib/ids.com"

begin
	call close(i_out)
	call mfree (IDS_FRAME(i_kt), TY_SHORT)
	call mfree (IDS_BITPL(i_kt), TY_SHORT)
	call mfree (IDS_SBUF(i_kt), TY_CHAR)
	call mfree (i_kt, TY_STRUCT)
	i_kt = NULL
end
