# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	"stdgraph.h"

# STG_LOCKCURSOR -- Lock the logical cursor number.  Called interactively by
# cursor mode in response to a ":.cursor N" command by the user.  When the
# cursor is not locked the logical cursor may be selected under program
# control.

procedure stg_lockcursor (new_cursor)

int	new_cursor		# desired new logical cursor
include	"stdgraph.com"

begin
	g_cursor = new_cursor
end
