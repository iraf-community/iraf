# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_INIT -- Initialize GKI i/o on a graphics stream.  Called by GOPEN to
# make the connection to either a metacode stream file or an inline kernel.
# If the stream has already been directed to a kernel we do nothing, else
# we initialize the stream as for a metacode file or remote kernel.  If
# gki_inline is called before gopen then this procedure is a nop.

procedure gki_init (stream)

int	stream			# graphics stream to be redirected
include	"gki.com"

begin
	if (gk_type[stream] == NULL) {
	    gk_type[stream] = TY_FILE
	    gk_fd[stream] = stream
	}
end
