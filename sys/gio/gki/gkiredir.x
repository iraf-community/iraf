# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<gki.h>

# GKI_REDIR -- Redirect (or set) a graphics stream.  All i/o will be to the
# file FD until the graphics stream is reset in another call to GKI_REDIR.
# The current encoded value for a stream is retured so that a subsequent call
# (with FD=0) may be made to undo the redirection.  A call with FD<0 may be
# used to stat the stream without changing anything.  NOTE: This procedure
# (or either GKI_INLINE_KERNEL or GKI_SUBKERNEL) must be called before using
# the GKI package for a graphics stream.

procedure gki_redir (stream, fd, old_fd, old_type)

int	stream			# graphics stream to be redirected
int	fd			# file to be connected to the stream
int	old_fd, old_type	# old values for later restoration

include	"gki.com"

begin
	if (fd == NULL) {
	    gk_type[stream] = old_type
	    gk_fd[stream]   = old_fd
	} else {
	    old_type = gk_type[stream]
	    old_fd   = gk_fd[stream]
	    if (fd > 0) {
		gk_type[stream] = TY_FILE
		gk_fd[stream] = fd
	    }
	}
end
