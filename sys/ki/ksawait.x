# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ki.h"

# KS_AWAIT -- Wait for i/o to the kernel server device driver given the node
# descriptor of a kernel server channel.  If the error bit is set on the
# node return immediately, since the channel will have been closed.

procedure ks_await (server, status)

int	server			# node descriptor index of server
int	status			# receives i/o status

int	and()
include	"kinode.com"

begin
	if (and (n_status[server], F_IOERR) == 0) {
	    call zawtks (n_kschan[server], status)
	    if (status == ERR)
		call ki_error (server)
	} else
	    status = ERR
end
