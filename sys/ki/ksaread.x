# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ki.h"

# KS_AREAD -- Read from the kernel server device driver given the node
# descriptor of a kernel server channel.  If the error bit is set on the
# node return error w/o doing any i/o, since the channel will have been closed.

procedure ks_aread (server, buf, maxbytes)

int	server			# node descriptor index of server
char	buf[ARB]		# i/o buffer
int	maxbytes		# maxbytes to read

int	and()
include	"kinode.com"

begin
	if (and (n_status[server], F_IOERR) == 0)
	    call zardks (n_kschan[server], buf, maxbytes, long(0))
end
