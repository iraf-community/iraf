# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"environ.h"

# ENV_INIT -- Called by the IRAF main to initialize the environment common
# upon process startup.

procedure env_init()

bool	first_time
int	kmalloc()
include	"environ.com"
data	first_time /true/

begin
	if (first_time) {
	    if (kmalloc (envbuf, LEN_ENVBUF, TY_SHORT) == ERR)
		call sys_panic (SYS_MFULL, "Out of memory")

	    call aclrs (threads, NTHREADS)
	    len_envbuf = LEN_ENVBUF
	    last = NULL
	    top = 1
	    first_time = false
	}
end
