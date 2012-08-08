# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	<chars.h>
include	"ki.h"

# KFGCWD -- Get the name of the current default directory.

procedure kfgcwd (outstr, maxch, nchars)

char	outstr[maxch]		# receives pathname of directory
int	maxch			# max chars out
int	nchars			# length of returned string

int	server, op
int	gstrcpy(), ki_sendrcv()
include	"kinode.com"
include	"kii.com"
define	err_ 91

begin
	# If the current directory resides on the local node pass the zfgcwd
	# request to the local kernel, else pass it to the kernel on the remote
	# (default) node.  Leave the CWD in p_sbuf.

	if (n_default == NULL || n_default == n_local) {
err_	    call zfgcwd (p_sbuf, SZ_SBUF, nchars)
	    call strupk (p_sbuf, p_sbuf, SZ_SBUF)
	    p_arg[2] = 1

	} else {
	    # If the current directory is on a remote node then there must be
	    # a connected kernel server attached to that node.

	    server = n_default

	    if (ki_sendrcv (server, KI_ZFGCWD, 0) == ERR) {
		n_default = n_local
		goto err_
	    }
	}

	# Return node // directory.

	op = gstrcpy (n_defaultnode, outstr, maxch) + 1
	if (op > 1) {
	    outstr[op] = FNNODE_CHAR
	    op = op + 1
	}

	nchars = op + gstrcpy (p_sbuf[p_arg[2]], outstr[op], maxch-op+1)
	call strpak (outstr, outstr, maxch)
end
