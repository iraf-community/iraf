# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"ki.h"

# KFSUBD -- Compute the net pathname of a subdirectory given the net pathname
# of the parent directory and the name of the subdirectory (the logical
# subdirectory names . and .. are supported).  If the subdirectory resides on
# the local node we call the local kernel to compute the new directory pathname.
# If the directory resides on a remote node we merely append to the virtual
# pathname, leaving resolution of the final pathname to the remote kernel when
# the file is referenced at runtime.

procedure kfsubd (osdir, maxch, subdir, nchars)

char	osdir[maxch]		#RW net pathname of directory
int	maxch			#RO max chars out in osdir string
char	subdir[ARB]		#RO receives pathname
int	nchars			#WO receives length of osfn string

int	delim, op
char	alias[SZ_ALIAS]
int	ki_gnode(), gstrcat()

begin
	if (ki_gnode (osdir, alias, delim) == LOCAL) {
	    # Directory is on the local node.

	    if (osdir[delim+1] == EOS) {
		call zfgcwd (osdir[delim+1], maxch - delim, nchars)
		if (nchars == ERR)
		    return
		call strupk (osdir[delim+1], osdir[delim+1], maxch-delim)
	    }
	    call zfsubd (osdir[delim+1], maxch - delim, subdir, nchars)
	    if (nchars != ERR)
		nchars = nchars + delim

	} else {
	    # File is on a remote node.  Do not map the filename; leave that
	    # to the kernel when the file is referenced at runtime.  The OSDIR
	    # string is assumed to be a concatenatable VFN (with node prefix).

	    op = gstrcat (subdir, osdir, maxch) + 1
	    if (op > 1 && osdir[op-1] != '/') {
		osdir[op] = '/'
		op = op + 1
		osdir[op] = EOS
	    }

	    nchars = op - 1
	}
end
