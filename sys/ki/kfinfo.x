# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<finfo.h>
include	"ki.h"

# KFINFO -- Get directory info on a file.

procedure kfinfo (osfn, fi, status)

char	osfn[ARB]		# packed os filename
long	fi[ARB]			# receives finfo structure
int	status			# answer; ok or err

int	server, i
int	ki_connect(), ki_sendrcv()
include	"kii.com"

begin
	server = ki_connect (osfn)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zfinfo (p_sbuf, fi, status)

	} else {
	    if (ki_sendrcv (server, KI_ZFINFO, 0) == ERR)
		status = ERR
	    else {
		status = p_arg[1]

		# The finfo structure is returned in ARG except for the
		# owner string, which is returned in sbuf.  Note that we
		# are passing longs in ints hence precision could conceivably 
		# be lost (most unlikely).

		do i = 1, FI_NINTFIELDS
		    fi[i] = p_arg[i+1]
		call strpak (p_sbuf, FI_OWNER(fi), FI_SZOWNER)
	    }
	}
end
