# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"


# KFRNAM -- Rename a file.  Both filenames must refer to the same node.

procedure kfrnam (old_osfn, new_osfn, status)

char	old_osfn[ARB]		#I packed old os filename
char	new_osfn[ARB]		#I packed new os filename
int	status			#O answer; ok or err

pointer	sp, fname
int	server1, server2, old, new
int	ki_connect(), ki_sendrcv(), strlen()
include	"kii.com"

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	server2 = ki_connect (new_osfn)
	call strcpy (p_sbuf[p_arg[1]], Memc[fname], SZ_FNAME)
	server1 = ki_connect (old_osfn)
	old = p_arg[1]

	if (server1 == NULL && server2 == NULL) {
	    # Both files reside on the local node.

	    call strpak (p_sbuf[old], p_sbuf[old], SZ_SBUF)
	    call strpak (Memc[fname], Memc[fname], SZ_FNAME)
	    call zfrnam (p_sbuf[old], Memc[fname], status)

	} else if (server1 == server2) {
	    # Both files reside on the same remote node.  Pack the two
	    # filenames into p_sbuf and send the request.

	    new = old + strlen(p_sbuf[old])+1 + 1
	    if (new + strlen(Memc[fname])+1 > SZ_SBUF)
		status = ERR
	    else {
		call strcpy (Memc[fname], p_sbuf[new], SZ_SBUF-new+1)

		p_arg[2]  = new
		p_sbuflen = SZ_SBUF

		if (ki_sendrcv (server1, KI_ZFRNAM, 0) == ERR)
		    status = ERR
		else
		    status = p_arg[1]
	    }

	} else {
	    # One file resides on a remote node.
	    status = ERR
	}

	call sfree (sp)
end
