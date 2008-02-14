# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include	<chars.h>
include	"ki.h"

# KFCHDR -- Change the default directory.  The default node is also set if
# the request is successful.

procedure kfchdr (dirname, status)

char	dirname[ARB]		# directory name
int	status

pointer	sp, fname, defnode
int	server, junk
int	ki_gnode(), ki_connect(), ki_findnode()
# int	ki_sendrcv()
include	"kinode.com"
include	"kii.com"

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (defnode, SZ_ALIAS, TY_CHAR)

	server = ki_connect (dirname)

	if (server == NULL) {
	    # Directory is on the local node.

	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zfchdr (p_sbuf, status)

	} else {
	    # Directory is on a remote node.  Pass the node relative chdir
	    # request on to the remote node and set the default node locally
	    # if the request is successful.

	    # Does not work yet.
	    #if (ki_sendrcv (server, KI_ZFCHDR, 0) != ERR)
	    #	status = p_arg[1]
	    #else
	    #	status = ERR

	    status = ERR
	}

	# Update the default node if the change directory request
	# is successful.

	if (status != ERR) {
	    call strupk (dirname, Memc[fname], SZ_PATHNAME)
	    junk = ki_gnode (Memc[fname], Memc[defnode], junk)
	    call strcpy (Memc[defnode], n_defaultnode, SZ_ALIAS)
	    n_default = ki_findnode (n_defaultnode)
	}

	call sfree (sp)
end
