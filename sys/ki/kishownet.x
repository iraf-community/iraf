# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

procedure ki_shownet (fd)

int	fd			# output file

int	i, j, n
int	ki_gethosts()
include	"kinode.com"

begin
	if (n_nnodes == 0)
	    n = ki_gethosts()

	call fprintf (fd, "Local node `%s' (%d), default node `%s', ")
	    call pargstr (n_localnode)
	    call pargi (n_local)
	    call pargstr (n_defaultnode)
	call fprintf (fd, "%d nodes in local network\n")
	    call pargi (n_nnodes)

	if (n_local == 0) {
	    call fprintf (fd, "Network interface disabled ")
	    call fprintf (fd, "(no entry for local node in dev$hosts)\n")
	}

	if (n_nnodes == MAX_NODES)
	    call fprintf (fd, "HOST NAME TABLE IS FULL\n")
	else if (n_nnodes <= 0)
	    return

	# Print node table.
	call fprintf (fd, "\n    NODE SERVER NREFS STATUS  ALIASES\n")
	do i = 1, n_nnodes {
	    call fprintf (fd, "%8d %6d %5d  %05o ")
		call pargi (i)
		call pargi (n_kschan[i])
		call pargi (n_nrefs[i])
		call pargi (n_status[i])

	    do j = 1, n_nalias[i] {
		call fprintf (fd, " %s")
		    call pargstr (n_alias[1,j,i])
	    }

	    call fprintf (fd, "\n")
	}

#	The following should no longer be needed as ki_mapname and the
#	"node!" syntax should prevent accidential aliasing of node names
#	and non-network related environment variables.
#
#	n = 0
#	do i = 1, n_nnodes
#	    do j = 1, n_nalias[i]
#		if (envfind (n_alias[1,j,i], Memc[text], SZ_FNAME) > 0) {
#		    if (n == 0)
#			call fprintf (fd, "\n")
#		    call fprintf (fd,
#			"Warning: node name `%s' is an alias for `%s'\n")
#			call pargstr (n_alias[1,j,i])
#			call pargstr (Memc[text])
#		    n = n + 1
#		}
end
