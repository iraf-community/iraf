# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"ki.h"

# KOPDPR -- Open a detached process.

procedure kopdpr (process, bkgfile, bkgmsg, jobcode)

char	process[ARB]		# packed osfn of process executable
char	bkgfile[ARB]		# packed osfn of bkg file
char	bkgmsg[ARB]		# control string for kernel
int	jobcode			# receives job code of process

size_t	sz_val
pointer	sp, osfn, alias
int	server, off, delim
int	ki_connect(), ki_sendrcv(), ki_getchan(), strlen(), ki_gnode()
include	"kii.com"

begin
	call smark (sp)
	sz_val = SZ_PATHNAME
	call salloc (osfn,  sz_val, TY_CHAR)
	sz_val = SZ_ALIAS
	call salloc (alias, sz_val, TY_CHAR)

	server = ki_connect (process)

	if (server == NULL) {
	    sz_val = SZ_SBUF
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, sz_val)
	    sz_val = SZ_PATHNAME
	    call strupk (bkgfile, Memc[osfn], sz_val)

	    # The bkg file must be on the same node as the process file.
	    if (ki_gnode (Memc[osfn], Memc[alias], delim) == REMOTE)
		jobcode = ERR
	    else {
		sz_val = SZ_PATHNAME
		call strpak (Memc[osfn+(delim+1)-1], Memc[osfn], sz_val)
		call zopdpr (p_sbuf, Memc[osfn], bkgmsg, jobcode)
	    }

	} else {
	    # Spawning of detached processes on remote notes is not really
	    # supported as of yet.  Add support for passing the bkgmsg; use
	    # node name in bkgmsg to submit bkg job to remote node.

	    off = p_sbuflen + 2
	    p_arg[2] = off
	    sz_val = ARB
	    call strupk (bkgfile, p_sbuf[off], sz_val)
	    p_sbuflen = off + strlen(p_sbuf[off])

	    if (ki_sendrcv (server, KI_ZOPDPR, 0) == ERR)
		jobcode = ERR
	    else
		jobcode = p_arg[1]
	}

	if (jobcode != ERR)
	    jobcode = ki_getchan (server, jobcode)

	call sfree (sp)
end
