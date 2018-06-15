# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KOPDPR -- Open a detached process.

procedure kopdpr (process, bkgfile, bkgmsg, jobcode)

char	process[ARB]		# packed osfn of process executable
char	bkgfile[ARB]		# packed osfn of bkg file
char	bkgmsg[ARB]		# control string for kernel
int	jobcode			# receives job code of process

pointer	sp, osfn, alias
int	server, off, delim
int	ki_connect(), ki_sendrcv(), ki_getchan(), strlen(), ki_gnode()
include	"kii.com"

begin
	call smark (sp)
	call salloc (osfn,  SZ_PATHNAME, TY_CHAR)
	call salloc (alias, SZ_ALIAS,    TY_CHAR)

	server = ki_connect (process)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call strupk (bkgfile, Memc[osfn], SZ_PATHNAME)

	    # The bkg file must be on the same node as the process file.
	    if (ki_gnode (Memc[osfn], Memc[alias], delim) == REMOTE)
		jobcode = ERR
	    else {
		call strpak (Memc[osfn+(delim+1)-1], Memc[osfn], SZ_PATHNAME)
		call zopdpr (p_sbuf, Memc[osfn], bkgmsg, jobcode)
	    }

	} else {
	    # Spawning of detached processes on remote nodes is not really
	    # supported as of yet.  Add support for passing the bkgmsg; use
	    # node name in bkgmsg to submit bkg job to remote node.

	    off = p_sbuflen + 2
	    p_arg[2] = off
	    call strupk (bkgfile, p_sbuf[off], ARB)
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


# KFODPR -- Fork a detached process.

int procedure kfodpr ()

int	jobcode			# receives job code of process
int	ki_getchan(), zfodpr()

begin
	jobcode = zfodpr ()

	if (jobcode == NULL)	# Child process
	    return NULL

	if (jobcode == ERR)
	    return ERR

	return ki_getchan (NULL, jobcode)
end


# KTSDPR -- Check if a detached process exists by sending a signal.

int procedure ktsdpr (jobcode)

int	jobcode

int	ztsdpr()
include	"kichan.com"

begin
	if (k_node[jobcode] == NULL)
	    return ztsdpr (k_oschan[jobcode])
	else
	    return ERR
end
