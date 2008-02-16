# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<chars.h>
include	"ki.h"

# KI_OPENKS -- Physically open a kernel server process on a remote node.
# Spawn the process and initialize the environment list and iraf root
# directory on the remote node.  The KS channel or NULL is returned as the
# function value.

int procedure ki_openks (node)

int	node			# node descriptor to open kernel on

bool	hostdep
pointer	sp, ksname, env, el, valp, ip, op, sv
int	show, kschan, nchars, status, junk

pointer	env_first(), env_next()
int	strlen(), stridx(), strncmp()
int	gstrcpy(), ki_send(), ki_receive()

data	show /NO/
include	"kii.com"
include	"kinode.com"
define	quit_ 91

begin
	call smark (sp)
	call salloc (ksname, SZ_FNAME, TY_CHAR)
	call salloc (sv, SZB_PACKET / SZB_CHAR, TY_CHAR)

	status = ERR

	# Our caller may have already prepped a packet in the kii common, which
	# we are going to clobber below.  Save packet and restore when done.

	call amovc (FIRSTINTFIELD, Memc[sv], SZB_PACKET / SZB_CHAR)

	# Spawn the kernel server process.

	call strpak (n_server[1,node], Memc[ksname], SZ_FNAME)
	call zopnks (Memc[ksname], READ_WRITE, kschan)
	if (kschan == ERR) {
	    call sfree (sp)
	    return (ERR)
	} else
	    n_kschan[node] = kschan

	# Read the environment list into a string buffer.  Scan the list once
	# to determine its size, then allocate the buffer and format a series
	# of "set name=value" lines in the buffer.
	# Note 4 + 1 comes from len("set ") + \n.

	nchars = 0
	for (el=env_first(valp);  el != NULL;  el=env_next(el,valp,show))
	    nchars = nchars + strlen (Memc[valp]) + 4 + 2 + 1

	call salloc (env, nchars, TY_CHAR)

	op = env
	for (el=env_first(valp);  el != NULL;  el=env_next(el,valp,show)) {
	    # Do not pass on the values of the host dependent environment
	    # variables HOST, IRAF, and TMP.  If we do not set the values
	    # here, the remote kernel will fetch the values automatically
	    # from the HSI global include file <iraf.h> on the server node.

	    hostdep = false
	    if (stridx (Memc[valp], "hit") > 0)
		hostdep = ((strncmp (Memc[valp], "host=", 5) == 0) ||
			   (strncmp (Memc[valp], "iraf=", 5) == 0) ||
			   (strncmp (Memc[valp], "tmp=",  4) == 0))

	    if (!hostdep) {
		call strcpy ("set ", Memc[op], ARB)
		op = op + 4
		for (ip=valp;  Memc[ip] != '=';  ip=ip+1) {
		    Memc[op] = Memc[ip]
		    op = op + 1
		}
		Memc[op] = '=';  op = op + 1
		Memc[op] = '"';  op = op + 1
		op = op + gstrcpy (Memc[ip+1], Memc[op], ARB)
		Memc[op] = '"';  op = op + 1
		Memc[op] = '\n'; op = op + 1
	    }
	}

	Memc[op] = EOS

	# Transmit the environment list to the server process, preceded by the
	# KI_ENVINIT instruction packet.  The ENVINIT function does not return
	# a status (to permit pipelining of multiple setenv packets).

	p_arg[1] = nchars
	if (nchars <= SZ_SBUF)
	    call strcpy (Memc[env], p_sbuf, nchars)

	if (ki_send (node, KI_ENVINIT, 0) == ERR)
	    goto quit_

	if (nchars > SZ_SBUF) {
	    # Transmit the data record.
	    call strpak (Memc[env], Memc[env], nchars)
	    call zawrks (kschan, Memc[env], nchars, long(0)) 
	    call zawtks (kschan, status)
	    if (status != nchars) {
		status = ERR
		goto quit_
	    }

	    # We do expect a status return for large packets.
	    if (ki_receive (node, KI_ENVINIT, 0) == ERR)
		goto quit_
	    if (p_arg[1] == ERR)
		goto quit_
	}

	status = OK
quit_
	if (status == ERR) {
	    call zclsks (kschan, junk)
	    n_kschan[node] = NULL
	}

	# Restore the caller's kii packet.
	call amovc (Memc[sv], FIRSTINTFIELD, SZB_PACKET / SZB_CHAR)

	call sfree (sp)
	return (status)
end
