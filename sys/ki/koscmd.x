# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<chars.h>
include	<ctype.h>
include	<mach.h>
include	"ki.h"

# KOSCMD -- Send a command to the host command interpreter on the remote
# node.  The output is captured in a file and returned to the caller on
# the local host.

procedure koscmd (oscmd, stdin_file, stdout_file, stderr_file, status)

char	oscmd[ARB]		# packed host command string
char	stdin_file[ARB]		# packed filename of stdin file
char	stdout_file[ARB]	# packed filename of stdout file
char	stderr_file[ARB]	# packed filename of stderr file
int	status

pointer	sp, remfn, locfn, lbuf, op
int	server, oscmd_status, inchan, outchan, nchars
int	ki_connect(), ki_sendrcv(), gstrcpy()
include	"kinode.com"
include	"kii.com"

begin
	server = ki_connect (oscmd)

	if (server == NULL) {
	    call strpak (p_sbuf[p_arg[1]], p_sbuf, SZ_SBUF)
	    call zoscmd (p_sbuf, stdin_file, stdout_file, stderr_file, status)

	} else {
	    if (ki_sendrcv (server, KI_ZOSCMD, 0) == ERR)
		status = ERR
	    else if (p_arg[1] == ERR)
		status = ERR

	    else {
		call smark (sp)
		call salloc (remfn, SZ_PATHNAME, TY_CHAR)
		call salloc (locfn, SZ_PATHNAME, TY_CHAR)
		call salloc (lbuf,  SZ_LINE, TY_CHAR)

		oscmd_status = p_arg[1]

		# Construct the network pathname of the remote spool file.
		op = remfn + gstrcpy (n_alias[1,1,server], Memc[remfn], ARB)
		Memc[op] = FNNODE_CHAR;  op = op + 1
		call strcpy (p_sbuf, Memc[op], ARB)
		call strpak (Memc[remfn], Memc[remfn], SZ_PATHNAME)

		# Open the spooled output file on the remote node.
		call kopntx (Memc[remfn], READ_ONLY, inchan)
		if (inchan == ERR) {
		    status = ERR
		    call sfree (sp)
		    return
		}

		# Open the output file on the local node.  Currently, all
		# output is sent to the designated stdout_file, and the other
		# redirection files are ignored if specified.  If no stdout
		# file is specified, write directly to the user terminal.

		call strupk (stdout_file, Memc[locfn], SZ_PATHNAME)
		if (Memc[locfn] != EOS) {
		    # Copy to a textfile on the local node.

		    call zopntx (stdout_file, APPEND, outchan)
		    if (outchan == ERR) {
			call kclstx (inchan, status)
			status = ERR
			call sfree (sp)
			return
		    }

		    repeat {
			call kgettx (inchan, Memc[lbuf], SZ_LINE, nchars)
			if (nchars > 0)
			    call zputtx (outchan, Memc[lbuf], nchars, status)
		    } until (nchars <= 0)

		    call zclstx (outchan, status)

		} else {
		    # Copy to the terminal on the local node, i.e., to the
		    # standard error output of the calling process.

		    repeat {
			call kgettx (inchan, Memc[lbuf], SZ_LINE, nchars)
			if (nchars > 0) {
			    Memc[lbuf+nchars] = EOS
			    call xer_putline (STDERR, Memc[lbuf])
			}
		    } until (nchars <= 0)
		}

		# Close and delete the remote spool file.
		call kclstx (inchan, status)
		call kfdele (Memc[remfn], status)

		status = oscmd_status
		call sfree (sp)
	    }
	}
end
