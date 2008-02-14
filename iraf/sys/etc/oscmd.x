# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<clset.h>
include	<error.h>
include	<knet.h>

# OSCMD -- Send a (machine dependent) command to the host operating system.
# Try to spool the standard output and error output in the named files if
# nonnull names for the files are given.  OK is returned if the command
# executes successfully.

int procedure oscmd (cmd, infile, outfile, errfile)

char	cmd[ARB]		# host command
char	infile[ARB]		# name of host command input file
char	outfile[ARB]		# name of file to receive output
char	errfile[ARB]		# name of file to receive error output

int	status, ip, ch
pointer	sp, cmdbuf, osin, osout, oserr, ostmp, op
errchk	fmapfn, mktemp, fclobber, flush, putline
int	clstati(), getci()
bool	fnullfile()

begin
	call smark (sp)
	call salloc (cmdbuf, SZ_COMMAND, TY_CHAR)
	call salloc (osin, SZ_PATHNAME, TY_CHAR)
	call salloc (osout, SZ_PATHNAME, TY_CHAR)
	call salloc (oserr, SZ_PATHNAME, TY_CHAR)
	call salloc (ostmp, SZ_PATHNAME, TY_CHAR)

	# If we are called from the root process, e.g., the CL, the ZOSCMD
	# primitive is called directly to transmit the host command, otherwise
	# the OS command is sent up to the parent (root) process which calls
	# ZOSCMD.  This is necessary because the ZOSCMD primitive will not
	# work from a subprocess on some systems, due to difficulties trying
	# to spawn the host command interpreter.

	if (clstati (CL_PRTYPE) != PR_CONNECTED) {
	    # Root process: send command directly to the host command
	    # interpreter.
		
	    # Pack command string and get OS versions of the filenames.
	    call strpak (cmd, Memc[cmdbuf], SZ_COMMAND)
	    if (infile[1] == EOS)
		call strpak ("", Memc[osin], SZ_PATHNAME)
	    else
		call fmapfn (infile, Memc[osin], SZ_PATHNAME)

	    # If output is directed to dev$null, save in temp file and delete.
	    if (fnullfile(outfile) || fnullfile(errfile))
		call mktemp ("tmp$null", Memc[ostmp], SZ_PATHNAME)
	    else
		Memc[ostmp] = EOS

	    if (outfile[1] == EOS)
		call strpak ("", Memc[osout], SZ_PATHNAME)
	    else if (fnullfile (outfile))
		call fmapfn (Memc[ostmp], Memc[osout], SZ_PATHNAME)
	    else {
		call fclobber (outfile)
		call fmapfn (outfile, Memc[osout], SZ_PATHNAME)
	    }

	    if (errfile[1] == EOS)
		call strpak ("", Memc[oserr], SZ_PATHNAME)
	    else if (fnullfile (errfile))
		call fmapfn (Memc[ostmp], Memc[oserr], SZ_PATHNAME)
	    else {
		call fclobber (errfile)
		call fmapfn (errfile, Memc[oserr], SZ_PATHNAME)
	    }

	    # Execute the command and wait for completion.
	    call zoscmd (Memc[cmdbuf], Memc[osin], Memc[osout], Memc[oserr],
		status)

	    # Discard output directed to dev$null.
	    if (Memc[ostmp] != EOS)
		iferr (call delete (Memc[ostmp]))
		    call erract (EA_WARN)

	} else {
	    # Connected subprocess.  Send the command to the parent process to
	    # be processed as a system directive by the pseudofile i/o system
	    # in the parent process.  Synchronous execution is desired, so wait
	    # for a status return from the parent process before returning.
	    # The redirection files are ignored in this mode.

	    call flush (CLOUT)

	    # Send command.
	    Memc[cmdbuf] = '!'
	    op = cmdbuf + 1
	    for (ip=1;  cmd[ip] != EOS && cmd[ip] != '\n';  ip=ip+1) {
		Memc[op] = cmd[ip]
		op = op + 1
	    }
	    Memc[op] = '\n'
	    Memc[op+1] = EOS
	    call putline (CLOUT, Memc[cmdbuf])
	    call flush (CLOUT)

	    # Get the return status, encoded as a nonnegative decimal integer.
	    for (status=0;  getci (CLIN, ch) != EOF;  )
		if (ch == '\n')
		    break
		else
		    status = status * 10 + TO_INTEG(ch)
	}

	call sfree (sp)
	return (status)
end
