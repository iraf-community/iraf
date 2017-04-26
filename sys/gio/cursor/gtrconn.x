# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

# GTR_CONNECT -- Connect a subprocess containing a graphics kernel task to a
# graphics stream.  The graphics kernel task is a conventional IRAF task 
# linked into the kernel process.  After spawning the subprocess, we command
# the process to run the named kernel task, then service the parameter
# requests from the task as it begins running.  Graphics i/o will be via one
# of the graphics streams, leaving STDIN, STDOUT, and STDERR free to access
# the corresponding streams in the parent (the CL).  A kernel may be opened
# either to drive a particular device (if devname is specified) or to drive
# a device selected at runtime.  If the kernel is opened to drive a particular
# device the device name in the OPENWS instruction will be ignored.  We require
# that the graphics kernel begin processing metacode immediately after
# receiving "yes" for the value of the parameter "generic", signifying that
# the caller wishes a generic kernel, i.e., cannot return the values of any
# kernel dependent parameters.

int procedure gtr_connect (kernfname, taskname, devname, stream, in, out)

char	kernfname[ARB]		# name of executable kernel file
char	taskname[ARB]		# name of kernel task
char	devname[ARB]		# device name or null string
int	stream			# graphics stream to connect process to
int	in, out			# input and output streams to process

pointer	sp, lbuf
int	pid
bool	streq()
int	propen(), getline()
errchk	propen, flush, getline, syserr

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	pid = propen (kernfname, in, out)
	call fprintf (out, "%s\n")
	    call pargstr (taskname)
	call flush (out)

	# Pass values of the kernel parameters.  For a kernel run as
	# part of the graphics system there are only three parameters,
	# the input file name (STDGRAPH, etc. for a connected kernel)
	# the device name if the kernel is to ignore device names in
	# OPENWS instructions, and "generic=yes", signifying that the
	# kernel dependent parameters are not to be requested.

	while (getline (in, Memc[lbuf]) != EOF) {
	    if (streq (Memc[lbuf], "=input\n")) {
		call fprintf (out, "%s\n")
		    switch (stream) {
		    case STDGRAPH:
			call pargstr ("STDGRAPH")
		    case STDIMAGE:
			call pargstr ("STDIMAGE")
		    case STDPLOT:
			call pargstr ("STDPLOT")
		    }
		call flush (out)
	    } else if (streq (Memc[lbuf], "=device\n")) {
		call fprintf (out, "%s\n")
		    call pargstr (devname)
		call flush (out)
	    } else if (streq (Memc[lbuf], "=generic\n")) {
		call putline (out, "yes\n")
		call flush (out)
		break
	    } else {
		call putline (STDERR, Memc[lbuf])
		call syserr (SYS_GKERNPARAM)
	    }
	}

	call sfree (sp)
	return (pid)
end
