# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# Procedures for opening and closing a list of logfiles.  Given the
# name of the CL parameter that contains the list, a dynamic array
# of descriptors for the open files is allocated.  The number of open
# log files is returned.  The files are time stamped both when opened
# and when closed.

# XT_LOGOPEN -- Open a list of log files and provide a sysid time stamp.

int procedure xt_logopen (logparam, prefix, logfd, stdflag)

char	logparam[ARB]		#I CL parameter specifying the list
char	prefix[ARB]		#I String to preceed sysid info
pointer	logfd			#O Pointer to array of open file descriptors
int	stdflag			#O Flag that STDOUT or ERR is in the list

int	loglist, nlogfd, fd, i
pointer	linebuf, fname, sp

int	clpopnu(), clplen(), clgfil(), open()
errchk	open

begin
	logfd = NULL
	stdflag = NO

	loglist = clpopnu (logparam)
	nlogfd = clplen (loglist)

	if (nlogfd > 0) {
	    call smark (sp)
	    call salloc (linebuf, SZ_LINE, TY_CHAR)
	    call salloc (fname, SZ_FNAME, TY_CHAR)
	    call malloc (logfd, nlogfd, TY_INT)

	    call sysid (Memc[linebuf], SZ_LINE)

	    for (i=1; clgfil (loglist, Memc[fname], SZ_FNAME) != EOF; i=i+1) {
		fd = open (Memc[fname], APPEND, TEXT_FILE)
		Memi[logfd+i-1] = fd
		if (fd == STDOUT || fd == STDERR)
		    stdflag = YES

		call fprintf (fd, "\n%s %s\n\n")
		    call pargstr (prefix)
		    call pargstr (Memc[linebuf])
		call flush (fd)
	    }

	    call sfree (sp)
	}

	call clpcls (loglist)
	return (nlogfd)
end


# XT_LOGCLOSE -- Close a list of log files and provide a sysid time stamp.

procedure xt_logclose (logfd, nlogfd, prefix)

pointer	logfd			#I Pointer to array of open file descriptors
int	nlogfd			#I Number of open files
char	prefix[ARB]		#I String to preceed sysid info

int	fd, i
pointer	linebuf, sp

errchk	close

begin
	if (nlogfd <= 0)
	    return

	call smark (sp)
	call salloc (linebuf, SZ_LINE, TY_CHAR)

	call sysid (Memc[linebuf], SZ_LINE)

	do i = 1, nlogfd {
	    fd = Memi[logfd+i-1]

	    call fprintf (fd, "\n%s %s\n\n")
		call pargstr (prefix)
		call pargstr (Memc[linebuf])

	    call close (fd)
	}

	call mfree (logfd, TY_INT)
	call sfree (sp)
end
