# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task	cl = onentry

# CL -- The main entry point of the CL.  Unlike most IRAF tasks, the CL task
# occupies a process all by itself and takes control immediately when the task
# is executed; the in-task interpreter never runs.  The ONENTRY procedure is
# used to achieve this.  ONENTRY gains control from the IRAF main at process
# startup, before the in task interpreter is entered.  The ONENTRY procedure is
# not called by the interpreter as the TASK statement suggests.  The purpose
# of the task statement is to give us an IRAF main.

int procedure onentry (prtype, bkgfile)

int	prtype			# process type flag (not used)
char	bkgfile[ARB]		# bkgfilename if detached process (not used)

char	pk_bkgfile[SZ_PATHNAME]
int	c_main()

begin
	call strpak (bkgfile, pk_bkgfile, SZ_PATHNAME)
	return (c_main (prtype, pk_bkgfile))
end
