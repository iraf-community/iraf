# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

task	ecl = t_ecl
procedure t_ecl()
begin
end

# CL -- The main entry point of the CL.  Unlike most IRAF tasks, the CL task
# occupies a process all by itself and takes control immediately when the task
# is executed; the in-task interpreter never runs.  The ONENTRY procedure is
# used to achieve this.  ONENTRY gains control from the IRAF main at process
# startup, before the in task interpreter is entered.  The t_cl procedure is
# not called by the interpreter as the TASK statement suggests.  The purpose
# of the task statement is to give us an IRAF main.

int procedure onentry (prtype, bkgfile, cmd)

int	prtype			# process type flag (not used)
char	bkgfile[ARB]		# bkgfilename if detached process (not used)
char	cmd[ARB]		# optional host command line

char	pk_bkgfile[SZ_PATHNAME]
char	pk_cmd[SZ_COMMAND]
int	c_main()

begin
	call strpak (bkgfile, pk_bkgfile, SZ_PATHNAME)
	call strpak (cmd, pk_cmd, SZ_COMMAND)
	return (c_main (prtype, pk_bkgfile, pk_cmd))
end
