# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PRENVSET -- Change the value of an environment variable in a connected child
# process, or in all connected subprocesses if pid=0.

procedure prenvset (pid, envvar, valuestr)

int	pid		# process id of child, or 0 for all subprocesses
char	envvar[ARB]	# name of environment variable
char	valuestr[ARB]	# value of environment variable
size_t	sz_val
pointer	sp, cmd

begin
	call smark (sp)
	sz_val = SZ_COMMAND
	call salloc (cmd, sz_val, TY_CHAR)

	call strcpy ("set ",   Memc[cmd], SZ_COMMAND)
	call strcat (envvar,   Memc[cmd], SZ_COMMAND)
	call strcat ("=",      Memc[cmd], SZ_COMMAND)
	call strcat (valuestr, Memc[cmd], SZ_COMMAND)

	call prupdate (pid, Memc[cmd], NO)
	call sfree (sp)
end
