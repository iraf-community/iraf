# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PRENVFREE -- Free any recently defined or redefined environment variables,
# updating the values of any redefined variables uncovered by the free
# operation in the specified connected subprocesses.

int procedure prenvfree (pid, marker)

int	pid		# pid of process to be updated, or 0 for all subprocs
int	marker		# stack pointer returned by ENVMARK

int	ev_pid
common	/prvcom/ ev_pid
int	locpr(), envfree()
extern	prv_reset()

begin
	ev_pid = pid
	return (envfree (marker, locpr (prv_reset)))
end


# PRV_RESET -- Reset the value of an environment variable in the specified
# connected subprocesses.

procedure prv_reset (name, value)

char	name[ARB]		# name of variable to be reset
char	value[ARB]		# new value

int	ev_pid
common	/prvcom/ ev_pid

begin
	call prenvset (ev_pid, name, value)
end
