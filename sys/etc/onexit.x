# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<error.h>

# ONEXIT -- Give system the EPA of a procedure to be executed when process
# shutdown occurs.

procedure onexit (user_proc)

extern	user_proc()			#I procedure to be posted
bool	first_time
int	epa, i
int	proc_list[MAX_ONEXIT], nprocs
common	/onexcm/ nprocs, proc_list
data	first_time /true/

begin
	# The first call is by the IRAF main at process startup time, with
	# a dummy argument.

	if (first_time) {
	    nprocs = 0
	    first_time = false
	    return
	}

	call zlocpr (user_proc, epa)

	# Ignore the call if the procedure has already been posted.
	# Otherwise tack address of proc onto list and return.

	for (i=1; i <= nprocs;  i=i+1)
	    if (epa == proc_list[i])
		return

	nprocs = nprocs + 1
	if (nprocs > MAX_ONEXIT)
	    iferr (call syserr (SYS_SONEXITOVFL))
		call erract (EA_WARN)

	proc_list[nprocs] = epa
end


# ONEXIT_REMOVE -- Remove a previously posted ONEXIT procedure.

procedure onexit_remote (user_proc)

extern	user_proc()			#I procedure to be posted

int	epa, i
int	proc_list[MAX_ONERROR], nprocs
common	/onexcm/ nprocs, proc_list

begin
	call zlocpr (user_proc, epa)
	for (i=1;  i <= nprocs;  i=i+1)
	    if (proc_list[i] == epa)
		proc_list[i] = 0
end


# XONEXIT -- Called at process shutdown time by the IRAF main to execute
# each posted user exit procedure.  Exit procedures are called in the order
# in which they were posted.  Try to survive errors so that all exit
# procedures may be called.  Do not take an error action or issue a warning
# message, since by the time we are called the CL has stopped listening to
# us (it might possibly be safer to panic).

procedure xonexit (exit_code)

int	exit_code			#I passed to exit handlers
int	nprocs_to_execute, i
int	proc_list[MAX_ONEXIT], nprocs
common	/onexcm/ nprocs, proc_list
errchk	zcall1

begin
	nprocs_to_execute = nprocs
	nprocs = 0

	for (i=1; i <= nprocs_to_execute;  i=i+1)
	    if (proc_list[i] != 0)
		iferr (call zcall1 (proc_list[i], exit_code))
		    ;
end
