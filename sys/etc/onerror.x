# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<syserr.h>
include	<error.h>

# ONERROR -- Give system the EPA of a procedure to be executed when task
# termination occurs (either normal task termination or task termination
# via error recovery).  Each procedure will be called with the task termination
# status, i.e., OK for normal termination, else the ERRCODE argument to ERROR.

procedure onerror (user_proc)

extern	user_proc()			#I procedure to be posted

int	epa, i
bool	first_time
int	proc_list[MAX_ONERROR], nprocs
common	/onercm/ nprocs, proc_list
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

	for (i=1;  i <= nprocs;  i=i+1)
	    if (epa == proc_list[i])
		return

	nprocs = nprocs + 1
	if (nprocs > MAX_ONERROR)
	    iferr (call syserr (SYS_SONERROVFL))
		call erract (EA_WARN)

	proc_list[nprocs] = epa
end


# ONERROR_REMOVE -- Remove a previously posted ONERROR procedure.

procedure onerror_remove (user_proc)

extern	user_proc()			#I procedure to be posted

int	epa, i
int	proc_list[MAX_ONERROR], nprocs
common	/onercm/ nprocs, proc_list

begin
	call zlocpr (user_proc, epa)
	for (i=1;  i <= nprocs;  i=i+1)
	    if (proc_list[i] == epa)
		proc_list[i] = 0
end


# XONERROR -- Called at task termination by the IRAF Main to execute each of
# the posted user error cleanup procedures (if any).  Procedures are executed
# in the order in which they were posted.  The task termination status is
# passed to the called procedure as the single argument to the procedure.
# The list of termination handlers is cleared when finished.

procedure xonerror (status)

int	status			#I task termination status (OK or error code)

int	nprocs_to_execute, i
int	proc_list[MAX_ONERROR], nprocs
common	/onercm/ nprocs, proc_list
errchk	zcall1

begin
	# Clear "nprocs" before calling user procedures, to ensure that
	# a reentrant call does not lead to an infinite loop (i.e., in the
	# event of an error during execution of a cleanup procedure). 
	# In principle this should not be necessary, since an error occurring
	# during error restart should result in a panic abort.

	nprocs_to_execute = nprocs
	nprocs = 0

	for (i=1; i <= nprocs_to_execute;  i=i+1)
	    if (proc_list[i] != 0)
		call zcall1 (proc_list[i], status)
end
