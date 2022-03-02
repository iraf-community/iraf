# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<error.h>
include	<syserr.h>
include	<clset.h>
include	<fset.h>
include	<ctype.h>
include	<printf.h>
include	<xwhen.h>
include	<knet.h>

.help iraf_main
.nf __________________________________________________________________________
The IRAF MAIN

    Task resident interpreter for interface to CL.  Supervises process startup
and shutdown, error restart, and task execution.  A process may contain any
number of tasks, which need not be related.  The iraf main allows a process to
be run either directly (interactively or in batch) or from the CL.  A brief
description of the operation of the Main is given here; additional documentation
is given in the System Interface Reference Manual.


EXECUTION

[1] The process containing the IRAF Main is run.  The PROCESS MAIN, a machine
    dependent code segment, gains control initially.  The process main
    determines whether the task is being run from as a connected subprocess,
    as a detached process, or as a host process, and opens the process
    standard i/o channels.  The process main then calls the IRAF Main, i.e., us.

[2] The IRAF Main performs the initialization associated with process startup
    and then enters the interpreter loop waiting for a command.  A number of
    special commands are implemented, e.g.:

	?		print menu
	bye		shutdown process
	chdir		change directory
	set		set environment variable or variables

    Any other command is assumed to be the name of a task.  The syntax of a
    task invocation statement is as follows:

	[$]task [<[fname]], ([[stream[(T|B)]]>[fname]])|([[stream]>>[fname]])

    Everything but the task name is optional.  A leading $ enables printing of
    the cpu time and clock time consumed by the process at termination.  Any
    combination of the standard i/o streams may be redirected on the command
    line into a file.  If the stream is redirected at the CL level redirection
    is shown on the command line but the filename is omitted.

[3] The communications protocol during task execution varies depending on
    whether or not we are talking to the CL.  If talking directly to the user,
    the interpreter generates a prompt, and the standard input and output is
    not blocked into XMIT and XFER commands.  Interactive parameter requests
    have the form "paramname: response" while CL/IPC requests have the form
    "paramname=\nresponse", where "response" is the value entered by the user.

[4] Task termination is indicated in interactive mode by generation of a prompt
    for the next command and in CL/IPC mode by transmission of the command
    "bye" to the parent process.  If a task terminates abnormally the command
    "error" is sent to the parent process or the terminal, and the Main reenters
    the interpreter loop.

A unique SYS_RUNTASK procedure is generated for each process at compile time by
performing string substitution on a TASK statement appearing in the source code.
The SYS_RUNTASK procedure contains the task dictionary, CALL statements for
each task, plus the special task "?".  The main itself, i.e. this file, is a
precompiled library procedure which has no direct knowledge of the commands
to be run.


ERROR RECOVERY

    If a task terminates abnormally two things can happen: [1] a panic abort
occurs, causing immediate shutdown of the process (rare), or [2] the IRAF Main
is reentered at the ZSVJMP statement by a corresponding call to ZDOJMP from
elsewhere in the system, e.g., ERRACT in the error handling code.

Error restart consists of the following steps:

    (1) The IRAF main is reentered at the point just after the ZDOJMP statement,
	with a nonzero error code identifying the error in STATUS.
    (2) The main performs error recovery, cleaning up the files system (deleting
	NEW_FILES and TEMP_FILES), clearing the stack, and calling any
	procedures posted with ONERROR.  At present the error recovery code does
	not free heap buffers or clear posted exception handlers.
    (3) The ERROR statement is sent to the CL.  An example of the
	error statment is "ERROR (501, "Access Violation")".
    (4) The main reenters the interpreter loop awaiting the next command from
	the CL.

Any error occuring during error restart is fatal and results in immediate
process termination, usually with a panic error message.  This is necessary
to prevent infinite error recursion.


SHUTDOWN

    The process does not shutdown when interrupted by the CL or during error
recovery, unless a panic occurs.  In normal operation shutdown only occurs when
the command BYE is received from the parennt process, or when EOF is read from
the process standard input.  Procedures posted during execution with ONEXIT 
will be called during process shutdown.  Any error occuring while executing
an ONEXIT procedure is fatal and will result in a panic abort of the process.
.endhelp _____________________________________________________________________

define	SZ_VALSTR		SZ_COMMAND
define	SZ_CMDBUF		(SZ_COMMAND+1024)
define	SZ_TASKNAME		32
define	TIMEIT_CHAR		'$'
define	MAXFD			5		# max redirectable fd's
define	STARTUP			0		# stages of execution
define	SHUTDOWN		1
define	IDLE			2
define	EXECUTING		3
define	DUMMY			finit		# any procedure will do


# IRAF_MAIN -- Execute commands read from the standard input until the special
# command "bye" is received, initiating process shutdown.  The arguments tell
# the process type (connected, detached, or host) and identify the process
# standard i/o channels and device driver to be used.

int procedure iraf_main (a_cmd, a_inchan, a_outchan, a_errchan,
	a_driver, a_devtype, prtype, bkgfile, jobcode, sys_runtask, onentry)

char	a_cmd[ARB]		# command to be executed or null string
int	a_inchan		# process standard input
int	a_outchan		# process standard output
int	a_errchan		# process standard error output
int	a_driver		# ZLOCPR address of device driver
int	a_devtype		# device type (text or binary)
int	prtype			# process type (connected, detached, host)
char	bkgfile[ARB]		# packed filename of bkg file if detached
int	jobcode			# jobcode if detached process
extern	sys_runtask()		# client task execution procedure
extern	onentry()		# client onentry procedure

bool	networking
int	inchan, outchan, errchan, driver, devtype
char	cmd[SZ_CMDBUF], taskname[SZ_TASKNAME], bkgfname[SZ_FNAME]
int	arglist_offset, timeit, junk, interactive, builtin_task, cmdin
int	jumpbuf[LEN_JUMPBUF], status, errstat, state, interpret, i
long	save_time[2]
pointer	sp

bool	streq()
extern	DUMMY()
int	sys_getcommand(), sys_runtask(), oscmd()
int	access(), envscan(), onentry(), stropen()
errchk	xonerror, fio_cleanup
common	/JUMPCOM/ jumpbuf
string	nullfile "dev$null"
data	networking /KNET/
define	shutdown_ 91

# The following common is required on VMS systems to defeat the Fortran
# optimizer, which would otherwise produce optimizations that would cause
# a future return from ZSVJMP to fail.  Beware that this trick may fail on
# other systems with clever optimizers.

common	/zzfakecom/ state

begin
	# The following initialization code is executed upon process
	# startup only.

	errstat = OK
	state = STARTUP
	call mio_init()
	call zsvjmp (jumpbuf, status)
	if (status != OK)
	    call sys_panic (EA_FATAL, "fatal error during process startup")

	# Install the standard exception handlers, but if we are a connected
	# subprocess do not enable interrupts until process startup has
	# completed.

	call ma_ideh()
	if (prtype == PR_CONNECTED)
	    call intr_disable()

	inchan  = a_inchan
	outchan = a_outchan
	errchan = a_errchan
	driver  = a_driver
	devtype = a_devtype

	# If the system is configured with networking initialize the network
	# interface and convert the input channel codes and device driver
	# code to their network equivalents.

	if (networking)
	    call ki_init (inchan, outchan, errchan, driver, devtype)

	# Other initializations.
	call env_init()
	call fmt_init (FMT_INITIALIZE)			# init printf
	call xer_reset()				# init error checking
	call erract (OK)				# init error handling
	call onerror (DUMMY)				# init onerror
	call onexit  (DUMMY)				# init onexit 
	call finit()					# initialize FIO
	call clopen (inchan, outchan, errchan, driver, devtype)
	call clseti (CL_PRTYPE, prtype)
	call clc_init()					# init param cache
	call strupk (bkgfile, bkgfname, SZ_FNAME)

	# If we are running as a host process (no IRAF parent process) look
	# for the file "zzsetenv.def" in the current directory and then in
	# the system library, and initialize the environment from this file
	# if found.  This works because the variable "iraf$" is defined at
	# the ZGTENV level.

	interactive = NO
	if (prtype == PR_HOST) {
	    interactive = YES
	    if (access ("zzsetenv.def",0,0) == YES) {
		iferr (junk = envscan ("set @zzsetenv.def"))
		    ;
	    } else if (access ("host$hlib/zzsetenv.def",0,0) == YES) {
		iferr (junk = envscan ("set @host$hlib/zzsetenv.def"))
		    ;
	    }
	}

	# Save context for error restart.  If an error occurs execution
	# resumes just past the ZSVJMP statement with a nonzero status.

	call smark (sp)
	call zsvjmp (jumpbuf, status)

	if (status != OK) {
	    errstat = status

	    # Give up if error occurs during shutdown.
	    if (state == SHUTDOWN)
		call sys_panic (errstat, "fatal error during process shutdown")

	    # Tell error handling package that an error restart is in
	    # progress (necessary to avoid recursion).

	    call erract (EA_RESTART)

	    iferr {
		# Call user cleanup routines and then clean up files system.
		# Make sure that user cleanup routines are called FIRST.

		call xonerror (status)
		call ma_ideh()
		call flush (STDERR)
		do i = CLIN, STDPLOT
		    call fseti (i, F_CANCEL, OK)
		call fio_cleanup (status)
		call fmt_init (FMT_INITIALIZE)
		call sfree (sp)
	    } then
		call erract (EA_FATAL)			# panic abort

	    # Send ERROR statement to the CL, telling the CL that the task
	    # has terminated abnormally.  The CL will either kill us, resulting
	    # in error restart with status=SYS_XINT, or send us another command
	    # to execute.  If we are connected but idle, do not send the ERROR
	    # statement because the CL will not read it until it executes the
	    # next task (which it will then mistakenly think has aborted).

	    if (!(prtype == PR_CONNECTED && state == IDLE))
		call xer_send_error_statement_to_cl (status)

	    # Inform error handling code that error restart has completed,
	    # or next error call will result in a panic shutdown.

	    call erract (OK)
	    call xer_reset ()
	    status = OK
	}

	# During process startup and shutdown the parent is not listening to
	# us, hence we dump STDOUT and STDERR into the null file.  If this is
	# not done and we write to CLOUT, deadlock may occur.  During startup
	# we also call the ONENTRY procedure.  This is a no-op for connected
	# and host subprocesses unless a special procedure is linked by the
	# user (for detached processes the standard ONENTRY procedure opens
	# the bkgfile as CLIN).  The return value of ONENTRY determines whether
	# the interpreter loop is entered.  Note that ONENTRY permits complete
	# bypass of the standard interpreter loop by an application (e.g. the
	# IRAF CL).

	if (state == STARTUP) {
	    # Redirect stderr and stdout to the null file.
	    if (prtype == PR_CONNECTED) {
		call fredir (STDOUT, nullfile, WRITE_ONLY, TEXT_FILE)
		call fredir (STDERR, nullfile, WRITE_ONLY, TEXT_FILE)
	    }

	    # Call the custom or default ONENTRY procedure.  The lowest bit
	    # of the return value contains the PR_EXIT/PR_NOEXIT flag, higher
	    # bits may contain a more meaningful 7-bit status code which will
	    # be returned to the shell.

	    i = onentry (prtype, bkgfname, a_cmd)
	    if (mod(i, 2) == PR_EXIT) {
		interpret = NO
		errstat = i / 2
		goto shutdown_
	    } else
		interpret = YES

	    # Open the command input stream.  If a command string was given on
	    # the command line then we read commands from that, otherwise we
	    # take commands from CLIN.

	    for (i=1;  IS_WHITE(a_cmd[i]) || a_cmd[i] == '\n';  i=i+1)
		;
	    if (a_cmd[i] != EOS) {
		cmdin = stropen (a_cmd, ARB, READ_ONLY)
		call fseti (cmdin, F_KEEP, YES)
		interpret = NO
		interactive = NO
	    } else
		cmdin = CLIN
	}

	# Interpreter loop of the IRAF Main.  Execute named tasks until the
	# command "bye" is received, or EOF is read on the process standard
	# input (CLIN).  Prompts and other perturbations in the CL/IPC protocol
	# are generated if we are being run directly as a host process.

	while (sys_getcommand (cmdin, cmd, taskname, arglist_offset,
	    timeit, prtype) != EOF) {

	    builtin_task = NO
	    if (streq (taskname, "bye")) {
		# Initiate process shutdown.
		break
	    } else if (streq (taskname, "set") || streq (taskname, "reset")) {
		builtin_task = YES
	    } else if (streq (taskname, "cd")  || streq (taskname, "chdir")) {
		builtin_task = YES
	    } else if (prtype == PR_CONNECTED && streq (taskname, "_go_")) {
		# Restore the normal standard output streams, following
		# completion of process startup.  Reenable interrupts.
		call close (STDOUT)
		call close (STDERR)
		call intr_enable()
		state = IDLE
		next
	    } else if (taskname[1] == '!') {
		# Send a command to the host system.
		junk = oscmd (cmd[arglist_offset], "", "", "")
		next
	    } else
		state = EXECUTING

	    if (builtin_task == NO) {
		if (timeit == YES)
		    call sys_mtime (save_time)

		# Clear the parameter cache.
		call clc_init()

		# Set the name of the root pset.
		call clc_newtask (taskname)

		# Process the argument list, consisting of any mixture of
		# parameter=value directives and i/o redirection directives.

		call sys_scanarglist (cmdin, cmd[arglist_offset])
	    }

	    # Call sys_runtask (the code for which was generated automatically
	    # by the preprocessor in place of the TASK statement) to search
	    # the dictionary and run the named task.

	    errstat = OK
            call mem_init (taskname)
	    if (sys_runtask (taskname,cmd,arglist_offset,interactive) == ERR) {
		call flush (STDOUT)
		call sprintf (cmd, SZ_CMDBUF,
		    "ERROR (0, \"Iraf Main: Unknown task name (%s)\")\n")
		    call pargstr (taskname)
		call putline (CLOUT, cmd)
		call flush (CLOUT)
		state = IDLE
		next
	    }
            call mem_fini (taskname)

	    # Cleanup after successful termination of command.  Flush the
	    # standard output, cancel any unread standard input so the next
	    # task won't try to read it, print elapsed time if enabled,
	    # check for an incorrect error handler, call any user posted
	    # termination procedures, close open files, close any redirected
	    # i/o and restore the normal standard i/o streams.

	    if (builtin_task == NO) {

		call flush (STDOUT)
		call fseti (STDIN, F_CANCEL, OK)

		if (timeit == YES)
		    call sys_ptime (STDERR, taskname, save_time)

		call xer_verify()
		call xonerror (OK)
		call fio_cleanup (OK)

		if (prtype == PR_CONNECTED) {
		    call putline (CLOUT, "bye\n")
		    call flush (CLOUT)
		}
		if (state != STARTUP)
		    state = IDLE
	    }
	}

	# The interpreter has exited after receipt of "bye" or EOF.  Redirect
	# stdout and stderr to the null file (since the parent is no longer
	# listening to us), call the user exit procedures if any, and exit.

shutdown_
	state = SHUTDOWN
	if (prtype == PR_CONNECTED) {
	    call fredir (STDOUT, nullfile, WRITE_ONLY, TEXT_FILE)
	    call fredir (STDERR, nullfile, WRITE_ONLY, TEXT_FILE)
	} else if (prtype == PR_HOST && cmd[1] == EOS && interpret == YES) {
	    call putci (CLOUT, '\n')
	    call flush (CLOUT)
	}

	call xonexit (OK)
	call fio_cleanup (OK)
	call clclose()

	return (errstat)
end


# SYS_GETCOMMAND -- Get the next command from the input file.  Ignore blank
# lines and comment lines.  Parse the command and return the components as
# output arguments.  EOF is returned as the function value when eof file is
# reached on the input file.

int procedure sys_getcommand (fd, cmd, taskname, arglist_offset, timeit, prtype)

int	fd			#I command input file
char	cmd[SZ_CMDBUF]		#O command line
char	taskname[SZ_TASKNAME]	#O extracted taskname, lower case
int	arglist_offset		#O offset into CMD of first argument
int	timeit			#O if YES, time the command
int	prtype			#I process type code

int	ip, op
int	getlline(), stridx()

begin
	repeat {
	    # Get command line.  Issue prompt first if process is being run
	    # interactively.

	    if (prtype == PR_HOST && fd == CLIN) {
		call putline (CLOUT, "> ")
		call flush (CLOUT)
	    }					
	    if (getlline (fd, cmd, SZ_CMDBUF) == EOF)
		return (EOF)

	    # Check for timeit character and advance to first character of
	    # the task name.

	    timeit = NO
	    for (ip=1;  cmd[ip] != EOS;  ip=ip+1) {
		if (cmd[ip] == TIMEIT_CHAR && timeit == NO)
		    timeit = YES
		else if (!IS_WHITE (cmd[ip]))
		    break
	    }

	    # Skip blank lines and comment lines.
	    switch (cmd[ip]) {
	    case '#', '\n', EOS:
		next
	    case '?', '!':
		taskname[1] = cmd[ip]
		taskname[2] = EOS
		arglist_offset = ip + 1
		return (OK)
	    }

	    # Extract task name.
	    op = 1
	    while (IS_ALNUM (cmd[ip]) || stridx (cmd[ip], "_.$") > 0) {
		taskname[op] = cmd[ip]
		ip = ip + 1
		op = min (SZ_TASKNAME + 1, op + 1)
	    }
	    taskname[op] = EOS

	    # Determine index of argument list.
	    while (IS_WHITE (cmd[ip]))
		ip = ip + 1
	    arglist_offset = ip

	    # Get rid of the newline.
	    for (;  cmd[ip] != EOS;  ip=ip+1)
		if (cmd[ip] == '\n') {
		    cmd[ip] = EOS
		    break
		}

	    return (OK)
	}
end


# SYS_SCANARGLIST -- Parse the argument list of a task.  At the level of the
# iraf main the command syntax is very simple.  There are two types of
# arguments, parameter assignments (including switches) and i/o redirection
# directives.  All param assignments are of the form  "param=value",  where
# PARAM must start with a lower case alpha and where VALUE is either quoted or
# is delimited by one of the metacharacters [ \t\n<>\\].  A redirection argument
# is anything which is not a parameter set argument, i.e., any argument which
# does not start with a lower case alpha.

procedure sys_scanarglist (cmdin, i_args)

int	cmdin			# command input stream
char	i_args[ARB]		# (first part of) argument list

int	fd
char	ch
bool	skip
pointer	sp, fname, args, ip, op
int	getlline()

begin
	call smark (sp)
	call salloc (args, SZ_CMDBUF, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call strcpy (i_args, Memc[args], SZ_CMDBUF)

	# Do not skip whitespace for param=value args on the command line.
	skip = false

	# Inform FIO that all standard i/o streams are unredirected (overridden
	# below if redirected by an argument).

	for (fd=1;  fd < FIRST_FD;  fd=fd+1)
	    call fseti (fd, F_REDIR, NO)

	# Process each argument in the argument list.  If the command line ends
	# with an escaped newline then continuation is assumed.  Arguments are
	# delimited by whitespace.

	for (ip=args;  Memc[ip] != '\n' && Memc[ip] != EOS;  ) {
	    # Advance to the next argument.
	    while (IS_WHITE (Memc[ip]))
		ip = ip + 1

	    # Check for continuation.
	    ch = Memc[ip]
	    if (ch == '\\' && (Memc[ip+1] == '\n' || Memc[ip+1] == EOS)) {
		if (getlline (cmdin, Memc[args], SZ_CMDBUF) == EOF) {
		    call sfree (sp)
		    return
		}
		ip = args
		next
	    } else if (ch == '\n' || ch == EOS)
		break

	    # If the argument begins with an alpha, _, or $ (e.g., $nargs)
	    # then it is a param=value argument, otherwise it must be a redir.
	    # The form @filename causes param=value pairs to be read from
	    # the named file.

	    if (ch == '@') {
		op = fname
		for (ip=ip+1;  Memc[ip] != EOS;  ip=ip+1)
		    if (IS_WHITE (Memc[ip]) || Memc[ip] == '\n')
			break
		    else if (Memc[ip] == '\\' && Memc[ip+1] == '\n')
			break
		    else {
			Memc[op] = Memc[ip]
			op = op + 1
		    }
		Memc[op] = EOS
		call sys_getpars (Memc[fname])

	    } else if (IS_ALPHA(ch) || ch == '_' || ch == '$') {
		call sys_paramset (Memc, ip, skip)
	    } else
		call sys_redirect (Memc, ip)
	}

	call sfree (sp)
end


# SYS_GETPARS -- Read a sequence of param=value parameter assignments from
# the named file and enter them into the CLIO cache for the task.

procedure sys_getpars (fname)

char	fname			# pset file

bool	skip
int	lineno, fd
pointer	sp, lbuf, ip
int	open(), getlline()
errchk	open, getlline

begin
	call smark (sp)
	call salloc (lbuf, SZ_CMDBUF, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)

	# Skip whitespace for param = value args in a par file.
	skip = true

	lineno = 0
	while (getlline (fd, Memc[lbuf], SZ_CMDBUF) != EOF) {
	    lineno = lineno + 1
	    for (ip=lbuf;  IS_WHITE (Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '#' || Memc[ip] == '\n')
		next
	    iferr (call sys_paramset (Memc, ip, skip)) {
		for (;  Memc[ip] != EOS && Memc[ip] != '\n';  ip=ip+1)
		    ;
		Memc[ip] = EOS
		call eprintf ("Bad param assignment, line %d: `%s'\n")
		    call pargi (lineno)
		    call pargstr (Memc[lbuf])
	    }
	}

	call close (fd)
	call sfree (sp)
end


# SYS_PARAMSET -- Extract the param and value substrings from a param=value
# or switch argument and enter them into the CL parameter cache.  (see also
# clio.clcache).

procedure sys_paramset (args, ip, skip)

char	args[ARB]		# argument list
int	ip			# pointer to first char of argument
bool	skip			# skip whitespace within "param=value" args

pointer	sp, param, value, op
int	stridx()

begin
	call smark (sp)
	call salloc (param, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_VALSTR, TY_CHAR)

	# Extract the param field.
	op = param
	while (IS_ALNUM (args[ip]) || stridx (args[ip], "_.$") > 0) {
	    Memc[op] = args[ip]
	    op = op + 1
	    ip = ip + 1
	}
	Memc[op] = EOS

	# Advance to the switch character or assignment operator.
	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	switch (args[ip]) {
	case '+':
	    # Boolean switch "yes".
	    ip = ip + 1
	    call strcpy ("yes", Memc[value], SZ_VALSTR)
	
	case '-':
	    # Boolean switch "no".
	    ip = ip + 1
	    call strcpy ("no", Memc[value], SZ_VALSTR)

	case '=':
	    # Extract the value field.  This is either a quoted string or a
	    # string delimited by any of the metacharacters listed below.

	    ip = ip + 1
	    if (skip) {
		while (IS_WHITE (args[ip]))
		    ip = ip + 1
	    }
	    call sys_gstrarg (args, ip, Memc[value], SZ_VALSTR)

	default:
	    call error (1, "IRAF Main: command syntax error")
	}

	# Enter the param=value pair into the CL parameter cache.
	call clc_enter (Memc[param], Memc[value])

	call sfree (sp)
end


# SYS_REDIRECT -- Process a single redirection argument.  The syntax of an
# argument to redirect the standard input is
#
#	< [fname]
#
# If the filename is omitted it is understood that STDIN has been redirected
# in the CL.  The syntax to redirect a standard output stream is
#
#	[45678][TB](>|>>)[fname]
#
# where [4567] is the FD number of a standard output stream (STDOUT, STDERR,
# STDGRAPH, STDIMAGE, or STDPLOT), and [TB] indicates if the file is text or
# binary.  If the stream is redirected at the CL level the output filename
# will be given as `$', serving only to indicate that the stream is redirected.

procedure sys_redirect (args, ip)

char	args[ARB]		# argument list
int	ip			# pointer to first char of redir arg

pointer	sp, fname
int	fd, mode, type
int	ctoi()
define	badredir_ 91
errchk	fredir, fseti

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Get number of stream (0 if not given).
	if (ctoi (args, ip, fd) <= 0)
	    fd = 0

	# Get file type (optional).
	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	switch (args[ip]) {
	case 'T', 't':
	    type = TEXT_FILE
	    ip = ip + 1
	case 'B', 'b':
	    type = BINARY_FILE
	    ip = ip + 1
	default:
	    type = 0
	}

	# Check for "<", ">", or ">>".
	while (IS_WHITE (args[ip]))
	    ip = ip + 1

	switch (args[ip]) {
	case '<':
	    ip = ip + 1
	    mode = READ_ONLY
	    if (fd == 0)
		fd = STDIN
	    else if (fd != STDIN || fd != CLIN)
		goto badredir_

	case '>':
	    ip = ip + 1
	    if (args[ip] == '>') {
		ip = ip + 1
		mode = APPEND
	    } else
		mode = NEW_FILE

	    if (fd == 0)
		fd = STDOUT
	    else {
		switch (fd) {
		case CLOUT, STDOUT, STDERR, STDGRAPH, STDIMAGE, STDPLOT:
		    ;
		default:
		    goto badredir_
		}
	    }

	default:
	    # Not a redirection argument.
	    call error (1, "IRAF Main: command syntax error")
	}

	# Set default file type for given stream if no type specified.
	if (type == 0)
	    switch (fd) {
	    case CLIN, CLOUT, STDIN, STDOUT, STDERR:
		type = TEXT_FILE
	    default:
		type = BINARY_FILE
	    }

	# Extract the filename, if any.  If the CL has redirected the output
	# and is merely using the redirection syntax to inform us of this,
	# the metafilename "$" is given.

	while (IS_WHITE (args[ip]))
	    ip = ip + 1
	
	if (args[ip] == '$') {
	    Memc[fname] = EOS
	    ip = ip + 1
	} else
	    call sys_gstrarg (args, ip, Memc[fname], SZ_FNAME)

	# At this point we have FD, FNAME, MODE and TYPE.  If no file is
	# named the stream has already been redirected by the parent and
	# all we need to is inform FIO that the stream has been redirected.
	# Otherwise we redirect the stream in the local process.  A locally
	# redirected stream will be closed and the normal direction restored
	# during FIO cleanup, at program termination or during error
	# recovery.

	if (Memc[fname] != EOS)
	    call fredir (fd, Memc[fname], mode, type)
	else
	    call fseti (fd, F_REDIR, YES)

	call sfree (sp)
	return

badredir_
	call error (2, "IRAF Main: illegal redirection")
end


# SYS_GSTRARG -- Extract a string field.  This is either a quoted string or a
# string delimited by any of the metacharacters " \t\n<>\\".

procedure sys_gstrarg (args, ip, outstr, maxch)

char	args[ARB]		# input string
int	ip			# pointer into input string
char	outstr[maxch]		# receives string field
int	maxch

char	delim, ch
int	op
int	stridx()

begin
	op = 1
	if (args[ip] == '"' || args[ip] == '\'') {
	    # Quoted value string.

	    delim = args[ip]
	    for (ip=ip+1;  args[ip] != delim && args[ip] != EOS;  ip=ip+1) {
		if (args[ip] == '\n') {
		    break
		} else if (args[ip] == '\\' && args[ip+1] == delim) {
		    outstr[op] = delim
		    op = op + 1
		    ip = ip + 1
		} else {
		    outstr[op] = args[ip]
		    op = op + 1
		}
	    }

	} else {
	    # Nonquoted value string.

	    for (delim=-1;  args[ip] != EOS;  ip=ip+1) {
		ch = args[ip]
		if (ch == '\\' && (args[ip+1] == '\n' || args[ip+1] == EOS))
		    break
		else if (stridx (ch, " \t\n<>\\") > 0)
		    break
		else {
		    outstr[op] = ch
		    op = op + 1
		}
	    }
	}

	outstr[op] = EOS
	if (args[ip] == delim)
	    ip = ip + 1
end
