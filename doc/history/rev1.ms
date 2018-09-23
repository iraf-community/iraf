.TL
Summary of Revisions in Version 1.2 of IRAF
.AU
Doug Tody
IRAF Group
.

1. Scope of Revisions

2. CL Modifications
    2.1. Process Cache
    2.2. Background Jobs
    2.3. I/O Redirection
    2.4. OS Escapes
    2.5. Timing Tasks
    2.6. Package Loading and Logout
    2.7. The Null File, Standard Streams
    2.8. CL Main

3. C Runtime Library
    3.1. Purpose
    3.2. Procedure Naming Convention
    3.3. Include Files
    3.4. UNIX Emulation
    3.5. System Calls

4. Program Interface
    4.1. Process Control Facilities
    4.2. Environment Facilities
    4.3. Error Recovery
    4.4. File I/O Modifications
    4.5. Formatted I/O Modifications
    4.6. Bit and Byte Primitives
    4.7. Vector Operators

5. Kernel Modifications

6. Math Library
    6.1. Curve fitting package
    6.2. Surface fitting package
    6.3. Image interpolation package
    6.4. Slatec
    6.5. ACM Algorithms

7. Applications Packages
    7.1 Dataio
    7.1.1. rdumpf
    7.2.2. rrcopy
    7.2.3. ridsfile
    7.2.4. ridsout
    7.2.5. ridsmtn
    7.1. Images
    7.2. Imred
    7.2.1. Echelle
    7.2.2. Cryomap
    7.2.3. Generic
    7.3. Onedspec
    7.4. Twodspec
    7.4.1. Multispec
    7.4.2. Longslit





1. CL Modifications

    A new version of the system has been released on all three 4.2 BSD UNIX
vaxes at NOAO.  The system has undergone major revisions, primarily to
remove UNIX dependencies in preparation for the port to VMS.  A number
of enhancements have also been made in the process.  Those modifications
or extensions of most interest to the user are summarized below.

1.1 Process Cache:

    The process cache is the key to good interactive response.  While the
operation of the process cache is completely automated and need not be
understood, the sophisticated user can obtain improved interactive response
by tuning the cache.

	prcache			show what is in the cache
	prcache task [,task]	lock a task in the cache
	flprcache		flush cache (except locked tasks)
	flprcache task [,task]	unlock a task and remove from cache
    
Where "task" is the name of a logical task, e.g. "directory".
When task A is present (locked) in the cache all tasks present in
the same physical process as task A are also cached (locked).
If one locks too many processes in the cache deadlock will occur.  
Flushing a task from the cache initializes the process and may
fix certain classes of bugs.

The process cache is no longer flushed when the current directory
is changed, when the environment changes (except when a
redefinition is uncovered by a bye), or when a task aborts.


1.2 Background Jobs:

    Background jobs are submitted by appending & to the command line,
as has always been the case.  There is a limit on the number of
bkg jobs that can be running at any one time (currently 4).

	cmd &			run command "cmd" in the background
	jobs			show status of background jobs
	service			service a query from the last bkg job
				    submitted
	service N		service a query from job N
	kill N [,M,...]		kill job number N

A bkg job needing service (waiting for parameter input) will
timeout and abort after a set period of time (currently 3 hours).


1.3 I/O Redirection:

    Is now compatible with the cshell, e.g., >& to redirect both the
standard output and the standard error output, ditto |& and >>&.
The standard error output of an OS escape issued from a script
task or subprocess is now redirected properly, hence output from
XC and MAKE may be spooled.


1.4 OS Escapes:

    !command	Now sends "command" to the shell defined by the UNIX
		environment variable SHELL or to /bin/sh if SHELL is
		not found.  The cshell user can therefore now issue

			! cmd >& spool &

		from within the CL.

    !!command	Always sends the command to the Bourne shell (/bin/sh).


1.5 Timing Tasks

    The cpu and clock time consumed by a compiled task may now be
measured by prefixing the task name with a $, e.g.:

	set | $sort | $match tty | $count

The times given are measured by the IRAF Main and hence do not
include the overhead of process initiation if it occurs.  This
feature is currently only available for external compiled tasks.
It is not currently available for script tasks, intrinsic
functions, or builtin tasks.


1.6 Package Loading and Logout

    The original package loading protocol has returned without the
drawbacks of the original.  When you type the name of a package
the package will become the "current package" (first package
searched when looking for a task), the prompt will change to
reflect the new current package, and a "bye" will be necessary
to return to the package.  It is no longer necessary to issue
a whole series of "bye" commands to log out of the CL, just
type "logout".

	packages		name the packages currently loaded
	??			print menus for all loaded packages
	?sys			print menu for (e.g.) package 'sys'

A package is "loaded" by typing its name, making the tasks
therein known to the CL.  Any number of packages may be 
simultaneously loaded and referenced without continually
changing the "current package".

Programmers: use the new "clbye" in package script tasks in
place of "cl" if the "cl" is the last command in the file,
i.e., if there is no epilogue.  This saves a file descriptor.
The function of "clbye" is otherwise completely identical to
that of "cl".


1.7 The Null File, Standard Streams

    IRAF now has a null file, known as "dev$null".  This is useful
for discarding the output of commands.  The null file may be
referenced anywhere an IRAF filename may be used.  The special
files "STDIN", "STDOUT", and "STDERR" are also quite useful.
These reference the standard i/o streams of the task in which
they are found but may be used wherever an ordinary filename
would be used, including in compiled programs.



System Software
.PP
A great deal of work was done on the core system software of IRAF (the virtual
operating system) to make concurrent operation of a single IRAF system on
UNIX, VMS, and other operating systems a reality.  It is not possible to
report on this in any detail without getting technical, hence a detailed
summary of the work is given as an appendix.
.PP
The highest level system software modifications involved changes in the
\fBsystem\fR package to reflect minor changes in file i/o caused by
implementation of the new kernel, and major changes to the command language (CL)
to remove UNIX dependencies.  The latter conversion involved the design and
implementation of a runtime C language library which includes a complete
emulation of the UNIX/C standard i/o package in terms of IRAF i/o, as well
as a C language callable interface to the IRAF virtual operating system.
.PP
Major modifications and extensions to the file i/o interface (FIO) were
required to make use of the new kernel and to support new software which
will be developed this fall.  Most notably a machine independent, table driven
filename mapping capability was implemented to provide a consistent file naming
syntax on both UNIX and VMS.
.PP
Process control facilities were added to the virtual operating system and
included in the C library for use by the CL.  A portable environment table
facility was implemented which is fully integrated with the file i/o and
process control faciltities, as well as interfaced to the C library.
Some real progress was made on interfacing the NCAR GKS graphics software
to IRAF, leading to production of sample plots on the graphics terminal
(via an intermediate metacode file) by the end of the quarter.  Most of the
research and preliminary design required for the IRAF database facilities
was carried out during the quarter.
.PP
Additional system software projects not required for the VMS port but desirable
for the fall pre-release of IRAF included modification of the \fBhelp\fR
facilities to permit precompilation of the help database, greatly improving the
interactive response, modifications to the TTY interface to permit
precompilation of terminal device tables, also as an efficiency enhancement,
and miscellaneous modifications and extensions to the programmer level
interfaces since we will soon have more people using the system and it will
therefore be that much harder to modify.

Virtual Operating System
.PP
Much work was done on the IRAF virtual operating system (kernel and i/o
packages) during this quarter to make routine concurrent operation of
a single IRAF system on UNIX, VMS, and other operating systems a reality.
Achieving this required not only implementation of equivalent kernels for
UNIX and VMS but also removal of the remaining UNIX dependencies from
the UNIX based IRAF development system.
.NH 4
UNIX and VMS Kernels
.PP
The 4.1BSD UNIX IRAF kernel was completed during the last quarter.  The VMS
kernel, built according to the same specifications as the (revised) UNIX
kernel, was received from STScI in mid-September.
.NH 4
File I/O (FIO)
.PP
A general machine independent, table driven filename mapping capability was
added to FIO to permit use of IRAF virtual filenames on a range of systems.
Achieving this was essential because filenames and directory references
are used extensively in source files in the system (and if they are machine
dependent the code is not portable).  Filename mapping will also help give
us a consistent user interface on all systems.  Implementation of filename
mapping was complicated by the lack of file locking on some of our target
systems.
.PP
In addition to modifying FIO to use the new kernel, the following capabilities
were added to FIO for the October release of the system:
.RS
.IP \(bu
Raw i/o to terminals.  Required for highly interactive programs (e.g. the
simple editor) and for cursor readback from the graphics terminal.
.IP \(bu
Recursive pushback.  Required for macro expansion in CL2 and the new SPP
compiler, and by the C runtime library LIBC (the C runtime library is
discussed below).
.IP \(bu
Temporary redirection of a open stream to a disk file.  Required to implement
i/o redirection in the IRAF Main, providing more efficient pipes between tasks.
.IP \(bu
Automatic deletion of subfiles when the main file is deleted.  This permits
use of the standard file delete command to delete imagefiles and database
files as well as ordinary files.
.IP \(bu
Limited support for multiple file versions as in VMS.
.IP \(bu
Addition of a driver for the "null file" device.
.RE
.NH 4
Process Control
.PP
Formerly, since only the command language (CL) required process control, all
process control was provided by accessing the UNIX system calls directly.
This saved us some time early in the project, but the VMS port required that
the virtual OS be expanded to include process control facilities.
The high level IRAF process control facilities for both connected and
detached processes have been defined, implemented, and tested.
Included are mechanisms for passing the environment list and current working
directory to a subprocess when it is spawned, record oriented binary
interprocess communication facilities, automatic subprocess shutdown
during error recovery, and multiplexing of the i/o streams of both parent
and child processes (i.e., access by the child to a terminal opened by the
parent process).
.NH 4
Environment Facilities
.PP
The UNIX environment (logical name) facilities have been replaced in IRAF
by a similar capability implemented entirely above the system interface
(i.e., it is portable).  In addition to being portable, the new facilities
are superior to those provided by UNIX since the environment name table is
hashed (access is more efficient), entries may be deleted, the table is
automatically passed to a child process at process creation time, and updating
of the table in a child process is supported without respawning the process.
.NH 4

