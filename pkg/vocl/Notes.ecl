
		ECL:  Enhanced CL Release Notes and User's Guide
		================================================

		           Michael Fitzpatrick
			     NOAO/IRAF Group
			        12/12/04

			   Revised: 5/28/05
								

********************************************************************************
Release History:
     02/10/05		** Alpha Release for testing
     05/06/05		** 2nd Alpha Release for testing
     06/07/05		** 1st Beta Release for testing


********************************************************************************

Table of Contents
-----------------

    Introduction

    Installation and Use
  	To Install the CL
  	Determine CL Version Type

    Error Handling
  	Introduction and Cautions
  	    Example Descriptions
  	Reporting Errors
	    Traceback
	Trapping Errors
	    The 'iferr' Syntax
	    The 'erract' Environment Variable
	    Error Handling: Then and Now
	New CL parameters
	What Errors Are NOT Trapped

    Command-line History and BackSpace Revisions
	Input Command Summary

    New Builtin Functions and Variables
	Error Functions
	String Functions
	Trig Functions
	Utility Functions
	Bitwise Operations

    Defined Constants

    Post-Release Notes


********************************************************************************

============
Introduction
============

    The primary goals of the ECL project were to 

	o  add an error-handling capability to the existing IRAF CL, 
	o  include other functionality which could improve the
	   scripting environment (e.g. pre-defined language constants
	   such as 'PI') and add any other features we found lacking
	   (e.g. missing trig functions and string utilities), and
	o  add commonly requested features.

Where possible, small enhancements such as a new utility builtin function
will be implemented in the "old" CL as well, however as scripts begin to
use the more advanced features scripts will naturally become less backward
compatible.  Future work will build on the version presented here with
the hope that users will migrate to the new system over a short time.

	This is a work in progress.  Users are encouraged to experiment with
features, request future enhancements, and to please report any errors or
problems to 
		iraf@noao.edu

New releases will be announced on the IRAF website (http://iraf.noao.edu)
following the addition of any new features or when critical bugs have been
fixed.



====================
Installation and Use
====================

	The ECL is being distributed in a self-extracting script file
rather than the traditional IRAF external package since it is meant to
overlay an existing IRAF system until the time when it becomes part of
the core distribution.  Since the script creates a new command link in
the unix system "local bin directory" and adds files to the IRAF source 
tree, it MUST be run as the root user (the script will terminate or ask
if you wish to proceed with a no-op installation otherwise).

The installation script does the following to your system:

    1)  Replaces the existing hlib$cl.csh script with a modified 
	version after creating a hlib$cl.csh.ORIG backup file

    2)  Creates an "ecl" command link in the same directory as the
	current "cl" IRAF command link.  Both links point to the same
	hlib$cl.csh script which checks for how it was called an 
	invokes the proper binary.

    3)  Moves the "ecl.e" binary to the proper iraf$bin.<arch> directory,
	changing the ownership to the 'iraf' user and setting the execute
	permissions on the file.

    4)  Creates a iraf$pkg/ecl directory and moves all ECL sources there.

The install script may be run from any directory on the system, it is
unpacked in /tmp and cleans up temp files when complete.   A "personal
installation" option is not implemented at this time but could be considered
later for users who don't have write permission on their IRAF tree.  Please
contact iraf@noao.edu for instructions on how to manually setup such a
system for personal use.


To Install the ECL
------------------

Step 1)	Download the distribution file appropriate for your system.  For
	example, 

        	% ftp iraf.noao.edu (140.252.1.1)
        	login: anonymous
        	password: [your email address]
        	ftp> cd pub
        	ftp> binary
        	ftp> get ecl_install_redhat.csh
        	ftp> quit

Step 2) Execute the script AS ROOT:

		% su				# become the root user
		# ./ecl_install_redhat.csh

	The script will prompt you for the local bin directory or any 
	iraf paths needed, simply accept the default values determined for
	your system or override them with others.

	Once executed, the ECL source and binaries will be installed in
	the system as described above.  The file you are reading right
	now is available as iraf$pkg/ecl/Notes.ecl and will be updated
	with post-release notes at the end of the file with each new
	release.

Step 3) Start the ECL from your normal IRAF login directory as either

		% ecl
	or
		% cl -ecl

	The second form of the command is needed on systems which mount
	IRAF from another machine since the CL command links are created
	at IRAF install time.  One reason for replacing the hlib$cl.csh
	script is to allow for the "-ecl" argument to override the binary
	to be used on systems where only the 'cl' command is available and
	so that the installation isn't required on all machines mounting
	a common IRAF.

	The default ECL prompt is now "ecl>" in the new version as a visual
	clue that the new system is being used.  Additionally, package prompts
	default to using the complete package name rather than the familiar
	2-character prefix as another clue.  This behavior can be changed
	by adding the string "nolongprompt" to the CL 'ehinit' parameter,
	e.g.

	       cl> cl.ehinit = cl.ehinit // " nolongprompt"


Except as described below, use of the ECL should be identical to the
traditional CL for most users.


Determining CL Version
----------------------

	As users begin to make regular use of features found only in the
ECL, the first error to be checked is that the script is running using the
proper version of the CL.  This needs to be done using features found in
both the ECL and traditional CL languages.  The simplest test, for either
package loading scripts or within tasks, is something like

	if (defpar ("$errno")) {
	    print ("You are using the ECL")
	} else {
	    print ("You are using the old CL")
	}




==============
Error Handling
==============

Introduction and Cautions
=========================
    
    The error-handling enhancements are composed of two elements: 

	o  the reporting of errors within scripts, and
	o  the ability to trap and recover those errors.  

The first case addresses the long-standing problem in which an error message
returned by a script gives a line number that has no basis in reality, and
which gives no useful information about the underlying task that created it.
In the second case, one often wants scripts to be able to trap errors from
compiled tasks so that some sort of cleanup can be done in order to allow
the script to continue, or so that an error status code can be examined
and some specific action taken (which may simply be to ignore the error).

    In the ECL, messages are now printed with the correct line number and
with a detailed traceback to the user's command-line showing more precisely
what was called at the time of the error.  New language constructs are
available which allow scripts to conditionally check for errors from
tasks they call and branch to code to deal with those errors.  Finally,
new ECL environment variables and builtin functions allow for limited
error-handling control over scripts already in the system which have not
been retrofitted to specifically trap errors.  Details of each of these
capabilities and examples of how they may be used by developers and users
are given below.  It is also worth discussing the types of errors which
can occur in a script task before getting into details about how they
might be handled by the user or script programmer.

Error conditions in the CL break down into roughly the following types:

          Error Type	       		Examples
          ----------			--------

    Compiled Task Errors    1) A call to a compiled task in the system
			       dies unexpectedly with an exception (e.g.
			       FPE, segmentation violation, etc)
			    2) A task aborts due to an error condition the
			       task has trapped and cannot recover (e.g.
			       invalid parameters, out of memory, etc).

    CL Internal Errors	    1) Script code performs an illegal operation
			       causing an exception (e.g. "i = j / k"
			       where 'k' is zero.
			    2) Script code triggers a runtime error within
			       the CL itself (e.g. "log (string_value)")

    CL Error Assertions	    1) Script programmer forces the task to exit
			       with a call to the CL error() builtin.
			    2) Script programmer simply prints and error
			       message indicating a problem and returns
			       without further processing.

All of these errors can be detected at some level, however not all of
them can be handled in a way which allows a calling script to recover
and continue executing, nor would it always make sense to do so.
Errors such as a floating-point-exception (FPE) may be data-dependent,
a segmentation violation may indicate a coding error in a compiled task
or a platform-specific bug, or an error in another script task may be
beyond the control of the scripter to fix.  Error assertions by a script
programmer are not meant to be recoverable, and in the second example
an arbitrary problem message cannot be trapped by the system.

    An error-handling capability in the ECL (or any language) is not a
panacea for all error conditions one might encounter, the best a script
programmer can hope to do is to trap an error and take some reasonable
action at the time.  The ECL offers a way for a script to print a more
meaningful error message, or at least abort gracefully after cleaning
itself up.  However, depending on the type of error,  *your* script may
still never run to completion until somebody else fixes *their* code.

    Lastly, it is also important to note that trapping an error means the
script finds itself in an unnatural state.  Proper recovery requires
that the script programmer understand the error condition as well as
the state of the script at that point of execution.  The error-handling
code must restore the script to a state where it can continue running
(if possible) and avoid potential side-effects caused by e.g. forgetting
to clean up intermediate files or reset counter variables.  New language
features mean new types of bugs can be introduced into a script, even if
the irony is that these new features are meant to trap bugs!


Example Descriptions
--------------------

	In the examples to follow we will make use of an ERRTEST package
distributed with the ECL source and containing the following tasks used
in the examples to follow:

     nested -- Test various error conditions from layered scripts
      nest0 -- Dummy layer for nested testing
    errtype -- Low-level script to test compiled and CL error conditions
    
        fpe -- Compiled task producing an arithmetic exception
     segvio -- Compiled task producing a segmentation violation
     spperr -- Compiled task invoking the SPP error() function
    


Reporting of Errors
===================
    
Traceback
---------

    The most obvious change to users will be in the traceback of errors
reported by the ECL.  As an example, suppose we have a test script
called NESTED that calls several layers of other scripts until it gets
to a compiled task called FPE which simply triggers a divide-by-zero
arithmetic exception.  The calling sequence we use is

	NESTED (type)			# toplevel test task
	    NEST0 (type)		# hidden script task
		ERRTYPE (type)		# script task
		    FPE ()		# compiled task giving the error

(The 'type' argument here is a code used to test various types of system
errors but its value isn't important to the current discussion.)  In the 
traditional CL, executing this script results in the following and familiar
message:
		cl> nested 1
		ERROR on line 72: floating point divide by zero
		    errtype (type=1)
		    nested (type=1)

There are a number of issues with the error report here we wish to correct:

    1)  The error is reported to be on line 72, but none of the scripts
        called invoke any task on that line, or even have that many lines,
        and so it is clearly wrong.
    2)  Was it the ERRTYPE script that caused an error or something else?
    3)  There is no mention of the FPE task we know to be the culprit.

These problems are resolved in the ECL where the error report now looks like:

	cl> nested 1
	ERROR: floating point divide by zero
	  "fpe ()"
	     line 15: errtest$errtype.cl
	     called as: `errtype (type=1)'
	  "errtype (type)"
	     line 13: errtest$nest0.cl (hidden task)
	     called as: `nest0 (type=1)'
	  "nest0 (type)"
	     line 11: errtest$nested.cl
	     called as: `nested (type=1)'

The traceback is more complete and begins with the task which actually
throws the error.  Checking the line numbers of the ERRTEST package
scripts we find that indeed FPE is called on line 15 of 'errtype.cl',
ERRTYPE is called from line 13 of 'nest0.cl', and so on.

    For each task in the calling sequence the format of the traceback is

	<script code fragment executing at the time of error>
	   LINE <number>: <script file containing line>
	   CALLED AS: <how this script was called>

The length of the traceback may be controlled with the new 'erract'
environment variable discussed in more detail below.  In short, 'erract'
allows the traceback to be suppressed entirely, to print information only
at the level where the error occurred, or to print a full calling stack
trace (default).


Trapping Errors
===================

The 'iferr' Syntax
------------------

    The ECL provides new language constructs to enable error actions, error
handling and recovery.  This syntax will already be familiar to SPP programmers
and will quickly become obvious to even novice script programmers.

    Error recovery is implemented using the IFERR and IFNOERR statements
to "post" an error handler that is called at the end of a block of code and
which checks for error conditions that may have occurred in that block.
The syntax for these statements is of the form:


    iferr { <statement> } 		ifnoerr { <statement> }
        <error action statement> 	    <success action statement>


    iferr { 				ifnoerr {
        <block of statements> 	    	    <block of statements>
    } then 				} then
        <error action statement> 	    <success action statement>


    iferr { 				ifnoerr {
        <block of statements> 		    <block of statements>
    } then { 				} then {
        <block of error stmts> 		    <block of success action stmts>
    } 					}


The IFERR is grammatically equivalent to the IF statement and means "if an
error occurs during the processing of the enclosed code, execute the error
action statement to follow".  IFNOERR is the same except that the sense
of the test is reversed and the action statements are executed only if the
enclosed code completes without error.  Additionally, these statements take
an ELSE clause allowing both forms of the test to be combined.  For example,


    iferr { 				ifnoerr {
        <block of statements> 		    <block of statements>
    } then { 				} then {
        <error stmts> 		    	    <success stmts>
    } else { 				} else {
        <success stmts>	    	    	    <error stmts>
    } 					}


In all cases 

    o  Curly braces around the code to be checked are required,
    o  Curly braces are required when any action is a compound block 
    o  The THEN statement is optional if a single statement is executed
       as part of the action block
    o  The THEN statement is required for a compound action or when using
       an ELSE clause
    o  It is a syntax error for a condition block to itself directly contain
       an IFERR or IFNOERR statement and action statements, i.e.  IFERR
       statements may not be nested


    To make effective use of these statements a few points need to be
kept in mind:

    o   The check for errors happens only after ALL statements in the
	condition block are executed;
    o   Statements which generate errors did not execute to completion,
	subsequent code relying on that result cannot be trusted
    o   Code in the condition block which executes following an initial
	error may itself trigger errors due to the failure of a previous
	statement or the resulting side-effects;

This implies that IFERR statements should be used to target only critical
pieces of code where a particular error condition might be expected, and/or
where an action block could reasonably react to that error.  As an example
of how ignoring these points could be problematic consider the code snippet:

	iferr {
	    task_a ()
	    task_b () | scan (x)
	    task_c (x)
	} then {
	    error (99, "An error occurred\n")
	}

All three tasks in the condition block will be executed, however the
behavior of the code being check depends on which task in the block fails;
If 'task_a' fails there may be no consequences for the remaining calls,
however if 'task_b' fails the value of 'x' may never be set and 'task_c'
may also fail (or at least produce spurious results).  Cascading errors like
this will also be trapped and the action statement will still execute, but
the system error message strings will be incomplete (more about that below).

    While it is possible to have a failure from each statement in a condition
block branch immediately to the action block by checking each statement
individually, doing so would permit poor programming practices such as
iteratively testing for the name of the failed task and taking different
recovery methods in the action block.  If this is actually required for the
script to recover cleanly, the recommended method is to put an IFERR block
around smaller pieces of code where the recovery statements relate more
directly to the code being checked.

Errors trapped by IFERR statements include:

    o  System exceptions (FPE, segfault, etc) thrown by compiled tasks
    o  SPP error() returns from compiled tasks
    o  CL script error() assertions

Below we discuss errors which cannot be trapped using the IFERR syntax as
well as strategies for how to handle those errors which can be detected.
We'll also see how to determine which task in a condition block failed
and why.


The 'erract' Environment Variable
----------------------------------

	The ECL has a new 'erract' environment variable used to control the
different aspects of the error handling.  This is a whitespace-delimited
string comprised of the following options:


    abort	Script task should abort at an error and begin error
		recovery back to the command-line

    noabort	Task should not abort, but continue execution if possible

    trace	Print a traceback of the calling sequence including all
		line numbers and calling statements

    notrace	Print only the error message, no linenumbers or calls

    clear	Clear the error params (i.e. $errmsg, $errnum, $errtask)
		at each new task call.  This reseets the params with each
		task invocation allowing them to be examined after each
		call regardless of whether the code is in an IFERR block.

    noclear	Do not clear the CL error params at each new task call,
		the params are only reset when an error is encountered.

    flpr	Automatically issue a 'flpr' when an error is seen.  This
		is used to flush any failed task from the process cache to
		avoid potential future problems caused by a corrupted task.

    noflpr	Do not issue a 'flpr' when an error is seen, tasks remain
		in the process cache, possibly in an error state.

    full	Print a complete traceback of the calling sequence.

    nofull	Print only the error report for the task causing the error
		and none of its parents.


The default value is set as:

    set erract = "abort trace flpr clear full"

Note that erract is implemented as an environment variable rather than
as a new CL parameter (similar to the ehinit/epinit params) in order to
minimize changes in the CL parameter file itself during the transition
to the ECL.  The difference is that the 'set' (ore 'reset') command must
be used to define the values, whereas with ehinit/epinit they may be
assigned directly.  For this variable it is also possible to (re)define
a single parameter without affecting other options, e.g.

    cl> show erract			# print options
    abort trace flpr clear full	
    cl> set erract = "noabort"		# reset one of them
    cl> show erract			# print options again
    noabort trace flpr clear full



Error Handling: Then and Now
----------------------------

    To better understand the new error detection and recovery behavior
(and to document this for future reference), let's look at the old error
mechanisms of the CL language:  Any command called from the CL executes in a
context defined by the task hierarchy initiating the command, i.e. from the
command-line CL prompt one has a "first" task context, scripts calling child
(compiled or script) tasks push a new CL context inheriting the current
CL environment and who's 'parent' is the context that invoked the task.

    In the traditional CL with an error occuring in a compiled task,
recovery first takes place in the SPP code who may choose to either handle
the error itself or may abort completely by doing a long-jump back to the
IRAF main() procedure (i.e. an EA_FATAL error type).  In this latter case,
the process binary (running as a detached process from the CL) sends an
error() command back to the CL telling it the task has terminated abnormally
(a normal task shutdown leaves the executable simply waiting for more input
from the CL, e.g. another task to execute).  This returned error() statement
is the same CL error() command one would use to abort a script task, and its
effect is to tell the CL to abort the current context and long-jump back
to the command-line after cleaning up running processes and freeing the
dictionary space (what the CL uses to catalog tasks/packages, parameters,
etc).  [NOTE: Whether it is a system exception or a programmer-posted error,
the error sent back to the CL has always included both the error code and
message, it is just that the CL has never made use of these until now.]
Similarly, errors which occur while running script tasks (e.g. 'task not
found' errors, invalid use of string values, divide-by-zero from local
script variables, etc) also end up in the same CL error() procedure via
internal procedure calls made while executing the script.

    Syntax errors are caught when the script is 'compiled' into the opcode
execution stack and are reported before the script begins to execute.
A script calling a child script containing a syntax error cannot trap
that error even though it will not be reported until the child script is
'compiled' just prior to execution.  We assume that all script tasks are
well-formed and free of ntax errors.

    ECL error recovery is somewhat simplified by the fact that errors,
either from external tasks or the execution of scripts, all converge in
a single procedure in the CL source code.  The trick is to modify the
runtime behavior of the CL so that once we know we have an error we can
branch to conditional code instead of simply jumping all the way back to the
command line.  Since we also wish to improve the error reporting we'd also
like make better use of information about how the failed code was called.

    The first step is to realize that when executing a script the CL
language is "compiled" into a series of 'opcode' instructions comprising an
intermediate runtime language (similar to assembly language).  Scripts are
run by executing the opcode instruction at the current 'program counter'
location, pushing task arguments or getting labels for jumps from the
dictionary, restoring a previous CL context, etc.  The compilation stage
already has information about the script line being parsed so by adding
this line-number to the opcode instruction it is now possible to trace a
fault in any opcode back to the originating line of the script, and from
there back up to the command line through the calling tree.  This extra
information makes the runtime size of the script slightly larger so
extremely large scripts may experience "dictionary full" problems not
previously seen (various CL buffer sizes were increased to help offset
this problem).  This relatively minor change is all that is required to
address the problems mentioned above in error reporting.

    Error trapping and recovery is done in a manner similar to the
implementation in SPP:  The IFERR statement isn't actually an instruction
in the runtime script, rather it is used to tell the parser to insert
code around the block to be checked using traditional IF statements.
As an example, consider

	iferr { 
	    task1 (arg1, arg2) 
	    task2 (arg1)
	} then {
	    recovery ()
	}

When compiled this is the equivalent of writing


	_errpsh () 
	task1 (arg1, arg2) 
	task2 (arg1)
	if (_errpop () != 0) {
	    recovery ()
	}

The _errpsh() is a hidden builtin function which "pushes" an error
structure onto the runtime stack, the _errpop() test at the end then
queries that structure to see whether any statement since the previous
push set the error flag and filled in the structure with the task name,
line number and other information.  The push also temporarily deactivates
the behavior of the error() function so it no longer aborts entirely,
allowing the script to continue after cleaning up the current error.

	In order to keep the model simple, nested iferr statements within
the same script are not currently implemented but are a possible future
enhancement.  Complications arise from examples such as

	iferr { 
	    task1 (arg1, arg2) 
	    iferr { task2 (arg1) } then 
	        recovery2 ()
	} then {
	    recovery1 ()
	}

Consider the case where task1() succeeds and task2() fails and is
recovered properly with the recovery2() procedure.  As far as the outer
IFERR block is concerned, did an error occur or not?  If the remainder of
the script depends on task2() succeeding then the answer is possibly 'no'
(depending on what the recovery does) and we should additionally call
the recovery1() procedure (who is responsible for dealing with an error
condition in that block), if there is no dependency then we may want
*any* failure to be considered, or perhaps even have a way to "clear"
error conditions within the block.  Now assume instead it is the first
task which fails and that triggers the second to fail because we depend
on the first succeeding, how should we post the error number/message for
the script?  We simply disallow nested IFERR statements for the moment
to avoid dealing with these complex interactions


New CL parameters
===================

	On order for script programmers to make use of errors that have
been trapped by the ECL, one generally needs access to the details of
that error, e.g. the message, task name, error number, etc.  To this end
the ECL implements several new pseudo-parameters and builtin functions
containing this information.  These include

    Param	    Function        Meaning
    -----	    --------        -------
    $errno	    errno()	    The system error number
    $errmsg	    errmsg()	    The system error message string
    $errtask	    errtask()	    Task which created the error

By default these parameters are re-defined as each task is called, in theory
allowing a script to trap errors without the IFERR by doing something like

	mytask1 () 
	if ($errno != 0) <statement>
	mytask2 () 
	if ($errno != 0) <statement> 
	    :

This behavior can be modified by the 'erract' environment variable 'clear'
or 'noclear' settings so that they only change when an error condition is
found (i.e. erract set to 'noclear', tasks which complete successfully
do not modify variables).

	Additionally, a new $err_dzvalue pseudo-parameter is defined to
be used by the CL interpreter when a divide-by-zero condition is encountered
in the CL itself.  (This value has no builtin function equivalent.)
This is an integer and will be cast to floating-point automatically if
needed, the default value of 1 (one) was chosen to allow the script to
continue executing but it should be noted that this value is only used
when an error is found within an IFERR block.  For example,

	ecl> = 1 / 0
	ERROR: integer divide by zero
	ecl> = 1. / 0.
	ERROR: floating divide by zero

However,

	ecl> iferr {
	>>>     = 1 / 0
	>>> } then ;;
	Warning on line 31 of : integer divide by zero - using $err_dzvalue = 1
	1

Note the warning message indicating the use of the parameter followed by the
result.



What Errors Are NOT Trapped
===========================

	As mentioned above, not all CL errors can or should be trapped
by the new system.  The (incomplete) list of error conditions which 
CANNOT be trapped during task execution using the IFERR or other new
features includes:

    o CL-language syntax errors
    o CL internal errors, for example
	- invalid procedure arguments (e.g. "parameter not found")
	- improper usage of intrinsic procedures (e.g. log(-10) )
	- operand type mis-matches (e.g. "s1 + x")
	- parser errors (e.g. newline in string)
    o CL runtime errors
	- too many background jobs (e.g. "no available job slots")
	- insufficient resource messages (e.g. out of memory)
	- can't read/write/create files (e.g. permissions problem on uparm$)
	- ambiguous task name
	- scan/print string exceeds max length
    o User-defined error messages and returns (i.e. the script writer
	outputs an error message and returns from the procedure but 
	does not use something like thea CL error() function to abort.
	For instance, a script prints "I can't run this on Tuesdays" and
	returns to the command-line but does not otherwise post an error
	condition for the calling context.

    

============================================
Command-line History and BackSpace Revisions
============================================

	The ECL now implements the common GNU Readline interface for input
handing meaning that many familiar tcsh-like features such as Up/Down-Arrow
history, Left/Right cursor-position movement, and tab-filename completion
are now understood in the IRAF environment.  It follows that many of
the problems encountered with the DEL/BS key to erase characters when
entering input on the commandline have also been eliminated on most
systems since the readline interface internally handles the delete-key
mappings imposed on most systems.  Tab-completion of task/params names
was not implemented in this initial release but could be added later.

	It is important to note that this implementation was done so as
to not interfere with the native IRAF ehist/epar cursor and history
mechanism.  From the ECL prompt, all commands recognized by readline()
interface (including user mappings defined in an ".inputrc" file) will
be honored.  If that command is ehist/epar or one of the recognized
IRAF history editing metacharacters then these will be processed in the
traditional IRAF manner.

	Should a problem with readline input be found, it can be disabled
from the user's session by adding the string "noreadline" to the CL
'ehinit' parameter, e.g.

	ecl> cl.ehinit = cl.ehinit // " noreadline"



Input Command Summary
---------------------
	
	The following Control/Meta key sequences are understood by the 
readline() interface for command input:

  Basic Commands

    Ctrl-b 	    Move cursor back one character. 
    Ctrl-f 	    Move cursor forward one character. 
    DEL 	    Delete the character to the left of the cursor. 
    Backspace 	    Delete the character to the left of the cursor. 
    Ctrl-d 	    Delete the character underneath the cursor. 
    Ctrl-_ 	    Undo the last editing command
    Ctrl-x Ctrl-u   Undo the last editing command

    Up-Arrow 	    Move up through the command-history list
    Down-Arrow 	    Move down through the command-history list
    Left-Arrow 	    Move cursor left one character on command line
    Right-Arrow     Move cursor right one character on command line

  Cursor Movement Commands

    Ctrl-a 	Move to the start of the line. 
    Ctrl-e 	Move to the end of the line. 
    Meta-f 	Move forward a word, where a word is composed of letters/digits.
    Meta-b 	Move backward a word. 
    Ctrl-l 	Clear the screen, reprinting the current line at the top.

  Text Deletion Commands

    Ctrl-k 	Kill the text from the current cursor position to the end of
		the line.
    Meta-d 	Kill from the cursor to the end of the current word, or, if
		between words, to the end of the next word. Word boundaries
		are the same as those used by Meta-f.
    Meta-DEL 	Kill from the cursor the start of the current word, or, if
		between words, to the start of the previous word. Word
		boundaries are the same as those used by Meta-b.
    Ctrl-w 	Kill from the cursor to the previous whitespace. This is
		different than Meta-DEL because the word boundaries differ.

    To yank (copy the most-recently-killed text from the kill buffer) the text
    back into the line:

    Ctrl-y 	Yank the most recently killed text back into the buffer at
		the cursor.
    Meta-y 	Rotate the kill-ring, and yank the new top. You can only do
		this if the prior command is Ctrl-y or Meta-y. 

  History Searching Commands

    Ctrl-r	Search backward through the history for a particular string
    Ctrl-s	Search forward through the history for a particular string
    ESC		Terminate the search
    Ctrl-g	Terminate the search and restore original line

	As each character of the search string is typed, Readline displays
    the next entry from the history matching the string typed so far. An
    incremental search requires only as many characters as needed to
    find the desired history entry.  To find other matching entries in
    the history list, type Ctrl-r or Ctrl-s as appropriate from the current
    search position.

    NOTE:  In many terminal settings the Ctrl-s key is mapped to the tty
   	'stop' character and the window will appear to no longer accept 
	input.  In these cases a Ctrl-q will normally return the terminal
	to proper function and so the forward search mechanism isn't
	generally recommended.



=====================
New Builtin Functions
=====================

Error-Handling Functions
------------------------

	The following builtin functions were added as alternatives to the
matching CL parameters.  The difference is almost entirely stylistic and
the rules about the longevity of the values described above apply in either
case.

      errmsg ()		Return last error message string (i.e. cl.$errmsg)
      errcode ()	Return last integer error code  (i.e. cl.$errno)
      errtask ()	Return taskname posting fatal error (i.e. cl.$errtask)

Examples:

	iferr {
	    sometask (par1, ....)
	} then {
	    printf ("Error in '%s': %s\n", errtask(), errmsg())
	    # or equivalently
	    printf ("Error in '%s': %s\n", $errtask, $errmsg)
	}



String Functions
----------------

	Beginning with V2.12.2 several new functions were added to the
CL to improve string handling and the provide complementary functions
to those which already exist.  Items marked with a '*' first appeared in
V2.12.2, all others are new to this release.  

New functions include:

    isindef (expr)							(*)
       	Can be used to check for INDEF values in expressions.  INDEF
       	values may be tested for equality, however when otherwise used
       	in a boolean expression the result of the boolean is also
       	INDEF.  This function can be used to trap this particular
       	case, or for INDEF strings/variable directly.  Result is a
       	boolean yes/no.

       	Example:
	    cl> junk = fscan (threshold, tval)
	    cl> if (isindef (tval) == yes) 
	    	error (0, "INDEF 'threshold' parameter value")

    strlwr (str)							(*)
    strupr (str)							(*)
       	Convert the string to lower/upper case, returns a string.     

       	Example:
	    cl> s1 = "test" ; s2 = "TEST"
	    cl> = strupr (s1) ; = strlwr (s2)
	    TEST
	    test

    strstr (str1, str2)							(*)
       	Search for first occurance of 'str1' in 'str2', returns index
       	of the start of 'str1' or zero if not found.

       	Example:
	    cl> = strstr ("imh", "imhead.imh")
	    1
	    cl> = strstr ("head", "imhead.imh")
	    3

    strldx (chars, str)							(*)
       	Complement to the stridx() which returns the last occurance of
       	any of 'chars' in 'str'.  Returns index of last char or zero
       	if not found.

       	Example:
	    cl> = strldx (".", "junk.fits")
	    5
		

    strlstr (str1, str2)						(*)
       	Search for last occurance of 'str1' in 'str2', returns index
       	of the start of 'str1' or zero if not found.

       	Example:
	    cl> = strlstr ("imh", "imhead.imh")
	    8

       [NOTE: String indices are 1-indexed in the CL]

    trim (str [, trimchars])
    triml (str [, trimchars])
    trimr (str [, trimchars])
	Trim any of the chars in 'trimchars' from the ends of 'str'.
	The trim() function removes chars from both the front and back
	of the string, triml() removes only from the left side of the
	string, and trimr() removes only from the right side.  If the
	'trimchars' argument is not specified the whitespace chars
	(tab and space) are assumed.  

			 
       	Example:
	    cl> printf ("'%s'\n", trim ("   test   "))
	    'test'
	    cl> = trimr ("/iraf/iraf///////", "/")
	    /iraf/iraf

	    To check for strings containing only whitespace:

	        if (trim (foo) == "")
	            error (0, "no legal value specified for 'foo'")


        The new string functions are particularly useful for dealing with
pathnames where one needs to find and extension, separate a file from a
path prefix, trim trailing slashes. and so on.

	Additionally, the existing substr() function has been modified to
allow a 'last' index greater than a 'first' index, in which case the return
string is reversed.


Trig Functions
--------------

	The following trigonometric functions have been added as new builtins
to the CL.  These complement existing functions as well as provide utility
versions to simplify degree/radian conversion.

      asin (arg)	    Inverse SIN, result in radians
      acos (arg)	    Inverse COS, result in radians

      rad (rad_arg)	    Convert arg in radians to degrees
      deg (deg_arg)	    Convert arg in degrees to radians

      dsin (deg_arg)	    Sine function, arg in degrees
      dcos (deg_arg)	    Cosine function, arg in degrees
      dtan (deg_arg)	    Tangent function, arg in degrees
      dasin (arg)	    Inverse sine function, result in degrees
      dacos (arg)	    Inverse cosine function, result in degrees
      datan2 (y, x)	    Inverse tangent function, result in degrees


Utility Functions
-----------------

  The following utility functions have been added.

      fp_equal (arg1, arg2) Floating point compare (w/in machine precision)
      hypot (x, y)	    Euclidean distance (i.e. sqrt (x*x + y*y))
      sign (arg)	    Sign of argument (-1 or 1)


  Examples:
	cl> = fp_equal (1.2345, 1.234)
	0
	cl> = hypot (3, 4)	# may also take real arguments
	5
	cl> = sign (-23)	# may also take real arguments
	-1


Bitwise Operations
------------------

	The following bitwise operands have been added in the V2.12.2b.  
Note that these are bitwise operands and not logical operands.  While there 
is presently no direct need for these they are seen as potentially useful
in e.g. evaluating bit-flags stored in image header keywords and support the
goal of providing a richer scripting language.

      not (arg1)	    Bitwise boolean NOT of an integer  
      and (arg1, arg2)	    Bitwise boolean AND of two integers
      or (arg1, arg2)	    Bitwise boolean OR of two integers
      xor (arg1, arg2)	    Bitwise exclusive OR of two integers

  Examples:
      cl> = radix (12, 2)	# print bit pattern of number 12
      1100
      cl> = radix (13, 2)	# print bit pattern of number 13
      1101

      cl> = and (12, 13)	# 1100 & 1101 == 1100
      12	
      cl> = or (12, 13)	# 1100 | 1101 == 1101
      13	
      cl> = xor (12, 13)	# (1100 & ~1101) | (~1100 & 1101) == 1
      1	

      cl> = not (12)
      -13
      cl> = radix (not(12), 2)
      11111111111111111111111111110011



=================
Defined Constants
=================

	The ECL also introduces the ability to use common numerical and
physical constants in scripts as part of the language keyword set.  Constants
are, by convention, always upper case identifiers and are listed in the table
below:

  Numerical constants				
  +---------------------------------------------------------------------+
  |  Name		| Value			 | Units		|
  +---------------------------------------------------------------------+
  |  BASE_E          	| 2.7182818284590452353	 |			|
  |  FOURPI          	| 12.566370614359172953	 |			|
  |  GAMMA           	| .57721566490153286061	 |			|
  |  HALFPI          	| 1.5707963267948966192	 |			|
  |  LN_10           	| 2.3025850929940456840	 |			|
  |  LN_2            	| .69314718055994530942	 |			|
  |  LN_PI           	| 1.1447298858494001741	 |			|
  |  LOG_E           	| .43429448190325182765	 |			|
  |  PI              	| 3.1415926535897932385	 |			|
  |  RADIAN          	| 57.295779513082320877	 |			|
  |  SQRTOF2         	| 1.4142135623730950488	 |			|
  |  SQRTOFPI        	| 1.7724538509055160273	 |			|
  |  TWOPI           	| 6.2831853071795864769	 |			|
  +---------------------------------------------------------------------+

  Physical constants				
  +---------------------------------------------------------------------+
  |  Name		| Value			 | Units		|
  +---------------------------------------------------------------------+
  |  AU              	| 1.49597870691e11       |  m            	|
  |  GRAV_ACCEL      	| 9.80665e0              |  m / sec^2    	|
  |  GRAV_CONST      	| 6.673e-11              |  m^3 / kg s^2 	|
  |  LIGHT_YEAR      	| 9.46053620707e15       |  m            	|
  |  PARSEC          	| 3.08567758135e16       |  m            	|
  |  SPEED_OF_LIGHT  	| 299792458.0            |  m / sec      	|
  |  SOLAR_MASS      	| 1.98892e30             |  kg           	|
  +---------------------------------------------------------------------+

  For example, these may be used in scripts as:

	area = (PI * radius ** 2) 	# Compute area of circle.
	rad = degrees / RADIAN		# Convert degrees to radians



===============================================================================
# Post-Release Notes
===============================================================================

