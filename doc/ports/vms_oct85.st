Space Telescope Science Institute	IRAF Notes  (File: iraf$vms/notes)

 
--------------------------------
Wednesday  23-OCT-1985 14:20:08.71
 
Subject: I/O redirection support for IRAF/VMS
 
Added the ability to do I/O redirection on the command line (Unix-style).

	interactive	$ cl
	i/o to files	$ cl  <infile  >outfile     (or >>outfile)

This allows people to run the CL in a VMS batch job or as a "background" 
subprocess, specifying the CL commands in a text file.


sys$os/zmain.c
	Added code to support I/O redirection  (parsing, file setup, etc.)

sys$os/zfiotx.c, zfiofi.c
	Fixed ZSEKTX to work correctly (had troubles with redirected i/o for
	some reason).

	Changed append mode access so it has RMS Update access (for seeking).

pkg$cl/exec.c
	Added a global variable 'logout_on_eof' (default=0), which is set
	by the VMS ZMAIN when I/O is redirected to files.  If the user forgets
	to put the 'logout' command in the infile, then EOF will get him out.
	Otherwise, it's very easy to fill up a disk with "ERROR: use 'logout'.."
	messages in a file, something we definitely want to avoid.

	This is only a TEMPORARY fix.  The same thing happens under Unix, so 
	maybe their should be some way of handling this in a general,
	system-independent way.  Will users use the CL with input from a file 
	instead of the terminal?  They are here (for now) and the capability 
	should probably be supported, since it is automatically supported 
	under Unix.

	This fix does not affect normal operation of the CL in interactive mode;
	the user still has to say 'logout' to get out of the CL.

	Note:  If a user has a parameter which has a range specified, and the
	value is out-of-range, while running the CL in this "OS-background"
	mode, it will probably just loop until death saying "parameter out of
	range", and fill up a disk with these error messages.  This is also
	a problem, but not sure how (or if) to fix it; the user should make
	sure that the parameters are indeed correct when running the CL this
	way to avoid this problem (?).
 
--------------------------------
Friday  25-OCT-1985 17:16:02.53
 
Subject: pkg$cl/eparam.c
 
Hacked EPARAM to fix a number of problems with multi-line prompts, arrays,
and other things.  There are still a few known "features", but they are so
obscure and such special cases that I'm not going to worry about them now
(there are easy ways to "reset" eparam to fix these offset problems).

Also, changes so that value field is not overwritten by prompt string.
 
--------------------------------
Wednesday  30-OCT-1985 09:40:22.45
 
Subject: Parameter cacheing  (pkg$cl/exec.c)
 
Parameter cacheing mechanism (pkg$cl/exec.c  mk_startupmsg, sys$etc/main.x,
clio$clcache.x, etc.) does not handle array parameters.  Eventually, it should,
but this will mean some non-trivial changes in all of these places.  There
are a few places in SDAS that do indeed have array parameters, so the course
of action for now is to not pass them in the startup message (as is done with
list-structured parameters).  This fix is a simple "if" statement in exec.c/
mk_startupmsg().
 
--------------------------------
Monday   4-NOV-1985 12:33:48.79
 
Subject: pkg$cl/grammar.y (ytab.c)  decl.c
 
Bug fixes for 'real' declarations with negative values for min and max.
 
--------------------------------
Monday   4-NOV-1985 14:34:10.38
 
Subject: pkg$cl/lex.sed lex.com (for lexyy.c)
 
Added editor functions to change (YYLMAX 200) --> (YYLMAX 2048), so lexical
analyzer can handle long prompts given in procedure scripts.
 
--------------------------------
Monday   4-NOV-1985 15:35:36.85
 
Subject: pkg$cl/linkcl.com [VMS]
 
Changed to generate the CLDATE file in MACRO rather than C (i.e. cldate.mar).
This is so IRAF/VMS sites don't need a C compiler to relink IRAF (one site had
this problem already).
 
--------------------------------
Tuesday   5-NOV-1985 16:22:14.70
 
Subject: pkg$cl/task.c
 
Additional check added before strncmp() call in ltasksrch() to check first
character first.  Some profiling on Unix showed this to cut down the calls to
strncmp() significantly. 
 
--------------------------------
Wednesday  20-NOV-1985 08:59:41.61
 
Subject: sys$os/  [VMS Kernel]
 
The VMS Kernel routines have been overhauled.  The large number of changes
consisted of: deleting obsolete code, consolidating common code segments,
some name changing, improving readability and consistency, and some tweaks
for efficiency.  Most of the changes were indeed cosmetic, in an effort to 
bring the code into conformance with the IRAF coding standards (though perhaps
not 100% there).  To list all the changes would be impossible, so here are
some of the major ones...

A number of files have been renamed, as well as support routine names, to be 
more descriptive (and to remove the "z" on many of the support routine names).

File name changes:
	zconvtim.c  -->  convtime.c
	zexit.c	    -->  exit.c
	zget*.c     -->  get*.c
	zprocname.c -->  procname.c
	ztrnlg.c    -->  tranlog.c
	zfiofi.c    -->  rms.c
	ast.c	    deleted (no longer needed)

Function name changes:
	_zopnfi, _zclsfi  -->  _rms_open, _rms_close
	_ztrnlg, _zcrelg  -->  _tranlog, _createlog
	_zrequestio       -->  _zqio
	_zsttfi		deleted
	others...

New files:
	dirname.c
	queue.c
	eprintf.c	(from Kitt Peak, VMS net stuff)

The file status functions (formerly in _zsttfi) have all been moved into each
of the ZFIO* drivers, with appropriate buffer size values in the include file
"vms.h".  Some of these values have been modified, where appropriate, or as a
result of recommendations from Kitt Peak.

No effort was made to completely redesign anything, though some files and
routines were restructured quite a bit.  The effect on executable sizes of 
these changes is negligible.

--------------------------------
Friday  22-NOV-1985 10:43:50.93
 
Subject: pkg$softools/boot/...
 
All of the VMS source code for the boot utilities (MKLIB, XC, support ...)
have been overhauled to look like the rest of IRAF (at least partially), just 
as the VMS Z routines.  Besides a few name changes because of Z-routine
name changes, there was only 1 functional modification:

	pkg$softools/boot/spp/xc.c	[VMS]
			     /dcl.c +
		Changed to use the function DCL instead of ZOSCMD.  DCL does
		everything ZOSCMD does without all the weird features to 
		support interactive commands.  The output is much more 
		consistent here as well, no longer generating ".;" and ".LOG;"
		files all over the place when running XC and MKLIB in batch
		or background.  (DCL is essentially an earlier, stripped-down
		version of what eventually became ZOSCMD).
 
--------------------------------
Monday  25-NOV-1985 15:41:56.73
 
Subject: Support for VMS batch jobs
 
The VMS kernel has been modified to support VMS batch jobs as well as
parallel subprocesses for IRAF background jobs.  Changes in the higher level
IRAF code is necessary to selectively use these two options.  Since the
calling sequence for ZOPDPR has been changed, the new one is currently 
commented out until these higher level changes are finished (at Kitt Peak).

[received CL and system lib changes, 9-DEC]

The changes include:

	ZOPDPR -- extra parameter 'batch_queue', null or "" for subprocesses,
		  a batch queue name for a VMS batch job.  A few new routines
		  in this module were added and everything was reorganized a
		  bit.

	ZMAIN  -- some extra checks were added to deal with the batch case.

	ZFIOTX -- In ZSEKTX, added a check for STDIN/STDOUT when a VMS batch
		  job.  This is necessary to avoid seek errors on NLA0: and
		  the batch log file.  (VMS really makes it hard to deal with 
		  I/O in a consistent manner.)

	queue.c - Contains VMS batch and print queue support functions.  Used
		  by ZOPDPR and ZFIOLP.
 
--------------------------------
Monday   2-DEC-1985 09:40:50.23
 
Subject: pkg$cl/builtin.c
 
Changed SZ_FNAME to SZ_PATHNAME in cledit() to handle longer VMS filenames
resulting from filename mapping.
 
--------------------------------
Monday   9-DEC-1985 08:27:39.00
 
Subject: pkg$cl/prcache.c
 
Interrupts were not being enabled if connected subprocess create failed.
Added call to intr_enable() in before returning NULL.
 
--------------------------------
Tuesday  10-DEC-1985 12:39:20.67
 
Subject: New version of IRAF/VMS from Kitt Peak.  Merge notes:
 
sys/os/*
	Remerged the VMS kernel.  No major problems, even though we both had 
	made a number of changes.  Will test it all out soon.

sys/os/net/decnet.c +
	   zfioks.c
	   Makelib
	   eprintf.c --> ../

	Added support for DECnet, though this is not fully tested yet.  This 
	is a compile time option (DECnet or TCP) in ZFIOKS and will not affect
	the TCP implementation.  The eprintf.c file was moved to the sys/os/ 
	directory, since some debugging code there uses it now as well.

sys/gio/gks/gtx.f
	Did NOT use KPNO version of file since it was calling f77upk with
	the wrong number of arguments.  There is no min-length arg to f77upk
	in either version of IRAF.

sys/osb/f77pak.f
	Changed DO loop to pop out when done, rather than going around until
	it reaches 9999.

sys/gio/ncarutil/sysint/uliber.f
	Following change need to compile on VMS.
		integer*2(80) sppmsg  -->  integer*2  sppmsg(80)

pkg/images/imarith/imadiv.x
		   imamax.x
		   imamin.x
		   (>>> imarith.x)
	POINTER --> PIXEL

pkg/imred/generic/Makelib, x_generic.x, generic.cl
	Removed references to 'cmdstr' in these files.  Assumed 'cmdstr' was
	moved to the system pkg, since no cmdstr.* files on the tape. (?)

sys/fio/vfnmap.x
	Reference to ztslee changed to zwmsec.

pkg/system/x_systest.x
	Removed reference to mtdevlist.

pkg/images/tv/display/t_mkdisplay.x
	Moved call to open_display_header to AFTER getting name of stdimage
	and frame number.  Was before it, resulting in dev$_0.imh, instead
	of dev$deanza_1.imh.

pkg/language/doc/*.hlp
	Some of these files were updated recently to correct errors, typos,
	misspellings, etc.  Forgot to note these changes before.

Testing re-merged system
	Tested out a number of the usual things: CL, test scripts, SYSTEM and
	LISTS pkgs, some graphics and image tasks, etc.  Everything seems to
	be working okay.  There are still a number of known bugs, especially
	in eparam and procedure scripts (history gets a 'begin' in the history
	for procedure scripts).  I'll be looking into these again soon.

VMS Batch Jobs
	Tested out the CL with both types of background jobs, subprocesses and
	batch.  Both work fine.  A logfile for the batch job case is kept
	in the user's sys/login directory with a name like "12345678_D_1.LOG",
	(i.e., like detached subprocess names).  This logfile is largely
	login garbage, but if the user does not redirect output, it will go
	here as well.  When things settle down and everything is working okay,
	we may want to have the logfile deleted, or add an option for keeping
	it or not.

	Logical names for batch queues work as expected.  For example, the 
	following can be done:

		$ define  b  sys$batch
		$ define  f  localfastbatch
		$ define  s  localslowbatch
			...

		cl> task >&task.out &f	   # IRAF bkg job to 'localfastbatch'

	This means that zopdpr.c can remain system-independent, with system
	dependent batch queues set up in logical names or spelled out fully
	when in the CL.

Magtapes
	Tested the tape stuff with all the new changes from Kitt Peak merged
	in.  Used mtexamine and rfits and was able to read in about 20 small
	FITS images and run some tasks from the images and plot pkgs on them 
	with no problems.  


======>  Sent tape of changes above to KPNO  (16-Dec-1985)  <=====

 
--------------------------------
Tuesday  17-DEC-1985 16:07:42.04
 
Subject: sys/os/zpanic.c
 
Bugs in formatting the error message fixed.
 
--------------------------------
Tuesday  17-DEC-1985 16:29:52.99
 
Subject: sys/os/zmain.c
 
Changed code in _get_prtype() to figure out whether we're a batch job
by examining the PCB status bits, instead of the process name.  If the user
sets his process name in the login.com file, then the background CL starts
up thinking it's a regular host process.


===> Sent above fixes to KPNO  17-Dec <===

 
--------------------------------
Wednesday  18-DEC-1985 12:36:34.43
 
Subject: dev/*.ed
 
Added NEXT_PAGE and PREV_PAGE sequences to edcap files for EDT, EMACS and VI.

edt --
	NEXT_PAGE        \033Ow		 keypad-7
	PREV_PAGE	 \033OP\033Ow	 gold-7

	(In edt, prev_page is really keypad-5,keypad-7, but this would
	 set direction backwards; gold-7 in edt is "command", which is not
	 used in eparam anyway.)

emacs --
	NEXT_PAGE	^V		^V
	PREV_PAGE	\eV		ESC-V
	PREV_PAGE	\ev		ESC-v

vi --
	NEXT_PAGE	^F		^F
	(Nothing added for PREV_PAGE; not sure what to use, since ^B and ^U 
	 are already being used.)
 
--------------------------------
Wednesday  18-DEC-1985 16:43:41.78
 
Subject: pkg/cl/builtin.c
 
Fixed a bug in clflprcache() so that everything is not flushed if the named
task is not found in the process cache.  Previously, if a user said 

	cl> flprcache notask
	Warning: 'notask' not in cache

Then everything would go anyway, including locked tasks.  The fix is simply
to do a 'continue' in the while loop when we print out this warning message.
 
--------------------------------
Wednesday  18-DEC-1985 18:44:37.98
 
Subject: sys/fmtio/lexdata.xi
 
Changed action table for  +/-  in state QRX from Rvt to QRX.  This is to fix
a bug when getting numbers like 1.0e-10 which returned nchars=3 instead of
nchars=7.  This code is (eventually) used to check the type of command line
arguments in the CL when lexmodes=yes, which is where the problem was first
noticed. 

The code in lexnum.x along with the action table are somewhat difficult to
follow and there may be other implications here which are not apparent, but
based on other entries in the table, this looked like it was indeed a bug.
Ran the debug code in sys/debug/nop.x (lex task) and things worked correctly
in all cases tested.
 
--------------------------------
Friday  20-DEC-1985 11:42:48.72
 
Subject: VMS batch jobs and DCL commands
 
When running a DCL command as an IRAF/VMS batch job (e.g. a foreign task), if
the output just goes into the logfile, it ends up being written into files with
names like .;1 and .;2, etc.  This goes back to all the old crazies with the
funny ZOSCMD to defeat slow spawning and still provide interactive
capabilities.  The user should redirect output in this case somewhere else and
not let it go to the VMS logfile.  Tried a few changes in zoscmd.c and open.c
to no avail, so I'm just going to let this slide for now. 
 
--------------------------------
Monday  23-DEC-1985 08:50:37.80
 
Subject: pkg/system/help/Help.hlp
 
Formatting change to fix indentation:

		.ls
		helpdb = "helpdb"
to
		.ls helpdb = "helpdb"
 
--------------------------------
Monday  23-DEC-1985 10:35:15.82
 
Subject: Help task (discussion)
 
Some SDAS users have voiced a "concern" over the help task.  When saying "help
abc", everything that matches "abc" is given, even if something has already
matched it in the current package.  This is confusing to novice users, even
though it is documented in the help that it will perform this way.  A more
concrete example: 

	cl> system
	sy> help co
		... concatenate
		... copy
		... count
		... imred.coude
		... language.command
		...
	
Would it be possible (and/or desireable) to stop the searching if help is found
somewhere in the current package for a named task or abbreviation?  

Another problem is getting help on something in a different package than what
was intended because of a common abbreviation (and no help file for the task in
the current package).  The fix for this might be to limit the help search to
the current loaded packages, unless an explicit pkg-template.module-template
were given by the user. 

To go one step further, a suggestion by some SDAS people was to have an extra
parameter that controlled the search.  For example, if matchall=yes, then help
would act as it does currently, otherwise, it would only search the current
package. 
 
--------------------------------
Monday  23-DEC-1985 17:32:26.42
 
Subject: CL Changes  Nov-Dec 1985
	 (T.McGlynn, J.Travisano)

grammar.y
	- delete opnl's in procedure declarations to eliminate s/r conflicts.
	- create LP and RP for parentheses to eliminate some expressions.
	- repositioned initialization processing.
	- modified var_decls slightly.
	- if/else fixes, so don't need { if ... }.
	- better syntax error reporting in scripts: gives (correct) line number,
	  points to offending position, and continues parsing script for errors.

history.c  gram.c
	- changes to support above grammar changes

history.c  exec.c  pfiles.c  decl.c
	- changes to deal with script line numbers correctly, i.e.
	  task->t_scriptln, which is used in syntax error messages; also,
	  a fix in the skip_to() function in decl.c.

history.c
	- problem of getting a "begin" in the history when running (or parsing)
	  a procedure script was fixed.  The original command is now recorded
	  in the history (and logfile) correctly.  Unfortunately, there is
	  one case where this does not work -- when get an error parsing the
	  parameter declarations in a procedure script.

eparam.c
	- format changes for real arrays (so exponent shows up)
	- tried to fix MOVE_END problem when going across page boundaries;
	  can't seem to find the bug, so for now just print out a message that 
	  says to use NEXT_PAGE a few times instead of MOVE_END.

pfiles.c  param.c  gram.c
	- added support for '\r' and '\f', so can be used in prompt strings 
	  for better formatting control  (\f not fully supported in EPAR yet)

modes.c  decl.c  pfiles.c
	- whitespace-only filename parameters are turned into null strings;
	  so users can check null filenames easily in a script (fn != "")
	  without having to deal with whitespace.  Filenames with whitespace
	  only are not really legal anyway (?).

modes.c 
	- bug fix to prevent trashing of enumerated parameters in certain
	  instances.  

param.h
	- minor typo   'ai' --> 'ar'
 
--------------------------------
Friday  27-DEC-1985 14:47:48.01
 
Subject: CL history editor
 
The following is another lexmodes=yes problem:

	pl> contour img1[*:16,*:16]
		...
	pl> surface ^^
	surface img1[*:16
	Warning: ...

The get-arguments code in pkg/cl/history.c checks for comma-delimited strings.
Putting the whole argument in quotes, i.e. "img1[*:16,*:16]", and then using
^^ works fine.  (No fix/change was made.)
 
--------------------------------
Friday   3-JAN-1986 14:59:06.87
 
Subject: Local vars in CL procedure scripts
 
It is generally assumed that local variables in CL procedure scripts, i.e.
those after the "begin" statement, will be initialized by the user with 
simple assignment statements.  There is one (possibly more) case where this
assumption can cause problems.  For example,

			...
		begin
			string  buf

			if (fscan (plist, buf) != EOF) ...
			...

Here, "buf" is used before being initialized, and the result is an error
saying "Attempt to use uninitialized local variable 'buf'".  Perhaps local,
uninitialized variables should be initialized automatically by the CL, but
have done nothing to fix/change this yet; just making a note of it.
 
--------------------------------
Tuesday   7-JAN-1986 15:37:59.59
 
Subject: Miscellaneous
 
These are various notes and suggestions by IRAF users at STScI and elsewhere.
I decided to finally add them all here since they were beginning to pile up
on various scraps of paper on my desk.  Some of them are minor bugs, but most
are suggestions of some kind.  Nothing has been done with any of them in terms
of IRAF changes.  Most of the text is from mail messages or typed in verbatim 
from paper copies.  Any notes by me (JayT) are indicated in brackets [].


From ST users:

CL
   Would it be possible to put a feature into iraf whereby you say
'task=xxx', which would then set a default so that you could just 
type lpar, or an input, without having to specify which task?  
Ie, instead of having to type 'xxx.infile=etc', you could specify
the task and then just type 'infile=etc', with iraf filling in
the task name.  This would not interfere with those who like the
system as it is, but could streamline things for those of us who
like the way AIPS does things.

Integer parameters
  We have encountered another peculiar feature of IRAF.  Suppose you have a
parameter that is declared as integer in a .par file, and the user attempts
to put in a value for that integer that exceeds the range of INT*4.  The cl
fills in that parameter with the value 'indef', a character string.  When
SDAS goes to read the parameter, we get a crash because the parameter is not
in integer form.  Now, it seems that a parameter declared as an integer ought
to be an integer no matter what, not a character string.  Wouldn't it be better
to leave the parameter value unchanged from its current or default value rather
than put in this string?


From RAL users:

CL Parser
	li> ?? | words | match ':' stop+| sort | table

	does NOT work correctly, but separating the "+|" to "+ |" works okay.

Help on parameter prompt
	It is very complicated to provide additional help at the prompt for
	parameter stage especially for non-string parameters.  This facility
	is almost essential to provide a user-friendly interface. 
	For example:
		A string specified in the parameter file is output if ? is
		typed in response to a prompt or possibly even enter the help
		system.

	[NOTE: One can make the prompt quite verbose, up to something
	like 2K characters; e.g., many SDAS prompts are long and multiline.]

Range check and default display in sexagesimal
	There needs to be an option in the parameter file to cause the range
	check and default information to be output in sexagesimal.  For example,
	it is ugly to type in 12:34:56.7 and have it reappear as 12.582417.

Date type
	I think there ought to be special facilities for handling dates.  At
	present you can't enter a date all in one line (except using a
	grotty fudge involving sexagesimal, which precludes proper range
	checking), and if you enter the Y,M,D separately you can get things
	like 1985 2 30 past the range checks.


From U. of Cal/San Diego (Doug Tudhope):

lists.gcursor [and imcursor]
	Get syntax error line 2.  [known "feature" since CL2 grammar]

	[NOTE: doing an "lpar gcursor" activates the graphics cursor, 
	but then get an error of "EOF encountered in list parameter..."]

plot.graph task
	If only 2 points are given, only the axes are drawn, e.g.

		pl> graph STDIN
		10 10
		20 20
		<EOF>
			axes, but no line!
		pl>

images.imtranspose
	Made a transposed output file of a real 190*800 image -> 800*190
	but neither onedspec.splot nor images.implot would work on it.
	they stopped with "reserved operand fault".
	When imcopy run on transposed image, got "pixel storage file truncated".
 
--------------------------------
Tuesday   7-JAN-1986 15:51:02.98
 
Subject: pkg/softools/boot/spp/xc.c  [VMS]
 
Added EXTEND_SOURCE option for calling Fortran compiler from XC.  This lets
source statements go up to column 132 (instead of 72).  One line added to
source:

fcompile() {
	...
	sp = strcpy (outbuf, F77);
+	sp = strcpy (sp, "/EXTEND");	/* Allow statement in cols 1-132 */
	if (portable)
	...
}
 
--------------------------------
Tuesday   7-JAN-1986 16:11:20.85
 
Subject: pkg/images/tv/display/deanza/*
 
Deanza now works with tv/display under VMS!  All of the source files and Deanza
libraries are here.  See the README file for more detailed information.

Also changed "pkg/image/tv/display/mkpkg.com" to make the Deanza code instead
of the IIS-dependent code. 
 
--------------------------------
Wednesday   8-JAN-1986 12:21:09.73
 
Subject: pkg/imred/.../*.e
 
The ONEDSPEC executables are copied all over the place into IMRED.  Comments in
the mkpkg.com files say this is to use different par files for different
directories.  There's GOT to be a better way, as this eats up a lot of
diskspace (and tape as well at distribution time), which we always seem to be
running up against here.
 
--------------------------------
Friday  10-JAN-1986 15:29:37.13
 
Subject: More SDAS concerns/suggestions
 
Environment variables
	When a script task ends or a user bye's from a package, all "set"
	declarations made in the task or since the package started are
	"popped" during task restoration.  Comments in pkg/cl/exec.c indicate
	that this is for keeping the environment the same across processes.

	However, this can lead to confusion.  If a user does a "set stdgraph="
	and later bye's out of a package, stdgraph reverts back to the 
	previous value, just as any other environment variable.  The same
	thing occurs in script tasks during the restore, making it difficult
	to have global environment variables.  Putting a "keep" in scripts at
	various places can get around this to some degree, but it's sometimes
	awkward.

Process cache
	SDAS has been set up under IRAF such that the user can choose which
	version of SDAS he wants (baseline|standard|develop).  This choice
	can be made when starting the sdas package or any of its packages.
	There is a problem, however, when running a package from 'standard'
	and then running the 'develop' version when the executable is already
	out there.  When a task is invoked, it will simply use the subprocess
	already in the cache, even though it's a different executable (with
	the same IRAF logical name).

	The suggestion that came up was to do an implicit "flprcache" whenever
	there is a "bye" to a package, i.e. when all the tasks associated with
	the executable are gone.  I've looked at this a little bit and don't
	see an easy way of determining when we're bye'ing from a package which
	has tasks that are in an executable.  I imagine there's a decent amount
	of overhead associated with checking all the tasks associated with
	an executable as well.  In general, getting rid of old executables
	does make sense if they're not longer referenced, since it frees up 
	system and user resources (especially important on VMS given the
	large process and executable sizes).

Table parameters
	SDAS and CDBS now have a new type of input file, a table, which may or 
	may not have a header as well.  They have asked if it would be easy
	to add a new parameter (or two) to IRAF, i.e. a table parameter and
	possibly a table-with-header parameter.

	I don't think this is a good idea, as it is something which only
	benefits SDAS and CDBS, and does very little if anything for IRAF.
	I have encouraged the SDAS people to find an SDAS-only solution to
	this problem using existing IRAF facilities.  One simple way is to
	have tasks which use tables to have a 'type' parameter which tells
	if the input file is a table or something else.
 
--------------------------------
Tuesday  14-JAN-1986 08:48:36.11
 
Subject: doc/clintro*
 
Copied the TeX source for the CL User's Intro doc from Peter's directory
to the general IRAF DOC directory.
 
--------------------------------
Tuesday  14-JAN-1986 10:49:06.01
 
Subject: Size of IRAF...
 
A concern has risen, once again, on the size of IRAF.  Garrett Jernigan
(Berkeley Space Sciences Lab) has mentioned problems finding diskspace to
handle all of IRAF.  The suggestion he made to Peter was "to split the source,
obj and executables into separate libraries for the distribution, or at least
put them into different sub-directories.  If either a tape could be generated
that only had executables, or that had the whole works, but allowed only the
required files for execution to be loaded it would be a real boon."  I guess
this means having something analagous to the bin/, src/ scheme in Unix.

I think a better solution to this is to add something to the installation docs
to describe how to load only parts of IRAF, i.e. everything, or no source,
etc., so the IRAF installer can load only the parts needed/desired.  Both Tar
and Backup can handle selective copying in some manner. 
 
--------------------------------
Tuesday  14-JAN-1986 11:37:46.40
 
Subject: CL strings
 
It is currently impossible to get the length of a string in the CL.  This
should be quite trivial to implement, but it is not.  The documentation on
"paramaters" states that p_length contains the maximum length of the string,
though in reality, saying =param.p_length simply prints the string, just as
=param does. 

There should be some way of getting the current length of a string, since it
could be used as the 'last' parameter in stridx, for getting the rest of a
string starting at column n.  ST users have continually asked me about ways to
get the current length of a string parameter (e.g., from within a CL script).

One solution is to change the meaning of some of the parameter attributes for
string parameters to something which makes more sense: 

	p_length  --  contains the current string length, updated whenever
		      the parameter is changed.
	p_max     --  contains the maximum string length

These would require changes primarily in param.c and gram.c.

Also, currently trying an =param.p_min or p_max on a string parameter results 
in an access violation.
