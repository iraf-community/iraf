Space Telescope Science Institute		iraf$local/notes.st 

[ Many of the below notes and bugs have long been fixed, however the CL   ]
[ changes have yet to be fully integrated into both ST and NOAO versions, ]
[ so we'll keep them around for a little longer.  Some notes have been    ]
[ added below (in [[ ]]) where necessary/appropriate.   JJT 8-Mar-1986    ]

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
 
Subject: CL history editor		[[ known feature ]]
 
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

lists.gcursor [and imcursor]		[[ fixed in feb release ]]
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
 
Subject: pkg/softools/boot/spp/xc.c  [VMS]	[[ now in vms/boot/spp/ ]]
 
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
 
Subject: pkg/images/tv/display/deanza/*		[[ updated in march ]]
 
Deanza now works with tv/display under VMS!  All of the source files and Deanza
libraries are here.  See the README file for more detailed information.

Also changed "pkg/image/tv/display/mkpkg.com" to make the Deanza code instead
of the IIS-dependent code. 
 
--------------------------------
Wednesday   8-JAN-1986 12:21:09.73
 
Subject: pkg/imred/.../*.e		[[ fixed with new iraf$bin/ dir ]]
 
The ONEDSPEC executables are copied all over the place into IMRED.  Comments in
the mkpkg.com files say this is to use different par files for different
directories.  There's GOT to be a better way, as this eats up a lot of
diskspace (and tape as well at distribution time), which we always seem to be
running up against here.
 
--------------------------------
Friday  10-JAN-1986 15:29:37.13
 
Subject: More SDAS concerns/suggestions
 
Environment variables	[[ discussed, to be resolved ]]
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
 
Subject: doc/clintro*	[[ missing again, noao has most recent version ]]
 
Copied the TeX source for the CL User's Intro doc from Peter's directory
to the general IRAF DOC directory.
 
--------------------------------
Tuesday  14-JAN-1986 10:49:06.01
 
Subject: Size of IRAF...	[[ resolved w/ Feb release ]]
 
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


===> Released new Public version of IRAF at STScI,  Mon Jan 13, 1986 <===

===> Sent Notes, CL changes, SDAS-related items to KPNO,  Thu Jan 16, 1986 <===

 
--------------------------------
Thursday  16-JAN-1986 13:58:15.69
 
Subject: pkg/cl/config.h
 
Changed DICTSIZE from 20000 to 25000 and STACKSIZ from 4000 to 8000, to deal
with large scripts and tasks with zillions of parameters now used at STScI
which have been causing some overflows.  The VMS CL's executable size is no
larger, since uninitialized arrays are allocated at run-time. 

Also changed NFGPROC from 4 to 3.  Originally changed in an effort to improve
performance, it's causing more problems with page-file quotas because of the
very large SDAS executables, so back it goes.
 
--------------------------------
Friday  17-JAN-1986 09:25:22.96
 
Subject: Loading packages from within procedure scripts	  [[ resolved below ]]
 
It is a known "feature" in the CL that packages cannot be loaded from within a
procedure script or from within braces.  An old-style script, like the current
IRAF package scripts, can contain statements which load packages (e.g. onedspec
loads the list and plot packages). 

Loading packages from within procedure scripts should be supported at some
point (if possible).  SDAS uses procedure scripts almost exclusively now, even
their package scripts are procedures. 

Checking into it a little bit, it appears that putting a `keep' statement in
the package script fixes this problem.  But this is not a good solution for
general interactive use.  A possible solution is to add an implicit `keep' when
a cl() call is made from a script called by a script (as in most package
scripts).  This may be the easiest solution and will require no changes to
existing package scripts. 
 
--------------------------------
Monday  20-JAN-1986 11:17:09.92
 
Subject: pkg/language/doc		[[ used Feb release instead ]]
 
A few minor changes to the help text; typos, corrections, added examples, etc.
 
--------------------------------
Monday  20-JAN-1986 16:42:18.87
 
Subject: pkg/cl/Makelib			[[ use mkpkg instead ]]
 
Corrected Makelib file to contain correct include files for CL source.
 
--------------------------------
Tuesday  21-JAN-1986 12:25:29.74
 
Subject: pkg/system/gripes.cl		[[ now in vms/hlib/ ]]
 
Changed to mail entire gripe text to IRAF development account, as in KPNO
version, and not just a message that a gripe has been made.  Also changed
to use `userid' for the From field instead of `home'.
 
--------------------------------
Tuesday  21-JAN-1986 15:37:48.27
 
Subject: pkg/cl/grammar.y
 
Added initialization code for the label list structure used for goto statements
in CL scripts.  Simply added "label1 = NULL;" statements to the three places in
the grammar where things are initialized.  The absence of this would eventually
cause access violations and other fatal side effects when goto's were used
again and again. 
 
--------------------------------
Wednesday  22-JAN-1986 09:12:46.73
 
Subject: pkg/cl/builtin.c		[[ fixed at noao too ]]
 
Two instances of
		flags != LT_PFILE;
changed to
		flags &= ~LT_PFILE;
 
--------------------------------
Friday  24-JAN-1986 08:11:29.20
 
Subject: pkg/cl/pfiles.c
 
Added line to reset parse state in certain circumstances within a script.
 
--------------------------------
Tuesday  28-JAN-1986 09:16:18.48
 
Subject: sys/fmtio/lexnum.x		[[ fixed at noao too ]]
 
A previous fix to lexdata.xi caused a bug in lexnum.x to show up, although the
relation between the two is unknown.  The stk_ip[] array in lexnum() was
declared as short; this would sometimes overflow since it was storing pointers
which could be quite large.  This showed up when a user tried to add a field in
an image header with 'hedit', resulting in an arithmetic trap.  The fix is to 
declare stk_ip[] as int instead of short. 
 
--------------------------------
Tuesday  28-JAN-1986 11:06:27.14
 
Subject: GIO and graphcap		[[ fixed in Feb release ]]
 
Setting stdgraph to tek4014 results in an "openws: illegal graphics window"
error message when running the graph task.  Using showcap to see what is
really in the graphcap entry shows that although it points back to tek4012,
all of the upper case values (X1,X2,Y1,Y2,etc) do not get set properly.
 
--------------------------------
Tuesday  28-JAN-1986 11:44:54.72
 
Subject: pkg/cl/exec.c			[[ #ifdef st stuff removed 3/11 ]]
 
Quick patch to allow packages to be loaded from within any type of script. This
fix has been #ifdef ST'd for now (ST is #define'd in config.h). Basically, when
a "cl" or "clbye" statement is encountered within a script, the package is kept
for the duration of the script task that called it. 
 
--------------------------------
Tuesday  28-JAN-1986 16:29:25.83
 
Subject: pkg/twodspec/longslit/apdefine.x	[[ fixed at noao too ]]
 
Was getting a lot of "adjustable array dimension errors" when running apdefine.
The following changes fixed them: 

	real  apdata[3, naps]	 -->  real  apdata[3, NAPS]
	real  imdata[npts]	 -->  real  imdata[ARB]

In the original code, if 'naps' or 'npts' were invalid (e.g., zero or negative)
when the arrays were declared, this error occurred. 
 
--------------------------------
Monday   3-FEB-1986 10:25:45.69
 
Subject: iraf$vms/loc.c		[[ ST tool, moved to local/ ]]
 
Threw together a quick and dirty Lines-of-Code counter to handle C, SPP,
Fortran, MACRO and Ratfor source files for IRAF.  It only counts non-blank and
non-comment lines, considering them "executable".  (NASA folks like this
software metric to see how much we're supporting.) 
 
--------------------------------
Monday   3-FEB-1986 13:00:23.77
 
Subject: pkg/twodspec/longslit/apextract.x	[[ fixed in Feb release ]]
 
Changed
	int	fopen(), ...
to
	int	open(), ...

Would result in a "bad file descriptor" error message in IRAF/VMS since open()
function was assumed to be real instead of int.
 
--------------------------------
Tuesday   4-FEB-1986 13:40:44.07
 
Subject: sys/os/zopcpr.c  [VMS Kernel]
 
Added the following initialization code before the call to ZOPNPR to prevent
error recovery problems which occurred occasionally. 

	*inchan = *outchan = XERR;
 
--------------------------------
Tuesday   4-FEB-1986 15:43:51.21
 
Subject: sys/tty/ttyputl.x	[[ see below... ]]
 
The parameter 'map_cc' in this routine is declared as 'int', but everything
that calls it (e.g. lprint, type, page, help), passes in a 'bool'.  This is
probably a bug and most certainly a possible portability problem.  I did not
change anything, but 'map_cc' should probably be treated as a 'bool' in this
routine.
 
--------------------------------
Wednesday   5-FEB-1986 13:47:01.28
 
Subject: pkg/cl/exec.c
 
In mk_startupmsg(), added redirection support for STDERR, so the iraf main will
know about it as well as STDIN and STDOUT (for STDERR, the string sent down is
" 5> $").  A low-level SPP routine used by SDAS for error logging depends on
knowing the redirection status of STDERR. 
 
--------------------------------
Wednesday   5-FEB-1986 16:30:12.55
 
Subject: sys/os/zfiopr.c  [VMS]		[[ in Feb release ]]
 
Made changes in the memory allocation strategy.  Instead of using _zmaloc and
then mapping to it, let the $crmpsc and $mgblsc system services create and map
the global memory sections.  This is a simpler and more standard way of doing
things, according to the examples in the VMS documentation.  The system service
$deltva is used to delete the sections. 

These changes were prompted by spurious errors when creating connected
subprocesses, with the following VMS error from $crmpsc: 

	SECTBLFUL, section table (process/global) is full

There is a system paramater (GBLPAGFIL) that controls this and may be too low.
However, the initial implementation of zfiopr with _zmaloc may have been
causing some troubles as well.  Memory sections may not have been fully
unmapped/deleted because of problems with memory (de)allocation in the context
of global pagefile sections.  The solution mentioned attacks this concern,
and it is better than changing system parameters in the long run.

Inter-process communication still works fine with this new implementation of
zfiopr and I haven't been able to recreate the error above, so I'm hoping it's
fixed and behaves better as far as deallocation of global pagefile sections.

Warning: DEC recommends that programs which allocate memory not mix allocation
techniques (lib$get_vm vs. $cretva/$expreg), so there still may be some strange
problems.  The full warning is in the VMS System Services manual and some notes
on it are in sys/os/zvaloc.c. 
 
--------------------------------
Wednesday  12-FEB-1986 09:07:16.44
 
Subject: sys/os/zmain.c  [VMS]
 
For interactive HOST and non-batch DETACHED processes, the error channel is now
set to the output channel explicitly.  It used to be opened as a separate
channel to the VMS SYS$ERROR.  This makes no difference within IRAF, as FIO
(and the CL) keeps track of them as separate entities.  It does, however, give
us more control over the VMS SYS$ERROR stream, for example, to redirect
SYS$ERROR to a file to get SDAS tracebacks, etc. 

Also modified to translate SYS$ERROR for the logical assignments made when a
connected subprocess (rather than assigning in/out/tt/command to "SYS$ERROR").
 
--------------------------------
Wednesday  12-FEB-1986 09:22:34.24
 
Subject: pkg/cl/eparam.c
 
A number of changes have been made:

	- bug fixes for arrays and multiline prompts, and movement in 
	  multiple pages when these things are around.
	- bug fixes for old patches - the "real" bugs have been found 
 	  and fixed.
	- enhancement for hidden parameters; now, Eparam output of hidden
	  params (when showall set in epinit) is similar to Lparam, i.e.,
	  `(name = value) prompt' and for arrays,  `(name) prompt \n values'
	- updated and added comments where appropriate (and necessary)
	- made all debugging/diagnostic code (within eparam) print at the
	  bottom of the screen, to prevent screen rolling while in eparam.
	  cldebug messages elsewhere still cause screen craziness, however.
	- a bit of code reorganizing and cleaning up

Eparam continues to get harder to debug.  Some of the changes here will
hopefully make future debugging easier.  At some point, much of this code
should probably be redesigned/rewritten (as mentioned before).
 
--------------------------------
Wednesday  12-FEB-1986 13:01:46.18
 
Subject: pkg/language/language.hd		[[ use Feb release instead ]]
 
Added entry for 'stridx', pointing at "string.hlp"
 
--------------------------------
Thursday  13-FEB-1986 13:08:00.00
 
Subject: pkg/cl/gquery.c
 
This is used by EPARAM for checking ranges and enumerated values.  Fixed some
bugs in the range message generation.  Old messages would stay around in a
static array and show up on different parameters in Eparam; modified to null
out string when no range specified.  Also did a little bit of cleanup on the
source file.
 
--------------------------------
Thursday  13-FEB-1986 13:13:01.10
 
Subject: sys/os/zxwhen.c  [VMS]
 
Changed TRACEBACK code (conditionally compiled to show VMS tracebacks rather
than recovering) to force traceback to a file as well as the terminal.  This
also gets rid of the double traceback seen when subprocesses abort.  The output
file is called ".ERROR", and will be written in the user's current directory
(if writable).
 
--------------------------------
Thursday  13-FEB-1986 17:12:52.80
 
Subject: pkg/cl/decl.c
 
Changes to deal with parameters declared in the procedure statement that are
later defined as hidden.  We print out a warning message and override the mode,
from hidden to auto.  If the user really wants a parameter hidden, it should
not be in the procedure statement, as this can screw up positional arguments. 
 
--------------------------------
Thursday  13-FEB-1986 17:54:59.57
 
Subject: abbreviations in the CL
 
This "feature" continually comes up here at ST.  A lot of people are writing
scripts here and always having problems with abbreviations.  The comments
in the abbrev() function in pkg/cl/modes.c says that only interactive (or
batch) tasks can be abbreviated, meaning tasks called from within scripts
must always be fully spelled out.

I imagine the original reason for this was to insure "good" script programming
practices at the expense of convenience.  However, when writing front ends that
use tasknames as parameters and then call lparam, eparam, etc. (e.g.,
system.mkscript), it forces the user to type the taskname in full, which makes
it inconsistent with normal interactive usage.
 
--------------------------------
Friday  14-FEB-1986 08:27:57.42
 
Subject: pkg/clpackage/clpackage.men		[[ fixed ]]
 
typo:  'utilties' --> 'utilities' for System package
 
--------------------------------
Friday  14-FEB-1986 09:35:58.21
 
Subject: package rearrangement for ST	[[ to be further resolved ]]
 
Put most of the KPNO-specific and general IRAF packages into a new package
called NOAO.  This is to avoid name conflicts and confusion with the ST
software, especially SDAS and CDBS.  The files affected are: 

		lib/clpackage.cl
	+	lib/noao.cl
		pkg/clpackage/clpackage.hd
		pkg/clpackage/clpackage.men
	+	pkg/clpackage/noao.hd
	+	pkg/clpackage/noao.men

CLPACKAGE now consists of:  sdas noao language lists softools system.
 
--------------------------------
Friday  21-FEB-1986 14:34:26.46
 
Subject: lib/clpackage.cl login.cl		[[ now vms/hlib/ ]]
 
Changed startup file and template login.cl so filewait="no" is the default.
Too many "Waiting for access to file" messages, especially on the logfiles,
which are in heavy use around here for lots of things besides what the CL
puts in them.
 
--------------------------------
Tuesday  25-FEB-1986 13:48:30.21
 
Subject: pkg/cl/builtin.c
 
clhidetask():  Removed check for tasks without parameter files.  This should
not matter when hiding a task.  The inability to hide foreign tasks prompted
this change. 
 
--------------------------------
Monday   3-MAR-1986 13:25:02.14
 
Subject: pkg/cl/eparam.c
 
Minor change in eparam to clear the bottom line before returning to the CL.
This clears the "<key> for Help" message from the command line, to avoid
confusion. 
 
--------------------------------
Thursday   6-MAR-1986 09:24:31.20
 
Subject: system package, copy and rename
 
Copy does not allow wildcard files as output, but rename does.  Thus, copy
seems to be modelled after the Unix cp whereas rename acts like the VMS task of
the same name.  This seems inconsistent and is sometimes annoying to users
here. 

Also, if the copy command cannot read the input file for some reason, the
resulting error message says that it can't create the output file (because the
call to zfmkcp fails).  This is misleading, and is due to the logic in
fio$fcopy and fmkcopy, which check only for the existence of the input file and
not whether it can be read. 


----------------------------------------------------------
Merge Notes -- NOAO Feb release 
Starting March 6, 1986  Jay Travisano

Mostly ST-specific notes, but some notes on VMS/IRAF...
----------------------------------------------------------

Read backup tape to [iraf.new...].  Will merge IRAFX changes into that and then
	backup whole thing; NOAO has made a lot more changes than we have. 

doc/vmsnotes.* --> doc/ports/*
	save old vmsnotes files, may be useful

vms/hlib/irafuser.com
	site-dependent changes; also added MYDISK job logical to deal with
	dollar signs in disk names.  Will change mkiraf.com to use MYDISK
	if can't get rid of dollar sign in home disk.  Also, reinstate
	IMDIRDISK for the IRAFTMP directory...
***
	We don't have system logical names without '$' in them and even the 
	physical names have them too (VAX cluster).  Having another system
	logical name without the '$' is not the answer here, since we have
	over 30 cluster disks.  Forcing IRAF sites to change system logicals
	is an unreasonable method to avoid the '$' (as the installation doc
	recommends).  It may be easy at NOAO but our experience here and
	at other VMS-only sites shows otherwise; system managers around here
	are even reluctant sometimes to raise users' paging file quotas, as
	it requires more pagefile space on the system disks.

	One solution to this is to have a fully supported (consistent) escape
	mechanism.  The "disk\\$newton:[irafx]" works in most cases 
	and is used currently at ST.  The MYDISK job logical for the home
	directory in the login.cl also helps out.

	A better solution perhaps is to change vfnmap/vfntrans so that 
	during logical dir translation, a check is added to see if we have
	an OS path.  If so, don't bother rescanning the string for more
	logical directories.  This would allow

		set junk = "disk$newton:[iraf.local.jay]"
		type junk$file

	and after the first translation of "junk$", it would realize that
	we've got an OS path and continue on with the "file" portion.
***

vms/hlib/libc/iraf.h
	site changes for ST; not crazy about site disk and dir names in this
	file which must be moved to sys$library.  assuming vms logicals
	will be set correctly so won't need to read this file at run-time.

vms/hlib/gripes.cl
	added subject to mail message
	use USERID instead of HOME for From field

vms/hlib/install*.com
	changed for ST; only install CL and SYSTEM package here

vms/hlib/irafemacs.com
	newer version of emacs command file, plus a few minor changes for IRAF

vms/hlib/zzsetenv.def
vms/hlib/login.cl
vms/hlib/mkpkg.inc
	setups for ST

vms/hlib/mkiraf.com
	use MYDISK job logical instead of SYS$DISK; avoids '$' garbage.

vms/os/net/zfioks.c
	turned on DECnet instead of TCP; never worked all that well, but we'll
	try it again

vms/os/zmain.c
	merged in our changes and updated comments

vms/os/zopcpr.c
	merged in old change missing from noao distribution

vms/os/zxwhen.c
	merged in new traceback handling stuff
	*NOTE*: this is currently turned ON for ST, for debugging of access
	violations and arithmetic faults.  To turn it off, #define NOTRACEBACK.

vms/os/rms.c
	(debug) references to EP2 changed to _EP2

vms/boot/spp/xc.c
	added /EXTEND qualifier for Fortran; allow statements in cols 1-132

dev/devices
	magtape entries for ST cluster

pkg/cl/tests/*
	Note in Unix notes file says this was deleted because it wasn't used 
	anymore.  In fact, it has been used here at ST for (crude) regression 
	testing when making CL changes.  Have moved these into my (Jay) 
	private directory in case they're needed in the future.

vms/boot/spp/rpp/mkpkg.com
	DCL syntax errors in "if f$search()" line:
		() screwed up
		"then" missing
	(I booted the tools from scratch, to test everything out)

pkg/cl/*
	used ST version as a base; merged in NOAO changes where necessary

pkg/cl/mkpkg
	added lines to lex and yacc the grammar, if stsci and vms, since we
	muck around with the grammar too.
	also, typo in install comment (move to bin$, not lib$)

vms/hlib/notes.com rcscom.com  -->  local/
	these are ST-specific tools; notes file (notes.st) will also be here

doc/ports/ <-- old vms/ctio.hlp vmsprog.hlp  (from old distribution)

backup current ST develop/experimental version (IRAFX) to tape
backup new merged version to IRAFX
remake...
testing...
everything looks good...

Networking w/ DECnet seems to work fine; tried remote file access, remote
subprocesses (will be useful for IDM access).  Tried very simple remote tape
access and didn't work; seemed to abort quickly though as if it failed on
local host.  The noao notes file says it won't work on VMS because of the
process structure; not sure what this means exactly -- looks like it should
work with the allocation built into the kernel server as it is now.  Will
look into it more later...

dev/hosts
	set up ST nodes
***
	took out the single quotes on the vms executable name (from noao).
	can't access  "node!iraf$file"  with them in, i.e., the "iraf$"
	logical doesn't get set up correctly.  appears that the quotes are 
	left over from some previous syntax.  don't think noao has vms 
	servers (for tcp) yet (?).
***

vms/hlib/irafuser.com
	changed most job logicals to process ones, since they aren't needed
	at the job level, and job table space is limited around here.
	(Process logicals are automatically copied when spawning DCL commands 
	or background jobs.)  So, now (at least for ST),
		job logicals:  IRAFDISK (IMDIRDISK) IRAFTMP IRAFIRAF IRAFHOST 
				IRAFHLIB
	    process logicals:  IRAFLIB IRAFBIN IRAFLIBC IRAFVMS batch-logicals

vms/hlib/mkiraf.com
	bypass sections that delete login.cl and uparm files;
	let a new version of the login.cl be made in case user must go
	back.  for uparm files, the 'out of date' message will be enough
	to tell the user to do an unlearn.  besides, the sdas par files
	don't change that much (or do they?)  In any event, I'd get lots of
	complaints if everyone's par files disappear when running mkiraf.

vms/gdev/
	made library as is, to resolve link references w/ irafks, display and
	cv.  For deanza at ST, probably won't use this interface; easier to
	use old deanza code until new display code and image interfaces are
	in place.

pkg/language/doc/edit.hlp
	typo: edcapf --> edcap

pkg/system/doc/directory.hlp
	typo: \fb --> \fB

pkg/*
	many packages list a 'revisions' task in the help menu but don't
	have one in the package; I imagine this is a future enhancement.

pkg/images/tv/display/mkpkg
                     /deanza/*
	Added temporary deanza code (vms and stsci dependent).  Changed mkpkg
	in display/ to make the deanza/ subdir instead.  

dev/graphcap
	added entry for `deanza'; no more .imh files...Yeah!
	added entries for Selanar-enhanced vt100 (vt100s), and Pericom.

dev/termcap
	added/updated ST entries.

vms/hlib/clpackage.cl
	changed 'page' to 'type'.  if a long motd, user may inadvertantly
	type ahead and get all screwed up, so just type the message out.

pkg/cl/grammar.l
	modified to accept double precision real numbers on input, e.g.

		cl> task.param=12d20

	used to give a syntax error.  Simple change in lex input, namely:
	[eE] --> [eEdD] for real numbers.

vms/as/README
vms/as/cstr.mar  +
	Added assembler versions of LIBC string routines; added to list in 
	README file.  These are much faster than the LIBC string routines
	written in C, and although they may not make a significant increase 
	in performance of the CL, we have them, so why not use them.

sys/libc/mkpkg
	Modified to use as$cstr.s if available, otherwise use the portable 
	routines written in C.

vms/boot/mkpkg/mkpkg.help
	typo:  \fmkpkg --> \fBmkpkg

math/mkpkg
	added "$purge lib$" at end of 'mathgen'

-----------------------
CL Logging Enhancements
Feb/Mar 1986
-----------------------

The following enhancements to the CL logging facilities were made:

	1) a 'putlog' builtin was added, so a user's script or program 
	   can log important messages and errors
	2) added logging from the CL itself, when script and executable
	   tasks are started/stopped (i.e., a task trace)
	3) errors are logged, from the CL, when from a script or executable
	4) background logging supported (a result of above changes)

These functions are parameter controlled, by the CL parameter `logmode', e.g.,

	cl> logmode = "commands background error trace"
			(nocommands nobackground noerror notrace)

The old `keeplog' parameter has the same function, turning the whole thing 
on or off.

Note:	To avoid file access conflicts (e.g. bkg logging), the environment 
	variable 'filewait' must be set to "no".  (This is now the default 
	at ST.)  As a result, some log messages, especially those from the 
	background, may still not get to the (single) logfile.  The use of 
	multiple logfiles is encouraged because of this, but SDAS developers 
	and testers prefer a single logfile with everything in it.  Not much 
	we can do to change their minds.


Source changes in pkg/cl/:

builtin.c
	Added clputlog() function for the 'putlog' builtin.
	Added code so that the clerror() function calls putlog with 
	the error message, when logging is on.

history.c
	Fixed the code for shared logfile access.  Modified so login/logout
	messages are not printed for background jobs.
	Added putlog() function, which formats the log messages from the
	user and/or the CL and writes to logfile.
	Added reset_logfile() function to reopen logfile if name is changed.
	This is only for share_logfile=no and has not been tested here,
	but it's just a close and reopen.

exec.c
	Added code to log a message when starting/stopping a script or
	executable task.  This is the "trace" feature.

bkg.c
	Added code to log a message when a background job is started or 
	completes.

errs.c
	Added code to log the error messsage if a script or executable task
	(and it hasn't already been logged by clerror()).	

param.h
	Added P_CL value for p_flags field of the param structure.  This
	is used to test (quickly) whether a parameter is a CL parameter.

param.c
	Added check in paramset() for a CL parameter (p_flags & P_CL);
	if so, call parse_clmodes() (below).

modes.c
	Added parse_clmodes(), to parse logging control parameter and
	epinit/ehinit parameters and to call reset_logfile() if logfile
	name is changed.

clmodes.h
	Added extern declarations for logging/epinit/ehinit variables

eparam.c
history.c
cl.par
	"epinit" and "ehinit" changed from environment variables to CL params
	as per mail discussion with Doug.  Removed all setoption() stuff as 
	this is now taken care of in parse_clmodes() (in modes.c).

hlib/clpackage.cl
	Removed epinit and ehinit env vars.

hlib/login.cl
	Added commented out default values for logging and epinit/ehinit
	parameters to the template login.cl file.

pkg/language/doc/cl.hlp
		 ehistory.hlp
		 eparam.hlp
	Modified due to move of epinit/ehinit vars to CL parameters and
	addition of new logging parameter.  

pkg/language/language.hd 
	     language.men
pkg/language/doc/logging.hlp   +
		 putlog.hlp   +
	Added help for the "logging" discussion and the "putlog" builtin.  
	The "logging" man page talks about the CL logging as a whole, 
	discussing the parameters involved in a little more detail than the 
	help on the cl.

-------------------

pkg/cl/*
	Cleaned up some of the CL source code; mostly just removed some
	old, useless cldebug statements, etc.  Also cleaned up grammar.y
	a lot, at least to make the style consistent and more readable.
	Gram.c and decl.c also got a bit of this as well.

pkg/cl/README
	Updated text; still referred to Unix "make" and VMS DCL/Makelib
	generation techniques.
 
--------------------------------
Monday  24-MAR-1986 12:01:48.70
 
Subject: VMS/IRAF Plotting
 
dev/graphcap
	Added entries for QMS, EQMS, and NULL (testing) for STScI.

vms/os/ vms.h
	zfipol.c
	mcsub.c   +
	mkpkg

Implemented ZFIOPL for VMS to use NCAR Metacode translator (MCVAX).  A
spoolfile is written in the necessary VMS file format by ZFIOPL.  On close,
MCVAX is started up as a subprocess and commands are sent to it to set the
device (from graphcap DD string), read the spoolfile, and plot until finished. 

This code may depend on peculiarities of the ST version of MCVAX which I don't
know about. 
 
--------------------------------
Tuesday  25-MAR-1986 11:53:12.75
 
Subject: vms/boot/bootlib/vfn2osfn.c
 
Changed to return a null string if an error occurs mapping the file; it used to
return the last result of a filename mapping operation.  This would to lead to
problems in mkpkg, e.g., 

	$delete dir$sub/file.c

If this file did not exist, the previous filename returned by vfn2osfn() would
be deleted.
 
--------------------------------
Wednesday  26-MAR-1986 12:04:53.81
 
Subject: mkpkg

vms/boot/mkpkg/tok.c
	$ifdef/$ifndef was not working -- bug when setting boolean variable; 
	fixed.

	changed os_cmd() to host_xc() for do_omake(), do_link(), do_xc(), so
	the VMS DOCVCL routine can be used (to eliminate some spawning).
 
vms/boot/mkpkg/host.c
	docvcl() -- don't check return status and change to XYES/XNO; 
		leave as is (OK or ...)

	added host_xc() routine to call docvcl().

vms/boot/spp/xc.c
	bug fix -- `firstobj' variable was not being reinited to NULL; caused
	problems with above host_xc() changes.  Added reinit in do_cleanup().

vms/boot/mkpkg/mkpkg
	added ../spp/xc.c dependency for xcsub.c
 
--------------------------------
Friday  28-MAR-1986 09:37:16.83
 
Subject: ttyputline and map_cc
 
pkg/system/lprint.x    
	   page.x  
	   type.x
	./help/houtput.x

	Changed map_cc from bool to int to work correctly with ttyputline(),
	which expects an int and checks for YES/NO.  In VMS Fortran, .true. is 
	encoded as a -1, not a 1 (YES).  To change ttyputline() would mean
	changing the LIBC interface c_ttyputline() so that it knew about this
	feature of the host Fortran, so let's use the int YES/NO method.
 
--------------------------------
Friday  28-MAR-1986 10:12:49.38
 
Subject: graphcap entry for selanar hirez 100xl
 
Not sure if this is a graphcap entry problem or a limitation on the selanar
itself:  When in cursor mode in a task (e.g. onedspec.splot), typing 'c' to
print the cursor position will print "x,y: ... ..." in the lower left hand
corner, but then immediately erase it.  Help via '?' does the same thing, i.e.,
gets printed and erased immediately. 
 
--------------------------------
Wednesday   2-APR-1986 13:22:46.16
 
Subject: pkg/cl/edcap.c
 
Added the editor name to the eparam help page.
 
--------------------------------
Wednesday   2-APR-1986 15:24:20.96
 
Subject: vms/boot/bootlib/dcl.c
 
Added interrupt support.  When ^C typed, return status of dcl() (and os_cmd)
will be SYS_XINT, so MKPKG can deal with the error as on Unix. 
 
--------------------------------
Wednesday   2-APR-1986 16:51:37.03
 
Subject: vms/boot/spp/...
 
xc.c 		fixed inconsistencies in error/status checking, removed
		old code, rearranged some functions (main to front, etc).,
		and general cleanup.
xc.com		command file to make xc (only)
mkpkg.com 	call xc.com
 
--------------------------------
Thursday   3-APR-1986 08:06:49.57
 
Subject: editor
 
Just realized that "editor" is still an environment variable, used by
eparam/ehistory and the edit builtin.  Epinit/ehinit have been changed to CL
parameters during the logging changes, but I forgot about this one. 

Should we change "editor" to a CL parameter, too?  If so, it would require
changes in  pkg/cl/edcap.c,builtin.c,cl.par  and the help files for 
eparam,ehistory,edit,cl. 
