.RP
.ND
.TL
A User's Guide to the IRAF Command Language
.AU
D. Tody
G. Jacoby
.AI
.K2 "" "" "*"
March 1984
.AB
This document presents an overview of the IRAF Command language
and how to use it.  The discussion is aimed at the first time
user and concentrates on how to execute tasks from the Command
Language.  The focus is the Command Language itself, as opposed
to the many packages and tasks comprising the IRAF system,
and on using the command language to run existing programs,
rather than adding new ones of one's own making.
.AE
.bp
.SH
Scope of this Document
.PP
This document is an introduction to the basic features of the IRAF
Command Language (CL), and is not intended to be a complete guide to
using and programming the IRAF system.  We concern ourselves here
primarily with the use of the CL to execute commands entered interactively
at the terminal.  The first time user rarely needs to do more than enter
commands and run an occasional background job.  The IRAF system and
applications packages are described elsewhere; much of the available
documentation is accessible at the terminal via the online \fBhelp\fR facilties.
Programming in the CL or in the IRAF environment is beyond the scope of
this document and is described in the \fICL Programmer's Manual\fR,
the \fIReference Manual for the IRAF Subset Preprocessor Language\fR,
the \fIProgrammer's Crib Sheet\fR, and other documents.
.NH
Introduction
.NH 2
Function of the Command Language
.PP
The basic function of the command language is to execute external tasks and
manage their parameters.
The CL provides a consistent interface between the user and all applications
programs, giving the user complete control over the parameters, data, and
system resources (graphics devices, etc.) used by IRAF programs.
The CL makes it easy to run individual programs by prompting the user with
the names of the available programs and by prompting for parameters as
necessary when a program is run.
Equally important, the CL provides powerful facilities for connecting
programs together to solve special problems.
.PP
The CL is primarily a \fIcommand\fR language, and only secondarily an
interpreted \fIprogramming\fR language.
To be a good command language, the CL must make
it as easy as possible to enter commands to perform common functions.
To this end the CL provides menus, prompting, minimum match abbreviations,
a concise command syntax for simple commands, parameter defaults, and a
history mechanism for correcting errors and for repeating previous commands
with a minimum of typing.
To be a good programming language the CL must
be reasonably efficient, able to evaluate complex expressions,
compile and run general procedures, and much more.
The present CL is a good command language; as a programming language
it is usable but needs a lot of work.
.PP
.NH 2
Capabilities of the CL
.PP
Besides fulfilling the basic function of a command language,
the CL is capable of performing the mundane operations of
a desk calculator, evaluating expressions, and some limited
programming functions. These simple features provide a very
powerful means of connecting and interacting with tasks.
This will become more apparent in the later discussion of usage.
.PP
The CL has many features that UNIX users will be familiar with.
For example, the output of any task may be redirected to a file
or to another task.  Beware, though, that there are many
differences.  The CL and the IRAF system present the user with a
complete environment which is independent of the underlying operating
system.  Users running IRAF under VMS, UNIX, AOS, or any other operating
system can type exactly the same commands while in the CL.
.NH
Getting Started
.NH 2
Setting up the IRAF environment
.PP
A visitor using IRAF at KPNO does not have to do anything special to run IRAF.
Computer support personnel will provide a login on one of the VAX computers
and configure the environment as necessary to run IRAF.  Staff members and
long term visitors will already have established themselves on a VAX and will
need to perform a few simple operations before the CL and IRAF can be used
for anything serious.
.PP
An IRAF session begins with entry of the command "\fBcl\fR" to run the CL.
When the CL starts up, it looks for a file called \fBlogin.cl\fR in the
"current" directory.  If the CL is run from a directory that does not
contain a login.cl file, it will indeed run and can be used for simple things
like evaluating numerical expressions, but it will not work properly for
all functions.  Therefore, decide what directory is to be your IRAF login
directory, and \fIalways run the CL from a properly configured IRAF login
directory\fR.  The login directory, once set up, can be used for any number
of sessions.  If you wish, you can set up several independent login
directories for working with different types of data.
.RS
.IP [1]
Decide on a login directory and go there.
.IP [2]
Type \fBmkiraf\fR.
.RE
.PP
That is all that is required.  In addition, you may wish to edit the
resulting login.cl file and change some of the defaults.
The default login file consists mostly of environment declarations
(\fBset\fR statements) defining directories, devices, and so on.
The use of the environment and the significance of the standard environment
variables are discussed in a later section.
.NH 2
Starting the CL
.PP
After configuring your IRAF directory, type the command "\fBcl\fP"
to run the command language.  After a bit the welcome message
will appear on your terminal, the first or root "menu" of IRAF will be
printed, and the \fBcl>\fP prompt will be issued indicating that the
CL is ready for the first command.
.KS
.TS
center tab(%);
l l l l l.
artdata%digiphot%images%local%softools
astrometry%dtoi%imred%nsurfbrt%system
database%filterphot%language%onedspec%twodspec
dataio%focas%lists%plot%utilities
.TE
.KE
.PP
Everything in the root menu of IRAF is a \fBpackage\fR.
A package is a set of tasks which are logically connected.
For example, the "plot" package contains
an assortment of tasks used to plot things.  One can "load" any package
by simply typing its name; loading a package makes the tasks in the package
known to the CL.  You must load a package before any of the tasks therein
can be run.
.PP
Once the CL prompt appears, a large number of tasks are available and
ready to be executed.  A list of all loaded packages and the tasks in each
package may be obtained by typing the command \fB??\fP.
This will list the tasks organized by package, starting with the "current"
package (note that the system comes up with the \fBclpackage\fR, \fBsystem\fR,
and \fBlanguage\fR packages already loaded).  The tasks are listed in the
order in which they are searched when you type a command.
Type \fB?\fP to list only the task names in the current package,
or \fB?\fIpackagename\fR to list the tasks in package "packagename".
.NH 2
Executing commands from the CL
.PP
At this point you may want to try executing a few simple commands.
The first thing to try is the \fBhelp\fR command.  This will give
additional information about the tasks and subpackages in the current package.
In this and all subsequent examples, the text typed by the user is shown
in boldface.
.DS
cl> \fBhelp\fR
.DE
For detailed information about a particular task, type "help" followed
by the name of the task for which help documentation is desired.
For example,
.DS
cl> \fBhelp help\fR
.DE
will print detailed information about the \fBhelp\fR program itself.
.PP
Now let's try running some tasks from the \fBsystem\fR package, which
is already loaded.  To list the file "login.cl" on the terminal enter the
following command:
.DS
cl> \fBpage login.cl\fP
.DE
The \fBpage\fR routine will pause at the end of each page of text,
waiting for you to type RETURN before displaying the next page.
To get a directory listing, type:
.DS
cl> \fBdir\fP
.DE
which is actually an abbreviation for "directory".
All package, task, and parameter names may be abbreviated
while working interactively.  Any abbreviation may be given which contains
sufficient characters to uniquely identify the name; if the abbreviation
is not unique, an error message is printed.
To list all your files with the ".cl" extension, you can type:
.DS
cl> \fBdir "*.cl"\fP
.DE
Note that the pattern string \fB*.cl\fP \fImust be enclosed in quotes\fR
or you will get a "syntax error" message from the CL.
This is because the CL is attempting to evaluate every command
as a possible aprithmetic expression and the asterisk is a valid
arithmetic operator.
Without the quotes, there is no way of knowing whether an argument to
a task is a string with odd characters or an arithmetic expression.
When in doubt, use quotes around strings and filenames.
Either apostrophes or quotes may be used to delimit strings in the CL.
.PP
Packages are entered the same way tasks are run,
i.e. by merely typing the name of the package or task as a command
(a package is in fact a special kind of task).
Enter the "utilities" package by typing:
.DS
cl> \fButilities\fP
.DE
or just:
.DS
cl> \fBut\fP
.DE
Note also that the prompt has changed from \fBcl>\fP to \fBut>\fP to
let you know you have entered another package, and that
a set of new tasknames is now available to you.
.PP
Upon entry to the \fButilities\fR package you will be presented with a
menu listing the tasks in the new package.  One of the utility programs
is the \fBprecess\fR program, used to precess lists of astronomical
coordinates.  The simplest way to run \fBprecess\fR is to type only
its name:
.DS
ut> \fBprecess\fP
.DE
and you will be prompted for the name of an input file containing a list
of coordinates to be precessed, and the years over which the
precession is to be computed.  If you do not have the coordinates in a
file give the file name as "STDIN" (it must be upper case), and 
you can enter the coordinates interactively from the terminal.
Any number of coordinates (input lines from the "file" STDIN) may be entered;
signal the "end of file" by typing the EOF sequence,
\fB<ctrl/z>\fR on the KPNO systems.
Coordinates are entered in pairs (RA DEC) in either decimal or sexagesimal
notation (i.e., 12.5 or 12:30:04.2).  If you have any problems type
\fBhelp precess\fR for additional information, including examples.
.PP
If you have a long list of coordinates to precess, try entering
them into a file.  The command
.DS
ut> \fBedit coords.1950\fP
.DE
will call up the default editor (\fBvi\fP on the UNIX systems) to edit the
file "coords.1950".
After creating your coordinate file and exiting the editor in the usual
fashion, you will be back in the CL.  Now try executing \fBprecess\fP taking
input from the file:
.DS
ut> \fBprecess coords.1950\fP
.DE
Of course the output will still be placed on the terminal, so
you may wish to redirect the output into a file as well:
.DS
ut> \fBprecess coords.1950, > coords.1984\fP
.DE
Note that \fIarguments must be delimited by commas\fR or you will
get a "syntax error" message from the CL.  The following UNIX-like command
will cause a syntax error:
.DS
ut> \fBprecess coords.1950 > coords.1984\fP             (illegal syntax)
.DE
.PP
If the coordinate list is very long, you may wish to process
the list as a background job.  Probably you will not like to be bothered
by the background task asking for parameters, so be sure to enter
all the necessary parameters on the command line.  If you are not
sure what parameters are required or in what order, the fastest way to
find out is to use the "list parameter" task \fBlparam\fR to find out:
.DS
ut> \fBlparam precess\fR
.DE
The \fBlparam\fR task will list the task's parameters in the order in
which they must be given on the command line, showing the current values
of the parameters and the prompt strings as well (more on this later).
To execute the task \fBprecess\fR in the background, type:
.DS
ut> \fBprecess coords.1950, 1950, 1984, > coords.1984 & \fP
.DE
The CL will be available for further interactive use and will
inform you when the background job is complete.
.PP
Now that you are a couple of layers deep into the CL, you may
wonder how to exit. If you type \fBbye\fP, you will exit the current
package and rise one level of loaded packages. If you type \fBbye\fP at the
CL prompt level, you will exit from the CL completely.
.NH
Basic Usage
.NH 2
CL parameters
.PP
The CL is actually a task and has a set of parameters which
can be used to direct its execution. For example, you may wish
to keep a record of all commands you type. The CL will do this
if you set the boolean parameter \fBkeeplog\fP to yes. (Boolean
parameters can assume only the values yes or no.) Simply type:
.DS
cl> \fBkeeplog = yes\fP
.DE
All subsequent commands will be written to the log file defined by the
string parameter \fBlogfile\fP which defaults to the file name
"uparm$logfile". You may choose to set this file name to "session1",
for example, by:
.DS
cl> \fBlogfile = 'session1'\fP
.DE
For a full list of CL variables type "lparam cl".  The CL variables
which affect the behavior of the CL and which you may wish to alter are:
.TS
center box;
cb s s
ci | ci | ci
l | l | l.
CL Parameters
_
parameter	default value	function
=
abbreviate	yes	accept abbreviations 
keeplog	no	record commands in a file
logfile	"uparm$logfile"	log file name
menus	yes	print menu when package changes
notify	yes	signal when background job terminates
.TE
.PP
The CL parameters are initialized to their default values in your
\fBlogin.cl\fR file.  Any changes made by assignment will be lost when
you log out of the CL (this is not true of the parameters of a normal
task).  To permanently change the value of a CL parameter, you should
edit your login.cl file.

.NH 2
Environment Variables
.PP
In addition to the CL parameters, which affect only the operation of
the CL, the CL maintains a table of "environment variables" which 
affect the operation of all IRAF programs.  Environment variables are
created or redefined with the \fBset\fR command; \fBset\fR without
any arguments prints the current list of variables.  The environment
list is used to define logical names for directories, to associate
physical devices with logical device names, and to provide control
over the low level functioning of the IRAF file i/o system.
.PP
The default environment list is created at login time, i.e., when the
CL is first run.
One may add new environment variables, or redefine old ones,
at any time during
a session with the \fBset\fR command.  Set declarations made during
execution, however, may be lost upon exit from a package.  To make
sure environment declarations last for a full session, they should be
made immediately after logging in.  To make environment declarations
permanent, they should be placed in your \fBlogin.cl\fR file.
.PP
A selection of the more important environment variables is shown in the
table below.  The permissible names of devices are system dependent;
the KPNO devices are "vt100", "vt640" (retrographics enhanced vt100),
"tek4012" or "4012", "versatec", and "imagen".
.TS
center box;
cb s s
ci | ci | ci
l | l | l.
Selected Environment Variables
_
variable	default value	usage
=
terminal	"vt100"	default terminal device
printer	"imagen"	default line printer device
stdgraph	"vt640"	name of graphics terminal
stdplot	"versatec"	batch plotter device
stdvdm	"uparm$vdm"	name of graphics metacode file
stdimage	"iism70"	image display device
clobber	no	clobber (overwrite) files
filewait	yes	wait for busy files to become available
imdir	"/tmp2/iraf/"	directory where bulk data is stored
.TE
.PP
File \fBclobber\fR refers to the UNIX-like feature of silently
overwriting an existing file when the output file to be created has
the same name.  If file clobber is disabled (\fBclobber=no\fR, the default)
neither the CL nor an IRAF program called from the CL will create a new
file if an old file exists with the same name; the program will abort
instead.  If \fBfilewait\fR is enabled (\fBfilewait=yes\fR, the default),
a program which cannot access an existing file will wait for it to become
available.  This happens when a job needs to write to a file which
is already opened for writing by another job.
.PP
The logical directory \fBimdir\fR is where the IRAF system will store
your image data.  IRAF images will appear to be created in your local
user directory, but in fact it is only the header file which goes there.
The pixels are put in a second file on one of the temporary files systems,
which are configured and managed with large datasets in mind.  These \fBpixel
storage files\fR are transparent to the user, but if you have a great
deal of data it may be more efficient to set up your own directory on
a temporary files system, and redefine \fBimdir\fR accordingly.
Having a private \fBimdir\fR also makes it convenient to save data on
tape and later restore it to disk; the header files are usually small
enough so that they need not be archived if the data is going to be restored
within a week or two.
.PP
The \fBset\fR statement is used to set environment variables.  For example,
to change the name of the graphics terminal from the KPNO default "vt640"
to "tek4012":
.DS
cl> \fBset stdgraph = tek4012\fR
.DE
To change the name of the default printer device from the default "imagen"
to "versatec" (perhaps because it is physically closer to your terminal).
.DS
cl> \fBset printer = versatec\fR
.DE
.PP
The \fBset\fR command is only used to change the \fBsystem wide defaults\fR
for output devices and such.  All IRAF programs which write to the line
printer or a graphics device also permit the device to be selected on the
command line.  The name of the terminal, and the terminal characteristics
(baud rate, etc.) are most conveniently set with the command \fBstty\fR in
the package \fBsystem\fR.
.NH 2
File and directory names
.PP
The IRAF system employs a virtual file system so that all file references
will look the same on any computer.  The IRAF primitives convert "virtual
file names" into their host operating system equivalents.  In general,
either the virtual file name or the equivalent system dependent file name
may be used in a command entered by the user.  The IRAF system itself 
uses only the virtual form for reasons of transportability.
.PP
The environment list described in the last section plays a fundamental
role in the mapping of virtual file names.  The environment list is used
to define logical directories, equating the system dependent name of the
directory to a logical name.  An example of a virtual file name is the
default logfile, "uparm$logfile".  The "uparm" field,
delimited by the $ character, is the logical directory; the file name
within that directory is "logfile".
.PP
Although file names cannot be abbreviated the way commands can,
pattern matching may be used to refer to many files by typing only a
short string (the pattern).  The pattern matching metacharacters are
identical to those used in the UNIX operating system.  For example,
to print all files having extensions of '.cl', type:
.DS
cl> \fBlprint '*.cl'\fP
.DE
To page through all files with the "cl" extension in the logical directory
"system":
.DS
cl> \fBpage 'system$*.cl'
.DE
To delete a list of files:
.DS
cl> \fBdelete 'file1,file2,file3'
.DE
.PP
.PP
Note the quotes around the pattern strings; these are required due to the
presence of the asterisk character in the first two examples, and the
comma in the final example.  To be more precise, a string need not be
quoted provided [1] it appears as an identifier (a name) in an argument
list not enclosed in parenthesis, and [2] the string contains only instances
of the alphanumeric characters, underscore, period, and dollar sign.
If the string contains any special characters, i.e., an arithmetic or
boolean operator, comma, question mark, etc., it must be quoted.
If in doubt, use quotes (either apostrophes or quotes will do).
.PP
For example, consider the following simple command:
.DS
cl> \fBdelete filex\fR
.DE
The name "filex" given here is actually ambiguous, i.e., it could be either
the name of a file (a string constant) or the name of a string \fIparameter\fR
set to the name of the file to deleted.  In this simple and common case,
the CL is will quietly assume that "filex" is the name of a file.
Either of the following forms are equivalent to this command and both are
unambiguous:
.DS
cl> \fBdelete "filex"\fR
.DE
.DS
cl> \fBdelete ("filex")\fR
.DE
The following command is also unambiguous, and specifies that the CL is to
take the name of the file to be deleted from the \fIparameter\fR "filename":
.DS
cl> \fBdelete (filename)\fR
.DE
.PP
Note also that in all the examples, a \fBsingle\fR string type argument,
the file matching template, is used to refer to a list of files.
A template is a string consisting of one or more filenames or patterns
delimited by commas.  This convention is employed by all IRAF tasks which
operate on lists of files.  Be careful not to confuse a file list template,
which is a string, with the argument list itself.  Thus,
.DS
cl> \fBdelete 'file1, file2, prog.*'\fR
.DE
is perfectly acceptable, while
.DS
cl> \fBdelete file, file2, 'prog.*'\fR
.DE
is incorrect.
.PP
Often it is useful to be able to use multiple directories to organize
data.  For example, you may have a directory for M87 data, and one for M8.
To print the name of your current directory, or the pathway through the
system to your directory, type
.DS
cl> \fBpath\fP
.DE
The system dependent pathname of the current directory will be printed
(\fBpath\fR is an abbreviation for "system.pathnames").
To change to a new directory, type:
.DS
cl> \fBchdir \fInewdir\fR
.DE
where \fInewdir\fR is either the name by which the directory is known to the
underlying operating system, or an IRAF logical directory name defined with
a \fBset\fP command.  For example to define the logical directory "M87"
on a UNIX based system, execute the following \fBset\fR command (note the
trailing '/'):
.DS
cl> \fBset M87 = "/usr/myname/iraf/M87/"
.DE
and then you may type either of the following commands to change the current
directory to "M87" (note \fIchdir\fR may be abbreviated to \fIch\fR).
.DS
cl> \fBchdir M87\fR
cl> \fBchdir "/usr/myname/iraf/M87"
.DE
.PP
Of course it is not necessary to change to a directory to reference the files
therein.  Your login directory, for example, already has the logical name
\fBhome\fR assigned to it.  The following command would page the \fBlogin.cl\fR
file in your home directory, regardless of the current directory:
.DS
cl> \fBpage home$login.cl\fR
.DE
.NH 2
Parameters
.PP
Nearly all tasks have a formally defined set of parameters associated with
them.  A task's parameters may be listed with the command
\fBlparam \fItaskname\fR.  For example, to list the parameters for the
task \fBdelete\fR, used to delete files:
.DS
cl> \fBlparam delete\fP
.DE
The following list will appear giving the parameter name, its
current value, and the prompt string associated with it.
.DS
.cs 1 18
.br
        files =         list of files to be deleted
     go_ahead = yes      ?
      (verify = no)     verify operation before deleting each file?
(default_acti = yes)    default delete action for verify query
        (mode = ql)             
.DE
.cs 1
.PP
Notice that there are two types of parameters, those with and without
parentheses around the \fIparam=value\fR fields.  The parameters not
enclosed in parentheses are called \fBpositional parameters\fR,
and will be queried for if not given on the command line.  The first
positional parameter will be set by the first "positional" argument on the
command line, the second positional parameter by the second positional argument,
and so on.  A \fBpositional argument\fR is an argument which is associated
with a parameter by its position in the command line; nonpositional arguments
refer to parameters by name and must follow the positional arguments.
The parameters enclosed in parentheses are called \fBhidden parameters\fR,
and are the topic of the next section.
.NH 3
Hidden parameters
.PP
The last three lines in the above example describe additional "hidden"
parameters.  The CL does not query for hidden parameters, but automatically
uses the default values.  A query will be generated only if there is no
default value, or if the default value is illegal for some reason.
Hidden parameters may be set on the command line, but unlike positional
parameters, the command line value will not be "learned", i.e., become
the new default value.  The default value of a hidden parameter may only
be changed by an explicit assigment, and one should exercise caution
in doing so, because it is easy to forget that the parameter has been changed.
.PP
Hidden parameters make it possible to easily change the behavior of a task,
achieving considerable flexibility without requiring many arguments on the
command line, or annoying queries for parameters.  Hidden parameters come close
to making it possible to please everybody, since the user can modify the default
behavior of a task to make it do what they want.  Hidden parameters can
also be dangerous if they are used improperly (i.e., for data dependent
parameters in scientific programs).
.PP
The \fBdelete\fR task is a good example of a task which it is nice to be
able to personalize.  The default behavior of \fBdelete\fR is to simply
delete the named file or files (provided they are not "protected" files).
File deletion is dangerous, particularly since a file name matching
template may be used to delete many files.  For example, the command
.DS
cl> \fBdelete '*'\fR
.DE
will delete \fIall\fR of the (unprotected) files in the current directory.
IRAF recognizes a number of special pattern matching metacharacters in
addition to "*", and one could easily get burnt if they were not familiar
with the use of file matching templates.
.PP
To eliminate this possibility you might want to change the default behavior
of \fBdelete\fR to interactively verify each file deletion.  This is done
by changing the value of the hidden parameter "verify", which defaults
to \fIno\fR.  Hidden parameters of type boolean (yes/no) may be overridden
temporarily on the command line, as follows:
.DS
cl> \fBdelete '*.dat', verify+\fR
.DE
or, equivalently,
.DS
cl> \fBdelete '*.dat', verify=yes
.DE
Either of these commands would cause a prompt to be issued naming
each file matching the template, and asking you if you want to delete
it (this will happen even in batch mode).  Setting a hidden parameter
on the command line only overrides the value of that parameter for
that call; the default value is not changed.  To permanently change
the default value of a hidden parameter, an explicit assignment is
required:
.DS
cl> \fBdelete.verify = yes\fR
.DE
.PP
This will "permanently" change the value of the "verify" parameter
to yes, causing all subsequent file deletions to be verified, unless
\fBdelete\fR is called with the argument "verify\(mi" or "verify=no"
on the command line.  The "permanently" is qualified because it may
be undone by another assignment, or by "unlearning" the \fBdelete\fR
parameters.  The \fBunlearn\fR task restores the system default values
of the parameters for a single task or for an entire package.
Thus,
.DS
cl> \fBunlearn delete\fR
.DE
will restore the parameters of the task \fBdelete\fR to their default
values, and
.DS
cl> \fBunlearn system\fR
.DE
will restore the defaults for \fIall of the tasks\fR in the
package "system".  If you want to restore the defaults for all the
parameters that ever were, delete the contents of the logical directory
\fBuparm\fR:
.DS
cl> \fBdelete "uparm$*.par"\fR
.DE
.NH 3
Specifying the parameters to a task
.PP
In many cases, it will be obvious what the arguments to a task
should be, either from the context of what the task does, or
from a parameter listing. If you are unsure how to proceed,
you can simply type the task name, and answer the questions.
Each prompt will include a minimum and maximum acceptable value
if one applies, and the current value of the parameter if
one exists. If you wish to retain the current value, simply
press RETURN. Otherwise, type in a new value.
.PP
Once you are familiar with the operation of a task, you can
enter the parameter values on the command line in the
order in which they appear in the \fBlparam\fR parameter listing.
A command line argument may be any general expression, much like
the arguments to a Fortran subroutine.  Parameters may also be
set using the "paramname = value" notation on the command line,
but any positional arguments must be given first.
.PP
For example, the precession task requires a file name (parameter \fIinput\fR),
a starting year (parameter \fIstartyear\fR), and an ending year (parameter
\fIendyear\fR); the latter two parameters specify the interval over
which the coordinate transformation is to occur.
This task can be executed in any of the following ways:
.DS
cl> \fBprecess STDIN,1950,1984\fP
.DE
.DS
cl> \fBprecess stdepoch=1984+i*4\fP
.DE
.DS
cl> \fBprecess\fP
.DE
In the last two cases, the CL will prompt for the missing information.
In the second case, an expression is used to compute the value of the
hidden parameter \fBstdepoch\fR.
Some legal value of the variable 'i' must have been specified previously,
otherwise the CL will ask for that as well.  The capability to specify
the values of parameters using expressions is most useful within
\fBwhile\fR loops; this is an advanced topic which will be touched on later.
.NH 2
Pipes and i/o redirection
.PP
We have already seen how tasks can take their input from either the
terminal or from a file, and send the output to either the terminal
or a file.  The capability to change the standard input and output
of a task on the command line is called \fBi/o redirection\fR.  The default
standard input and output for a task is the user terminal.
.PP
The \fBpipe\fR syntax is a powerful kind of i/o redirection.
A "pipe" is formed by connecting the output of one task
to the input of another task; an arbitrary number of tasks may be connected
together in this way to form a single pipe command.
UNIX users will already be familiar with the concept and uses of pipes,
but beware that CL pipes differ from UNIX pipes in that the tasks
execute serially rather than concurrently (nothing comes out of the
end of the pipe until all the input has been read in).  Note also that
CL queries are not affected by the use of i/o redirection or pipes.
.PP
A simple example of the use of a pipe is redirecting the output of a command
to the line printer.  This can be done with i/o redirection as follows.
.DS
cl> \fBhelp help, > tempfile\fR
cl> \fBlprint tempfile\fR
cl> \fBdelete tempfile\fR
.DE
The pipe notation accomplishes the same thing and is far more concise:
.DS
cl> \fBhelp help | lprint\fR
.DE
.PP
For a more sophisticated example of the use of pipes, load the \fBlists\fR
package and try out the following command:
.DS
li> \fB?? | words | match ":",stop+ | sort | table\fR
.DE
This sequence takes the list of menus produced by \fB??\fR, breaks it into
a list of words, filters out the lines that contain the colon character
(the package names), sorts the list, and prints a super menu listing the
tasks in all loaded packages.
.NH 2
Command Syntax
.PP
The form of a task call is the task name, optionally followed by an
argument list.  The argument list may optionally be enclosed in
parenthesis.  The argument list consists of a list of expressions
delimited by commas.  Simple filename or string arguments appearing
in unparenthesized argument lists need not be quoted (see \(sc3.3).
Any positional arguments must be given first, followed by
"keyword = value" assignments, switches ("param+"), and i/o redirection
assignments ("> file").  The latter three types of arguments may
appear in any order.  Commas may be used as placeholders to skip
positional arguments that need not be set, as shown in the following
example:
.DS
cl> \fBtype "coords.1950" | precess ,1950,1984.3\fR
.DE
.PP
The form of a command is not limited to solitary calls to tasks.
For example, several tasks may be called in sequence on a single
command line, using the semicolon character to delimit each call:
.DS
cl> \fBclear; dir\fR
.DE
This command clears the terminal screen, then lists the files in the
current directory.  If the command sequence to be executed is too
big to fit on a single line, it can be enclosed in curly brackets:
.DS
.cs 1 18
cl> \fB{\fR
>>> \fBclear\fR
>>> \fBdirectory\fR
>>> \fBbeep\fR
>>> \fB}\fR
.DE
.cs 1
An arbitrary number of commands may be entered in this way and executed
as a single unit.  The prompt will change to \fB>>>\fR after the first
line to signal that the CL requires more input before it can execute the
command.  If an argument list is too large to fit all on one line,
continuation is understood if the last item on a line is a comma,
the "pipe" character ("|"), or an operator (i.e., '+' or '||').
.DS
.cs 1 18
cl> \fBgraph "pix[*,5],pix[*,10],pix[*,15]", po+, marker=circle,\fR
>>> \fBxlabel=column, ylabel=intensity, title = "lines 5, 10, and 15"\fR
.DE
.PP
.cs 1
.NH 2
Aborting tasks
.PP
Any task may be aborted by typing the interrupt sequence (<ctrl/c> on
the KPNO systems).  Control will return to the point at which the last
interactive command was entered.  When an IRAF program run from the CL
is interrupted, it will usually perform some cleanup functions, deleting
partially written files and so on.  If an error (or another interrupt)
should occur during error recovery, the program will issue the following
message:
.DS
PANIC: Error recursion during error recovery
.DE
.PP
A panic abort is usually harmless, but may result in some half-written
dreg files being left behind.
.NH 2
Background Jobs
.PP
Any command, including multiline commands involving calls to several tasks,
may be executed in the background by appending the character "&" to the
end of the command block.  The CL will print out the job number of the
background job and return control to the terminal.  Background job numbers
are always small integers in the range 1 to N, where N is the maximum
permissible number of background jobs (typically 3-6).
.DS
cl> \fBcontour 'm92', dev=stdplot &\fR
[1]
cl>
.DE
If the task runs to completion, and if the CL \fBnotify\fR parameter is
set to yes, the message "\fB[1] done\fR" will be printed on your terminal
when the task completes.  If the background job writes to the standard
output, and the standard output has not been redirected, the output of
the background job will come out on your terminal mixed in with the output
from whatever else you are doing.
.PP
If sometime during the processing of a background job, the job finds that
it needs to query for a parameter, the message
.DS
[1] stopped waiting for parameter input
.DE
will appear on your terminal.  It is not necessary to respond to such a
request immediately; when a convenient point is reached, respond as
follows:
.DS
cl> \fBservice 1\fR
.DE
The prompt string from the background job will be printed, just as if
you were running the job interactively.  Respond to the query and the
background job will continue executing.  If you do not respond to the
request for service from a background job, it will eventually time
out and abort.
.PP
The \fBkill\fR command may be used to abort a background job.  The argument
is the logical job number printed by the CL when the background job
was spawned (or a list of jobs to be killed):
.DS
cl> \fBkill 1\fR
.DE
Sometimes it is desirable to wait for a background job to complete before
resuming interactive work.  For example, you might reach a point where
you cannot proceed until the background job has finished writing a file.
The \fBwait\fR command is used to wait for \fIall\fR currently running
background tasks to complete (this is a deficiency which will be corrected
at some point):
.DS
cl> \fBwait; beep\fR
.DE
.PP
There is at present no really nice way to get status on the progress of
background jobs.  The command "\fBspy v\fR" will command the host
operating system to print
the processor status (in a system dependent form), including information
on the status of all running processes.
.NH 2
Sending commands to the host operating system
.PP
Sometimes it is necessary to send a command to the host operating system.
Any command may be sent to the underlying operating system by prefacing
the command with the escape character "!".  The rest of the command line
will be passed on unmodified.  For example, to read your mail on a UNIX
system,
.DS
\fBcl> !mail\fR
.DE
Upon exiting the mail routine, you will be back in the CL.
.NH
Advanced Usage
.NH 2
The History mechanism
.PP
The CL \fBhistory mechanism\fR keeps a record of recent commands and provides
a way of reusing those commands to enter new commands with a minimum of
typing.  In particular, the last command entered can easily be edited to
correct an error, without having to retype the entire command.
The history mechanism should not be confused with the logfile; the history
mechanism does not permanently record commands, and the logfile cannot
be used to save typing.
.PP
The \fBhistory\fR command is used to display the last few commands entered:
.DS
.cs 1 18
cl> \fBhistory\fR
101 urand 200,2 | graph po+, marker=circle, szmarker=.03
102 help graph | lprint
103 history
cl>
.DE
.cs 1
By default, the last 15 commands entered are printed, each preceeded by
the command number.  To print the last N commands, add the argument N to
the \fBhistory\fR command line; this will become the new default (-N will
not change the default).
.PP
Given the history record sequence shown above, any of the following
commands could be used to repeat command 101:
.DS
cl> \fB^101\fR
.DE
.DS
cl> \fB^-3\fR
.DE
.DS
cl> \fB^ur\fR
.DE
.DS
cl> \fB^?mark?\fR
.DE
.PP
The history command \fB^ur\fR finds the last command \fIbeginning\fR with the
string "ur", and executes it.  The command \fB?mark?\fR finds the last
command \fIcontaining\fR the string "mark", and executes it (the trailing
? is optional if it is the last character on the line).
A \fB^\fR alone would merely repeat the last command entered.
The recalled command will be echoed for verification.
.PP
Repeating a command in this fashion can be dangerous, of course, if you 
make an error and recall the wrong command (watch out for \fB^delete\fR).
To play it safe, append the string \fB:p\fR to the history command:
.DS
cl> \fB^ur:p\fR
urand 200,2 | graph po+, marker=circle, szmarker=.03
cl>
.DE
This will recall the command and enter it as the most recent command in
the history buffer, but will not execute it.  One can then type \fB^\fR
to execute the command.  Often it is more useful to recall an old command
and edit it slightly.  This is done as follows:
.DS
.cs 1 18
cl> \fB^ur:p\fR
urand 200,2 | graph po+, marker=circle, szmarker=.03
cl> \fB^circle^box\fR
urand 200,2 | graph po+, marker=box, szmarker=.03
.DE
.cs 1
This sequence would recall command 101, change the string "circle" to
"box", and execute the new command.  The same notation may be used to
correct errors in the last command entered.  By default, only the first
matched substring is replaced; a trailing \fB^g\fR may be added to replace
all matched substrings.
.PP
Often it is useful to be able to reuse the \fBarguments\fR of a previous
command.  The notation \fB^^\fR refers to the first argument in
of the last command entered, \fB^$\fR to the last argument of the last
command, and \fB^*\fR to the whole argument list.  Thus,
.DS
cl> \fBdir "file1, file2, *.cl, home$junk", op=l\fR
cl> \fBlprint ^^\fR
.DE
would print a long-form directory listing of the files specified by the
template, then print the same files on the line printer.
.PP
One of the most useful features of the history mechanism is the ability to
repeat a command with additional arguments appended.  Almost any
history command may be followed by some extra text which is appended to the
command recalled from the history.  For example,
.DS
cl> \fBurand 200,2 | graph po+\fR
cl> \fB^^, title = "200 random numbers"\fR
urand 200,2 | graph po+, title = "200 random numbers"
.DE
In this case the notation \fB^^\fR refers to the \fIentire last command\fR
entered.  The notation is unambiguous because the \fB^^\fR appears at
the start of the command line; do not confuse it with the use of \fB^^\fR
to reference the first argument.
.NH 2
Expressions and intrinsic functions
.PP
We have already seem some simple examples of CL expressions.  The CL has a
conventional modern expression syntax which should be familiar to most
users.  The following operators are provided:
.DS
.cs 1 18
+  \(mi  *  /      the conventional arithmetic operators
**              exponentiation
//              string concatenation
<   < =         less than, less than or equals
>   > =         greater than, greater than or equals
!=  = =         not equal, equal (2 equal signs)
&&  ||          \fBand\fR, \fBor\fR
!               \fBnot\fR
.DE
.cs 1
.PP
Parenthesis may be used to alter the default order of evaluation of an
expression.  Quotes are not optional in expressions or anywhere inside
parenthesis; identifiers are assumed to be the names of parameters.
.PP
All of the Fortran intrinsic functions are provided, with the exception
of the hyperbolic and complex functions (the CL has no complex datatype).
The datatypes supported by the CL are \fBboolean\fR, \fBinteger\fR, \fBreal\fR,
\fBstring\fR, and several exotic types discussed in the Programmer's Guide.
Mixed mode expressions involving integers and reals are permitted.
Explicit type conversion is implemented with the intrinsic functions
\fBint\fR, \fBreal\fR, and \fBstr\fR, the latter converting an argument
of any datatype into a string.
.PP
The CL provides a special type of statement for evaluating expressions
and printing the value out on the terminal.  The form of the statement
is an expression preceeded by the equals sign:
.DS
= \fIexpression\fR
.DE
If you prefer, the more conventional and more general \fBprint\fR statement
can be used with the same results:
.DS
\fBprint (\fIexpression [, expression, ...] \fB)\fR
.DE
For example, try entering the following expressions and see if you
can predict the results:
.DS
cl> \fB= (sin(.5)**2 + cos(.5)**2)\fR
.DE
.DS
cl> \fB= (mod (int(4.9), 2) == 0)\fR
cl> \fB^int^nint\fR
.DE
.DS
cl> \fB= "map" // radix (512, 8)\fR
.DE
.DS
cl> \fB= delete.verify\fR
.DE
.NH 2
Image Sections
.PP
All IRAF programs which operate upon images may be used to operate on
the entire image (the default) or any \fBsection\fR of the image.
A special notation is used to specify image sections.  The section
notation is appended to the file name of the image, much like an
array subscript is appended to an array name in a conventional programming
language.  If no section is specified, the entire image will be used.

.KS
.TS
center;
ci ci
l l.
section	refers to

pix[]	whole image
pix[i,j]	the pixel value (scalar) at [i,j]
pix[*,*]	whole image, two dimensions
pix[*,\(mi*]	flip y-axis
pix[*,*,b]	band B of three dimensional image
pix[*,*:s]	subsample in y by S
pix[*,l]	line L of image
pix[c,*]	column C of image
pix[i1:i2,j1:j2]	subraster of image
pix[i1:i2:sx,j1:j2:sy]	subraster with subsampling
.TE
.KE

.PP
A limited class of coordinate transformations may be specified using image
sections (but transpose is \fInot\fR one of them).
The "match all" (asterisk), flip, subsample, index, and range notations
shown in the table may be combined just about any way that makes sense.
Some examples have already appeared in the text.  As a simple example,
the following command will graph line 10 of the image "pix" (the \fBgraph\fR
utility is in the \fBplot\fR package):
.DS
cl> \fBgraph "pix[*,10]"\fR
.DE
To generate a contour plot of an 800 pixel square, two dimensional image
on the graphics terminal, subsampling by a factor of 16 in both dimensions:
.DS
cl> \fBcontour "pix[*:16,*:16]"\fR
.DE
To display the fifth x,z plane of the three dimensional image named "cube"
on frame 1 of the image display device (\fBdisplay\fR is in the
subpackage \fBtv\fR of the package \fBimages\fR):
.DS
cl> \fBdisplay "cube[*,5,*]", 1\fR
.DE
.NH 2
Statements and inline scripts
.PP
Since this is not a reference manual, we will not attempt to present a
complete definition of the syntax of the command language.
Nonetheless it can be helpful to understand a few of the more useful
types of statements.  The last section introduced two statements,
the \fBimmediate\fR statement (\fI=expr\fR) and the \fBprint\fR statement.
The \fBassignment\fR statement should also be familiar, but there is
more to it than first meets the eye.
.PP
The assignment statement is most often used to set the value of a parameter.
Since most parameters are local parameters belonging to some task, the
"dot" notation must be used to name both the task and the parameter.
Thus, in \(sc3.4.1 we used the statement
.DS
cl> \fBdelete.verify = yes\fR
.DE
to set the value of the "verify" parameter belonging to the task "delete".
The task \fBdelete\fR belongs to the \fBsystem\fR package.  If there
happened to be another task named \fBdelete\fR in the searchpath, we
would have to specify the package name as well to make the assignment
unambiguous:
.DS
cl> \fBsystem.delete.verify = yes\fR
.DE
In this unfortunate situation of two tasks with the same name in different
packages, we would also have to specify the package name explicitly
just to be able to run the task:
.DS
cl> \fBsys.del \fIfiles\fR
.DE
.PP
Often we do not want to simply assign a value to a parameter,
but rather we want to increment, decrement, or scale a parameter.
These functions can all be performed with assignment statements
in the CL, using the assignment operators \fB+=\fR, \fB\(mi=\fR, \fB*=\fR,
\fB/=\fR and \fB//=\fR.  For example, to increment the value of
a parameter, we would use the \fB+=\fR "assignment" statement:
.DS
cl> \fBsum_of_squares  +=  (x ** 2)\fR
.DE
This statement increments the parameter "sum_of_squares" by the value of
the expression \fB(x**2)\fR.
.PP
The CL provides the \fBif\fR, \fBif else\fR, and \fBwhile\fR statements
for controlling the flow of execution in a command.  These statements
are quite useful for writing little loops to do things at the command
level.  For example, to print the values of the first ten powers
of two:
.DS
.cs 1 18
cl> \fBi=1; j=2\fR
cl> \fBwhile (i <= 10) {\fR
>>> \fB    print (j)\fR
>>> \fB    j *= 2\fR
>>> \fB    i += 1\fR
>>> \fB}\fR
.DE
.cs 1
.PP
This example illustrates the use of the builtin CL variables \fBi\fR
and \fBj\fR.  A number of variables are provided in the CL for interactive
use; for a full listing type \fBlparam cl\fR.  In short, the integer
variables provided are \fBi,j,k\fR, the real variables are \fBx,y,z\fR,
the string variables are \fBs1,s2,s3\fR, and the booleans are 
\fBb1,b2,b3\fR.
.PP
Note the parenthesized argument list in the call to \fBprint\fR in the above
loop.  If the parameter \fBj\fR were not enclosed in parenthesis,
the CL would interpret it as a \fIstring\fR rather than a parameter,
and erroneously print "j" each time through the loop.  To avoid nasty
surprises like this, \fIalways enclose argument lists in parenthesis in
loops and within scripts\fR.  As a rule, if arguments lists are not
parenthesized when entering simple commands, but are always parenthesized
in loops and scripts, the CL will probably do what you expect it to.
.PP
A \fBlist structured\fR parameter is also provided for reading lists
(i.e., of file names).  Lists are especially useful for setting up batch
jobs.  For example, suppose we want to make a series of contour plots
on the standard plotter device.  This can be done by interactively
entering a command to produce each plot, but this is tedious, so
we prepare a list of "image sections" to be plotted, one per line
in the text file "sections".  The following command could then be used
to generate the plots in the background:
.DS
.cs 1 18
cl> \fBlist = "sections"\fR
cl> \fBwhile (fscan (list, s1) != EOF)\fR
>>> \fB    contour (s1, device = "stdplot") &\fR
.DE
.cs 1
.PP
If one starts writing commands very much more complicated than these
examples, it is time to learn about \fBscript tasks\fR.  A script task
with an arbitary set of local parameters, calling the i/o functions built
into the CL, can be used to write actual programs at the CL level.
That is too complicated for us to go into here, but there are plenty of
examples of CL scripts tasks (files with ".cl" extensions) in the system,
and more detailed information is given in the CL Programmer's Guide.
