.RP
.TL
Sun/IRAF Installation and User Notes
.AU
Doug Tody
.AI
.K2 "" "" "*"
.br
August 1987

.AB
This document deals with the Sun specific details
of installing and maintaining Sun/IRAF, leaving the basics to be covered in the
\fIUNIX/IRAF Installation and Maintenance Guide\fR.
In particular, the major part of the system installation or updating procedure
is given in the latter document.  We present here the additional steps to be
performed to install Sun/IRAF.  The graphics and imaging facilities developed
for IRAF running under the SunView window system are discussed.  The support
provided for the various Sun floating point hardware options is described,
including instructions for configuring the system for a specific floating
point option, or configuring a single version of the system to support multiple
floating point options simultaneously at runtime, e.g., when accessing the
system via NFS from multiple nodes.  Finally, the system resources required
for an efficient implementation are discussed, including the minimum disk and
memory requirements, and the effect on system performance to be expected when
running IRAF on a diskless node.
.AE

.pn 1
.bp
.ce
.ps +2
\fBContents\fR
.ps -2
.sp 3
.sp
1.\h'|0.4i'\fBIntroduction\fP\l'|5.6i.'\0\01
.sp
2.\h'|0.4i'\fBInstalling Sun/IRAF\fP\l'|5.6i.'\0\01
.br
\h'|0.4i'2.1.\h'|0.9i'The Sun/IRAF Distribution Tape\l'|5.6i.'\0\02
.br
\h'|0.4i'2.2.\h'|0.9i'Additions to the Install Script\l'|5.6i.'\0\03
.sp
3.\h'|0.4i'\fBFloating Point Options\fP\l'|5.6i.'\0\04
.br
\h'|0.4i'3.1.\h'|0.9i'Implementation\l'|5.6i.'\0\05
.br
\h'|0.4i'3.2.\h'|0.9i'Maintaining Multiple Binary Versions\l'|5.6i.'\0\05
.br
\h'|0.4i'3.3.\h'|0.9i'Programming Notes\l'|5.6i.'\0\06
.br
\h'|0.4i'3.4.\h'|0.9i'Installing IRAF with a Single BIN Subdirectory\l'|5.6i.'\0\07
.br
\h'|0.4i'3.5.\h'|0.9i'Custom Installations\l'|5.6i.'\0\07
.sp
4.\h'|0.4i'\fBGraphics and Image Display Under SunView\fP\l'|5.6i.'\0\08
.br
\h'|0.4i'4.1.\h'|0.9i'Getting Started with Windows\l'|5.6i.'\0\08
.br
\h'|0.4i'4.2.\h'|0.9i'Vector Graphics Capabilities\l'|5.6i.'\0\08
.br
\h'|0.4i'4.3.\h'|0.9i'Image Display Capabilities\l'|5.6i.'\0\09
.br
\h'|0.4i'4.4.\h'|0.9i'Using the Workstation with a Remote Compute Server\l'|5.6i.'\0\011
.sp
5.\h'|0.4i'\fBHost System Requirements\fP\l'|5.6i.'\0\011
.br
\h'|0.4i'5.1.\h'|0.9i'Memory Requirements\l'|5.6i.'\0\012
.br
\h'|0.4i'5.2.\h'|0.9i'Disk Requirements\l'|5.6i.'\0\012
.br
\h'|0.4i'5.3.\h'|0.9i'Diskless Nodes\l'|5.6i.'\0\012

.nr PN 0
.bp
.NH
Introduction
.PP
The version of UNIX/IRAF for the Sun workstations is identical to the 4.3BSD
UNIX version of IRAF except for the addition of support for vector graphics
and image display in the SunView window environment, and the need to support
the variety of hardware floating point options available on these workstations.
Direct support for Sun/IRAF is currently available only for the Sun-3 series
workstations with the Motorola 68881 floating point coprocessor or the Sun
floating point accelerator (FPA).  Sun/IRAF will also run on the older Sun-2
machines with optional Sky floating point board, but additional effort is
required to configure the system, and the poor floating point performance of
these old machines makes the effort somewhat questionable.  Support for the
new RISC based Sun-4 series should be available in late 1987.
.PP
The Sun hardware and operating system and windowing software is evolving
rapidly, as is the vector graphics and image display software developed by
the IRAF project for these systems.  It is commonplace for software changes
to be required in the Sun/IRAF release to track revisions in the Sun operating
system software.  Anyone contemplating running IRAF on a Sun workstation
should obtain the latest release of the Sun/IRAF software from the IRAF
project, and should obtain and install new releases of Sun/IRAF as they
appear, to ensure that the software continues to function correctly as well
as to benefit from the functional enhancements continually being made to the
software.

.NH
Installing Sun/IRAF
.PP
The installation procedure for Sun/IRAF is identical to that documented in
the \fIUNIX/IRAF Installation and Maintenance Guide\fR with the following
additions:
.RS
.IP \(bu
Due to the need to support a variety of floating point configurations,
two separate versions of the system binaries are supplied, one optimized for
the 68881 floating point coprocessor and one optimized for the Sun FPA.
Additional steps are required to copy these from the distribution tape or
tapes to disk, and to install them in the runtime system.
.IP \(bu
Sun/IRAF uses the same basic \fIinstall\fR script as generic BSD UNIX/IRAF,
with additional steps added at the end to install the SunView based graphics
and image display programs (GTERM and IMTOOL).
.RE
.PP
We assume in what follows that the reader is already familiar with the generic
system installation and updating procedures.  No attempt is made here to
distinguish between the procedures used to initially install Sun/IRAF,
or to update an existing installation, since there is no significant difference
between the two procedures from the point of view of the current document.
.NH 2
The Sun/IRAF Distribution Tape
.PP
Sun/IRAF is normally distributed either on a single 9 track 6250 bpi tape,
or on one or more 1/4 inch cartridge tapes.
While a generic BSD distribution tape contains only the single rooted
filesystem \fIiraf\fR (\fL./\fR on the tar tape),
the Sun/IRAF distribution tape or tapes contain the three independent rooted
filesystems \fIiraf\fR, \fIbin.f68881\fR, and \fIbin.ffpa\fR, as shown in
the table below.  The difference in the size of the bin filesystems is due to 
the fact that the system is distributed already configured for the f68881
floating point option, hence the f68881 objects and libraries are stored in
the main system, whereas those for the ffpa option have been archived in
\fLbin.ffpa\fR.  If the system is reconfigured for ffpa the situation will be
reversed (the system size is the same for either floating point option).
.TS
center;
ci ci ci
l l l.
filesystem	size	description

\fLiraf\fR	33 Mb	the main IRAF system
\fLbin.f68881\fR	17 Mb	f68881 objects and executables
\fLbin.ffpa\fR	24 Mb	ffpa objects and executables
.TE
.PP
The significance and use of the bin directories is the subject of \(sc3.
Our concern here is with restoring these filesystems to disk from the
distribution tape or tapes.  The order in which the filesystems are stored
on the distribution tape or tapes is irrelevant provided the tape containing
the desired filesystem can be located; the contents of each tape are marked
on the tape label.
.PP
The reason for the multiple independent filesystems is to give us more
flexibility in how we allocate disk space, by breaking the system up into
more manageable chunks.  The three filesystems may be read in on separate
UNIX filesystems if desired, depending upon where sufficient free space
can be found (note that the system can be \fIstripped\fR following
installation to minimize disk usage).\(dg
.FS
\(dgThe current bin directories are large, but a future release of the Sun
operating system software is expected to include a shared library facility,
which should allow us to extract the VOS modules currently linked into
each IRAF executable into a single shared file.  When this facility becomes
available we expect a dramatic reduction in the size of the system binaries
and in the disk and memory requirements of Sun/IRAF (part of this gain may
subsequently be lost, however, due to the larger binaries likely to be
required by the RISC architecture used in the Sun-4 series).
.FE
Depending upon the floating point hardware available on the machines in the
local network, it may or may not be necessary to restore both bin directories
to disk (see \(sc3.4).  For example, on our software development system
\fIiraf\fR is rooted at \fL/usr/iraf\fR,
and the two bin directories are maintained on a separate large filesystem.
Both bin directories are needed because multiple nodes,
operating with or without the Sun FPA, may access the same copy of the system
via NFS (the \fIfswitch\fR option is not used for efficiency reasons).
.LP
To restore the main iraf system to disk, while logged in as `iraf':
.DS
\fL% cd $iraf/..
% tar -xpf /dev/\fIdevice\fP iraf\fR
.DE
.LP
Note that this is \fInot\fR the same procedure used to restore a generic BSD
distribution tape to disk, since the command is executed in the parent
directory which will contain the iraf filesystem, rather than in the root
directory of the iraf filesystem itself.
.LP
To restore one of the bin directories, e.g., \fLbin.f68881\fR:
.DS
\fL% cd \fIbindir\fP
% tar -xpf /dev/\fIdevice\fP bin.f68881\fR
.DE
where \fIbindir\fR is the pathname of the directory which will have
\fLbin.f68881\fR as a subdirectory.
.PP
The final step is to tell IRAF where the bin directories are located.
This is done with the two symbolic links \fLbin.f68881\fR and \fLbin.ffpa\fR
in the IRAF root directory \fL$iraf\fR.  Continuing with our example above,
.DS
\fL% cd $iraf
% ls -F
bin@          dev/        local/      noao/       unix/
bin.f68881@   doc/        math/       pkg/
bin.ffpa@     lib/        mkpkg       sys/
% rm bin.f68881
% ln -s \fIbindir\fP/bin.f68881 .\fR
.DE
.LP
Note that in Sun/IRAF the subdirectory `\fLbin\fR' is also a symbolic link,
in this case pointing to one of the logical subdirectories \fLbin.f68881\fR
or \fLbin.ffpa\fR.  Do not tamper with the value of \fLbin\fR, as this variable
is used by the system to keep track of the current floating point option,
and will be set to the correct value for your system at a later stage of the
installation procedure (see \(sc3.2).
.PP
The same sequence of commands (with \fIffpa\fR substituted for \fIf68881\fR)
may be repeated to restore the \fLbin.ffpa\fR directory to disk and link it
with the main system.  If one of the bin directories has not been restored
to disk then the corresponding symbolic link should be deleted to indicate
that the floating point option is not supported on the local system.
.NH 2
Additions to the Install Script
.PP
Sun/IRAF uses the same install script (the cshell script \fL$hlib/install\fR)
as generic BSD UNIX/IRAF, with some additional commands at the end of the
script to install the GTERM and IMTOOL programs in SunView.  Follow the
installation procedures given in the \fIUNIX/IRAF Installation and Maintenance
Guide\fR.  The install script will run as described therein with the following
additional text and questions at the end:
.DS
\fL------------ Custom Suntools Installation -------------
install custom suntools (GTERM, IMTOOL)? (yes):\fR
.DE
Hit return to proceed with the installation of the custom \fLsuntools\fR
executable included with Sun/IRAF.  After a while the following question will
be printed:
.DS
\fLinstall custom version of /usr/bin/suntools executable? (yes):\fR
.DE
.LP
If you answer yes to this question, the IRAF version of \fLsuntools\fR is
installed as \fL/usr/bin/suntools\fR, replacing the standard Sun version of
suntools which is moved to \fL/usr/bin/suntools.BAK\fR.  The IRAF version of
suntools is identical to the standard Sun version except that the GTERM and
IMTOOL programs have also been linked in, increasing the size of the suntools
executable by a modest 50 Kb or so.  Two new UNIX commands \fLgterm\fR and
\fLimtool\fR are also added to \fL/usr/bin\fR; these are merely links to
the \fL/usr/bin/suntools\fR executable, like the standard SunView tools.
.PP
If you choose not to install the IRAF version of suntools in place of the
standard suntools executable, the \fLgterm\fR and \fLimtool\fR commands will
instead be created in the LOCALBIN directory (e.g., \fL/usr/local/bin\fR),
as links to the uninstalled custom IRAF version of suntools
\fL$iraf/local/suntools.e\fR.  This might be desirable if an incompatible
custom suntools executable has already been installed, or if the version of
Sun/IRAF being installed was linked for an older version of SunView than that
on the local system.  The disadvantage of this approach is that users may end
up running two versions of the suntools executable, which is very large
(e.g., 800 Kb), significantly increasing physical memory and swap space
requirements, and the possibility of excessive paging or filling up of the
swap area.  Adding GTERM and IMTOOL to the system suntools executable as we
have done will be the most efficient solution in most cases and is recommended
for production systems.\(dg
.FS
\(dgUser sites with special requirements may wish to construct their own custom
version of suntools, using the standard tools in \fL/usr/lib/libtoolmerge.a\fR,
and the GTERM and IMTOOL object modules supplied with Sun/IRAF.
The procedure for doing this is described in the SunView documentation;
see \fL$iraf/local/sun/suntools\fR for an example.
.FE

.NH
Floating Point Options
.PP
The Sun Fortran compiler currently provides five different choices for how the
compiler is to deal with floating point: \fB-fsoft\fR, \fB-fswitch\fR,
\fB-f68881\fR, \fB-ffpa\fR, and \fB-fsky\fR.  Which is best for a particular
program depends upon how much floating point computing the program does,
and upon the floating point hardware available on the machine on which the
program will be run.  If the wrong choice is made the program may run
inefficiently, or it may not run at all.
.PP
The IRAF system as currently distributed includes code compiled with the
-fsoft, -f68881, and -ffpa compiler options.  The -fsoft (software floating
point) option is used in the HSI (Host System Interface) code for maximum
portability, since this code does little if any floating point computing.
The -f68881 and -ffpa options are used in the main IRAF system since they are
the most efficient choice for this floating point intensive science software.
The -fswitch option is not used because it is significantly less efficient
than either -f68881 or -ffpa in some applications (-fswitch is hardware
independent but adds a fixed overhead of three branch instructions to each
floating point instruction in a program, whereas -f68881 and -ffpa generate
inline instructions for each of the two primary floating point hardware
options).  The -fsky option is useful only for the old Multibus based Sun-2
workstations, which are no longer directly supported by the IRAF project.
.PP
To summarize, there are two types of executables in IRAF: the system oriented
HSI executables, and the science and user oriented main IRAF executables.
The HSI executables are compiled for software floating point and hence will
run on any workstation.  The main IRAF executables are provided in two versions
to provide the most efficient software for the two main hardware floating point
options found on the Sun-3 workstations.  Whenever a user logs in, the IRAF
system will automatically select the most efficient binary version of IRAF
for the current host machine.  Note that even at a single site there may be
no single best choice, as multiple machines, operating with and without the
Sun FPA, may be simultaneously accessing the same version of IRAF.
.NH 2
Implementation
.PP
The two separate versions of the main IRAF system executables are maintained
in the logical directories \fLbin.f68881\fR and \fLbin.ffpa\fR in the IRAF
root directory.  These logical directories may be either symbolic links to
external directories (the default), or they may be ordinary subdirectories
of the root IRAF directory.  In some system configurations, e.g., a standalone
workstation, there will be only a single bin directory; in a custom system
configuration there could conceivably be more than two such binary versions
of the system, e.g., on a software development system one might want to
maintain one or more additional binary versions of the system compiled for
profiling.  Such customizations are easy to make, hence we shall concentrate
on the most common cases of one or two binary versions here.
.TS
center;
ci ci
l l.
object	function
.sp
\fLcl\fR	user command to start cl; symbolic link to $hlib/cl.csh
\fL$hlib/cl.csh\fR	cshell script to select IRAFBIN to be used, and start cl
\fLlogin.cl\fR	resets value of IRAF logical `bin' to value of IRAFBIN
\fLIRAFBIN\fR	unix pathname of bin directory to be used
\fLbin\fR	symbolic link to either bin.f68881 or bin.ffpa
\fLbin.f68881\fR	-f68881 objects and executables
\fLbin.ffpa\fR	-ffpa objects and executables
.TE
.PP
When the user types `\fLcl\fR' to start the CL in Sun/IRAF, the cshell script
\fL$hlib/cl.csh\fR is run, rather than directly executing the CL process.
The function of this script is to select the binary version of IRAF to be used,
indicate this selection by defining the unix environment variable IRAFBIN,
and exec the CL process in the indicated bin directory, i.e., execute
the file \fL${IRAFBIN}cl.e\fR.  When the CL runs it executes the commands in
the \fLlogin.cl\fR file, one of which transfers the value of IRAFBIN to the
IRAF logical directory `\fLbin\fR'.  Thereafter, all process spawns will
automatically execute processes in the bin subdirectory selected at login time.
Once in the CL, the command
.DS
\fLcl> show bin\fR
.DE
may be entered to see which bin subdirectory is being used.
.PP
Since there are only two bin subdirectories in the default system
configuration, making the best choice is easy; if the local host has an FPA,
\fLbin.ffpa\fR is used, otherwise \fLbin.f68881\fR is used (it is assumed that
any Sun-3 intended for scientific data processing is equipped with either the
68881 floating point coprocessor or the FPA).  If either subdirectory does
not exist then the other will automatically be used.  If the environment
variable IRAFBIN is defined before running the CL, runtime selection is
disabled, allowing the user to force one or the other version of the binaries
to be used, e.g., when updating or testing the software.
.NH 2
Maintaining Multiple Binary Versions
.PP
While dynamic runtime selection of the binary version of the system to be
used is easy to implement for production data processing, it is not so easy
for software development.  The problem is, that while all the runtime
executables are contained in a single bin directory which it is easy to
replicate, there are many object modules and object libraries scattered
throughout the system which get involved when software development takes place.
We could solve this problem by the brute force technique of replicating the
entire system, but that is not an acceptable solution due to the amount of
disk space involved, and the maintenance problems associated with multiple
versions of the same source files.
.PP
The current solution to this problem is to maintain only a single version of
the system for software development, which at any one time is configured for
a single floating point option.  To convert the system to a different floating
point option we simply archive and delete all the object modules and libraries
for the current floating point option, then restore the object modules and
libraries for the new configuration.  It would also have been possible to
have the system maintain multiple versions of each object module and library
throughout the system directories, but we rejected this option as being too
unwieldy and inefficient.
.LP
For example, to configure the system for software development using the FPA:
.DS
\fL% cd $iraf
% mkpkg ffpa\fR
.DE
The command to configure the system for the 68881 is \fLmkpkg f68881\fR.
Use \fLmkpkg showfloat\fR to find out the current configuration.
Only the IRAF system manager can change the system wide floating point
configuration.
Software development in a network of workstations will normally take place
with the system configured for the 68881, since the diskless nodes used for
software development are not likely to have the optional FPA hardware.
.PP
For example, one might work for some time with the system configured for the
68881, editing any number of files scattered throughout the system in the
process.  To update the FPA version of the system, one might enter the
following commands:
.DS
\fL% cd $iraf
% (mkpkg ffpa; mkpkg; mkpkg f68881) >& spool &\fR
.DE
This would update the FPA version of the system in the background, finding
and recompiling all modules which had been edited since the last update of
the FPA version of the system, meanwhile spooling the output in the file
\fLspool\fR for later review, and restoring the system to the default f68881
configuration when done.  Users could continue to use either version of the
system while this is taking place; switching the binary version of the system
used for software development has no effect on normal runtime execution of
the system.
.NH 2
Programming Notes
.PP
When developing programs within the IRAF environment (this includes both
SPP/VOS and IMFORT programs) programs must be compiled and linked with the
same float option as that used for the objects and libraries in the main IRAF
system, or a link error such as "Undefined: f68881_used" will result.
The easiest way to avoid this problem is to include the following command
in the \fL.login\fR file of anyone who uses the Sun/IRAF programming
environment:
.DS
\fLsetenv FLOAT_OPTION `ls -l $iraf/bin | sed -e 's+^.*bin\.++'`\fR
.DE
.LP
This will automatically sense the float option currently set in IRAF and
set FLOAT_OPTION to the same value.  The IRAF root directory is assumed to 
have been defined as the environment variable `\fLiraf\fR' before the command
shown is executed.  See \fL$iraf/local/.login\fR for examples.
.NH 2
Installing IRAF with a Single BIN Subdirectory
.PP
The special case of an IRAF system with a single bin subdirectory is probably
common enough to be worth discussing separately.  Configuring the system for
a single bin subdirectory is desirable to conserve disk space in cases where
all nodes in the local network which will be accessing IRAF run the same
floating point hardware, e.g., because there is only one node, or because
no nodes have the FPA.
.PP
Configuring the system for a single \fLbin.f68881\fR bin subdirectory is
easiest since the distributed system comes already configured for this option.
The procedure is as follows:
.RS
.IP \(bu
Restore the \fIiraf\fR and \fIbin.f68881\fR filesystems to disk,
setting the link \fLbin.f68881\fR in the root iraf directory.
.IP \(bu
Delete the link \fLbin.ffpa\fR in the root iraf directory.
.RE
.LP
To configure the system for a single \fLbin.ffpa\fR subdirectory:
.RS
.IP \(bu
Restore the \fIiraf\fR and \fIbin.ffpa\fR filesystems to disk,
setting the link \fLbin.ffpa\fR in the root iraf directory.
.IP \(bu
Delete the link \fLbin.f68881\fR in the root iraf directory.
.IP \(bu
Enter the command \fLmkpkg ffpa\fR from the root iraf directory.
.RE
.LP
When the latter command is entered the system will warn that the f68881
objects are being deleted without being archived, but this not a problem
since you will not be using the f68881 version of the system anyhow.
.NH 2
Custom Installations
.PP
The Sun/IRAF distribution is intended to include support for the most popular
floating point options so that user sites can simply install and use the
binary distribution without having to recompile the system.  Nonetheless,
since the system configuration procedures are fully automated it is not
difficult to customize the installation should one need to do so.
.PP
For example, suppose we want to extend the system by adding support for
a binary version of the system compiled for \fL-fswitch\fR.  This could be
done with the following sequence of commands (entered while logged in
as `\fLiraf\fR'):
.DS
.TS
l l.
\fL% cd $iraf\fR
\fL% mkdir \fIbindir\fP/bin.fswitch\fR	# create \fLbin.fswitch\fR directory
\fL% ln -s \fIbindir\fP/bin.fswitch .\fR	# link it to iraf
\fL% $hlib/mkfloat.csh fswitch\fR	# configure iraf for -fswitch
\fL% mkpkg >& spool &\fR	# recompile the system
.TE
.DE
.PP
Recompiling the full system takes 12-24 hours depending upon the speed of
the host system.  Note the explicit use of the \fLmkfloat.csh\fR script to
reconfigure IRAF for the new floating point type; a better solution would be
to edit the root \fLmkpkg\fR file to add support for the command
"\fLmkpkg fswitch\fR".  If by adding support for the new float option we
obsolete any of the original ones, they may be deleted merely by deleting
the bin directory and the associated link in the iraf root (be sure to
change the float option for the system to something else first!).

.NH
Graphics and Image Display Under SunView
.PP
Sun/IRAF may be used from an ordinary video or graphics terminal, or from
a Sun console running either as a terminal or under the SunView windowing
system (IRAF can also be run from other window systems, e.g., X or NeWS,
but we do not directly support these yet).  Operation of the system from a
conventional terminal or from the Sun console in terminal emulation mode
is the same as for generic BSD UNIX/IRAF, hence does not concern us here.
.PP
The standard SunView system provides two window tools which can be used to
run IRAF, \fIshelltool\fR and \fItektool\fR.  \fIshelltool\fR is ok but cannot
do graphics.  \fItektool\fR can do graphics but is too inflexible, and is
not much good as a terminal since it cannot scroll or erase text.
To provide a reasonable combination of video terminal and vector graphics
capabilities, we have extended SunView by adding a general purpose virtual
graphics terminal window tool called \fIgterm\fR.  A simple but useful
prototype image display server called \fIimtool\fR is also included in the
current Sun/IRAF release.
.PP
Both programs are implemented at the SunView level as general purpose window
tools, and are potentially useful independently of IRAF (especially
\fIgterm\fR, which is upwards compatible with \fIshelltool\fR but with a
Tektronix compatible graphics capability added).
Detailed documentation on the basic operation and use of these programs is
given in the \fIgterm\fR(l) and \fIimtool\fR(1) manual pages.
Our concern in this document is with the interaction with and use of these
programs in IRAF.
.NH 2
Getting Started with Windows
.PP
The graphics and image display tools provided with IRAF operate within the
SunView windowing environment much like the standard tools provided with
SunView.  It would be inappropriate to try to document the basic operation
of SunView here.  For those who are familiar with UNIX but have little
experience with the window system, the \fIsuntools\fR(1) manual page is a
good place to start.  This tells how to start up the window system, explains
the basic mouse and function key operations used to dynamically adjust windows,
and how to set up the \fB.suntools\fR file so that the windows come up the
way you want when you start suntools.
.PP
Someone just getting started may want to copy
the \fL.suntools\fR, \fL.defaults\fR, \fL.ttyswrc\fR, \fL.rootmenu\fR, etc.,
files in \fL$iraf/local\fR to get started.
These should serve only as a starting
point; there is no one best window configuration for every user.
Everyone customizes the windows to suit their own preferences and type of work.
The window system is extremely dynamic, and the experienced user may make
frequent changes to the positions, sizes, number, iconic or open state, etc.,
of the their windows depending upon what they are doing.  The examples may
help suggest some possibilities which would otherwise go unnoticed.
.NH 2
Vector Graphics Capabilities
.PP
Virtually everything in IRAF can be run on a Sun workstation by running the
CL in a \fIgterm\fR graphics window.  To IRAF, \fIgterm\fR appears to be
just another graphics terminal, hence it is driven by the STDGRAPH graphics
kernel, has its own set of logical device entries in the \fLgraphcap\fR file,
and is driven from the CL via an ASCII data stream interface consisting of a
mixture of text and graphics commands and data.
.PP
The following IRAF logical device names are defined for the \fIgterm\fR
virtual graphics terminal:
.KS
.TS
center;
l l.
\fLgterm\fR	same as \fLgterm40\fR
\fLgterm24\fR	gterm with 24x80 text window (like a vt100)
\fLgterm34\fR	gterm with 34x80 text window (the Sun default)
\fLgterm40\fR	gterm with 40x80 text window (the IRAF default)
\fLgterm54\fR	gterm with 54x80 text window (tallest possible window)
.TE
.KE
.PP
A set of standard text window sizes are provided to make it as easy as
possible to tell IRAF the window size (IRAF currently cannot automatically
sense changes in the size of the terminal text window).  In addition,
we recommend setting up a \fL.ttyswrc\fR file defining function keys
to change the window size, e.g. (this example is taken from the file
\fL$iraf/local/.ttyswrc\fR):
.KS
.DS
\fLmapi F3 =gcur\\\\n
mapi F4 =imcur\\\\n
mapo R1 ^[[8;24;80t
mapo R2 ^[[8;34;80t
mapo R3 ^[[8;40;80t
mapo R5 ^[[8;54;80t\fR
.DE
.KE
.PP
In this case, F3 and F4 provide single keystroke entry to cursor mode,
and R1, R2, R3, and R5 are for resizing the window.  For example, if R1 is
hit to change the text window size to 24 lines, one would need to enter
the following command in the CL to inform it of the new window size:
.DS
\fLcl> stty gterm24\fR
.DE
.LP
Arbitrary size windows may also be set up if desired, and these are
occasionally useful, e.g., for viewing 132 column wide tabular output.
Use a command such as \fLstty nlines=54 ncols=132\fR to inform IRAF of
the new window size, so that commands like \fLpage\fR and \fLhelp\fR will
work properly.
.PP
Note that the graphics window may also be resized and moved about on the
screen.  A number of predefined standard window sizes are provided via the
gterm \fIsetup panel\fR, ranging from pretty small to the full screen.
The graphics window may also be interactively adjusted to some arbitrary
size, but the advantage of the predefined window sizes is that they all
have the same standard aspect ratio and size in characters (35x80).
The F8 and F9 function keys are used to direct output to the graphics window
and back to the text window, and to clear either window.  Hardcopies may
be made of either window or the full screen via menu selection in the
\fIframe menu\fR.  Multiple IRAF sessions running in multiple gterm windows
are possible; using different colors makes it easier to remember which window
is which. 
.PP
Aside from the dynamic nature of windows in the SunView environment,
operation of IRAF from a gterm window is straightforward and should present
no problems for someone already familiar with the use of IRAF on a conventional
graphics terminal.  Further information is given in the gterm manual page,
\fIgterm\fR(l).
.NH 2
Image Display Capabilities
.PP
A basic image display capability is provided for
IRAF running in the SunView environment by the prototype \fIimtool\fR
display server.  The current \fIimtool\fR program provides a basic display
capability, including programmed access from the IRAF environment to load
images, interactive windowing of the display, pseudocolor, pseudo-greyscale
image hardcopy, and a rudimentary image cursor readback capability.
\fIimtool\fR runs as a display server, meaning that it sits idle most of
the time, waiting for some client, e.g., IRAF, to send it an image to be
displayed via some form of interprocess communication.  The current program
has some serious limitations, e.g., there is only a single frame buffer which
is fixed in size at 512 square, overlaid graphics and text is not supported,
zoom, pan, blink, and split screen are not provided, and fully interactive
cursor readback is not implemented.  We plan to implement all of these
features in a future version of the program.
.PP
The IRAF interface to the display server is the \fIdisplay\fR program in
the TV package.  Since reading from the display is not currently implemented,
only entire frames can be displayed; writing a series of subrasters to the
display in separate steps is not supported and will cause the display program
to hang up trying to read from the display.  None of the programs in the TV
package other than \fIdisplay\fR is supported for use with \fIimtool\fR,
but since most interaction with the display is via the mouse these old
programs would not be very useful anyhow.
.PP
The display server has the capability of displaying the cursor (mouse)
position in world (image pixel) coordinates as the mouse is moved about
in the window.  In addition, text file cursor lists can be generated and
displayed.  For this to work properly the user must define the UNIX
environment variable WCSDIR, e.g., by including a statement such as the
following in their \fL.login\fR file:
.DS
\fLsetenv WCSDIR /\fIuserdir\fP/iraf/uparm/\fR
.DE
This defines the directory into which the \fIdisplay\fR program is to place
the WCS (world coordinate system) information when an image is displayed,
and from which \fIimtool\fR is to retrieve the WCS information to be able
to map screen coordinates to image pixel coordinates.  Any directory may
be used, provided the user has write permission on the directory; \fLuparm\fR
is an obvious choice, as shown in the example.  Note the trailing /.
.PP
By default, the cursor lists generated or read by imtool will be placed in
WCSDIR; this can be overridden in the imtool \fIsetup panel\fR if desired.
The cursor lists generated by imtool may be input to IRAF programs which
read the image cursor \fIimcur\fR by redirecting the cursor onto a list file.
For example, if \fLuparm$frame.1\fR is a cursor list file,
.DS
\fLcl> cl.imcur = "uparm$frame.1"\fR
.DE
will cause the global CL image cursor parameter \fIimcur\fR to take cursor
input from the named file, rather than trying to read the image cursor
interactively (which is not yet implemented).  Tasks which read the image
cursor will normally have their own private \fIimcur\fR type parameters
which may be redirected onto a list in the same fashion.
.PP
To avoid having a program try to read the hardware image cursor interactively
when the cursor parameter is not redirected onto a list, it is recommended
that the following command be entered to cause the CL to satisfy interactive
image cursor reads by having the user type in the cursor value on the
keyboard:
.DS
\fLcl> reset stdimcur = "text"\fR
.DE
Since \fIimtool\fR can be set up to constantly display the cursor coordinates
in image pixel coordinates, it is not difficult to read the coordinates being
displayed in the image window, and type them into the CL running in the
\fIgterm\fR text window.  To do this, you will have to move the mouse back
to the gterm window (this may not be necessary if "click-to-type" is enabled);
to do so without losing the image cursor coordinates, hold down the middle
mouse button while moving the mouse back to the text window.
.PP
Further information on the use of cursors in IRAF is given in the \fLcursors\fR
manual page in IRAF, and in the \fIGIO Design Document\fR.
Further information on the operation of imtool is given in the online manual
page, i.e., \fIimtool\fR(l).
.NH 2
Using the Workstation with a Remote Compute Server
.PP
The most common mode of operation with a workstation is to run Sun/IRAF under
SunView on the workstation, accessing files either on a local disk, or on a
remote disk via a network interface (ND, NFS, IRAFKS, etc.).  It is also
possible, however, to run SunView on the workstation, but to run IRAF on
a remote node, e.g., some powerful compute server such as a large VAX,
a minisupercomputer such as an Alliant or Convex, or even a Cray, possibly
quite some distance away.  This is done by logging onto the workstation,
starting up SunView and a \fIgterm\fR window, logging onto the remote machine
with \fIrlogin\fR, \fItelnet\fR, or whatever, and starting up IRAF on the
remote node.  After IRAF comes up you need only type \fLstty gterm40\fR to
tell the remote IRAF that you are running in a gterm window.
.PP
In this mode one is effectively using the workstation as a sort of super
terminal with powerful graphics and image display capabilities.  You get
the best of both worlds, i.e., a state of the art user interface, and the
compute power of a large machine.  It matters very little what operating
system is actually running on the remote machine, so long as it also runs
IRAF.  Except for the details of the login sequence, operation is completely
transparent; gterm does not care whether the process it is talking to is
on a local or remote node.  The only drawback is that the current prototype
display server can only be used with processes running on the local node.
This will be fixed in the final display server, which will provide full
network transparency (even with the prototype it is possible to arrange
a network interface via the IRAFKS, but it probably isn't worth it).

.NH
Host System Requirements
.PP
Most installations of Sun/IRAF are currently on the Sun-3 series of machines.
There have been a few Sun-2 installations as well; it is not difficult to
get IRAF running on a Sun-2, but the new machines are so much faster for
scientific applications that it is hardly worth the trouble.  NOAO has a
Sun-4 on order, hence we will be able to support IRAF on this machine about
as soon as it becomes available to the community.
.PP
All current installations have been on workstations with 1192x900 screens.
There should be no serious problems running the software on larger screens,
but we have not yet been able to test this option.  There does not seem to
be much difference between the 8 and 10 bit framebuffers, although when we
start overlaying graphics in the image display the 10 bit framebuffer should
have an advantage.  A color workstation is recommended for most scientific
applications; you need the multiple bitplanes for image display anyhow, and
a color monitor does not cost that much more.  Color graphics is more pleasant
and causes less eye strain when working with the workstation for long periods
of time.  We do not yet make much use of color graphics in astronomical
applications, but eventually it is sure to become the norm.
.PP
The Apple Laserwriter option (or any comparable laser printer with a Postscript
interface) is recommended.  This device represents one of the best buys
currently available in the laser printer market.  It is a smart device
with a sophisticated Postscript interface which can be used for screencopies,
including pseudo-greyscale image hardcopy, for high quality graphics output,
and for high quality typeset text.
.PP
The optional FPA makes about a factor of three difference in performance in
many floating point intensive applications, possibly more in highly specialized
applications.  It is probably well worth getting for a production data
processing system, but it is probably not worthwhile for personal systems.
Starting with the Sun-4 series the fast floating point is integrated into the
main cpu board, and it seems likely that this trend will continue, eventually
reducing the (currently ridiculous) number of floating point options.
.PP
We still have not realized the full potential of the interactive window system
running on a color display for image processing.  It is our feeling at this
point that the image processing capability of the standard color workstation
is great enough to serve the needs of most people without having to buy a
separate dedicated image display device.  Eventually everyone will have a
color image display capability in their desktop workstation, and dedicated
image displays will continue to be reserved for specialized applications.
.NH 2
Memory Requirements
.PP
The windowing systems used in these workstations tend to be very memory
intensive (especially the current implementation of SunView).
Interactive performance will suffer greatly if the system pages a lot.
Memory is getting cheaper fast.  No Sun-3 system, including personal diskless
nodes, should be configured with less than 8 Mb of main memory;
12-16 Mb is recommended if you plan to do a lot of image processing.
The Sun-4s have a RISC architecture which is likely
to increase the memory requirements of the system, possibly by as much as a
factor of two.  We will get some of this back when shared libraries are
introduced, but our applications programs will use more memory in the future
(for greater speed), so plan on large memories.
.NH 2
Disk Requirements
.PP
The amount of disk required by a user depends greatly on the application,
so it is hard to recommend a minimum disk size.  For an image processing
system the minimum one can safely recommend is probably one large Eagle
class disk, i.e., about 400-600 Mb.  If the system is intended for serious
data processing, 1-2 Gb is recommended.  With a 140 Mb disk you might have
to take drastic measures to even get all the software to run, let alone find
space for data so that you can use the system to do something useful.
Don't forget the swap area, required by the large amount of virtual memory
used by the windowing system; the minimum safe swap area size is something
like 30 Mb.  Plan on at least 140 Mb just for the software and maybe a swap
area.  NFS offers a cheap way to gain access to larger amounts of disk,
and the performance is not bad.
.NH 2
Diskless Nodes
.PP
Diskless nodes offer a highly cost effective way to provide users with a
great user interface and a lot of personal computing power at minimum cost.
The i/o performance of a diskless node is significantly less than that of a
disked node, but so long as the node is equipped with sufficient main memory
to avoid excessive paging, interactive performance should be fine.
The performance for image processing is acceptable in most circumstances,
particularly if the applications being run tend to be cpu bound (this is
usually the case for an interactive system being used for data analysis).
It is easy to offload the occasional i/o bound application to execute directly
on the remote file or compute server, if i/o bandwidth is a problem.
