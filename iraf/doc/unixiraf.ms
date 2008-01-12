.RP
.TL
UNIX/IRAF Installation and Maintenance Guide
.AU
Doug Tody
.br
Steve Rooke
.AI
.K2 "" "" "*"
.br
August 1987

.AB
The procedure for installing IRAF on a UNIX host is described.
Procedures are given both for the initial system installation,
and for updating an existing installation with a new version of IRAF.
The interfaces for site specific devices such as video terminals,
graphics terminals, printers, plotters, image displays, and so on are
discussed, and the steps to be taken to interface a new device are
described.
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
2.\h'|0.4i'\fBInstalling the System\fP\l'|5.6i.'\0\02
.br
\h'|0.4i'2.1.\h'|0.9i'Initial System Installation\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.1.\h'|1.5i'Create the `iraf' account\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.2.\h'|1.5i'Read the distribution tape\l'|5.6i.'\0\03
.br
\h'|0.9i'2.1.3.\h'|1.5i'Run the INSTALL script\l'|5.6i.'\0\03
.br
\h'|0.9i'2.1.4.\h'|1.5i'Relink the system if necessary\l'|5.6i.'\0\04
.br
\h'|0.4i'2.2.\h'|0.9i'Updating an Existing IRAF Installation\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.1.\h'|1.5i'Save Locally Modified Files\l'|5.6i.'\0\05
.br
\h'|0.9i'2.2.2.\h'|1.5i'Read the Distribution Tape or Tapes\l'|5.6i.'\0\05
.br
\h'|0.9i'2.2.3.\h'|1.5i'Restore Site-Dependent Files\l'|5.6i.'\0\05
.br
\h'|0.9i'2.2.4.\h'|1.5i'Run the INSTALL Script\l'|5.6i.'\0\06
.br
\h'|0.4i'2.3.\h'|0.9i'Configuring the IRAF Device Tables\l'|5.6i.'\0\07
.br
\h'|0.9i'2.3.1.\h'|1.5i'$hlib/zzsetenv.def\l'|5.6i.'\0\07
.br
\h'|0.9i'2.3.2.\h'|1.5i'$iraf/dev/devices\l'|5.6i.'\0\07
.br
\h'|0.9i'2.3.3.\h'|1.5i'$iraf/dev/termcap\l'|5.6i.'\0\07
.br
\h'|0.9i'2.3.4.\h'|1.5i'$iraf/dev/graphcap\l'|5.6i.'\0\07
.br
\h'|0.9i'2.3.5.\h'|1.5i'Network Tables\l'|5.6i.'\0\08
.sp
3.\h'|0.4i'\fBTesting the System\fP\l'|5.6i.'\0\08
.sp
4.\h'|0.4i'\fBReconfiguring the System\fP\l'|5.6i.'\0\09
.br
\h'|0.4i'4.1.\h'|0.9i'Bootstrapping the System\l'|5.6i.'\0\09
.br
\h'|0.4i'4.2.\h'|0.9i'Relinking the System\l'|5.6i.'\0\09
.br
\h'|0.9i'4.2.1.\h'|1.5i'Relinking individual packages\l'|5.6i.'\0\010
.br
\h'|0.4i'4.3.\h'|0.9i'Autogeneration of the System (SYSGEN)\l'|5.6i.'\0\010
.br
\h'|0.4i'4.4.\h'|0.9i'New Ports\l'|5.6i.'\0\011
.sp
5.\h'|0.4i'\fBTuning Considerations\fP\l'|5.6i.'\0\012
.br
\h'|0.4i'5.1.\h'|0.9i'Precompiling TERMCAP and GRAPHCAP Entries\l'|5.6i.'\0\012
.br
\h'|0.4i'5.2.\h'|0.9i'Stripping the System to Reduce Disk Consumption\l'|5.6i.'\0\012
.sp
6.\h'|0.4i'\fBInterfacing New Graphics Devices\fP\l'|5.6i.'\0\013
.br
\h'|0.4i'6.1.\h'|0.9i'Graphics Terminals\l'|5.6i.'\0\013
.br
\h'|0.4i'6.2.\h'|0.9i'Graphics Plotters\l'|5.6i.'\0\013
.br
\h'|0.4i'6.3.\h'|0.9i'Image Displays\l'|5.6i.'\0\014
.sp
7.\h'|0.4i'\fBThe IRAF Directory Structure\fP\l'|5.6i.'\0\014

.nr PN 0
.bp
.NH
Introduction
.PP
The procedure for installing IRAF on a UNIX host is complicated by the great
variety of computers which run UNIX, by the many small differences in the
operating system software on these machines, and by the differences in the
hardware configurations of the various machines.  Fortunately, however,
most sites installing UNIX/IRAF will be installing IRAF on a system which
is already directly supported by NOAO, in which case the installation should
be straightforward.
.PP
This system installation and maintenance guide describes those operations and
procedures which are the same regardless of the peculiarities of each vendor's
UNIX implementation.  To install or update IRAF on a specific machine you
will need both this manual and the installation notes for your particular
host system.  For example, to install IRAF on a Sun workstation you will
follow the instructions given in this manual, plus those given in the
\fISun/IRAF Installation Notes\fR.
.PP
The distribution tape is a snapshot of one of our local (NOAO/Tucson) UNIX/IRAF
systems.  The device and system configuration tables come configured for our
system, and will have to be modified as part of the installation process.
These modifications are discussed in detail in this document.
To simplify the installation process as well as future upgrades, we have tried
to isolate the site dependent files to the minimum number of directories, i.e.,
\fLdev\fR, \fLhlib\fR (a subdirectory of \fL$iraf/unix\fR), and \fLlocal\fR and its
subdirectories.  The remainder of the system should not require any
modifications.
.sp
.TS
center;
cb s s
l l l.
IRAF HOTLINE
.sp
telephone	\fL(602) 323-4160\fP
internet	iraf@noao.arizona.edu
BITnet	iraf@noao.arizona.edu
	    (via Wisconsin gateway until 1 Dec 87)
SPAN/HEPnet (DECnet)	draco::iraf or 5356::iraf
UUCP/Usenet	seismo!noao!iraf, ihnp4!noao!iraf,
	    uunet!noao.arizona.edu!iraf (after 1 Sept 87)
.TE
.PP
An installation typically takes about an hour, somewhat longer if relinking is
necessary.  Interfacing new graphics terminals, plotters, or image displays
can take considerably longer, of course, and we will only touch upon such
matters in the installation guide.  Feel free to contact the IRAF hotline for
assistance if any problems should arise during the installation, or for help
in interfacing new devices.

.NH
Installing the System
.PP
The procedures to be followed to install the system depend upon whether you
are installing IRAF for the first time, or merely updating an existing IRAF
installation.  If you updating an existing installation to the latest IRAF
release you have less work to do, as you will have already modified the
site-dependent files as part of the previous installation.
Proceed to \(sc2.2 if that is the case.

.NH 2
Initial System Installation
.PP
This section documents the procedure for installing a binary distribution
tape prepared on a compatible (VAX, Sun, or Microvax) system at NOAO.
The procedure outlined in this section is also used to install a source
distribution, the difference being that when one is done it is still
necessary to do a sysgen to compile and link the system.
If you have already installed IRAF and are merely installing a new release
of the system, proceed to \(sc2.2 instead of following the procedure
outlined here.
.LP
The basic system installation procedure is as follows:
.RS
.IP \(bu
create the `iraf' account
.IP \(bu
read the distribution tape or tapes (UNIX \fItar\fR format)
.IP \(bu
run the \fLinstall\fR script
.IP \(bu
configure the device tables
.RE
.PP
This should suffice for the basic installation of a full binary distribution.
If executables were not included with the distribution (a "you relink" type
distribution) then relinking will also be necessary, but this is easy.
In some cases it may be desirable to delete all binaries and recompile the
entire system, e.g., to reconfigure the system for different floating point
hardware.  Once the basic system is up and running it will still be necessary
to provide interfaces for the terminals, printers, plotters, and so on in
use at your site, in order to have a fully functional system.

.NH 3
Create the `iraf' account
.PP
The first step is to set up an account for user `iraf'.
IRAF is a big system and we do not recommend trying to install it as a
subdirectory of someone's personal account.  IRAF system maintenance
should be performed by the IRAF system manager while logged into the `iraf'
account, to ensure that the environment is properly configured to run the
system management tools.  Ordinary users should not have write permissions
on the IRAF directories to avoid accidental deletion of critical files.
.PP
The pathname to the root directory does not matter, but it is wise to keep
it short to speed up pathname resolution and to minimize the possibility
of filename truncation.  A typical IRAF root directory is "\fL/usr/iraf\fR"
or "\fL/u/iraf\fR".  The account should be set up to use \fL/bin/csh\fR as the
standard shell.  The disk partition containing IRAF should have about 60 Mb
of space available for the system; if necessary, the system can be stripped
later to save space, but the full 60 Mb is required during system installation.
.PP
In what follows, we refer to the IRAF root directory as "\fL$iraf\fR", as if it
were a cshell \fIsetenv\fR environment variable.  Note that during the
initial stages of the installation this variable may not yet be defined,
hence you should be prepared to manually substitute the actual pathname
when entering the commands shown.
.PP
Contrary to common practice,
the login directory for `iraf' is not \fL$iraf\fR as one would expect,
but \fL$iraf/local\fR.  This is done so that all local files may be kept
in one place, apart from the standard system.  This simplifies system
updates and makes it easier to keep track of locally added or modified files.
When creating the iraf account be sure to set up the root directory in this
way, else when you later login as `iraf' you will not access the
standard \fL.login\fR etc. files in the \fL$iraf/local\fR directory.

.NH 3
Read the distribution tape
.PP
The system is distributed on one or more UNIX \fItar\fR format tapes or
cassettes.  Login as IRAF, go to the IRAF root directory, and enter the
\fLtar\fR command shown for each tape in the distribution kit.
The order in which the tapes are read is not important.
Note that the \fL$iraf/local\fR directory, \fL.login\fR file,
etc., will not be created
until the tape is read, so the login will not be completely successful,
but this is harmless.  Note also that the environment variable \fIiraf\fR
has probably not yet been defined, so you must substitute the actual
pathname to the iraf root directory when entering the commands shown below.
.DS
\fL% cd $iraf
% tar -xpf /dev/\fIdevice\fR
.DE
The full system contains something like 7000 files in over 250 directories;
printing the filenames (e.g., with \fLtar -tv\fR) as the tape is read is not
recommended, as this can slow things down considerably.
It typically takes a half hour or so to restore the tape or tapes to disk.
.NH 3
Run the INSTALL script
.PP
Once the system has been restored onto disk, most of the remaining work
required for the basic installation is done by the semi-automated \fIinstall\fR
script supplied with the system.  The install script edits various IRAF system
configuration files to reflect the pathname to IRAF on the local system,
and installs a few links in standard UNIX directories to define the UNIX
level tasks necessary for the user to start up IRAF.
.PP
The install script may be run in "do-nothing" mode by specifying the -n
option on the command line, as in the example below.  This is recommended
the first time the script is run so that you can see what questions will be
asked and what the script will do, without actually doing anything.
.DS
\fL% cd $iraf/unix/hlib
% install -n\fR
.DE
The script will make educated guesses regarding the pathname to the iraf
root directory, the pathname to the UNIX directory where locally added
commands are normally put, and the pathname to the public scratch directory
(\fLimdir\fR) where bulk image data is to be stored,
and then ask for you to enter
the pathnames to these directories, prompting with the values it has guessed.
If the prompted value is acceptable hit return to accept it, otherwise
you should enter a new value.
.PP
As mentioned earlier, typical values for the IRAF root directory are
\fL/usr/iraf\fR or \fL/u/iraf\fR.
If the actual directory is more obscure the script
will fail to find it and you must enter the actual path.  Locally added UNIX
level tasks are most commonly put in a directory like \fL/usr/local/bin\fR,
but any directory in the normal user's search path will do, even \fL/usr/bin\fR
if you have not defined a \fLlocal/bin\fR directory on your system.
.PP
Most likely the script will not find any good place for \fLimdir\fR,
and will prompt
with \fL/tmp\fR for lack of anything better.  This will work during the initial
system testing, but is not recommended as a permanent solution as your image
files will be deleted every time the system is rebooted.  On the other hand,
we do not recommend storing bulk image data directly in the user directories
as the disk management policies for large but short-lived data files tend to
be different than those for small but long-lived user files.  The best solution
is to set aside a large area (on our system it is \fL/tmp2\fR,
\fL/tmp3\fR, etc.) which
is not backed up daily or weekly, and in which files which have not been
modified for two weeks or so are automatically deleted.  Another advantage of
a separate area for large data files on BSD derived systems is that when you
make the files system you can specify a larger than normal file system block
size to increase the i/o bandwidth to disk.
.LP
Once you are happy with the installation procedure, become super user and
run the install script for real, without leaving the \fLhlib\fR directory:
.DS
\fL% su
% install\fR
.DE
You should now log out entirely and log back in as `iraf'.
Type "\fLecho $iraf\fR"
to verify that the system knows what the root directory is.  If you have
installed a full binary distribution on a compatible machine you should be
able to run IRAF by entering the commands \fBmkiraf\fR to configure the IRAF
environment for user `iraf' (while still in the \fL$iraf/local\fR login
directory) and \fBcl\fR to start up the IRAF Command Language (CL).
If the CL does not run you must relink or recompile and relink the system,
as described in the next section.

.NH 3
Relink the system if necessary
.PP
Relinking the IRAF system is necessary if the distribution was shipped
without the executables in the \fLbin\fR directory (e.g. a `you-relink'
system).  In order to relink, the system must first have been
\fIbootstrapped\fR; see \(sc4.1 if in doubt or if it is necessary
to perform a bootstrap.  `You-relink' and of course `load-and-go'
distributions are already bootstrapped; source only distributions are not.
.PP
Relinking is covered in greater detail in \(sc4.2.  Briefly, to relink the
entire system while spooling the output, run \fLmkpkg\fR from the IRAF root
directory:
.DS
\fL% cd $iraf
% mkpkg >& spool &\fR
.DE
See \(sc4.3 for information on inspecting a spoolfile for errors.
A relink-only sysgen might take a half hour or so depending on the
hardware configuration and system loading.
.PP
If IRAF is being installed on a host machine which is binary incompatible
with the distributed system (e.g., because the local host uses floating point
hardware not available at NOAO) then it may be necessary to strip the binaries
and recompile the system from scratch.  Recompiling the full system can easily
take an entire day (of computer time, not person time).

.NH 2
Updating an Existing IRAF Installation
.PP
Skip to \(sc2.3 if you are installing IRAF for the first time.  This section
is for sites upgrading to a new version of IRAF.
.PP
Updating a UNIX/IRAF installation is similar to the initial installation
procedure.  The main difference is that local modifications made to the
site dependent files should be saved and then merged back into the generic
system from the distribution tape.  The typical update process consists of
the following steps:
.RS
.IP \(bu
Save any locally modified files.
.IP \(bu
Delete the old system.
.IP \(bu
Read the distribution tape.
.IP \(bu
Restore site-dependent files.
.IP \(bu
Run the \fLinstall\fR script.
.IP \(bu
Relink the system if necessary.
.RE
.NH 3
Save Locally Modified Files
.PP
Login as IRAF.  Ordinarily the only directories containing files you may wish
to save are \fLdev\fR, \fLhlib\fR (\fL$iraf/unix/hlib\fR), and \fLlocal\fR.
The safest and easiest way to do this is to save the entire contents of
those directories.  For example:
.DS
\fL% cd $iraf
% mkdir O
% mv dev local unix/hlib O\fR
.DE
Many variations on this are of course possible.  The basic idea is to move
the old directories to a nonstandard place so that they are not deleted or
overwritten when we install the new system.
.NH 3
Read the Distribution Tape or Tapes
.PP
Having temporarily preserved all directories containing the locally modified
files, it is now time to delete the old system and read in the new one.  If
you are the cautious type you may wish first to preserve the entire
existing IRAF system on an archive tape (e.g. if something were to have
happened to the distribution tape en route).  The old system may be deleted
as follows (assuming IRAF owns all the files in \fL$iraf/*\fR):
.DS
\fL% cd $iraf
% rm -rf [a-z]*\fR
.DE
This will preserve the old site local files in subdirectory "O".
Now read the distribution tape or tapes:
.DS
\fL% tar -xpf /dev/\fIdevice\fR
.DE
.LP
Repeat this for each tape in the distribution set.  The order in which the
tapes are restored is not important.
.NH 3
Restore Site-Dependent Files
.PP
You can either merge the contents of locally modified files saved from
the previous installation into their counterparts in the new system, or edit
the new ones from scratch.  Whichever is easier depends on how much editing
you had to do in the first place, which probably depends upon the complexity
of your local computer network and the number of peripheral devices.
Any of the site configuration files might have been modified in the new
release of IRAF, so in general one cannot simply replace such a file with
the one that was saved.
.PP
Merging means inspecting the two files (newly distributed vs. its
saved counterpart) for differences, and deciding how to update the new
file with local device names, directory pathnames, etc.
The UNIX "\fLdiff\fR" utility may be useful for this,
depending on how much a file has changed, and how extensively it
was modified locally.  One thing that is a big help is to keep track of
all local changes made to the standard system in a local "notes" file;
when the next system update occurs or when the installation is repeated on
another cpu at the local site, one can then go down the list and make all
the same changes to the newly installed system.
.KS
.TS
center;
ci ci ci
l l l.
directory	files	[and possibly]
.sp
\fLdev\fR	devices, graphcap, termcap	[, hosts, hostlogin, uhosts]
\fLhlib\fR	zzsetenv.def	[, mkpkg.inc, mkpkg.sf]
\fLlocal\fR	(any local additions)	[, all . (hidden) files]
.TE
.KE
.PP
The files which should be edited, via a diff/merge operation on the saved
files or otherwise, are summarized in the table above.
It is likely that only some of these files will have been modified at a
given site; the ones in brackets are used at IRAF networking sites, for
nonstandard compilation, and possibly window-system startup files for
workstations.  In principle the entire contents of the \fLlocal\fR
directories are site-dependent and may be preserved in toto during an
update, but this is not true for all versions of IRAF, and sometimes
we add things to these directories which user sites may be useful to user
sites as well.
.PP
The \fLinstall\fR script, new in IRAF V2.5, takes care of the editing
that previously was done manually on such files as \fLhlib$mkiraf.csh\fR
and \fLhlib$libc/iraf.h\fR, so it is not necessary to diff/merge these files.
.NH 3
Run the INSTALL Script
.PP
Use of the \fLinstall\fR script is described in detail in \(sc2.1.3, and is
summarized only briefly here.  To preview its effects (still logged in as
`iraf'):
.DS
\fL% cd $iraf/unix/hlib
% install -n\fR
.DE
If you are satisfied, login as superuser and execute it for real, still
in the \fLhlib\fR directory:
.DS
\fL% su
% install\fR
.DE
Then log out entirely and back in as `iraf'.  Verify that the system
knows where the root directory is, using "\fLecho $iraf\fR".  At this
point you can test the system as described in the last paragraph of
\(sc2.1.3.  If everything works, the installation is complete, and the
remainder of this manual will serve for system maintenance or when
device configurations change.  If the CL does not run, you must relink
or recompile and relink the system as described in \(sc2.1.4.

.NH 2
Configuring the IRAF Device Tables
.PP
The following files should now be edited to define the default terminal,
printer, editor, and so on for your system.  Any part of this can be left
until later if desired.
.NH 3
$hlib/zzsetenv.def
.PP
This file contains the name of the default editor, the default names of all
the standard devices, and a number of other definitions which are not site
dependent and which can therefore be ignored.  To be accessible by the IRAF
system, each local device named must also have an entry in the \fLtermcap\fR
file (terminals and printers) or \fLgraphcap\fR file (graphics terminals and
image displays) in \fLdev\fR.  There must also be an \fIeditor\fL.ed\fR
file in \fLdev\fR
for the named editor; EDT, EMACS, and VI are currently supported.
Edit the string to the right of the equals sign for the following entries.
Sample values are shown.
.DS
\fLset editor	= "vi"
set printer	= "imagen"
set stdgraph	= "vt640"
set stdimage	= "iism70l"
set stdplot	= "versatec"
set terminal	= "vt640"\fR
.DE
For example, you may wish to change the default editor to "\fLemacs\fR",
the default printer to "\fLversatec\fR", the default image display to
"\fLiism75\fR", and the default terminal to "\fLvt100\fR".  Note that the set of
devices for which interfaces are already available is steadily growing
but always finite.  The issues of interfacing new graphics and image
display devices are discussed further in \(sc6.
.NH 3
$iraf/dev/devices
.PP
This file contains a list of the allocatable devices (primarily tape drives)
for the local system.  It should be obvious how to change it by reading the
comments in the file and studying the current values, which are for our system.
.NH 3
$iraf/dev/termcap
.PP
There must be entries in this file for all local terminal and printer
devices you wish to access from IRAF (there is no \fLprintcap\fR file in IRAF).
The entry for a printer contains one special (nonstandard termcap) entry,
called DD.  This consists of three fields: node!device, the template for the
temporary spoolfile, and the UNIX command to be used to dispose of the file to
the printer.  On Berkeley UNIX it is rarely necessary to make use of the node
name capability, since the UNIX \fLlpr\fR already provides this capability.
.PP
If you have a new terminal which has no entry in the termcap file provided,
you probably already have an entry in the UNIX termcap file.  Simply copy it
into the IRAF file; both systems use the same termcap database format.
However, if this is also a graphics terminal with a device entry in
\fLdev$graphcap\fR, you should add a `\fL:gd\fR' capability to the termcap
entry.  If the graphcap entry has a different name from the termcap entry,
make it `\fL:gd=\fIgname\fR'.
.NH 3
$iraf/dev/graphcap
.PP
There must be entries in this file for all graphics terminals, batch plotters,
and image displays accessed by IRAF programs.  Weird graphics terminals will
need a new entry.  The IRAF file \fLsys$gio/doc/gio.hlp\fR contains docs for
graphcap.  A printed copy of this document is available upon request, however
once IRAF is up you may find it easier to generate your own copy using
\fLhelp\fR, as follows:
.DS
\fLcl> cd sys$gio/doc
cl> help gio.hlp fi+ | lprint\fR
.DE
The manual page for the \fBshowcap\fR task should also be printed since this
utility is useful for generating new graphcap entries.  More focused
documentation will be available eventually.  Help is available for those who
need it via the IRAF Hotline.  We ask that new graphcap entries be sent
back to us so that we may include them in the master graphcap file for other
sites to use.
.NH 3
Network Tables
.PP
The \fLdev\fR directory also contains a number of files (\fLhosts\fR,
\fLhostlogin\fR, and \fLuhosts\fR)
used by the IRAF network software.  We depend upon the networking capabilities
of IRAF heavily at NOAO to access image displays, printers, files, etc. resident
upon remote nodes (the IRAF network interface is also capable of spawning
subprocess and background jobs on remote nodes, and works even when the nodes
run different host operating systems, e.g., both UNIX and VMS).
.PP
We expect that most sites will not need this capability initially, hence
documentation of the networking software will be left for later.  For sites
that want to try it out, all that is necessary to enable networking is to
edit the three networking files in the \fLdev\fR directory, and install IRAF on
the other nodes.  It does not matter what native operating system runs on the
remote nodes, so long as it runs IRAF as well.  The source for the network
interface is in the \fLsys$ki\fR directory, and a discussion of the conceptual
design of the interface is given in the \fLREADME\fR file in that directory.

.NH
Testing the System
.PP
Once the system has been installed or updated and the principal device tables
configured, it is time to perform a few simple tests to make sure the system
is working properly.  At this point it would probably be wise to read the
CL User's Guide, if you have not already done so.  Once the IRAF environment
is configured with \fLmkiraf\fR (discussed earlier) one need only enter the
"cl" command to start up the CL.
.DS
\fL% cl\fR
.DE
After a bit IRAF should print the message of the day and the root IRAF menu,
and issue the \fLcl> \fR prompt.
Once in the CL, you will probably have magtape and printer access, are likely
to have graphics terminal access, and very possibly will not have either
image display access or graphics plotter access.  If the graphics terminal
capability is ready, the next step is to run the IRAF test procedure to
verify that all is working well, as well as try out a few of the many tasks
in the system.  If the graphics terminal is not up yet, there is probably
no point in running the test procedure.  To run the test procedure,
familiarize yourself with the documentation in Volume 1A of the
\fIIRAF User Handbook\fR and follow the instructions therein.

.NH
Reconfiguring the System
.NH 2
Bootstrapping the System
.PP
All current normal IRAF distributions come with the system already bootstrapped.
A bootstrap should not be necessary unless one is doing something unusual,
e.g., attempting a new port.
.PP
A bootstrap is like a full system sysgen, except that it is the host
system interface (kernel and bootstrap utilities) which are compiled and
linked, rather than IRAF itself.  The system must be bootstrapped before
a sysgen is possible, since the bootstrap utilities are required to do a
sysgen.  The two operations are distinct because only the bootstrap is
machine dependent; everything else works the same on all IRAF systems.
.PP
The bootstrap operation is necessary when installing the system from a
source only distribution tape.  We assume that the files in the host system
interface have already been configured for the host system.  If this is
not the case, then we are doing a port, which is a lot more ambitious than
a simple bootstrap.
.PP
To bootstrap UNIX/IRAF, go to the \fLunix\fR directory and interpret the shell
script \fLmkpkg.csh\fR.  This takes about 45 minutes, so the output should be
spooled in a file.  Note that (for no good reason) the files have a \fL.csh\fR
extension even though we have chosen to use the Bourne shell to execute
the commands therein.
.DS
\fL% cd $iraf/unix
% sh -x mkpkg.csh >& spool &\fR
.DE
A bootstrap recompiles everything whether it needs to or not, so it is
usually not necessary to delete the binaries before doing a full bootstrap.

.NH 2
Relinking the System
.PP
Relinking the system is necessary if the system was shipped without
the executables in the \fLbin\fR directory.  In order to relink the system must
first have been bootstrapped.  When the system is shipped without
executables (to save space) it will already have been bootstrapped.
If it is necessary to bootstrap the system, e.g., given a source only
distribution, go read \(sc4.1 before trying to relink the system.
.PP
The system is relinked by the bootstrap utility program \fBmkpkg\fR.
The \fLmkpkg\fR program is driven by the \fLmkpkg\fR files found in all IRAF
source directories.  Running \fLmkpkg\fR in a directory causes the
contents of that directory, normally an applications package or library,
to be updated, along with the contents of all subdirectories referenced
by the root \fLmkpkg\fR file.
.PP
Running \fLmkpkg\fR in the root IRAF directory causes the entire system to
be updated.  If the system libraries are all up to date, the effect is to
relink the system.  If the system has been modified and some library modules
have to be recompiled and inserted into the system libraries, then the
operation is a partial sysgen.  If there are no libraries or object libraries
then we have a full sysgen, which is the topic of the next section.
The \fLmkpkg\fR utility is documented in detail in Volume 1B of the
IRAF User Handbook, in the manual pages for the \fLsoftools\fR package.
.LP
To relink all of IRAF, go to the root and run \fLmkpkg\fR, i.e.,
.DS
\fL% cd $iraf
% mkpkg\fR
.DE
The program will inspect all source modules and verify that the system
libraries are up to date, then relink the system executables, followed by
all the applications executables, installing the executable files in the
\fLbin\fR directory.
.NH 3
Relinking individual packages
.PP
It is just as easy to relink the individual IRAF packages, e.g., after
a bug has been fixed, or if an update of a package is received by electronic
mail.  This would normally be done from within IRAF since the next step
is to test the new package.  We will use the \fLcl> \fR prompt in the
examples, but since \fLmkpkg\fR is a bootstrap utility (IRAF foreign task),
usage is the same both in IRAF and in UNIX.
.LP
To relink a package and install the new executable in \fLbin\fR:
.DS
\fLcl> mkpkg update\fR
.DE
To relink a package, leaving the executable in the package directory for
testing or debugging prior to installation:
.DS
\fLcl> mkpkg\fR
.DE
To install an already linked executable after testing:
.DS
\fLcl> mkpkg install\fR
.DE
To update only the package library without relinking (this assumes that the
name of the library is \fLlibpkg.a\fR):
.DS
\fLcl> mkpkg libpkg.a\fR
.DE
The "update", "install", and "libpkg.a" identifiers are entry points in the
\fLmkpkg\fR file, which may be read to see what is going on.
.PP
As an actual example of a package relink, suppose we wanted to increase the
size of the stack area in the CL to 8000 elements, e.g., because the CL was
running out of space at runtime.  We would go to the CL directory, edit
the file \fLconfig.h\fR, change the value of \fLSTACKSIZ\fR to \fL8000\fR,
and then run \fLmkpkg\fR:
.DS
\fLcl> cd cl
cl> ed config.h
cl> mkpkg update\fR
.DE
Since \fLconfig.h\fR is included by nearly all the CL source files, the entire
package would be recompiled unnecessarily, but it is safer that way.

.NH 2
Autogeneration of the System (SYSGEN)
.PP
A full system sysgen is necessary when installing a source only version
of the system.  The system must first have been bootstrapped; see \(sc4.1
if this has not yet been done.  A full sysgen may also be necessary if
a binary distribution has been received but it is later discovered that it is
necessary or desirable to recompile the system.  In this case the existing
libraries and objects \fImust be deleted\fR before the sysgen, else the sysgen
will be nothing more than a relink.  The \fBrmbin\fR utility is used to
descend a directory tree, deleting all binary files therein.  Note that
the ONLY case in which it is necessary to use \fLrmbin\fR is when we wish
to force the entire system to be recompiled.  The \fLrmbin\fR task is yet
another bootstrap utility, and is documented in the manual pages for the
\fLsoftools\fR package.
.DS
\fL% cd $iraf
% rmbin -vi bin lib pkg sys\fR
.DE
This will delete all binaries in the portable part of the system, excluding
the \fLhost\fR or \fL$iraf/unix\fR directories (if you run \fLrmbin\fR on the
\fL$iraf/unix\fR directories, you will have to bootstrap the system as well).
.PP
Before starting the full system sysgen it may be desirable to review the
switches in the file \fLmkpkg.inc\fR in the \fLhlib\fR directory.
This is the global include file for the \fLmkpkg\fR utility,
and contains various switches controlling \fLmkpkg\fR, e.g., which packages
will be compiled, and the default compiler and linker flags.
.PP
Since a full sysgen takes a long time and generates a lot of output which
later has to be reviewed, they are always run in batch with the output
redirected, e.g.:
.DS
\fL% cd $iraf
% mkpkg >& spool &\fR
.DE
To watch what is going on after this command has been submitted and while
it is running, try
.DS
\fL% tail -f spool\fR
.DE
Sysgens are restartable, so if the sysgen aborts for any reason, simply
fix the problem and start it up again.
.PP
A full sysgen generates a lot of output, too much to be safely reviewed for
errors by simply paging the spool file.  Enter the following command to review
the output (this assumes that the output has been saved in a file named
\fLspool\fR).
.DS
\fL% mkpkg summary\fR
.DE
It is normal for a number of compiler messages warning about assigning
character data to an integer variable to appear in the spooled output.
These are harmless on most (but not all) machines, and are due to questionable
coding practices in the old NCAR graphics utilities and some of the math
library routines, all of which are coded in Fortran (SPP code never causes
such problems!).
.PP
The discussion up to now has centered on the full system sysgen.
Partial sysgens are actually much more common.  For example, if an important
bug is fixed in the VOS or in the IRAF kernel, a (partial) sysgen should be
conducted to recompile the affected modules and relink the system.
An example of this occurs when the \fLtermcap\fR or \fLgraphcap\fR entries
for important local devices are cached by running the \fBttycompile\fR
task (another \fLsoftools\fR utility).  A sysgen is required after regenerating
the cache tables, since these must be compiled and linked into the affected
programs to have any effect.

.NH 2
New Ports
.PP
We recommend that you contact the IRAF group for assistance if you are
contemplating porting IRAF to a new host machine.
To port IRAF to a new version of UNIX requires some modifications to the
files in the host interface directories (\fL$iraf/unix/\fR...),
followed by a full
sysgen and lots of testing.  Typically most of the code in the host interface
can be used without change, with only minor changes to the remaining code,
since the different versions of UNIX are quite similar (as compared to,
for example, a port to a completely different operating system like VMS or
AOS/VS).
.PP
Note that even when the target system is a "fully compatible" 4.X BSD UNIX
system, some changes are likely to be necessary since all of these systems
are slightly different.  For example, the SUN and ISI implementations of
BSD UNIX employ different assemblers, and the Fortran compilers on the
two systems have diverged considerably.  A port to Bell System V or one of
the UNIX look alikes would certainly require some changes to the IRAF kernel,
particularly in the exception handling, process control, and network interface
facilities.
.PP
The changes required are likely to be minor for someone sufficiently familiar
with both IRAF and UNIX, but should not be underestimated.  Every new port
produces its share of compile time errors (due to differences in the Fortran
compilers) and finds a few new bugs, sometimes in the IRAF software but more
often in the host Fortran compiler, now that IRAF has been ported to so many
systems.  A thorough understanding of both IRAF and the host system is required
to rapidly isolate such bugs.

.NH
Tuning Considerations
.PP
There are two things that are commonly done to tune UNIX/IRAF for a
particular host system:
.RS
.IP \(bu
Precompile selected \fLtermcap\fR and \fLgraphcap\fR entries
.IP \(bu
Strip the system to reduce disk consumption
.RE
.LP
The most important optimization is precompilation of the termcap and graphcap
entries of the devices most commonly used at the local site, particular when
running IRAF on a slow machine.  Stripping the system is inadvisable if the
system is to be used for software development, but is normally desirable when
installing a production version of IRAF on a small system with limited disk
space, e.g., a workstation.

.NH 2
Precompiling TERMCAP and GRAPHCAP Entries
.PP
Precompilation of a termcap or graphcap entry is a technique used to
speed runtime access of the entry for that device.  If the entry is not
precompiled the termcap or graphcap file must be physically opened and
scanned at runtime to read the desired entry.  This causes a noticeable
delay of as much as a second when clearing the terminal screen or plotting
a graph, hence it is usually worthwhile to cache the entries for commonly
used video and graphics terminals.  It is not worthwhile for printers,
plotters, and image displays.
.PP
The system comes with selected termcap and graphcap entries already
precompiled.  To see which devices are precompiled, page the cache data
files, \fLdev$cachet.dat\fR (for termcap) and \fLdev$cacheg.dat\fR
(for graphcap).
To cache a different set of entries one must regenerate these files with the
\fBmkttydata\fR task in the \fLsoftools\fR package, and then do a full
sysgen with the \fLmkpkg\fR utility.  Detailed instructions are given in
the manual page for \fLmkttydata\fR.
.PP
Note that if you wish to precompile and relink the system to cache selected
termcap or graphcap device entries and plan to strip the system as well to
save disk space, \fIyou must cache the termcap and graphcap entries before
stripping the system\fR, because once the system is stripped you cannot
relink.

.NH 2
Stripping the System to Reduce Disk Consumption
.PP
If the system is to be installed on multiple cpus, or if a production
version is to be installed on a workstation, it may be necessary or desirable
to strip the system of all non-runtime files to save disk space.
This equates to deleting all the sources and all the reference manuals and
other documentation, excluding the online manual pages.  A special utility
called \fBrmfiles\fR (in the \fLsoftools\fR package, of course) is provided
for this purpose.  It is not necessary to run \fLrmfiles\fR directly to strip
the system.  The preferred technique is to enter the commands given below.
The example is for the cshell for consistency with the rest of this document,
but this could be done from within the CL as well.
.DS
\fL% cd $iraf
% mkpkg strip\fR
.DE
This will preserve all runtime files, permitting use of the standard system
as well as user software development.  The size of the system is reduced
from about 50 Mb (megabytes) to around 26 Mb.  One can optionally enter the
command \fLmkpkg stripall\fR to delete the system libraries as well, but this
saves only another couple of Mb and a full sysgen or a tape reload will be
required to regain the capability to link user programs with the IRAF
libraries (including IMFORT), or relink the IRAF executables.
.PP
Note:  if the \fLvms\fR directory is present at the \fL$iraf\fR root,
it may be removed entirely.  On a full system it takes up over 3Mb, and
may be present on some distribution tapes for UNIX/IRAF cut on our master
development system.

.NH
Interfacing New Graphics Devices
.PP
There are three types of graphics devices that concern us here.
These are the graphics terminals, graphics plotters, and image displays.

.NH 2
Graphics Terminals
.PP
The IRAF system as distributed is capable of talking to just about any
conventional graphics terminal and most workstations' terminal emulators,
using the \fLstdgraph\fR
graphics kernel supplied with the system.  All one need do to interface to a
new graphics terminal is add a new graphcap entry for the device.  This can
take anywhere from a few hours to a few days, depending on one's level of
expertise, and the perversity of the device in question.  Be sure to check
the contents of the \fLdev$graphcap\fR file to see if the terminal is already
supported, before trying to write a new entry.  Assistance in interfacing new
graphics terminals is available via the IRAF Hotline.

.NH 2
Graphics Plotters
.PP
The current IRAF system comes with several graphics kernels used to drive
graphics plotters.  The standard plotter interface is via the SGI graphics
kernel, which has largely replaced the older \fLNCAR\fR kernel used in earlier
versions of IRAF (in those earlier versions the \fLNCAR\fR kernel was called
the \fLstdplot\fR kernel).  Further information on the SGI plotter interface
is given in the paper \fIThe IRAF Simple Graphics Interface\fR, a copy of which
is included with the installation kit.
.PP
SGI device interfaces for most
plotter devices already exist, and adding support for new devices is
straightforward.  Sources for the SGI device translators supplied with the
distributed system are maintained in the directory \fL$iraf/unix/gdev/sgidev\fR.
NOAO serves as a clearinghouse for new SGI plotter device interfaces;
contact us if you do not find support for a local plotter device in the
distributed system, and if you plan to implement a new device interface let
us know so that we may help other sites with the same device.
.PP
The older \fLNCAR\fR kernel is used to generate NCAR metacode and can be
interfaced to an NCAR metacode translator at the host system level to get
plots on devices supported by host-level NCAR metacode translators.
The host level NCAR metacode translators are not included in the standard
IRAF distribution, but public domain versions of the NCAR implementation for
UNIX systems are widely available.  A site which already has the
NCAR software may wish to go this route, but the SGI interface will provide
a more efficient and simpler solution in most cases.
.PP
The remaining possibility with the current system is the \fLcalcomp\fR kernel.
Many sites will have a Calcomp or Versaplot libary (or Calcomp compatible
library) already available locally.  To make use of such a library to get
plotter output on any devices supported by the interface, one may copy
the library to the \fLhlib\fR directory and relink the Calcomp graphics
kernel.
.PP
A graphcap entry for each new device will also be required.  Information on
preparing graphcap entries for graphics devices is given in the GIO design
document, and many actual working examples will be found in the graphcap
file.  The best approach is usually to copy one of these and modify it.

.NH 2
Image Displays
.PP
The current IRAF system does not yet have a well defined and well isolated
device independent image display interface.  Work on such an interface is
currently underway; contact the IRAF group at NOAO for further information
on the status of the new display interfaces.  Further information on the
image display interfaces currently available may be found in the
\fIIRAF Newsletter\fR.
.PP
If there is no IRAF interface for your device, the best approach at present is
to use the IMFORT interface and whatever non-IRAF display software you
currently have to construct a host level Fortran or C display program
(a number of people have also managed to construct an interface by hacking
the IRAF display software in \fLpkg$images/tv/display\fR).
The IMFORT library provides host system Fortran or C programs with access
to IRAF images on disk.  Documentation on the IMFORT interface is available in
\fIA User's Guide to Fortran Programming in IRAF -- The IMFORT Interface\fR,
Doug Tody, September 1986, a copy of which is included in the IRAF User
Handbook, Volume 1A.

.NH
The IRAF Directory Structure
.PP
The current full UNIX/IRAF directory structure is documented graphically in
the appendix.  The main branches of the tree are the following; beneath the
directories shown are some 250 subdirectories, the largest directory trees
being \fLsys\fR, \fLpkg\fR, and \fLnoao\fR.  The entire contents of all
directories other than \fLunix\fR, \fLlocal\fR, and a few configuration files
in \fLdev\fR are fully portable, and are identical in all installations
of IRAF sharing the same version number.
.DS
\fLbin        \fR- installed executables
\fLdev        \fR- device tables (\fLtermcap\fR, \fLgraphcap\fR, etc.)
\fLdoc        \fR- assorted IRAF manuals
\fLlib        \fR- the system library; object libraries, global files
\fLlocal      \fR- iraf login directory; locally added software
\fLmath       \fR- sources for the mathematical libraries
\fLnoao       \fR- packages for NOAO data reduction
\fLpkg        \fR- the IRAF applications packages
\fLsys        \fR- the virtual operating system (VOS)
\fLunix       \fR- the UNIX host system interface (HSI = kernel + bootstrap utilities)
.DE
.LP
The contents of the \fLunix\fR directory (host system interface) are
as follows:
.DS
\fLas         \fR- assembler sources for UNIX/IRAF
\fLboot       \fR- bootstrap utilities (mkpkg, rtar, wtar, etc.)
\fLgdev       \fR- graphics device interfaces (SGI device translators)
\fLhlib       \fR- host dependent runtime files
\fLmc68000    \fR- Motorola 68xxx assembler sources
\fLmkpkg.sh   \fR- executed to bootstrap the UNIX/IRAF HSI
\fLos         \fR- OS interface routines (UNIX/IRAF kernel)
\fLrmbin.sh   \fR- executed to delete binary files in subdirectories
.DE
.PP
If you will be working with the system much at the system level, it will be
well worthwhile to spend some time exploring these directories and gaining
familiarity with the system.
