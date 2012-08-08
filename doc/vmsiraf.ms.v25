.RP
.TL
VMS/IRAF Installation and Maintenance Guide
.AU
Doug Tody
.br
Steve Rooke
.AI
.K2 "" "" "*"
July 1987

.AB
.ti 0.75i
Instructions are presented for installing, updating, and maintaining the VMS
version of the portable IRAF system (Image Reduction and Analysis Facility).
Step by step procedures are presented both for the initial IRAF installation
and for updating an existing installation to the latest release of the system.
Procedures for improving performance, minimizing disk and memory usage, and
interfacing site dependent devices such as video or graphics terminals,
plotters, image displays, and magnetic tape drives are discussed.
.AE

.pn 1
.bp
.sp 2.5i
.ce
.ps 11
.ps +2
\fBHistorical Notes\fR
.ps -2
.sp 3
The initial port of IRAF to VMS was carried out during 1984 and early 1985
at STScI (the Space Telescope Science Institute) by Jay Travisano
and Fred Romelfanger, working under the supervision of Peter Shames, with
some assistance early on from Jim Rose.  The major system components
implemented during this period were the SPP compiler (XC) and the OS
interface routines for VMS, as specified in \fIA Reference Manual for the
IRAF System Interface\fR, Doug Tody, May 1984.
.sp
Completion of VMS/IRAF took place at NOAO in the fall of 1985.  This period
saw the definition of a major new interface, the HSI (host system interface),
and the implementation of the \fLmkpkg\fR system maintenance utility and
many of the other HSI bootstrap utilities.  This eliminated the final
differences between the different versions of IRAF, allowing software to be
routinely ported between UNIX/IRAF, VMS/IRAF, and so on, without any changes
to the program sources or configuration control files, and allowing full
automation of the system configuration control procedures.
.sp
The initial implementation of shared libraries for VMS/IRAF was carried out
in the fall of 1986 by Dennis Crabtree of DAO (the Dominion Astrophysical
Observatory, UBC, Canada), while on leave at STScI.

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
2.\h'|0.4i'\fBInitial System Installation\fP\l'|5.6i.'\0\02
.br
\h'|0.4i'2.1.\h'|0.9i'Create the IRAF Account\l'|5.6i.'\0\02
.br
\h'|0.4i'2.2.\h'|0.9i'Read the BACKUP Tape\l'|5.6i.'\0\03
.br
\h'|0.4i'2.3.\h'|0.9i'Edit the System Configuration Files\l'|5.6i.'\0\03
.br
\h'|0.9i'2.3.1.\h'|1.5i'[IRAF.VMS.HLIB]IRAFUSER.COM\l'|5.6i.'\0\03
.br
\h'|0.9i'2.3.2.\h'|1.5i'[IRAF.VMS.HLIB.LIBC]IRAF.H\l'|5.6i.'\0\05
.br
\h'|0.9i'2.3.3.\h'|1.5i'[IRAF.VMS.HLIB]INSTALL.COM\l'|5.6i.'\0\05
.br
\h'|0.4i'2.4.\h'|0.9i'Relink If Necessary\l'|5.6i.'\0\06
.br
\h'|0.4i'2.5.\h'|0.9i'Complete the Initial System Configuration\l'|5.6i.'\0\06
.br
\h'|0.4i'2.6.\h'|0.9i'Edit the System Dependent Device Files\l'|5.6i.'\0\07
.br
\h'|0.9i'2.6.1.\h'|1.5i'[IRAF.VMS.HLIB]ZZSETENV.DEF\l'|5.6i.'\0\07
.br
\h'|0.9i'2.6.2.\h'|1.5i'[IRAF.DEV]DEVICES\l'|5.6i.'\0\07
.br
\h'|0.9i'2.6.3.\h'|1.5i'[IRAF.DEV]TERMCAP\l'|5.6i.'\0\08
.br
\h'|0.9i'2.6.4.\h'|1.5i'[IRAF.DEV]GRAPHCAP\l'|5.6i.'\0\08
.br
\h'|0.9i'2.6.5.\h'|1.5i'IRAF Networking\l'|5.6i.'\0\08
.sp
3.\h'|0.4i'\fBUpdating an Existing IRAF Installation\fP\l'|5.6i.'\0\09
.br
\h'|0.4i'3.1.\h'|0.9i'Save Locally Modified Files\l'|5.6i.'\0\09
.br
\h'|0.4i'3.2.\h'|0.9i'Read the Distribution Tape or Tapes\l'|5.6i.'\0\09
.br
\h'|0.4i'3.3.\h'|0.9i'Merge Saved Files Back Into the New System\l'|5.6i.'\0\010
.br
\h'|0.4i'3.4.\h'|0.9i'Relink If Necessary\l'|5.6i.'\0\011
.br
\h'|0.4i'3.5.\h'|0.9i'Update VMS\l'|5.6i.'\0\011
.sp
4.\h'|0.4i'\fBTesting a Newly Installed or Updated System\fP\l'|5.6i.'\0\011
.sp
5.\h'|0.4i'\fBSystem Maintenance\fP\l'|5.6i.'\0\012
.br
\h'|0.4i'5.1.\h'|0.9i'Recompiling or Relinking the System\l'|5.6i.'\0\012
.br
\h'|0.9i'5.1.1.\h'|1.5i'Bootstrapping the HSI\l'|5.6i.'\0\013
.br
\h'|0.9i'5.1.2.\h'|1.5i'Updating the System Libraries\l'|5.6i.'\0\013
.br
\h'|0.9i'5.1.3.\h'|1.5i'Relinking the IRAF Shared Library\l'|5.6i.'\0\014
.br
\h'|0.9i'5.1.4.\h'|1.5i'Relinking the Main IRAF System\l'|5.6i.'\0\015
.br
\h'|0.9i'5.1.5.\h'|1.5i'Relinking or Updating IRAF Packages\l'|5.6i.'\0\015
.br
\h'|0.4i'5.2.\h'|0.9i'Tuning Considerations\l'|5.6i.'\0\016
.br
\h'|0.9i'5.2.1.\h'|1.5i'Installing Executable Images\l'|5.6i.'\0\016
.br
\h'|0.9i'5.2.2.\h'|1.5i'Rendering Large Runtime Files Contiguous\l'|5.6i.'\0\017
.br
\h'|0.9i'5.2.3.\h'|1.5i'Precompiling TERMCAP and GRAPHCAP Entries\l'|5.6i.'\0\017
.br
\h'|0.9i'5.2.4.\h'|1.5i'Stripping the System to Reduce Disk Consumption\l'|5.6i.'\0\018
.br
\h'|0.4i'5.3.\h'|0.9i'VMS Quotas and Privileges Required to Run IRAF\l'|5.6i.'\0\018
.sp
6.\h'|0.4i'\fBInterfacing New Graphics Devices\fP\l'|5.6i.'\0\019
.br
\h'|0.4i'6.1.\h'|0.9i'Graphics Terminals\l'|5.6i.'\0\019
.br
\h'|0.4i'6.2.\h'|0.9i'Graphics Plotters\l'|5.6i.'\0\020
.br
\h'|0.4i'6.3.\h'|0.9i'Image Displays\l'|5.6i.'\0\020
.sp
7.\h'|0.4i'\fBThe IRAF Directory Structure\fP\l'|5.6i.'\0\021

.nr PN 0
.bp
.NH
Introduction
.PP
Installing VMS/IRAF is easy and fast, provided one takes the trouble to read
and follow the instructions in this installation guide.  Instructions are given
both for the initial system installation (\(sc2, immediately following)
and for updating an existing installation (\(sc3).  Variations on the
recommended system configuration are certainly possible and may be necessary
at some sites due to restrictions or quotas, but not following the standard
installation procedure or departing from the recommended system configuration
may cause the system to function incorrectly, inefficiently, or not at all.
.PP
The distribution tape is a snapshot of our local (NOAO/Tucson) VMS/IRAF system.
The device and system configuration tables come configured for our system,
and will have to be modified as part of the installation process.
These modifications are discussed in detail in this document.
To simplify the installation process as well as future upgrades, we have tried
to isolate the site dependent files to the minimum number of directories, i.e.,
\fLdev\fR, \fLhlib\fR, and \fLlocal\fR and its subdirectories (the equivalent
VMS pathnames are \fL[IRAF.DEV]\fR, \fL[IRAF.VMS.HLIB]\fR,
and \fL[IRAF.LOCAL]\fR).
The remainder of the system should not require any modifications.
.PP
An installation typically takes about an hour, perhaps longer if relinking is
necessary.  Interfacing new graphics terminals, plotters, or image displays
can take considerably longer, of course, and we will only touch upon such
matters in the installation guide.  Feel free to contact the IRAF hotline for
assistance if any problems should arise during the installation, or for help
in interfacing new devices.
.sp
.TS
center;
cb s s
l l l.
IRAF HOTLINE
.sp
telephone	\fL(602) 323-4160\fP
bitnet	\fLiraf%draco@\fP\fR...\fP	(via the Wisconsin gateway)
internet	\fLiraf@noao.arpa\fP
span	\fLdraco::iraf\fP	(draco = 5356)
uucp	\fLseismo!noao!iraf\fP
.TE

.NH
Initial System Installation
.PP
It is only necessary to follow the full installation procedure if IRAF has
not yet been installed on your system.  If an old version of IRAF has already
been installed, proceed to \(sc3.
.PP
Briefly, the steps to be followed to install IRAF for the first time are the
following.  This summary is provided only to introduce the basic installation
procedure: it is important to read the detailed installation instructions
for additional details, to avoid certain errors or omissions which are
otherwise highly likely (IRAF does not always follow VMS conventions).
.RS
.IP \(bu
Login as SYSTEM and create an account for user IRAF, root directory
\fL[IRAF]\fR, login directory \fL[IRAF.LOCAL]\fR, to be used by the IRAF
system manager to maintain IRAF.
.IP \(bu
Login as IRAF and run the \fLBACKUP\fR utility to restore the IRAF distribution
tape or tapes to disk.  Make sure the files belong to user IRAF.
.IP \(bu
Edit the basic IRAF system configuration files \fLIRAFUSER.COM\fR and
\fLIRAF.H\fR (in \fLhlib\fR and \fLhlib/libc\fR), e.g., to change the system
dependent disk and batch queue names, and define the VMS pathnames to
the major IRAF directories.
.IP \(bu
Login again as SYSTEM and [1] copy the \fLIRAF.H\fR file edited above
to the VMS system library directory \fLSYS$LIBRARY\fR,
[2] to the \fLSYLOGIN.COM\fR file, add a global symbol \fLiraf\fR which when
executed by the user will execute the \fLIRAFUSER.COM\fR file to define the
rest of the IRAF logicals, and
[3] edit and execute the \fLINSTALL.COM\fR script (in \fLhlib\fR) to "install"
in system shared memory at least the IRAF shared library, and probably some of
the most frequently used executables as well.
.RE
.PP
Although the system installation is not yet complete,
any user should now be able to run IRAF, i.e.,
run the \fLiraf\fR command procedure to define the IRAF logicals,
run the \fLmkiraf\fR command procedure to initialize the IRAF environment,
and type the command \fLcl\fR to start up the IRAF Command Language (CL).
Following the basic installation procedure shown here,
it will still be necessary to modify the IRAF device tables to tell the system
about the local magtape devices, line printers, terminals, plotters, and so on.
This should be done from within the running IRAF system to facilitate
testing of the new device interfaces, and is an ongoing activity as new
devices are interfaced to the system.

.NH 2
Create the IRAF Account
.PP
Login as SYSTEM and run the \fLAUTHORIZE\fR utility to create an account for
user `\fLIRAF\fR'.  Select a disk device with sufficient free space for the
system (see \(sc2.2 below).  If necessary, the system can be stripped after
installation to save space (\(sc5.2.4), but the full amount of space will be
needed during installation.  There is no need to worry about VMS quotas and
privileges yet; this is not a concern during installation and is discussed
in a later section (\(sc5.3).
.DS
\fL$ run sys$system:authorize
UAF>\fR (etc)
.DE
The root directory for the IRAF account should be set to \fL[IRAF]\fR,
as this name is explicitly assumed in various places in the configuration files.
We recommend that the login directory for the IRAF account be set to
\fL[IRAF.LOCAL]\fR rather than \fL[IRAF]\fR as one would expect, to provide
a place to keep all the miscellaneous files required locally to maintain the
system without cluttering up the standard IRAF system directories.
This will simplify the installation of future updates since
it makes it obvious what is part of the standard system and what has been
added locally, and having all the local stuff in a separate subdirectory will
make it easier to ensure that it is not lost when the next version of the
system is installed.

.NH 2
Read the BACKUP Tape
.PP
Login as IRAF (ignore the \fLLOGIN.COM\fR not found error message)
and run the VMS \fLBACKUP\fR utility to read the BACKUP tape or tapes provided.
The tape contains approximately 7400 files in 260 directories, for a total
of 39 Mb or 76,700 512-byte blocks (including binaries).
The system as distributed uses a shared library to reduce disk and
memory requirements.
If IRAF is relinked and run without the shared library (e.g., if two or
more versions of IRAF are installed on the same machine), the system will
take up about 49 Mb or 98,300 blocks.
.DS
\fL$ mount/foreign \fItape\fL:
$ set default \fIdisk\fL:[iraf]
$ backup/rew \fItape\fL:iraf.bck/select=[iraf...] [...]\fR
.DE
In this and all the following examples, names like \fIdisk:\fR, \fItape:\fR,
etc. denote site dependent device names for which you must supply values.
The tape may be restored while logged in as SYSTEM if desired, but the switch
\fL/OWNER=[IRAF]\fR should be appended to give the IRAF system manager
(anyone who logs in as IRAF) write permission on the files.
.PP
It typically takes twenty minutes to half an hour to read the tape on a
lightly loaded system.

.NH 2
Edit the System Configuration Files
.PP
The files listed below must be edited before the system can be run.
The principal change required is to change the pathnames of the major IRAF
logical directories for the local machine.  If any of these files are edited
while logged in as SYSTEM, be sure to do a \fLSET FILE/OWNER=[IRAF]\fR to
restore ownership of the edited file to IRAF.
.NH 3
[IRAF.VMS.HLIB]IRAFUSER.COM
.PP
This file defines the VMS logical names and symbols needed to run IRAF.
The site dependent ones are grouped at the beginning of the file.

.IP \fLIRAFDISK\fR
.br
Set to the name of the disk the \fL[IRAF]\fR directory is on.

.IP \fLIMDIRDISK\fR
.br
Set to the name of a public scratch device, if available.
The \fLmkiraf\fR script will try to create the default user image storage
directories on this disk, e.g., \fLIMDIRDISK:[\fIuser\fP]\fR.
All potential IRAF users should have write permission and quota on this
device.
.sp
A public scratch device is desirable because this is where bulk image data
will be stored by default.  It is often best for this to be on a different
disk than that used for user logins, to minimize the amount of disk that has
to be backed up on tape, and to permit a different quota and file expiration
date policy to be used for large datafiles than is used for the small,
relatively long lived files normally kept on the user disk.

.IP \fLTEMPDISK\fR
.br
Set to the name of a public scratch device and create a public directory
\fL[IRAFTMP]\fR on this device.
The device may be the same as is used for \fLIMDIRDISK\fR if desired.
The IRAF logical directory "\fLtmp\fR"
(known as \fLIRAFTMP\fR in the \fLIRAFUSER.COM\fR file)
is defined as \fLTEMPDISK:[IRAFTMP]\fR.\(dg
.FS
\(dgIf local system management policies require that private \fLtmp\fR and
\fLimdir\fR directories be defined for each user, you can define these
directories for each user as subdirectories of their \fLSYS$LOGIN\fR
directory.  One way to do this is define \fLimdir\fR to be the same as
\fLtmp\fR, and modify the definition of \fLIRAFTMP\fR in the
\fLIRAFUSER.COM\fR file as shown below.
.sp
.nf
\fL    $ dir := 'f$logical ("SYS$LOGIN")'            \fR(add to irafuser.com)\fL
    $ off := 'f$locate ("]", dir)'
    $ dir := 'f$extract (0, off, dir)'".IRAFTMP]"
    $ define/job iraftmp 'dir'\fR
.fi
.sp
As a final step, edit the file \fLLOGIN.CL\fR in the \fLhlib\fR directory
(this is the master template \fLLOGIN.CL\fR file), replacing the \fLU_IMDIR\fR
therein by \fLtmp$\fR, and in the file \fLMKIRAF.COM\fR, also in \fLhlib\fR,
replace the block of statements beginning \fLimdir_file :=\fR with the
following:
.sp
.nf
\fL    $ if (f$search ("sys$login:iraftmp.dir") .nes. "") then goto endif_mktmp
    $     create/directory iraftmp
    $ endif_mktmp:                                \fR(add to mkiraf.com)
.fi
.sp
Thereafter, execution of the \fLmkiraf\fR command procedure by a user will
cause a private \fLtmp\fR directory (\fLIRAFTMP = [\fIuser\fP.IRAFTMP]\fR)
to be created in their VMS login directory if it does not already exist.
The \fLLOGIN.CL\fR file will be configured to place both temporary files
and pixel storage files in this directory by default.
.FE
.sp
The IRAF system will create temporary files in this directory at runtime.
These files are always deleted immediately after use (unless a task aborts),
hence any files in "\fLtmp\fR" older than a day or so are garbage and should
be deleted.  It is best if "\fLtmp\fR" points to a public directory which
is cleaned periodically by the system, e.g., whenever the system is booted,
so that junk temporary files do not accumulate.  This is a good reason for
using a single public directory for this purpose rather than per-user private
directories.  The files created in \fLtmp\fR are rarely very large.

.IP \fLFAST,BATCH,SLOW\fR
.br
These are the logical names of the standard IRAF logical batch queues.
They should be redefined to reference the queues used on your machine,
e.g., the standard VMS batch queue \fLSYS$BATCH\fR (all three names
may point to the same batch queue if desired).  The fast queue is intended
for small jobs to be executed at interactive priorities, the batch queue
is for medium sized jobs, and the slow queue is for large jobs that need
a lot of system resources and are expected to run a long time.

.NH 3
[IRAF.VMS.HLIB.LIBC]IRAF.H
.PP
This file (often referred to as \fL<iraf.h>\fR)
is required to compile any of the C source files used in IRAF.
Most sites will not need to recompile the C
sources and indeed may not even have the DEC C compiler, but the file is
also used by the runtime system in some cases to resolve logical names,
hence must be edited and installed in the VMS system library.
Change the following directory names as required for your system,
referencing only system wide logical names in the new directory pathnames.
.DS
.TS
center;
ci ci
n l.
IRAF Logical Directory	VMS directory

\fL\&HOST\fR	\fLIRAFDISK:[IRAF.VMS]\fR
\fL\&IRAF\fR	\fLIRAFDISK:[IRAF]\fR
\fL\&TMP\fR	\fLTEMPDISK:[IRAFTMP]\fR (may vary\(dg)
.TE
.DE
.FS
\(dgIf local system restrictions forbid a public \fLIRAFTMP\fR directory,
set this entry to the pathname of a suitable directory in IRAF, e.g.,
\fL[IRAF.LOCAL.UPARM]\fR.  This should work in most circumstances,
since it is most likely to be the
IRAF system manager who runs a program that accesses these pathnames.
.FE
.PP
These directory definitions are referenced only if logical name translation
fails for some reason, as sometimes happens on VMS systems for various reasons.
It is therefore essential that only system wide logical names be used in
these directory pathnames.  Do not use job or process logicals.  Do not
change the order in which the entries appear, or otherwise alter the syntax;
the kernel code which scans \fL<iraf.h>\fR is very strict about the syntax.

.NH 3
[IRAF.VMS.HLIB]INSTALL.COM
.PP
This command procedure is run by the system manager, or by the system at
boot time, to install the IRAF shared library and selected executable images.
Installing images in system memory is necessary to permit memory sharing in
VMS, and can greatly reduce physical memory usage and paging on a busy system.
Installing images also consumes system global pages, however, of which there
is a limited supply.  Hence, one should only install those images which will
be used enough to make it worthwhile.
.PP
Edit the \fLINSTALL.COM\fR file to select those executable images to be
installed (by commenting out those \fInot\fR to be installed).
The shared library \fLs_iraf.exe\fR should always be installed if IRAF is
going to be used at all, or the system will execute very inefficiently (the
IRAF shared library is used by all executing IRAF processes).
Normally the images \fLcl.exe\fR and \fLx_system.exe\fR
should also be installed, since these are used by anyone using IRAF, as well
as by all batch IRAF jobs.  If IRAF is heavily used and sufficient global
pages are available it is also desirable to install \fLx_images.exe\fR and
\fLx_plot.exe\fR, since virtually everyone uses these executable images
frequently when using IRAF.  It is probably not worthwhile to install any
other images, unless usage at the local site involves heavy use of specific
additional executable images.

.NH 2
Relink If Necessary
.PP
If you received a full binary distribution of VMS/IRAF and you are not running
an old version of VMS, you do not need to relink the system and may proceed
to \(sc2.5.  A "you relink" distribution is shipped without binaries so of
course requires linking.  Linking may also be necessary when running a version
of VMS older than that installed on our NOAO system when the executables in
a binary release were linked, since the VMS operating system may refuse to
run executables linked on a possibly incompatible "future" revision of VMS.
.PP
In any case, relinking the system is easy and fast.  The basic procedure is
outlined below; additional information is given in \(sc5.1.  The following
commands should be executed while logged in as IRAF.
.DS
\fL$ set default [iraf]
$ mkpkg update\fR
.DE
.PP
By default, the system is linked against the IRAF shared library
\fLS_IRAF.EXE\fR which should already be present in the \fL[IRAF.BIN]\fR
directory after restoring the distribution tape.  This speeds up linking and
considerably reduces the size of the resultant executables.  It should not
be necessary to relink the HSI executables (in \fLhlib\fR), as these are
linked with \fL/NOSYSSHARE\fR on our system and are included in all VMS/IRAF
distributions.

.NH 2
Complete the Initial System Configuration
.PP
Login again as SYSTEM and copy the \fLIRAF.H\fR file to the system library,
ensuring that the file has world read permission:\(dg
.FS
\(dgOn a VMS cluster, make sure the \fLIRAF.H\fR file is added to
\fLSYS$COMMON:[SYSLIB]\fR rather than \fLSYS$SYSROOT:[SYSLIB]\fR,
so that all nodes in the cluster may transparently access the file.
The same precaution is needed when editing the \fLSYLOGIN.COM\fR file,
which will probably want to go into \fLSYS$COMMON:[SYSTEM]\fR.
The \fLSYSTARTUP.COM\fR file, on the other hand,
should go into \fLSYS$SYSROOT:[SYSTEM]\fR if the bootstrap procedure is
different for each node.
.FE
.DS
\fL$ set default sys$library
$ copy \fIdisk\fL:[iraf.vms.hlib.libc]iraf.h []
$ set protection = (w:r) iraf.h\fR
.DE
.PP
Add the following statement to the system \fLSYLOGIN.COM\fR file, making the
appropriate substitution for \fIdisk\fR.
.DS
\fL$ iraf :== "@\fIdisk\fL:[iraf.vms.hlib]irafuser.com"\fR
.DE
.PP
Add the following statement to the \fLSYSTARTUP.COM\fR file, read by the
system at startup time.  This is necessary to cause the IRAF images to be
reinstalled in system memory when the system is rebooted.
.DS
\fL$ @\fIdisk\fP:[iraf.vms.hlib]install.com\fR
.DE
.PP
Lastly, while still logged in as SYSTEM, type in the above command
interactively to perform the initial image installation.  It may be necessary
to increase the number of system global pages for the command to execute
successfully.  If any of the images have been installed before (e.g., if
the first attempt fails part way through), remember to deinstall the old
images before attempting to install the new ones.

.PP
At this point it should be possible for any user to run IRAF.  To verify this,
log out and log back in as IRAF.  The default \fLLOGIN.COM\fR in the IRAF
login directory \fL[IRAF.LOCAL]\fR will automatically execute the \fL'iraf'\fR
command to read the \fLIRAFUSER.COM\fR file and define the IRAF logicals.
Type \fLmkiraf\fR to initialize the IRAF \fLuparm\fR directory and create
a new \fLLOGIN.CL\fR (a \fLpurge\fR is necessary to delete the old one).
Typing \fLcl\fR next should cause the CL to run.  If the CL runs successfully,
the screen should be cleared (if the terminal is set correctly) and the
message of the day printed.  You can then type \fLlogout\fR to stop the CL
and return to DCL, or stay in the CL to edit and test the device files
described in the next section.  When logging back into the CL (as `IRAF'),
always return to the \fL[IRAF.LOCAL]\fR directory before logging in.
A little command \fLh\fR (for `home') is defined in the default IRAF
\fLLOGIN.COM\fR file for this purpose.

.NH 2
Edit the System Dependent Device Files
.PP
The files listed below must be edited before IRAF can be used with the
video terminals, graphics terminals, plotters, printers, magtape devices,
and so on in use at the local site.
.NH 3
[IRAF.VMS.HLIB]ZZSETENV.DEF
.PP
This file contains the name of the default editor, the default names of all
the standard devices, and a number of other definitions which are not site
dependent and which can therefore be ignored.  To be accessible by the IRAF
system, each local device named must also have an entry in the \fLTERMCAP\fR
file (terminals and printers) or \fLGRAPHCAP\fR file (graphics terminals,
image displays, and plotters) in \fL[IRAF.DEV]\fR.
There must also be an "\fLeditor.ED\fR"
file in \fL[IRAF.DEV]\fR for the named editor; EDT, EMACS, and VI are currently
supported (VI support is approximate).  Edit the quoted value in the
following entries to change the defaults.  Sample values are shown.
.DS
\fLset editor	= "vi"
set printer	= "imagen"
set terminal	= "vt640"
set stdgraph	= "vt640"
set stdimage	= "iism70l"
set stdplot	= "versatec"\fR
.DE
For example, you may wish to change the default editor to "\fLedt\fR",
the default printer to "\fLvmsprint\fR",
the default image display to "\fLiism75\fR",
and the default terminal to "\fLvt100\fR".
Note that only a limited number of image displays are currently supported.
Most video terminals, graphics terminals, and plotters are already supported
by the current system.
.NH 3
[IRAF.DEV]DEVICES
.PP
This file contains a list of the allocatable devices (primarily tape drives)
for the local system.  It should be obvious how to change it by reading the
comments in the file and studying the current values, which are for our system.
By convention drives are assigned the logical names \fLmta\fR, \fLmtb\fR,
and so on.  Note that in a VMS cluster where all nodes access the same version
of IRAF, the tape drives for all systems must be defined in this one file,
even though an individual drive may not be accessible on all nodes.
.NH 3
[IRAF.DEV]TERMCAP
.PP
There must be entries in this file for all video terminal, graphics terminal,
and printer devices you wish to access from IRAF.
The printer is easy, since the
"\fLvmsprint\fR" entry provided should work on any VMS system.
To prepare entries for other devices, simply copy the "\fLvmsprint\fR"
entry and change the queue name from \fLSYS$PRINT\fR to the name of the queue
for the new printer.\(dg
.FS
\(dgIf the printer device is on a remote node which is not clustered with
the local node but which is accessible via DECNET, IRAF networking must be
used to access the remote device.  IRAF networking is also frequently used
to access devices on non-VMS (e.g., UNIX) nodes.  Contact us for assistance
to help configure your system for IRAF networking.
.FE
Any number of these entries may be placed in the termcap file,
and there can be multiple entries or aliases for each device.
If you have a new terminal which has no entry in the termcap file
provided, a new entry will have to be added (termcap is widely used,
so it is highly likely that someone somewhere will already have written it).
A copy of the UNIX manual page documenting the termcap database is appended
in case you need to construct a new termcap entry for some device.
.NH 3
[IRAF.DEV]GRAPHCAP
.PP
There must be entries in this file for all graphics terminals, batch plotters,
and image displays accessed by IRAF programs.  Many graphics terminals are
already supported; page the file to determine if entries are already available
for the graphics terminals in use at your site, before attempting to write a
new one.  The IRAF file \fLsys$gio/doc/gio.hlp\fR contains a description of
the graphcap database and instructions for adding an entry for a new device.
A printed copy of this document is available upon request, however once
IRAF is up you may find it easier to generate your own copy using \fLhelp\fR,
as follows:
.DS
\fLcl> cd sys$gio/doc
cl> help gio.hlp fi+ | lprint\fR
.DE
The manual page for the \fLshowcap\fR task should also be printed since this
utility is useful for generating new graphcap entries.
More focused documentation will be available eventually.
Telephone consultation via the IRAF Hotline is available for those
who need it.  We ask that new graphcap entries be sent back to us so
that we may include them in the master graphcap file for other sites
to use.

.NH 3
IRAF Networking
.PP
The \fLdev\fR directory also contains the files (\fLHOSTS\fR, \fLHOSTLOGIN\fR,
and \fLUHOSTS\fR), which are used by the IRAF networking software.
Sites which have a local TCP/IP based network can enable IRAF networking
by editing these files, substituting the hostnames and network
addresses of the machines in the local network for the entries currently
in the files, and by installing IRAF on the other nodes (or enough of IRAF
to run the IRAF kernel server).  It does not matter what native operating
system runs on the remote nodes, so long as it runs IRAF as well.
.PP
We also provide limited support for DECNET based networking, but the current
system as distributed must be relinked before DECNET networking can be used.
As this capability is still under development, please contact the Hotline for
assistance and advice if you wish to use the IRAF DECNET interface.

.NH
Updating an Existing IRAF Installation
.PP
Skip to \(sc4 if you have just completed the initial IRAF installation.
This section is for sites upgrading to a new version of IRAF.
.PP
Updating is very similar to the initial installation (\(sc2).
The main difference is that material which has been added to the site
dependent files since the initial installation should be saved and then
merged back into the generic system from the distribution tape.
The typical update process consists of the following steps:
.RS
.IP \(bu
Save any site-specific files.
.IP \(bu
Delete the old system.
.IP \(bu
Restore the new system to disk from the distribution tape.
.IP \(bu
Merge contents of saved site-specific files into new system.
.IP \(bu
Relink the system if necessary.
.IP \(bu
Update any "installed" images, the \fL<iraf.h>\fR file, etc.
.RE
.PP
Variations on this basic procedure are possible and may be preferred at
some sites.  For example, sites which make heavy use of IRAF and which have
sufficient disk space may wish to install the new version of the system as
a separate version on disk, e.g., calling the new version of the system
the new `IRAF' and keeping the old version around as `IRAFO'.  At NOAO we
maintain three separate versions of the system, IRAF (the current default user
system), IRAFO (the old system, for backup), and IRAFX (the developmental
system).  Sites which wish to maintain multiple versions of the system
should beware that only one version of the system can use the shared library,
due to global section name conflicts.  For efficiency reasons the default
user version of the system `IRAF' should use the shared library.
The remaining versions of the system must be relinked (\(sc5.1.4) to keep
them from erroneously trying to use the shared library from a different
release of IRAF.

.NH 2
Save Locally Modified Files
.PP
Login as IRAF.
Ordinarily the only directories containing files you may wish to save are
\fLdev\fR, \fLhlib\fR, and \fLlocal\fR.
The safest and easiest way to do this is to save the entire contents of those 
directories.  For example:
.DS
\fL$ set default [iraf]
$ rename dev.dir,local.dir,[.vms]hlib.dir [\fIdirectory\fP]\fR
.DE
This would physically move (not copy) the \fLdev\fR, \fLlocal\fR, and
\fLhlib\fR directories to the named directory, which must be on the same
disk device as \fL[IRAF]\fR.  Many variations on this are possible.
The destination directory should be somewhere outside the \fL[IRAF...]\fR
directory tree, else it may get deleted when we delete the old system, below.

.NH 2
Read the Distribution Tape or Tapes
.PP
Having temporarily preserved all the locally modified files, it is now
time to delete the old system and read in the new one.  If you are
the cautious (prudent?) type you may wish to first preserve the entire
existing IRAF system on a backup tape (e.g. if something were to have
happened to the distribution tape en route).  Using only the standard
VMS utilities, the old system may be deleted as follows (assuming IRAF
owns all the files in \fL[IRAF...]\fR).
.DS
\fL$ set default [iraf]
$ set protection=(owner:wd) [...]*.*;*
$ delete [...]*.*;*
    \fR(repeat with <ctrl/b> until done)
.DE
.LP
It is normal for the \fLdelete\fR command shown to generate lots of messages
during execution warning that it cannot delete directories because they are
not yet empty.  When the command finally executes without any warning messages
this means the directory tree has been fully deleted.
.LP
Now read the new distribution tape(s).
.DS
\fL$ mount/foreign \fItape\fL:
$ set default \fIdisk\fL:[iraf]
$ backup/rew \fItape\fL:iraf.bck/select=[iraf...] [...]\fR
.DE
.LP
Instead of explicitly deleting the old system it is possible to read the
distribution tape with \fLBACKUP/REPLACE\fR, but this really isn't any
faster since an implicit delete of each existing file is implied, and
if any files or directories have been renamed in the new release, junk
files or directories will be left behind.

.NH 2
Merge Saved Files Back Into the New System
.PP
You can either merge the contents of locally modified files saved from
the previous installation into their counterparts in the new system, or edit
the new ones from scratch.  Whichever is easier depends on how much editing
you had to do in the first place, which probably depends upon the complexity
of your local computer network and the number of peripheral devices.
Any of the site configuration files might be modified in the distributed
system from version to version of IRAF, so in general one cannot simply
replace such a file with the one that was saved.
.PP
Merging means inspecting the two files (newly distributed vs. its
saved counterpart) for differences, and deciding how to update the new
file with local device names, directory pathnames, etc.
The VMS \fLDIFFERENCES\fR utility may or may not be useful for this,
depending on how much a file has changed, and how extensively it
was modified locally.  One thing that is a big help is to keep track of
all local changes made to the standard system in a local "notes" file;
when the next system update occurs or when the installation is repeated on
another cpu at the local site, one can then go down the list and make all
the same changes to the newly installed system.
.KS
.TS
center;
ci ci
l l.
directory	files
.sp
\fLdev\fR	devices, graphcap, termcap
\fLhlib\fR	irafuser.com, install.com, zzsetenv.def, login.cl
\fLhlib/libc\fR	iraf.h (\(-> sys$library:iraf.h)
.TE
.KE
.PP
The files which must be edited, via a diff/merge operation on the saved
files or otherwise, are summarized in the table above.
If you have added packages or tasks of your own to IRAF this would be
a good time to merge them back into the new system and relink them,
although this may be deferred until the new system is up and running if
desired.

.NH 2
Relink If Necessary
.PP
If you received a fully binary distribution of VMS/IRAF and you are not
running an old version of VMS, you do not need to relink and may proceed
to the next section.  See \(sc2.4 for further information on relinking the
system.

.NH 2
Update VMS
.PP
These next operations require system privileges, so login as SYSTEM.
Perform the following series of operations:
.RS
.IP \(bu
The \fL<iraf.h>\fR file may have changed in the new version of the system,
so it should be updated in the VMS system library \fLsys$library\fR.
.IP \(bu
Run the VMS \fLINSTALL\fR utility and deinstall the previously installed IRAF
shared library and executables from the old system, if any.
(In \fLINSTALL\fR, use \fL/list\fR\(dg
.FS
\(dgThe actual \fLINSTALL\fR syntax used is site dependent.
.FE
to see what is currently installed, and \fI<file>\fL/del\fR to
delete individual images; note that actual image deletion will not occur
until any currently executing processes which reference the shared image
terminate).
.IP \(bu
Execute the \fLinstall.com\fR command procedure in \fLhlib\fR to install the
shared library and selected executables from the new system.
.RE
.PP
In a VMS cluster, the shared images must be deinstalled and
reinstalled on all nodes in the cluster which share the same version of IRAF.
Aside from the additional step of deinstalling any previously installed
shared images, these steps are as discussed in \(sc2.5; please refer to that
section for additional information.

.NH
Testing a Newly Installed or Updated System
.PP
Before testing a newly installed or upgraded system it is wise to read the
\fICL User's Guide\fR, the revisions notes, and the list of known bugs,
if one has not already done so.  Before starting the CL you should
do what a regular user would do, i.e., run \fLmkiraf\fR to initialize
the IRAF environment, and then edit the \fLLOGIN.CL\fR file created by
\fLmkiraf\fR as desired.  Any modifications you wish to preserve across
another \fLmkiraf\fR may be placed into the optional \fLLOGINUSER.CL\fR file
to avoid having to edit the \fLLOGIN.CL\fR file.
.DS
\fL$ set default [iraf.local]
$ mkiraf\fR
.DE
.LP
Once the IRAF environment is configured one need only enter the command
`\fLcl\fR' to start up the CL.  After a bit IRAF should print the message of
the day and type out the root IRAF menu, then issue the "\fLcl> \fR" prompt
indicating that it is ready for the first command.
This assumes that the `\fLiraf\fR' command is executed in the \fLLOGIN.COM\fR
file at login time; if this is not the case, `\fLiraf\fR' must first be entered
to define \fLcl\fR and the other VMS logical names and symbols used by IRAF.
.DS
\fL$ cl\fR
.DE
.LP
Once in the CL, you will probably have magtape and printer access, are likely
to have graphics terminal access, and very possibly will not have either
image display access or graphics plotter access.  If the graphics terminal
capability is ready the next step is to run the IRAF test procedure to
verify that all is working well, as well as try out a few of the many tasks
in the system.  If the graphics terminal is not up yet, there is probably
no point in running the test procedure.  To run the test procedure, read
the documentation in the \fIIRAF User Handbook\fR, Volumn 1A, and follow the
instructions therein.  You may also wish to run some of the benchmarks
described in the benchmarks paper included with the installation materials,
to make sure that your VMS system (user quotas and working set) is configured
properly for efficient execution of IRAF.

.NH
System Maintenance
.NH 2
Recompiling or Relinking the System
.PP
The prelinked executables supplied on the distribution tape should
execute on most systems without need to relink.  Relinking is necessary
[1] when installing a "you relink" version of the system, [2] if the local
system runs a significantly older version of VMS than that used when the
distribution tape was made (VMS is conservative and may refuse to execute
images which were linked on a "future" version of the system, i.e., any
release of VMS with a higher version number than the locally installed
system, because the VMS shared libraries may have changed in the newer
revision of the system), or [3] after a bug fix or other revision to a
system library.\(dg
.FS
\(dgIf a module in one of the system libraries is changed it may be necessary
to rebuild the IRAF shared library \fL[IRAF.BIN]S_IRAF.EXE\fR before relinking
the system.
.FE
.PP
The system generation procedures are fully automated hence it is easy
to modify, recompile, or relink all or any portion of the system.  The full
system generation procedure, starting from the system sources, is as follows:
.RS
.IP \(bu
Bootstrap the HSI (done with VMS \fL.COM\fR files).  Requires the DEC C
compiler and old versions of \fLLIBSYS\fR and \fLLIBVOPS\fR, used for VOS
filename mapping.  User sites should \fInever\fR need to bootstrap VMS/IRAF
unless some catastrophe occurs, e.g., an important HSI binary file is deleted
and the VMS/IRAF distribution tape can no longer be located.
.IP \(bu
Recompile the main system libraries.  All VMS/IRAF distributions include
full object libraries, hence it should never be necessary to fully recompile
the system object libraries from scratch, but updating the system libraries
may be necessary before changes to system files will take effect, e.g., when
caching new termcap or graphcap device entries (\(sc5.2.3).
.IP \(bu
Rebuild the IRAF shared library if used (the system can be linked without
the shared library if desired).  This will be necessary if a module
is updated in a system library, e.g., to fix a bug or change a compile time
option (as when caching new termcap or graphcap entries).  The shared library
must be rebuilt for changes to modules in the main system object libraries
to have any effect.  Relinking the shared library may also be necessary on
old versions of VMS; this is indicated if after relinking with the shared
library provided on the distribution tape, VMS still refuses to run the
newly linked process (make sure an old executable is not being run mistakenly
because it is still "installed").
.IP \(bu
Recompile and relink the applications packages.  Relinking is necessary when
installing a "you relink" version of the system, and may be necessary when
installing the distributed system on an old version of VMS.  The system must
be relinked before changes to the shared library or the system object libraries
will have any effect.  Note also that any installed images (\(sc2.3.3) must
be deinstalled and reinstalled after relinking, before changes to the installed
executables will take effect.
.RE
.PP
All VMS/IRAF distributions come with the bootstrap already performed, and with
the system and package object libraries already compiled and the shared library
already built.  A fully binary distribution includes the executables as well,
and can be run as is.
.PP
Once the system has been bootstrapped the HSI bootstrap utility program
\fLmkpkg\fR is used for all system maintenance.
Documentation for this and the other utilities in the SOFTOOLS package
may be found in the \fIIRAF User Handbook\fR, and is also available in
the online system via \fLhelp\fR.  The \fLmkpkg\fR program,
like all the HSI utilities, may be run either from DCL or from the IRAF CL.

.NH 3
Bootstrapping the HSI
.PP
There are two different sets of executables in the system, the so-called
"bootstrap utilities" (part of the HSI, or host system interface), and the
regular system executables.  The bootstrap utilities are the \fL.exe\fR files
in \fLhlib\fR, e.g., \fLmkpkg\fR, \fLrtar\fR, and so on.  The regular IRAF
executables are those maintained in the \fLbin\fR directory.
.PP
The bootstrap utilities are required for system maintenance of the main system
and hence must be linked first.  However, unless a bug fix or other revision
is made to one of the main system libraries (particularly \fLlibos\fR), there
should be no reason for a user site to ever need to bootstrap the system.
The bootstrap utilities are linked with the option \fL/NOSYSSHARE\fR, hence
they should run on any version of VMS.
.PP
The bootstrap utilities are written in C, and the DEC C compiler is required
to compile or link these utilities.  The C compiler is \fInot\fR required to
run the prelinked binaries distributed with the system.  To recompile and link
the bootstrap utilities, i.e., to "bootstrap" IRAF, enter the commands shown
below.  Note that the HSI is always recompiled during a bootstrap; there
is no provision for merely relinking the entire HSI (some individual programs
can however be relinked manually by doing a \fLmkpkg update\fR in the program
source directory).
.DS
\fL$ set default [iraf.vms]
$ set verify
$ @rmbin.com
$ @mkpkg.com\fR
.DE
.PP
The bootstrap should take 30 to 45 minutes on an unloaded 11/780.
Once the bootstrap utilities have been relinked and installed in \fLhlib\fR,
the main system may be relinked.

.NH 3
Updating the System Libraries
.PP
Updating the system libraries, i.e., recompiling selected object modules and
inserting them into the system object libraries, is necessary if any system
source files are edited or otherwise modified, as when using the
\fLmkttydata\fR utility program to cache new termcap or graphcap device
entries.
.PP
The system libraries may be updated either from within the IRAF CL, or from
DCL.  For example, from within DCL:
.DS
\fL$ set def [iraf]
$ mkpkg syslibs \fR[\fPmathlibs\fR]\fR
.DE
This command will run the \fLmkpkg\fR utility, which will update each system
library in turn, descending into the tree of source directories and checking
the date of each object module therein against the dates of the source files
and dependency files used to produce the object module, and recompiling any
object modules which are out of date.  In the worst case, e.g., if the
system libraries have been deleted, the entire VOS (virtual operating system)
will be recompiled.  If it is known that no changes have been made to the
math library source files, the optional \fLmathlibs\fR argument may be
omitted to save some time.

.NH 3
Relinking the IRAF Shared Library
.PP
The IRAF shared library (\fL[IRAF.BIN]S_IRAF.EXE\fR) is constructed by
linking the contents of the four main IRAF system libraries \fLLIBEX\fR,
\fLLIBSYS\fR, \fLLIBVOPS\fR, and \fLLIBOS\fR.  Hence, the system libraries
must exist and be up to date before linking the shared library.  The shared
library must be installed in \fLbin\fR before it can be used to link any
applications programs.  At present, the shared library does not use
transfer vectors, hence the full system must be relinked following any
changes to the shared library.  Note that since the shared library is
normally installed in system memory, all installed IRAF images should be
deinstalled and later reinstalled whenever the shared library is rebuilt.
IRAF will not be runnable while this is taking place.
.PP
In the current release of IRAF (V2.5) the procedure for building the shared
library has not yet been integrated into the automatic system generation
procedure.  Furthermore, utility tasks in the main IRAF system are currently
used to build the shared library, so it is necessary to have at least part
of the main system functioning before the shared library can be built.  These
problems (and the transfer vector problem) will be fixed in a future release
of VMS/IRAF.
.PP
The shared library is rebuilt from within a running IRAF system providing
at a minimum the three executables \fLCL.EXE\fR, \fLX_SYSTEM.EXE\fR, and
\fLX_LISTS.EXE\fR.  If these executables do not yet exist or not runnable,
rebuild them as follows.  Note the use of the \fL-z\fR link flag to link
the new executables without the shared library.
.DS
\fL$ set default [iraf.pkg.cl]
$ mkpkg update lflags=-z
$ set default [iraf.pkg.system]
$ mkpkg update lflags=-z
$ set default [iraf.pkg.lists]
$ mkpkg update lflags=-z\fR
.DE
.LP
Now login to the CL and proceed to the directory \fLhlib$share\fR, which
contains the code used to build the shared library.
.DS
\fL$ set default [iraf.local]
$ cl
    \fR(cl starts up...)\fP
cl> cd hlib$share\fR
.LP
.DE
The following commands will rebuild the shared library and install it in
\fLbin\fR.  This takes ten minutes or so.
.DS
\fLcl> cl < makeshare.cl
    \fR(takes a while)\fP
cl> mkpkg install
cl> purge
cl> logout\fR
.DE
.PP
The system library can be updated (\(sc5.1.2) and the shared library rebuilt
while IRAF is in use by normal users, however as soon as the command
\fLmkpkg install\fR is executed (moving the new \fLS_IRAF.EXE\fR to
\fLbin\fR) execution of normal user programs is at risk and one should
kick all IRAF users off the machine and deinstall any installed IRAF images.
The system may now be relinked and the newly linked shared library and images
reinstalled, after which the system is ready for normal use again.

.NH 3
Relinking the Main IRAF System
.PP
The following DCL commands may be entered to relink the main IRAF system,
installing the newly linked executables in \fLbin\fR.  Note that any
IRAF executables that have been installed in system memory must be
deinstalled and reinstalled (\(sc2.3.3) following the relink.
.DS
\fL$ set default [iraf]
$ mkpkg update\fR
.DE
.PP
The command shown causes the full system to be relinked (or recompiled and
relinked if any files have been modified) using the default compile and
link switches defined in the global \fLmkpkg\fR include file \fLmkpkg.inc\fR
in \fLhlib\fR.  Any system wide changes in how the system is generated should
be implemented by editing the contents of this file.  For example, the default
file supplied with the system includes the following line:
.DS
\fL$set    LFLAGS  = ""            # default XC link flags\fR
.DE
This switch defines the switches to be passed to the HSI bootstrap utility
program \fLxc\fR to link the IRAF executables (no switches are given, hence
the default link action will be taken).
The switch \fL-z\fR may be included in the \fLxc\fR command line to link
an executable directly against the system object libraries, rather than
using the shared library.  Hence, to relink IRAF without using the IRAF
shared library (as is necessary for all but one system when there are multiple
versions of IRAF simultaneously installed on the same machine) we would change
the definition of \fLLFLAGS\fR as follows:
.DS
\fL$set    LFLAGS  = "-z"          # default XC link flags\fR
.DE
.PP
A full system relink takes approximately 30 minutes, or significantly less
if the shared library is used.  A full system compile and relink takes 9-24
hours depending upon the host system.

.NH 3
Relinking or Updating IRAF Packages
.PP
Should it be necessary to fix a bug in one of the applications packages,
it is very easy to recompile and relink the package and install the new
executable.  After editing the affected files, enter any of the following
commands (this is normally done from within the CL):
.DS
.TS
l l.
\fLmkpkg\fR	Makes an executable in the package directory.
\fLmkdebug\fR	Same, but with the debugger linked in.  Linking with the
	debugger does not defeat use of the shared libraries.
\fLmkpkg install\fR	Used after "mkpkg" (but not mkdebug!) to
 	install the new executable in BIN.
\fLmkpkg udpate\fR	Relinks the new executable and installs it in
 	BIN all in one shot.
.TE
.DE
.LP
The preferred command is \fLmkpkg update\fR when it is desired to update
the package library, relink the package executable (or executables), and
install these in \fLbin\fR for use in the runtime system.
.PP
If extensive revisions are contemplated, a copy of the entire package should
be made and moved out of the system, and that version of the package modified,
rather than modifying the standard package, which will only make updating the
system more difficult.  It will be necessary to delete or replace the installed
executables in \fLbin\fR before the revised ones can be used.
One need only redefine or modify the directory pathname in the \fLtask\fR
declaration for the package to cause a different version of the package
to be used.  It is not necessary to physically install locally modified code
in the main system to make use of it, and this is not recommended as it may
be lost when the system is subsequently updated.

.NH 2
Tuning Considerations
.PP
There are a number of things that can be done to tune VMS/IRAF for a
particular host system:
.sp
.RS
.IP \(bu
Install the executables to permit shared memory.
.IP \(bu
Render the large runtime files contiguous to increase i/o efficiency.
.IP \(bu
Precompile selected \fLTERMCAP\fR and \fLGRAPHCAP\fR entries.
.IP \(bu
Strip the system to reduce disk consumption.
.RE

.NH 3
Installing Executable Images
.PP
The standard distribution of VMS/IRAF is configured to use a shared library
for the bulk of the IRAF runtime system code.  Use of this shared library
considerably reduces the disk requirements of the system, while reducing
runtime system memory usage and process pagein time, and speeding up process
links during software development.
.PP
If the goal of minimizing physical memory utilization is to be achieved
when running IRAF, it is essential that the IRAF shared library be "installed"
in VMS system shared memory.  Further memory savings through memory sharing
can be achieved if some of the more commonly used IRAF executables are also
installed.  This becomes increasingly important as the number of IRAF users
increases, but some memory savings may result even if there is only one user,
since a single user may spawn batch jobs.  Hence, the shared library
\fLs_iraf.exe\fR should always be installed if IRAF is used at all, and
\fLcl.exe\fR and \fLx_system.exe\fR are used by all IRAF jobs and should
be installed if there is usually at least one IRAF user on the system.
If there are usually several IRAF users on the system \fLx_images.exe\fR and
\fLx_plot.exe\fR are probably worth installing as well.
.PP
Installation of images in system shared memory is done with the VMS
\fLINSTALL\fR utility,
which requires SYSTEM privilege to execute (see also \(sc2.3.3).
Installation of the IRAF shared library and other images is most conveniently
done by executing the \fLINSTALL.COM\fR command procedure in \fLhlib\fR.
This file should be reviewed and edited during system installation to
select those images to be installed on the local host.  The file may be
directly executed to temporarily install the images, but to permanently
install the images the system bootstrap procedure should be modified to
execute the \fLINSTALL.COM\fR script (or explicitly install the images by
name) whenever the system is booted.  Note that any currently installed
images must be explicitly deinstalled before a new version can be installed,
and that the global pages used by an installed image are not actually freed
until any and all processes using those global pages terminate.
.PP
The procedure for installing images will fail if there are insufficient
global pages available in the system (for example, the number of global
pages required to install the shared library in VMS/IRAF V2.5 is about 760).
If the system has insufficient free global pages to run the \fLINSTALL.COM\fR
script one must either increase the number of system global pages (which
requires rebooting the system), or one must edit the \fLINSTALL.COM\fR
script to reduce the number of images to be installed.

.NH 3
Rendering Large Runtime Files Contiguous
.PP
The speed with which large disk files can be read into memory in VMS can
degrade significantly if the disk is highly fragmented, which causes a
large file to be physically stored in many small fragments on the disk.
IRAF performance as well as VMS system throughput can therefore be improved
by rendering frequently referenced large files contiguous.  Examples of
files which it might pay to render contiguous are the executables in
\fLbin\fR, all of the runtime files in \fLdev\fR, and the large system
libraries in \fLlib\fR (this last is only useful to speed software
development, i.e., linking).
.PP
The easiest way to render a file contiguous in VMS is to use
\fLCOPY/CONTIGUOUS\fR to copy the file onto itself, and then purge the
old version.  Various nonstandard utility programs are available commercially
which will perform this function automatically.  The contents of an entire
disk may be rendered contiguous or nearly contiguous by backing up the disk
onto tape and then restoring it onto a clean disk.

.NH 3
Precompiling TERMCAP and GRAPHCAP Entries
.PP
Precompilation of a termcap or graphcap entry is a technique used to
speed runtime access of the entry for that device.  If the entry is not
precompiled the termcap or graphcap file must be physically opened and
scanned at runtime to read the desired entry.  This causes a noticeable
delay of as much as a second when clearing the terminal screen or plotting
a graph, hence it is usually worthwhile to cache the entries for commonly
used video and graphics terminals.  It is generally not worthwhile for
printers, plotters, and image displays.
.PP
The system comes with selected termcap and graphcap entries already
precompiled.  To see which devices are precompiled, page the cache data files,
\fLdev$cachet.dat\fR (for termcap) and \fLdev$cacheg.dat\fR (for graphcap).
To cache a different set of entries one must regenerate these files with the
\fLmkttydata\fR task in the SOFTOOLS package, and then do a full sysgen relink
with the \fLmkpkg\fR utility.  Detailed instructions are given in the manual
page for \fLmkttydata\fR.\(dg
.FS
\(dg\fBImportant Note\fR: If the system is configured to use the IRAF shared
library (the default), you must update the system libraries (\(sc5.1.2) and
rebuild the shared library (\(sc5.1.3) before relinking the system \(sc5.1.4),
else the changes to the termcap and graphcap cache files will have no effect.
The use of the shared library is peculiar to VMS/IRAF and is not discussed
in the \fLmkttydata\fR documentation.
.FE

.NH 3
Stripping the System to Reduce Disk Consumption
.PP
If the system is to be installed on multiple cpu's, or on a particularly
small host like a MicroVAX, it may be necessary or desirable to strip the
system of all non-runtime files to save disk space.  This is done by deleting
all the program sources and all the sources for the reference manuals and
other printed documentation, but not the online manual pages.  A special
utility called \fLrmfiles\fR (in the SOFTOOLS package) is provided for this
purpose.  It is not however necessary to run \fLrmfiles\fR directly to strip
the system.  This may be done either from DCL or from within the CL.
.DS
\fL$ set default [iraf]
$ mkpkg strip\fR
.DE
This will preserve all runtime files, permitting use of the standard runtime
system as well as user software development.  The size of the system (the
figures are for V2.5) is reduced from about 39 Mb (megabytes) to around 13 Mb.
One can optionally enter the command "\fLmkpkg stripall\fR" to delete
the system libraries as well, but this saves only another couple of Mb and
precludes all IRAF related software development, including user written
Fortran programs (IMFORT), hence is not recommended.

.NH 2
VMS Quotas and Privileges Required to Run IRAF
.PP
The only special privilege required by IRAF is TMPMBX, which is probably
already standard on your system.  Systems with DECNET capabilities should also
give their users NETMBX privilege, although it is not required to run IRAF.
No other privileges are required or useful for normal activities.
.PP
Although privileges are no problem for VMS/IRAF,
it is essential that the IRAF user have sufficient VMS quota,
and that the system tuning parameters be set correctly,
otherwise IRAF will not be able to function well or may not function at all.
If a quota is exceeded, or if the system runs out of some limited resource,
the affected VMS system service will return an error code to IRAF and the
operation will fail (this frequently happens when trying to spawn a connected
subprocess).  The current recommended ranges of per-user quotas are summarized
below.
.KS
.TS
center;
ci ci ci
l n n.
quota	minimum	recommended
.sp
\fLBYTLM\fR	20480	30720
\fLPGFLQUOTA\fR	15000	30720
\fLPRCLM\fR	5	10
\fLWSDEFAULT\fR	512	512
\fLWSEXTENT\fR	4096	WSMAX
\fLJTQUOTA\fR	2048	2048
.TE
.KE
.PP
The significance of most of these quotas is no different for IRAF than for
any other VMS program, hence we will not discuss them further here.
The \fLPRCLM\fR quota is especially significant for IRAF since an IRAF job
typically executes as several concurrent processes.  The \fLPRCLM\fR quota
determines the maximum number of subprocesses a root process (user) may have.
Once the quota has been reached process spawns will fail causing the IRAF
job or operation to abort.
.PP
The minimum number of subprocesses a CL process
can have is 1 (\fLx_system.e\fR).  As soon as a DCL command is executed via
OS escape a DCL subprocess is spawned, and we have 2 subprocesses.
The typical process cache limit is 3, one slot in the cache being used by
\fLx_system.e\fR, hence with a full cache we have 4 subprocesses (the user
can increase the process cache size if sufficient quota is available to
avoid excessive process spawning when running complex scripts).  It is common
to have one graphics kernel connected, hence in normal use the typical
maximum subprocess count is 5.  However, it is conceivable to have up to
3 graphics kernel processes connected at any one time, and whenever a
background job is submitted to run as a subprocess a whole new subprocess
tree is created.  Hence, it is possible to run IRAF with a \fLPRCLM\fR of
5, but occasional process spawn failures can be expected.  Process spawn
failures are possible even with a \fLPRCLM\fR of 10 if subprocess type batch
jobs are used (the default), but in practice such failures are rare.
If all batch jobs are run in batch queues it should be possible to work
comfortably with a \fLPRCLM\fR of 5-6, but in practice users seem to prefer
to not use the batch queues, except for very large jobs.
.PP
Since IRAF uses memory efficiently the working set parameters do not
seem critical to IRAF, provided the values are not set unrealistically low,
and provided \fLWSEXTENT\fR is set large enough to permit automatic growth
of a process working set when needed.  Configuring VMS to steal pages from
inactive processes is not recommended as it largely cancels the effect of
the process cache, causing process pagein whenever a task is executed.
It is better to allow at least a minimum size working set to each process.
.PP
In addition to sufficient per user authorized quota, the system tuning
parameters must be set to provide enough dynamically allocatable global
pages and global sections to handle the expected load.  If these parameters
are set too small, process connects will fail intermittently, usually when
the system load is high.  Each subprocess needs about 10 global pages when
activated (IRAF uses global pages and shared memory for interprocess
communications, due to the relatively low bandwidth achievable with the
VMS mailbox facilities).
With IRAF in heavy use (i.e., a dozen simultaneous users) this can easily
reach a requirement for several hundred additional global pages.
Each installed image and subprocess also needs at least one, usually two,
global sections.  The system parameters on our 8600 (which is probably a
worst case example) are currently set to \fLGBLPAGES\fR = 12220
and \fLGBLSECTIONS\fR = 230.

.NH
Interfacing New Graphics Devices
.PP
There are three types of graphics devices that concern us here.
These are the graphics terminals, graphics plotters, and image displays.
The topic of interfacing to these devices has already been discussed in
prior IRAF Newsletters, hence the discussion given here will be brief.

.NH 2
Graphics Terminals
.PP
The IRAF system as distributed is capable of talking to just about any
conventional graphics terminal and most workstations, using the \fLSTDGRAPH\fR
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
The current IRAF system comes with several graphics kernels usable to drive
graphics plotters.  The standard plotter interface is via the SGI graphics
kernel, which has largely replaced the older \fLNCAR\fR kernel used in earlier
versions of IRAF (in those earlier versions the \fLNCAR\fR kernel was called
the \fLSTDPLOT\fR kernel).  Further information on the SGI plotter interface
is given in the paper \fIThe IRAF Simple Graphics Interface\fR, a copy of which
is included with the installation kit.  SGI device interfaces for most VAX/VMS
plotter devices already exist, and adding support for new devices is
straightforward.  Sources for the SGI device translators supplied with the
distributed system are maintained in the directory \fL[iraf.vms.gdev.sgidev]\fR.
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
VAX/VMS (called MCVAX) are widely available.  A site which already has the
NCAR software may wish to go this route, but the SGI interface will provide
a more efficient and simpler solution in most cases.
.PP
The remaining possibility with the current system is the \fLCALCOMP\fR kernel.
Many sites will have a Calcomp or Versaplot library (or Calcomp compatible
library) already available locally.  To make use of such a library to get
plotter output on any devices supported by the interface, one may copy
the library to the \fLhlib\fR directory and relink the Calcomp graphics
kernel.  The following commands should suffice to install the calcomp or
Versaplot library, and relink and install the Calcomp graphics kernel in IRAF:
.DS
\fL$ set default [iraf.vms.hlib]
$ copy \fI<library_file>\fP libcalcomp.olb
$ set default [iraf.sys.gio.calcomp]
$ mkpkg update\fR
.DE
A graphcap entry for each supported device may also be required.
.PP
Note that sites having a Versatec plotter but lacking the system software
driver \fLLVDRIVER\fR (or whose only driver is Versatec's RASM, e.g.
\fLSYS$SYSTEM:RASM.EXE\fR) must link the Calcomp graphics kernel with
the Versaplot library (usually \fLSYS$LIBRARY:PHASE1.OLB\fR -- copy to
libcalcomp as shown above) and then use that kernel to access those plotters.
In that case you will still need to execute the RASM task after the
kernel finishes executing.  Sites in this situation should contact us
for advice on how to make plotting easier.

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
The current full VMS/IRAF directory structure is documented graphically in
the appendix.  The main branches of the tree are the following; beneath the
directories shown are some 250 subdirectories, the largest directory trees
being \fLsys\fR, \fLpkg\fR, and \fLnoao\fR.  The entire contents of all
directories other than \fLvms\fR, \fLlocal\fR, and a few configuration files
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
\fLvms        \fR- the VMS host system interface (HSI = kernel + bootstrap utilities)
.DE
.LP
The contents of the \fLvms\fR directory (the VMS host system interface) are
as follows:
.DS
\fLas         \fR- assembler sources for VMS/IRAF
\fLboot       \fR- bootstrap utilities (mkpkg, rtar, wtar, etc.)
\fLgdev       \fR- graphics device interfaces (SGI device translators)
\fLhlib       \fR- host dependent runtime files
\fLmkpkg.com  \fR- executed to bootstrap the VMS/IRAF HSI
\fLos         \fR- OS interface routines (VMS/IRAF kernel)
\fLrmbin.com  \fR- executed to delete binary files in subdirectories
.DE
.PP
If you will be working with the system much at the system level, it will be
well worthwhile to spend some time exploring these directories and gaining
familiarity with the system.
