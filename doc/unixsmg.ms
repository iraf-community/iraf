.RP
.TL
UNIX/IRAF Site Manager's Guide
.AU
Doug Tody
.AI
Central Computer Services
.br
.K2 "" "" "\(dg"
.br
June 1989

.AB
An IRAF \fIsite manager\fR is anyone who is responsible for installing and
maintaining IRAF at a site.  This document describes a variety of site
management activities, including configuring the device and environment
tables to provide reasonable defaults for the local site, adding interfaces
for new devices, configuring and using IRAF networking, the installation
and maintenance of layered software products (external packages),
and configuring a custom site LOCAL package so that local software may be
added to the system.  Background information on multiple architecture
support and the software management tools provided
with the system is presented.  The procedures for rebooting IRAF and
performing a sysgen are described.  The host system resources
required to run IRAF are discussed.
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
2.\h'|0.4i'\fBSystem Setup\fP\l'|5.6i.'\0\02
.br
\h'|0.4i'2.1.\h'|0.9i'Installing the System\l'|5.6i.'\0\02
.br
\h'|0.4i'2.2.\h'|0.9i'Configuring the Device and Environment Tables\l'|5.6i.'\0\02
.br
\h'|0.9i'2.2.1.\h'|1.5i'Environment definitions\l'|5.6i.'\0\02
.br
\h'|0.9i'2.2.2.\h'|1.5i'The template LOGIN.CL\l'|5.6i.'\0\03
.br
\h'|0.9i'2.2.3.\h'|1.5i'The DEVICES file\l'|5.6i.'\0\03
.br
\h'|0.9i'2.2.4.\h'|1.5i'The DEVICES.HLP file\l'|5.6i.'\0\03
.br
\h'|0.9i'2.2.5.\h'|1.5i'The TERMCAP file\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.6.\h'|1.5i'The GRAPHCAP file\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.7.\h'|1.5i'Configuring IRAF networking\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.8.\h'|1.5i'Configuring the IRAF account\l'|5.6i.'\0\06
.br
\h'|0.9i'2.2.9.\h'|1.5i'Configuring user accounts for IRAF\l'|5.6i.'\0\06
.br
\h'|0.4i'2.3.\h'|0.9i'Tuning Considerations\l'|5.6i.'\0\06
.br
\h'|0.9i'2.3.1.\h'|1.5i'Caching TERMCAP and GRAPHCAP device entries\l'|5.6i.'\0\07
.br
\h'|0.9i'2.3.2.\h'|1.5i'Stripping the system to reduce disk usage\l'|5.6i.'\0\07
.sp
3.\h'|0.4i'\fBSoftware Management\fP\l'|5.6i.'\0\07
.br
\h'|0.4i'3.1.\h'|0.9i'Multiple architecture support\l'|5.6i.'\0\07
.br
\h'|0.4i'3.2.\h'|0.9i'Layered software support\l'|5.6i.'\0\09
.br
\h'|0.4i'3.3.\h'|0.9i'Software management tools\l'|5.6i.'\0\09
.br
\h'|0.4i'3.4.\h'|0.9i'Modifying and updating a package\l'|5.6i.'\0\10
.br
\h'|0.4i'3.5.\h'|0.9i'Installing and maintaining layered software\l'|5.6i.'\0\11
.br
\h'|0.4i'3.6.\h'|0.9i'Configuring a custom LOCAL package\l'|5.6i.'\0\12
.br
\h'|0.4i'3.7.\h'|0.9i'Updating the full IRAF system\l'|5.6i.'\0\13
.br
\h'|0.9i'3.7.1.\h'|1.5i'The BOOTSTRAP\l'|5.6i.'\0\13
.br
\h'|0.9i'3.7.2.\h'|1.5i'The SYSGEN\l'|5.6i.'\0\13
.sp
4.\h'|0.4i'\fBInterfacing New Graphics Devices\fP\l'|5.6i.'\0\14
.br
\h'|0.4i'4.1.\h'|0.9i'Graphics terminals\l'|5.6i.'\0\14
.br
\h'|0.4i'4.2.\h'|0.9i'Graphics plotters\l'|5.6i.'\0\14
.br
\h'|0.4i'4.3.\h'|0.9i'Image display devices\l'|5.6i.'\0\15
.br
\h'|0.4i'4.4.\h'|0.9i'Network access to graphics devices\l'|5.6i.'\0\15
.sp
5.\h'|0.4i'\fBHost System Requirements\fP\l'|5.6i.'\0\16
.br
\h'|0.4i'5.1.\h'|0.9i'Memory requirements\l'|5.6i.'\0\16
.br
\h'|0.4i'5.2.\h'|0.9i'Disk requirements\l'|5.6i.'\0\16
.br
\h'|0.4i'5.3.\h'|0.9i'Diskless nodes\l'|5.6i.'\0\16
.sp
\fBAppendix A.\0The IRAF Directory Structure\fP\l'|5.6i.'\0\17
.nr PN 0
.bp

.NH
Introduction
.PP
Once the IRAF system has been installed it will run, but there remain many
things one might want to do to tailor the system to the local site.
Examples of the kinds of customizations one might want to make are the
following.
.RS
.IP \(bu
Edit the default IRAF environment definitions to provide reasonable
defaults for your site.
.IP \(bu
Make entries in the device descriptor tables for the devices in use at
your site.
.IP \(bu
Code and install new device interfaces.
.IP \(bu
Enable and configure IRAF networking, e.g., to permit remote image
display, tape drive, or file access.
.IP \(bu
Perform various optimizations, e.g., stripping the system to reduce disk
usage, or caching terminal device entries to minimize runtime searches
of the device tables.
.IP \(bu
Extend the system by installing layered software products.
.IP \(bu
Configure a custom LOCAL package so that locally developed software
may be installed in the system.
.RE
.PP
This document provides sufficient background information and instructions
to guide the IRAF site manager in performing such customizations.  Additional
help is available via the IRAF HOTLINE (\f(CW602 323-4160\fP), or by sending
mail to \f(CWiraf@noao.edu\fP (internet) or \f(CW5355::iraf\fP (SPAN).
Contributions of interfaces developed for new devices, or any other software
of general interest, are always welcome.
.PP
The IRAF software is organized in a way which attempts to isolate, so far
as possible, the files or directories which must be modified to tailor the
system for the local site.  Most or all changes should affect only files
in the \f(CWlocal\fP, \f(CWdev\fP, and \f(CWhlib\fP (\f(CWunix/hlib\fP)
directories.  Layered software products, including locally added software,
reside outside of the IRAF core system directory tree and are maintained
independently of the core system.
.PP
To facilitate periodic core system updates it is suggested that
the site manager keep a log of all changes to files in the core system.
A complete record of all modifications made to the core system by NOAO to
prepare the current release will be found in the \fBsystem notes file\fP in
the IRAF directory \f(CWlocal\fP; this is the primary source of technical
documentation for each release and should be consulted if questions arise
regarding any of the system level features added in a new release of the
core system.  The system notes files for older releases will be found in
the \f(CWdoc\fP directory.

.NH
System Setup
.NH 2
Installing the System
.PP
The procedure for installing or updating a UNIX/IRAF system is documented in
the appropriate UNIX/IRAF Installation Guide (e.g.,
the \fIBSD-UNIX/IRAF Installation Guide\fP for BSD/IRAF).
In short, an IRAF tape or network distribution is
obtained from NOAO and installed according to the instructions given in
the installation guide.  The result is a complete IRAF system, including both
sources and executable binaries for the architectures to be supported.
The system will have been modified to reflect the new IRAF root directory
and should run, but will otherwise be a generic IRAF distribution.
To get the most out of an IRAF installation it will be necessary to perform
some of the additional steps outlined in the remainder of this document.

.NH 2
Configuring the Device and Environment Tables
.PP
Teaching IRAF about the devices, network nodes, external programs,
and other special resources available at a site is largely a matter of
editing a standard set of device descriptor and environment setup files,
all of which are simple text files.
The versions of these files provided with the distribution are simply
those in use on the NOAO system from which the tapes were made,
at the time the tapes were generated.
Hence while these files may be useful as examples of properly
configured descriptor files, the defaults, and many specific device entries,
may be meaningless for a different site.  This is harmless
but it may be confusing to the user if, for example, the default printer
doesn't exist at your site.
.PP
The device and environment files also contain much material which any site
will need, however, so care must be taken when editing the files.  Important
changes may be made to the global portions of these files as part of any
IRAF release.  To facilitate future updates, it is wise where possible to
isolate any local changes or additions so that they may be copied into the
new (distributed) version of the file in a future update.
.NH 3
Environment definitions
.PP
Since IRAF is a machine and operating system independent, distributed system
it has its own environment facility apart from that of the host system.
Host system environment
variables may be accessed as if they are part of the IRAF environment
(which is sometimes useful but which can also be dangerous), but if the
same variable is defined in the IRAF environment it is the IRAF variable
which will be used.  The IRAF environment definitions, as defined at CL
startup time, are defined in a number of files in the \f(CWhlib\fP directory.
Chief among these is the \f(CWzzsetenv.def\fP file.  Additional user
modifiable definitions may be given in the template \f(CWlogin.cl\fP
file (see \(sc2.2.2).
.PP
The \f(CWzzsetenv.def\fP file contains a number of environment definitions.
Many of these define IRAF logical directories and should be left alone.
Only those definitions in the header area of the file should need to be
edited to customize the file for a site.  It is the default editor,
default device, etc. definitions in this file which are most likely to
require modification for a site.
.PP
If the name of a default device is modified, the named device must also
have an entry in the \f(CWtermcap\fR file (terminals and printers) or
the \f(CWgraphcap\fR file (graphics terminals and image displays) in
\f(CWdev\fR.  There must also be an \fIeditor\f(CW.ed\fR file in \f(CWdev\fR
for the default editor; EDT, EMACS, and VI are examples of currently
supported editors.
.PP
Sample values of those variables most likely to require modification for
a site are shown below.
.DS
\f(CWset editor      = "vi"
set printer     = "versatec"
set stdimage    = "imt512"
set stdplot     = "lw"
.DE
.PP
For example, you may wish to change the default editor to "\f(CWemacs\fR",
the default printer to "\f(CWlw\fR", or the default image display to
"\f(CWiism75\fR". 
Note that the values of \f(CWterminal\fP and \f(CWstdgraph\fP, which also
appear in the \f(CWzzsetenv.def\fP file, have little meaning except for
debugging processes run standalone, as the values of the environment
variables are reset automatically by \f(CWstty\fP at login time.
The issues of interfacing new graphics and image display
devices are discussed further in \(sc4.
.NH 3
The template LOGIN.CL
.PP
The template login.cl, \f(CWhlib$login.cl\fP in IRAF filename nomenclature,
is the file used by \f(CWmkiraf\fP to produce the user login.cl file.
The user login.cl file, after having possibly been edited by the user,
is read by the CL every time a new CL is started, with the CL processing
all environment and task definitions, package loads, etc., in the login
file.  Hence this file plays an important role in establishing the IRAF
environment seen by the user.
.PP
Examples of things one might want to change in the template login.cl
are the commented out environment definitions, the commented out CL
parameter assignments, the foreign task definitions making up the default
\f(CWuser\fP package, and the list of packages to be loaded at startup
time.  For example, if there are host tasks or local packages which
should be part of the default IRAF operating environment at your site,
the template login.cl is the place to make the necessary changes.
.NH 3
The DEVICES file
.PP
This file \f(CWdev$devices\fP should contain an entry for all allocatable
devices (primarily tape drives) for the local system.  The comments in the
file, plus the sample entries shown, should suffice to document what should
be entered.  The logical names of magtape devices must begin with "mt";
IRAF magtape devices are typically named "mta", "mtb", and so on.
.PP
Allocation of a device in UNIX/IRAF involves modifying the ownership and
access modes of the special files for the device in \f(CW/dev\fP.
Since most UNIX systems do not provide any host level device allocation
facilities the device files are manipulated directly by IRAF via a special
program, \f(CWalloc.e\fP, which is given suid-root permissions when the
IRAF install script is run.
.PP
When a magtape device is allocated by a user,
the user owns the device and has exclusive access to the device.
Failure to deallocate a device, e.g., because the user kills the CL
rather than logging out (as may happen when a network login connection is
broken) can leave the device still allocated to the user,
rendering it inaccessible by other users unless they log into IRAF to
reallocate the device.  A host level program \f(CWallocate\fP,
currently not part of the standard UNIX/IRAF distribution,
is available if it is desired to be able to control device
allocation at the host level without having to log into IRAF.
The IRAF device allocation facilities will work properly even when a
tape drive is remotely accessed over the network.
.NH 3
The DEVICES.HLP file
.PP
All physical devices that the user might need to access by name should be
documented in the file \f(CWdev$devices.hlp\fP.  Typing
.DS
\f(CWcl> help devices\fP
.DE
or just
.DS
\f(CWcl> devices\fP
.DE
in the CL will format and output the contents of this file.  Note that it is
the IRAF name of the device, as given in files such as \f(CWdev$devices\fP,
\f(CWtermcap\fP, etc., which should appear in this help file.
.NH 3
The TERMCAP file
.PP
There must be entries in this file for all local terminal and printer
devices you wish to access from IRAF (there is currently no \f(CWprintcap\fR
file in IRAF).  The entry for a printer contains one special (nonstandard
termcap) entry, called DD.  This consists of three fields: node!device,
the template for the temporary spoolfile, and the UNIX command to be used
to dispose of the file to the printer.  On Berkeley UNIX derived systems
it is rarely necessary to make use of the node name capability, since 
\f(CWlpr\fR already provides this capability.
.PP
If you have a new terminal which has no entry in the IRAF termcap file,
you may already have an entry in the host system termcap file.  Simply copy it
into the IRAF file; both systems use the same basic termcap file format.
If the terminal in question is a graphics terminal a corresponding entry
is required in the \f(CWdev$graphcap\fR file, and you must add a `\f(CW:gd\fR'
capability to the termcap entry to signify that the terminal supports
vector graphics.  If the graphcap entry has a different name than the
termcap entry, the format `\f(CW:gd=\fIgname\fR' should be used instead.
.NH 3
The GRAPHCAP file
.PP
There must be entries in this file for all graphics terminals, batch plotters,
and image displays accessed by IRAF programs.  New graphics terminals will
need a new entry.  The IRAF file \f(CWsys$gio/doc/gio.hlp\fR contains
documentation describing how to prepare graphcap device entries.
A printed copy of this document is available upon request, however
once IRAF is up you may find it easier to generate your own copy using
\f(CWhelp\fR, as follows:
.DS
\f(CWcl> cd sys$gio/doc
cl> help gio.hlp fi+ | lprint\fR
.DE
which will print the document on the default IRAF \f(CWprinter\fP device.
.PP
The HELP pages for the IRAF \fBshowcap\fR and \fBstty\fR tasks should also be
printed as these utilities are useful for generating new graphcap entries.
Help preparing new graphcap device entries is available if needed.
We ask that new graphcap entries be sent back to us so that we may include
them in the master graphcap file for all to benefit.
.NH 3
Configuring IRAF networking
.PP
The \f(CWdev\fR directory contains several files (\f(CWhosts\fR,
\f(CWhostlogin\fR, and \f(CWuhosts\fR) used by the IRAF network interface.
The IRAF networking capabilities are used to access image displays, printers,
magtape devices, files, etc. resident on any node in a local area network.
Nodes do not necessarily have to have the same architecture, or even run
the same operating system, so long as they can run IRAF.
.PP
To enable IRAF networking for a UNIX/IRAF system, all that is necessary is
to edit the file \f(CWdev$hosts\fP (the \f(CWuhosts\fP file is not used
with UNIX/IRAF).  Make an entry for each logical node in the local network
in the format
.DS
\fInodename\fR [ \fIaliases\fR ] ":" \fIirafks.e-pathname\fR
.DE
Note that there may be multiple logical entries for a single physical node.
There must be an entry in the host system file \f(CW/etc/hosts\fP for each
physical node referenced in the IRAF table.
.PP
To enable IRAF networking for a node, the BSD/UNIX \fBhostname\fR must appear
as a primary name or alias somewhere in the IRAF host table.  During process
startup, the IRAF VOS looks for the system name for the current host and
automatically disables networking if this name is not found.  Hence IRAF
networking is automatically disabled when the distributed system is first
installed - unless you are unlucky enough to have installed the system on
a host with the same name as one of the nodes in the NOAO host table.
.PP
Once IRAF networking is configured, the following command may be typed in
the CL to verify that all is well:
.DS
\f(CWcl> netstatus\fP
.DE
This will print the host table and state the name of the local host.
If there are any problems a message will be printed describing the problem.
.PP
For IRAF networking to be of any use, it is necessary that IRAF be installed
on at least two systems.  In that case either system can serve as the server
for an IRAF client (IRAF program) running on the other node.  Note that it
is not necessary to install IRAF separately on each node, i.e., a single
copy of IRAF may be NFS mounted on all nodes.  If it is not possible to
install IRAF on a node for some reason (either directly or using NFS) it is
possible to manage by installing only enough of IRAF to run the IRAF kernel
server.  Contact IRAF site support if you need to configure things in this
manner.
.PP
UNIX/IRAF currently supports only TCP/IP networking using the BSD interface.
Networking between any heterogeneous collection of systems is possible
provided they support TCP/IP based networking.  If the local network includes
any VMS systems, a VMS client can access a Sun server, but the reverse is
not possible as at present we do not support a TCP/IP based IRAF kernel server
for VMS.
.PP
Once IRAF networking is enabled, objects resident on the server node may be
accessed from within IRAF merely by specifying the node name in the object
name, with a "\fInode!\fP" prefix, e.g., if \f(CWfoo\fP is a network node,
.DS
\f(CWcl> page foo!hlib$motd
cl> allocate foo!mta
cl> devstatus foo!mta\fP
.DE
.PP
The user will be prompted for their password on the remote node \fIeach time an
IRAF client process connects to the node\fP.  This can be defeated if desired
by creating a \f(CW.irafhosts\fP file in the user's UNIX home directory
containing their login name and password for each node they may wish to access
with IRAF (see \f(CWdev$hostlogin\fP for information on the file syntax).
Obviously, there may be security problems associated with placing
a password in a file in this manner, and the file should be read protected
if used.  A possible alternative is to set up a public account on the server
node, possibly with restricted permissions.  It is also possible to use a
combination of both public and private accounts by making multiple logical
entries for the server in the IRAF host table.
.PP
To keep track of where files are in a distributed file system, IRAF uses
\fBnetwork pathnames\fP.
A network pathname is a name such as \f(CWfoo!/tmp3/images/m51.pix\fP,
i.e., a host or IRAF filename with the node name prepended.  This feature
can confuse the user when combined with the use of NFS to cross mount
disks at the host level, due to the use of a network pathname for the
pixel file pathname in an IRAF image.  There are various ways the problem
can be avoided, e.g., [1] use only IRAF networking for image access,
[2] set \f(CWimdir\fP so that pixel files are located relative to the
header file directory, rather than by absolute network pathname,
or [3] use only NFS for image access, e.g., configure the host table so
that all client hostnames appear to be aliases for the server (effectively
disabling IRAF networking in the default case), using secondary logical
entries to enable networking for things like remote image display and
magtape accesses.  At present there is no ideal solution to this problem
but any of the above approaches will work.
.PP
Even on a workstation where NFS is routinely available, IRAF networking
can be an interesting alternative to NFS for file accesses, especially where
the files being accessed are IRAF images.  The problem is that accessing bulk
image data over the network via NFS can place a very heavy load on the
server.  While IRAF networking may not be able to match the i/o bandwidth of
NFS, it does limit the load on the server and hence may improve the system
throughput and response time.  This is especially important when the server
in question is not a dedicated file server but rather a general purpose
compute and file server, which users may log into directly, or which is
also used for large batch jobs due to its great disk, memory, and number
crunching capabilities.  In such a case doing heavy image processing via
NFS will drive the load on the server way up, aggravating any users
trying to work directly on the server.  A combined strategy of running
large jobs directly on the server, plus using IRAF networking for IRAF based
interactive image processing on the clients, can avoid this problem.
.NH 3
Configuring the IRAF account
.PP
The IRAF account, i.e., what one gets when one logs into UNIX as "iraf",
is the account used by the IRAF site manager to work on the IRAF system.
Anyone who uses this account is in effect a site manager, since they have
permission to modify, delete, or rebuild any part of IRAF.  For these and
other reasons (e.g., concurrency problems) it is recommended that all routine
use of IRAF be performed from other accounts.
.PP
If the system has been installed according to the instructions given in the
installation guide the login directory for the IRAF account will be
\f(CWiraf$local\fP.  This directory contains a \f(CW.login\fP file which
defines the host environment seen by the IRAF account.
The \f(CW.login\fR may be customized to reflect one's personal preferences
if desired, but care must be taken to avoid deleting environment definitions
or other statements which are essential to the correct operation of IRAF.
.PP
Before starting up IRAF from the iraf account, following the installation of
a new system, it will be necessary to execute the \f(CWmkiraf\fP task.
This will initialize (optionally) the \f(CWuparm\fP directory for the iraf
account, and create a new \f(CWlogin.cl\fP file.  It may also be necessary
to edit the \f(CW.login\fP file to modify the way the environment variable
\f(CWIRAFARCH\fP is defined.  This variable, required for software
development if the system is configured for multiple architecture support,
but optional for merely running IRAF, must be set to the name of the desired
machine architecture, e.g., \f(CWvax\fP, \f(CWmips\fP, \f(CWsparc\fP, etc.
If it is set to the name of an architecture for which there are no binaries,
e.g., \f(CWgeneric\fP, the CL may not run.  The alias \f(CWsetarch\fP,
defined in the iraf account \f(CW.login\fP, is convenient for setting the
desired architecture for IRAF execution and software development.
.NH 3
Configuring user accounts for IRAF
.PP
User accounts should be loosely modeled after the IRAF account.  All that
is \fIrequired\fP for a user to run IRAF is that they run \f(CWmkiraf\fP
in their desired IRAF login directory before starting up the CL.
Defining \f(CWIRAFARCH\fP in the user environment
is not required unless the system has been configured for more than one
architecture and 
the user will be doing any IRAF based software
development (including IMFORT).

.NH 2
Tuning Considerations
.PP
There are two things that are commonly done to tune UNIX/IRAF for a
particular host system:
.RS
.IP \(bu
Precompile selected \f(CWtermcap\fR and \f(CWgraphcap\fR entries.
.IP \(bu
Strip the system to reduce disk consumption.
.RE
.PP
The most important optimization is precompilation of the termcap and graphcap
entries of the devices most commonly used at the local site, particularly when
running IRAF on a slow machine.  Stripping the system is undesirable if the
system is to be used for software development, but is normally desirable when
installing a production version of IRAF on a small system with limited disk
space.

.NH 3
Caching TERMCAP and GRAPHCAP device entries
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
files, \f(CWdev$cachet.dat\fR (for termcap) and \f(CWdev$cacheg.dat\fR
(for graphcap).
To cache a different set of entries one must regenerate these files with the
\fBmkttydata\fR task in the \f(CWsoftools\fR package, and then do a full
sysgen-relink with the \f(CWmkpkg\fR utility.  Detailed instructions are
given in the manual page for \f(CWmkttydata\fR.
.PP
Note that if you wish to relink the system to cache selected termcap or
graphcap device entries and plan to strip the system as well to save disk
space, \fIyou must cache the termcap and graphcap entries before
stripping the system\fR, because once the system is stripped you cannot
update the system libraries.
.NH 3
Stripping the system to reduce disk usage
.PP
Depending upon the local system configuration, it may be necessary or
desirable to strip the system of all non-runtime files to save disk space.
This means deleting all the sources and all the reference manuals and
other documentation, excluding the online manual pages.  A special utility
called \fBrmfiles\fR (in the \f(CWsoftools\fR package) is provided
for this purpose.  It is not necessary to run \f(CWrmfiles\fR directly to strip
the system.  The preferred technique is to enter the commands given below.
The example is for the cshell for consistency with the rest of this document,
but this could be done from within the CL as well.
.DS
\f(CW% cd $iraf
% mkpkg strip\fR
.DE
.PP
This will preserve all runtime files, permitting use of the standard system
as well as user software development.  Note that only the IRAF core system
is stripped, i.e., if you want to strip any external layered software products,
such as the NOAO package, a \f(CWmkpkg strip\fP must be executed separately
for each - \f(CWcd\fP to the root directory of the external package first.
A tape backup of a system should always be made before the system is stripped;
keep the backup indefinitely as it may be necessary to restore the sources
in order to, e.g., install a bug fix or add-on software product.

.NH
Software Management
.NH 2
Multiple architecture support
.PP
It is becoming increasingly common for the computing facilities at a site to
consist of a heterogeneous network of workstations and servers.  These machines
will often have different architectures, or will be configured with different
hardware, e.g., different floating point accelerators, or single and multiple
cpus.
.PP
Since IRAF is a large system it is undesirable to have to maintain a separate
copy of IRAF for each machine architecture or hardware configuration on a
network.  For this reason IRAF provides support for multiple architectures
within a single copy of IRAF.  To be accessible by multiple network clients,
this central IRAF system will typically be NFS mounted on each client.
.PP
Multiple architecture support is implemented by segregating the IRAF sources
and binaries into different directory trees.  The IRAF sources are architecture
independent and hence sharable by machines of any architecture.  All of the
architecture dependence is concentrated into the binaries, which are collected
together into the so-called BIN directories, one for each architecture.
The BIN directory contains all the object files, object libraries, executables,
and shared library images for an architecture, supporting both IRAF execution
and software development for that architecture.  A given system can support
any number of BIN directories, and therefore any number of architectures.
.PP
In the case of the core IRAF system, there must be a separate copy of IRAF
for each \fIoperating system\fP, since each operating system will in general
require a different HSI (IRAF host system interface).  The HSI for a given
operating system can support any number of different architectures if
properly configured, but a separate copy of the HSI will be required for
each operating system.  On the other hand, layered packages such as NOAO or
LOCAL are operating system independent, hence a single copy can include BINs
for any combination of operating systems or architectures.
.PP
In IRAF terminology, when we refer to an \fBarchitecture\fP what we really
mean is a type of BIN.  The correspondence between BINs and hardware
architectures is not necessarily one-to-one, i.e., multiple BINs can exist
for a single compiler architecture by compiling the system with different
compilation flags, as different versions of the software, and so on.
.PP
Examples of some currently supported UNIX operating systems and architectures
are shown below.
.DS
.TS
center;
ci ci ci
l l l.
Architecture	Operating System	Description
.sp
vax	VAX BSD/UNIX	Standard Berkeley UNIX
vax	VAX Ultrix	Similar to VAX/BSD UNIX
mips	MIPS Ultrix	MIPS dependencies (RISC architecture)
ieee	Convex	Convex with IEEE floating point
native	Convex	Convex with native floating point
generic	any	no binaries
pg	any	compiled with -pg option for profiling
.TE
.DE
.PP
Most of these correspond to different operating systems or hardware
architectures.  The exceptions are the \f(CWgeneric\fP architecture,
which is what the distributed system is configured to by default
(to avoid having any architecture dependent binary files mingled with
the sources), and the \f(CWpg\fP architecture, which is not normally
distributed to user sites, but which is a good example of a custom software
architecture used for software development.
.PP
When running IRAF on a system configured for multiple architectures,
selection of the BIN (architecture) to be used is controlled by the UNIX
environment variable \f(CWIRAFARCH\fP, e.g.,
.DS
\f(CW% setenv IRAFARCH ieee\fP
.DE
would cause Convex/IRAF to run the IRAF binaries compiled to use the IEEE
floating point format, corresponding to the BIN directory \f(CWbin.ieee\fP.
Once inside the CL one can check the current architecture by entering one
of the following commands (the output in each case is shown as well).
.DS
\f(CWcl> show IRAFARCH
ieee\fP
.DE
or
.DS
.cc #
\f(CWcl> show arch
.ieee\fP
#cc
.DE
.PP
If \f(CWIRAFARCH\fP is undefined at CL startup time a default architecture
will be selected based on the current machine architecture, the available
floating point hardware, and the available BINs.
.PP
The \f(CWIRAFARCH\fP variable controls not only the architecture of the
executables used to run IRAF, but the libraries used to link IRAF programs,
when doing software development from within the IRAF or host environment.
.PP
Additional information on multiple architecture support is provided in the
system notes file for V2.8, file \f(CWnotes.v28\fP in the \f(CWdoc\fP directory.

.NH 2
Layered software support
.PP
An IRAF installation consists of the \fBcore system\fP and any number of
\fBexternal packages\fP, or \fBlayered software products\fP.  As the name
suggests, layered software products are layered upon the core IRAF system.
Layered software depends upon the core system for all of its functionality
and is portable to any computer which already runs IRAF.  Any number of
layered products can be combined with the core system to produce the IRAF
system used at a specific site.  Due to disk space limitations it is likely
that a given site will not wish to have all the available layered software
products installed and on line at any one time.
.PP
The support provided for layered software is essentially the same as that
provided for maintaining the core system itself.  Each "external package"
(usually this refers to a tree of packages) is a system in itself, similar
in structure to the core IRAF system.  Hence, there is a LIB, one or more BINs,
a HELP database, and all the sources and runtime files.  A good example of
an external package is the NOAO package.  Except for the fact that NOAO
happens to be rooted in the \f(CWiraf\fP directory, NOAO is equivalent to
any other layered product, e.g., STSDAS, PROS, CTIOLOCAL, KPNOLOCAL, etc.
Other layered products should be rooted somewhere outside IRAF to simplify
updates.

.NH 2
Software management tools
.PP
IRAF software management is performed with a standard set of tools,
consisting of the tasks in the SOFTOOLS package, plus the host system
editors and debuggers.  Some of the most important and often used tools for
IRAF software development and software maintenance are the following.
.sp
.RS
.IP \f(CWmkhelpdb\fP 20
Updates the HELP database of the core IRAF system or an external package.
The core system, and each external package, has its own help database.
The help database is the machine independent file \f(CWhelpdb.mip\fP in the
package library (LIB directory).  The help database file is generated with
\f(CWmkhelpdb\fP by compiling the \f(CWroot.hd\fP file in the same directory.
.IP \f(CWmkpkg\fP 20
The "make-package" utility.  Used to make or update package trees.
Will update the contents of the current directory tree.  When run at
the root iraf directory, updates the full IRAF system; when run at the
root directory of an external package, updates the external package.
Note that updating the core IRAF system does not update any external
packages (including NOAO).  When updating an external package, the
package name must be specified, e.g., "\f(CWmkpkg -p noao\fP".
.IP \f(CWrmbin\fP 20
Descends a directory tree or trees, finding and optionally listing or
deleting all binary files therein.  This is used, for example, to strip
the binaries from a directory tree to leave only sources, to force
\f(CWmkpkg\fP to do a full recompile of a package, or to locate all the
binaries files for some reason.  IRAF has its own notion of what a binary
file is.  By default, files with the "known" file extensions
(.[aoe], .[xfh] etc.) are classified as binary or text
(machine independent) files immediately,
while a heuristic involving examination of the file data
is used to classify other files.  Alternatively, a list of file extensions
to be searched for may optionally be given.
.IP \f(CWrtar,wtar\fP 20
These are the portable IRAF tarfile writer (\f(CWwtar\fP) and reader
(\f(CWrtar\fP) programs.  About the only reasons to use these with UNIX/IRAF
are if one wants to move only the machine independent or source files
(\f(CWwtar\fP, like \f(CWrmbin\fP, can discriminate between machine
generated and machine independent files), or if one is importing files
written to a tarfile on a system such as VMS/IRAF, where the files are blank
padded and the trailing blanks need to be stripped with \f(CWrtar\fP.
.IP \f(CWxc\fP 20
The X (SPP) compiler.  This is analogous to the UNIX \f(CWcc\fP except
that it can compile ".x" or SPP source files, knows how to link with the
IRAF system libraries, knows how to read and use the environment of external
packages, and so on.
.RE
.sp
.PP
The SOFTOOLS package contains other tasks of interest, e.g., a program
\f(CWmktags\fP for making a tags file from SPP source files for the
\f(CWvi\fP editor, a HELP database examine tool, and other tasks.
Further information on these tasks is available in the online HELP pages.

.NH 2
Modifying and updating a package
.PP
IRAF applications development (including revisions to existing programs)
is most conveniently performed from within the IRAF environment, since
testing must be done from within the environment.  The usual development
cycle is as follows.  This takes place within the \fIpackage directory\fP
containing all the sources and mkpkg-files for the package.
.RS
.IP \(bu
Edit one or more source files.
.IP \(bu
Use \f(CWmkpkg\fP to compile any modified files, or files which include a
modified file, and relink the package executable.
.IP \(bu
Test the new executable.
.RE
.PP
The \f(CWmkpkg\fP file for a package can be written to do anything,
but by convention the following commands are usually provided.
.sp
.RS 
.IP "\f(CWmkpkg\fP" 20
Same as \f(CWmkpkg relink\fP below.
.IP "\f(CWmkpkg libpkg.a\fP" 20
Updates the package library, compiling any files which have been modified
or which reference include files which have been modified.  Private package
libraries are intentionally given the generic name \f(CWlibpkg.a\fP to
symbolize that they are private to the package.
.IP "\f(CWmkpkg relink\fP" 20
Rebuilds the package executable, i.e., updates the package library and
relinks the package executable.  By convention, this is the file
\f(CWxx_foo.e\fP in the package directory, where \fIfoo\fP is the
package name.
.IP "\f(CWmkpkg install\fP" 20
Installs the package executable, i.e., renames the \f(CWxx_foo.e\fP
file to \f(CWx_foo.e\fP in the global BIN directory for the system
to which the package \fIfoo\fP belongs.
.IP "\f(CWmkpkg update\fP" 20
Does everything, i.e., a \fIrelink\fP followed by an \fIinstall\fP.
.RE
.sp
.PP
If one wishes to test the new program before installing it one should do
a \fIrelink\fP (i.e., merely type "mkpkg" since that defaults to relink),
then run the host system debugger on the resultant executable.  The process
is debugged standalone, running the task by typing its name into the
standalone process interpreter.  The CL task \f(CWdparam\fP is useful
for dumping a task's parameters to a text file to avoid having to answer
innumerable parameter queries during process execution.  If the new program
is to be tested under the CL before installation, a \fItask\fP statement
can be interactively typed into the CL to cause the CL to run the "xx_"
version of the package executable, rather than old installed version.
.PP
When updating a package other than in the core system, the \fB-p\fP flag,
or the equivalent \f(CWPKGENV\fP environment variable, \fImust\fP be used
to indicate the system or layered product being updated.  For example,
"\f(CWmkpkg -p noao update\fP" would be used to update one of the packages
forming the NOAO system of packages.
.PP
The CL \fBprocess cache\fP can complicate debugging and testing if one
forgets that it is there.  Recall that when a task is run under the CL,
the executing process remains idle in the CL process cache following
task termination.  If a new executable is installed while the old one
is still in the process cache, the \f(CWflprcache\fP command must be
entered to force the CL to run the new executable.  If an executable is
currently running, either in the process cache or because some other
user is using the program, it may not be possible to set breakpoints
under the debugger.
.PP
A full description of these techniques is beyond the scope of this manual,
but one need not be an expert at IRAF software development techniques to
perform simple updates.  Most simple revisions, e.g., bug fixes or updates,
can be made by merely editing or replacing the affected files and typing
.DS
\f(CWcl> mkpkg update\fP
.DE
plus maybe a \f(CWflpr\fP if the old executable is still sitting idle
in the process cache.

.NH 2
Installing and maintaining layered software
.PP
The procedures for installing layered software products are similar to those
used to install the core IRAF system, or update a package.
Layered software may be distributed in source only form, or with binaries;
it may be configured for a single architecture, or may be preconfigured
to support multiple architectures.  The exact procedures to be followed
to install a layered product will in general be product dependent, and should
be documented in the installation guide for the product.
.LP
In brief, the procedure to be followed should resemble the following:
.RS
.IP \(bu
Create the root directory for the new software, somewhere outside the
IRAF directories.
.IP \(bu
Restore the files to disk from a tape or network archive distribution file.
.IP \(bu
Edit the core system file \f(CWhlib$extern.pkg\fP to "install" the new
package in IRAF.  This file is the sole link between the IRAF core system
and the external package.
.IP \(bu
Configure the package BIN directory or directories, either by restoring
the BIN to disk from an archive file, or by recompiling and relinking the
package with \f(CWmkpkg\fP.
.RE
.LP
As always, there are some little things to watch out for.
When using \f(CWmkpkg\fP on a layered product, you must give the name
of the system being operated upon, e.g.,
.DS
\f(CWcl> mkpkg -p foo update\fP
.DE
where \fIfoo\fP is the system or package name, e.g., "noao", "local", etc.
The \fB-p\fP flag can be omitted by defining \f(CWPKGENV\fP in your
UNIX environment, but this only works for updates to a single package.
.PP
An external system of packages may be configured for multiple architecture
support by repeating what was done for the core system (except that multiple
operating systems can be supported in layered software if desired).
One sets up several BIN directories, one for each architecture, named
\f(CWbin.\fIarch\fR, where \fIarch\fP is "vax", "vux3", "mips", "ieee",
"sparc", etc.  These directories, or symbolic links to the actual directories,
go into the root directory of the external system.  A symbolic link \f(CWbin\fP
pointing to an empty directory \f(CWbin.generic\fP, and the directory itself,
are added to the system's root directory.  The system is then stripped of its
binaries with \f(CWrmbin\fP, if it is not already a source only system.
Examine the file \f(CWzzsetenv.def\fP in the LIB to verify that the definition
for the system BIN (which may be called anything) includes the
string "\f(CW(arch)\fP", e.g.,
.DS
\f(CWset noaobin = "noao$bin(arch)/"\fP
.DE
.PP
The binaries for each architecture may then be generated by configuring the
system for the desired architecture and running \f(CWmkpkg\fP to update the
binaries, for example,
.DS
\f(CWcl> cd foo
cl> mkpkg sparc
cl> mkpkg -p foo >& spool &\fP
.DE
where \fIfoo\fP is the name of the system being updated.  If any questions
arise, examination of a working example of a system configured for multiple
architecture support (e.g., the NOAO packages) may reveal the answers.
.PP
Once installed and configured, a layered product may be deinstalled merely
by archiving the package directory tree, deleting the files, and commenting
out the affected lines of \f(CWhlib$extern.pkg\fP.  With the BINs already
configured reinstallation is a simple matter of restoring the files to disk
and editing the \f(CWextern.pkg\fP file.

.NH 2
Configuring a custom LOCAL package
.PP
Anyone who uses IRAF enough will eventually want to add their own software
to the system, by copying and modifying the distributed versions of programs,
by obtaining and installing isolated programs written elsewhere, or by writing
new programs of their own.  A single user can do this by developing software
for their own personal use, defining the necessary \fItask\fP statements etc.
to run the software in their personal \f(CWlogin.cl\fP file.  To go one step
further and install the new software in IRAF so that it can be used by
everyone at a site, one must configure a \fBcustom local package\fP.
.PP
The procedures for configuring and maintaining a custom LOCAL package are
similar to those outlined in \(sc3.5 for installing and maintaining
layered software, since a custom LOCAL will in fact be a layered software
product, possibly even something one might want to export to another site
(although custom LOCALs often contain non-portable or site specific software).
.PP
To set up a custom LOCAL, start by making a local copy of the
\fBtemplate local\fP package that comes with the distributed system.
If you make a source only tar archive of \f(CWiraf$local\fP and install
it as outlined in \(sc3.5, you will have a custom LOCAL.  The purpose
of the template LOCAL is to provide the framework necessary for a external
package; a couple of simple tasks are provided in the template LOCAL
to serve as examples.  Once you have configured a local copy of the template
LOCAL and gotten it to compile and link, it should be a simple matter to
add new tasks to the existing framework.

.bp
.NH 2
Updating the full IRAF system
.NH 3
The BOOTSTRAP
.PP
All current IRAF distributions come with the system already bootstrapped,
i.e., the host system interface (HSI) comes with the HSI binaries already
built.  A bootstrap should not be necessary unless one is doing something
unusual, e.g., installing a bugfix or making some other revision to the HSI.
.PP
A bootstrap is like a full system sysgen, except that it is the host
system interface (kernel and bootstrap utilities) which are compiled and
linked, rather than IRAF itself.  The system must be bootstrapped before
a sysgen is possible, since the bootstrap utilities are required to do a
sysgen.  The two operations are distinct because only the bootstrap is
machine dependent; everything else works the same on all IRAF systems.
.PP
To bootstrap UNIX/IRAF, go to the \f(CWunix\fR directory and enter the
commands shown below.  This takes 1/2 to 2 hours depending on the machine,
so the output should be
spooled in a file.  One should only attempt a bootstrap from the IRAF
account, to ensure that the environment is configured properly and that
all the necessary permissions are in order.
.DS
\f(CW% cd $iraf/unix
% reboot >& spool &\fP
.DE
.PP
There are actually two types of bootstrap, the "hard" bootstrap starting from
a source only system, called the NOVOS bootstrap, and the usual or VOS
bootstrap, performed once the IRAF system libraries \f(CWlibsys.a\fP
and \f(CWlibvops.a\fP exist.  To configure IRAF starting from a completely
source only system, one would have to do a NOVOS bootstrap, make the system
libraries, then do a VOS bootstrap, after which one could perform a full
sysgen to build the main IRAF system.
.NH 3
The SYSGEN
.PP
A full system sysgen is necessary when installing a source only version
of IRAF, e.g., when an IRAF distribution is obtained from the IRAF network
archive.  We assume that the system has already been bootstrapped, since
most IRAF distributions include a completely configured HSI.
.PP
To do a full sysgen of IRAF one merely runs \f(CWmkpkg\fP at the IRAF root.
If the system is configured for multiple architecture support one must repeat
the sysgen for each architecture.  Since a full sysgen takes a long time and
generates a lot of output which later has to be reviewed, it is best to run
the job in batch mode with the output redirected.  For example to update
the \f(CWsparc\fP architecture binaries:
.DS
\f(CW% cd $iraf
% mkpkg sparc
% mkpkg >& spool &\fR
.DE
To watch what is going on after this command has been submitted and while
it is running, try
.DS
\f(CW% tail -f spool\fR
.DE
Sysgens are restartable, so if the sysgen aborts for any reason, simply
fix the problem and start it up again.  How long the sysgen takes depends
upon how much work it has to do.  The worst case is if the system and
applications libraries have to be recompiled.  If the system libraries
already exist they will merely be updated.
.PP
A full sysgen generates a lot of output, too much to be safely reviewed for
errors by simply paging the spool file.  Enter the following command to review
the output (this assumes that the output has been saved in a file named
\f(CWspool\fR).
.DS
\f(CW% mkpkg summary\fR
.DE
It is normal for a number of compiler messages warning about assigning
character data to an integer variable to appear in the spooled output
if the full system has been compiled.  There should be no serious error
messages if a supported and tested system is being recompiled.
.PP
The above procedure only updates the core IRAF system.  To update a layered
product one must repeat the sysgen process for the layered system.
For example, to update the \f(CWvax\fP binaries for the NOAO package:
.DS
\f(CW% cd $iraf/noao
% mkpkg vax
% mkpkg -p noao >& spool &\fP
.DE
This must be repeated for each supported architecture.  Layered systems are
completely independent of one another and hence must be updated separately.

.NH
Interfacing New Graphics Devices
.PP
There are three types of graphics devices that concern us here.
These are the graphics terminals, graphics plotters, and image displays.
.NH 2
Graphics terminals
.PP
The IRAF system as distributed is capable of talking to just about any
conventional graphics terminal or terminal emulator, using the \f(CWstdgraph\fR
graphics kernel supplied with the system.  All one need do to interface to a
new graphics terminal is add new graphcap and termcap entries for the device.
This can take anywhere from a few hours to a few days, depending on one's
level of expertise, and the characteristics of the device.  Be sure to check
the contents of the \f(CWdev$graphcap\fR file to see if the terminal is already
supported, before trying to write a new entry.  Useful documentation for
writing graphcap entries is the GIO reference manual and the HELP pages for
the \f(CWshowcap\fP and \f(CWstty\fP tasks (see \(sc2.2.6).  Assistance with
interfacing new graphics terminals is available via the IRAF Hotline.
.NH 2
Graphics plotters
.PP
The current IRAF system comes with several graphics kernels used to drive
graphics plotters.  The standard plotter interface is the SGI graphics kernel,
which is interfaced as the tasks \f(CWsgikern\fP and \f(CWstdplot\fP in the
PLOT package.  Further information on the SGI plotter interface is given in
the paper \fIThe IRAF Simple Graphics Interface\fR, a copy of which is
included with the IRAF installation kit.
.PP
SGI device interfaces for most plotter devices already exist, and adding
support for new devices is straightforward.  Sources for the SGI device
translators supplied with the distributed system are maintained in the
directory \f(CW$iraf/unix/gdev/sgidev\fR.
NOAO serves as a clearinghouse for new SGI plotter device interfaces;
contact us if you do not find support for a local plotter device in the
distributed system, and if you plan to implement a new device interface let
us know so that we may help other sites with the same device.
.PP
The older \f(CWNCAR\fR kernel is used to generate NCAR metacode and can be
interfaced to an NCAR metacode translator at the host system level to get
plots on devices supported by host-level NCAR metacode translators.
The host level NCAR metacode translators are not included in the standard
IRAF distribution, but public domain versions of the NCAR implementation for
UNIX systems are widely available.  A site which already has the
NCAR software may wish to go this route, but the SGI interface will provide
a more efficient and simpler solution in most cases.
.PP
The remaining possibility with the current system is the \f(CWcalcomp\fR kernel.
Some sites will have a Calcomp or Versaplot library (or Calcomp compatible
library) already available locally.  To make use of such a library to get
plotter output on any devices supported by the interface, one may copy
the library to the \f(CWhlib\fR directory and relink the Calcomp graphics
kernel.
.PP
A graphcap entry for each new device will also be required.  Information on
preparing graphcap entries for graphics devices is given in the GIO design
document, and many actual working examples will be found in the graphcap
file.  The best approach is usually to copy one of these and modify it.
.NH 2
Image display devices
.PP
Most image display is now done with workstations, running a windowing system
such as X, News, or SunView.  Workstations may be used as "super terminals"
for running UNIX/IRAF on any supported host system.  Dedicated image display
devices are still important, but are an expensive option which is justified
only for particularly demanding applications.
.PP
For most workstations image display now means using the X window system,
or some variant such as DECwindows (DEC) or X11/NeWS (Sun).  Image display
for IRAF under the MIT X window system is currently provided by the SAOIMAGE
display server (originally called XIMAGE) developed by CFA and distributed by
the IRAF project.  The standard image display facility for a Sun workstation
running the SunView window system is currently IMTOOL.  More comprehensive
support for graphics and image display under X, including the major vendor
systems such as X11/NeWS and DECwindows, is planned for the future; contact
NOAO for updated information on the degree of support available for any of
these systems.
.PP
Interfaces for specific hardware image display devices are also available,
although a fully general display interface is not yet provided by IRAF.
Only the IIS model 70 and 75 are currently supported directly by NOAO.
Interfaces for other devices are possible using the current datastream
interface, which is based on the IIS model 70 datastream protocol with
extensions for passing the WCS, image cursor readback, etc. (see the ZFIOGD
driver in \f(CWunix/gdev\fP).  This is how all the current displays, e.g.,
imtool and ximage, and the IIS devices, are interfaced, and there is no reason
why other devices could not be interfaced to IRAF via the same interface.
Eventually this prototype interface will be obsoleted and replaced by a
more general interface.
.NH 2
Network access to graphics devices
.PP
Any IRAF device may be accessed either locally, with the device resident
on the network node on which IRAF is being run, or remotely, using IRAF
networking to access the remote device.  The procedure for configuring
IRAF networking is described in \(sc2.2.7.  
.PP
Unlike system resources such as directories, files, and magtape devices,
for which the user specifies the node name explicitly when the device is
accessed, graphics devices are normally accessed by device name only,
with the system making the networking connection transparently to the user.
In the case of a plotter or printer, the network connection is either
defined in the IRAF graphcap or termcap entry for the device (in the
\f(CWDD\fP or device dispose string), or is made at the host system level
by \f(CWlpr\fP.  The network node on which an image display device resides
is specified in the graphcap entry for the device.
.sp
.TS
center;
ci ci
l l.
variable	description
.sp
\f(CWlpnode\fP	Line printer devices
\f(CWplnode\fP	Plotter devices
\f(CWnode\fP	The user's workstation
.TE
.sp
.PP
Network nodes in graphcap and termcap entries may be either actual network
node names, or the names of environment variables that may be defined to
point to network nodes, allowing the user to specify the network node
dynamically at runtime.  Examples of logical device nodes are shown in
the table above.  These appear in graphcap or termcap entries as embedded
logical node name references, e.g.,
.DS
\f(CW:DD=plnode!apl,tmp$sgk,!{ sgidispatch sgi2uapl $F -l$(XO)\fP...
.DE
.PP
The environment variable \fBnode\fP, if defined, specifies the hostname
of the user workstation.  Currently this is only used for image display on
the user workstation, but in principle it could be used to access any other
workstation resident resource as well.  For example, if the user is logged
in on a workstation named "fred", which is running an IRAF compatible display
server, one would enter
.DS
\f(CWcl> reset node = fred\fP
.DE
to cause the IRAF image display software running on the central server to
display images on workstation "fred".

.NH
Host System Requirements
.PP
UNIX/IRAF is currently supported on VAXes running Berkeley UNIX or Ultrix,
on the DECstation (MIPS cpu) running the MIPS version of Ultrix,
on the Alliant, Convex, HP, Masscomp,
and on all models of Sun workstations and servers.  An Apollo port
is in progress.  Lick Observatory has ported IRAF to the ISI.  The VMS and
DG AOS-VS operating systems are supported in addition to UNIX based systems.
New ports are always in progress, and anyone interested in running IRAF on
a new computer should contact the IRAF project to determine the status of
IRAF support for the system in question.
.NH 2
Memory requirements
.PP
The amount of memory required to run IRAF is more dependent upon how the
system will be used than upon anything having to do with IRAF.  Any large
server should have more than enough memory to run IRAF.  A workstation will
need enough memory to run IRAF, UNIX, and the window system without paging
heavily; 8-12 Mb is the minimum for such a system.
.NH 2
Disk requirements
.PP
The amount of disk required by a user depends greatly on the application,
so it is hard to recommend a minimum disk size.  For a system with access
to a central server, a diskless system or 200-300 Mb of local SCSI disk
is fine.  For a standalone system with no access to large server, 500-600 Mb
is about the minimum.  A server should have several Gb of fast disk.
.NH 2
Diskless nodes
.PP
For applications such as programming or daily login support, a diskless node
connected to a large file server is a cost effective approach delivering
good performance.  Some local disk for boot, swap, and local file storage
and server independence is desirable but not essential.
.PP
For IRAF applications involving heavy image processing, one is inevitably
going to want to run large batch image processing jobs directly on the server,
implying that a \fIcompute\fP rather than \fIfile\fP server is what is
needed (i.e., one will want to avoid heavy NFS loading on the server).
A diskless node is still viable, but one will want to run jobs which
involve heavy disk i/o directly on the server, reserving the workstation
for the interactive things, e.g., graphics and image display, and compute
bound image analysis tasks.  Small SCSI disks are getting cheap enough
that almost any color workstation equipped with say, 12-16 Mb of memory,
probably warrants several Mb of local disk for server independence, swap,
and local file storage.

.bp
.SH
Appendix A.  The IRAF Directory Structure
.PP
A graph of the current full UNIX/IRAF directory structure is given at the
end of this document.  The main branches of the tree are summarized below.
Beneath the directories shown are some 300 subdirectories, the largest
directory trees being \f(CWsys\fR, \f(CWpkg\fR, and \f(CWnoao\fR.
The entire contents of all directories other than \f(CWunix\fR, \f(CWlocal\fR,
and \f(CWdev\fR are fully portable, and are identical in all installations
of IRAF sharing the same version number.
.DS
\f(CWbin        \fR- the IRAF BIN directories
\f(CWdev        \fR- device tables (\f(CWtermcap\fR, \f(CWgraphcap\fR, etc.)
\f(CWdoc        \fR- assorted IRAF manuals
\f(CWlib        \fR- the system library; global files
\f(CWlocal      \fR- iraf login directory; locally added software
\f(CWmath       \fR- sources for the mathematical libraries
\f(CWnoao       \fR- packages for NOAO data reduction
\f(CWpkg        \fR- the IRAF applications packages
\f(CWsys        \fR- the virtual operating system (VOS)
\f(CWunix       \fR- the UNIX host system interface (HSI = kernel + bootstrap utilities)
.DE
.LP
The contents of the \f(CWunix\fR directory (host system interface) are
as follows (this will vary somewhat depending upon the system):
.DS
\f(CWas         \fR- assembler sources
\f(CWbin        \fR- the HSI BIN directories
\f(CWboot       \fR- bootstrap utilities (mkpkg, rtar, wtar, etc.)
\f(CWgdev       \fR- graphics device interfaces (SGI device translators)
\f(CWhlib       \fR- host dependent library; global files
\f(CWos         \fR- OS interface routines (UNIX/IRAF kernel)
\f(CWreboot     \fR- executable script run to reboot the HSI
\f(CWshlib      \fR- shared library facility sources
\f(CWsun        \fR- gterm and imtool sources
.DE
.PP
If you will be working with the system much at the system level, it will be
well worthwhile to spend some time exploring these directories and gaining
familiarity with the system.
