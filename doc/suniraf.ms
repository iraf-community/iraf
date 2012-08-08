.RP
.de XS
.DS
.ps -1
.vs -2p
.ft CB
..
.de XE
.DE
.ft R
.ps
.vs
..
.TL
Sun/IRAF Installation Guide
.AU
Doug Tody
.AI
IRAF Group
.br
.K2 "" "" "\(dg"
.br
June 1989
.br
Revised July 1992

.AB
This document describes how to install IRAF on a Sun workstation or server,
or update an existing installation.  Both standalone and networked, multiple
architecture configurations are described.  Only those issues which one must
understand to install Sun/IRAF are discussed here; a companion document,
\fISun/IRAF Site Manager's Guide\fR, deals with other issues such as
interfacing new devices, configuring the IRAF networking system, Sun/IRAF
shared libraries, adding layered software, and so on.
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
\h'|0.4i'2.1.\h'|0.9i'Prepare the root IRAF directory\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.1.\h'|1.5i'If updating an existing IRAF installation...\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.2.\h'|1.5i'If installing IRAF for the first time...\l'|5.6i.'\0\03
.br
\h'|0.4i'2.2.\h'|0.9i'Install the files\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.1.\h'|1.5i'Installing from a network distribution\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.2.\h'|1.5i'Installing from tape\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.3.\h'|1.5i'Configuring the BIN directories\l'|5.6i.'\0\05
.br
\h'|0.9i'2.2.4.\h'|1.5i'Deleting unused HSI binaries\l'|5.6i.'\0\06
.br
\h'|0.4i'2.3.\h'|0.9i'Merge local revisions back into the new system\l'|5.6i.'\0\07
.br
\h'|0.4i'2.4.\h'|0.9i'Run the INSTALL Script\l'|5.6i.'\0\07
.sp
3.\h'|0.4i'\fBSystem Checkout\fP\l'|5.6i.'\0\08
.sp
\fBAppendix A.\0A Complete Example\fP\l'|5.6i.'\0\10
.nr PN 0
.bp

.NH
Introduction
.PP
Before installing Sun/IRAF, one must 1) obtain an appropriate Sun/IRAF
distribution, e.g., from the IRAF network archive on \f(CWiraf.noao.edu\fR
(or by ordering a tape distribution from NOAO), 2) select the server or node
on which the system is to be installed and arrange for sufficient disk space
to hold the system, and 3) set aside sufficient time to do the
installation.  If these directions are followed carefully and mistakes are
avoided the basic installation should only take a half hour or so.
Additional time may be required to customize the system to configure the
local tape drives and other devices, set up IRAF networking, and so on.
.PP
At the present time there are two distributions of Sun/IRAF,
representing the range of systems currently marketed by Sun, all of which
are supported by the IRAF project.  These are the following:
.RS
.IP \(bu
All Sun-3 and Sun-4 workstations running SunOS-4
.IP \(bu
The 386i (Roadrunner), running SunOS-4
.RE
.PP
V2.10 Sun/IRAF supports SunOS versions 4.0.3 through 4.1.2 (the current
release as of July 1992).  Solaris 2.0, due out in 1993, is not currently
supported.  V2.10 will be the last IRAF release supporting the 386i since
this machine is obsolete and is no longer being supported by Sun.  Sun
is also dropping support for the Sun-3 machines, however IRAF will continue
to support these as long as their use remains widespread (we encourage
everyone to upgrade these machines to sparcstations).
.PP
The amount of disk space required to install IRAF depends upon the system
configuration, primarily the number of architectures one needs to support
(sparc, 386i, f68881 and ffpa (Sun-3), and so on.  The main system,
including both the core system and NOAO package sources, requires about 47
Mb, less if stripped after installation.  Each core system binary requires
about 15-17 Mb and each NOAO package binary requires about 12 Mb (assuming
OS-4 and shared libraries).  The actual numbers will vary slightly depending
upon the architecture.

.NH
Installing Sun/IRAF
.PP
Although the details of how Sun/IRAF is installed or updated depend upon the
type of distribution and the desired local system configuration, the basic
procedure is always the same.  First one obtains the distribution files,
then one follows the procedure outlined below to install the system.  Most
of these steps should be performed while logged in as IRAF; superuser
permission is required in the final stages of the installation, to run the
\fIinstall\fP script.
.PP
System managers familiar with the installation of typical UNIX programs
should beware that IRAF, being a large system in its own right and not a
UNIX program, does not always follow to the usual conventions.  It is wise
to read and adhere to the installation instructions to avoid problems.
.XS
\fR# Prepare the root IRAF directory.\fP
if new installation
    create iraf account
else if updating an old installation
    save locally modified files; delete old system

\fR# Install the files.\fP
login as iraf
unpack the core system distribution
configure the BIN directories

\fR# Merge local revisions into new system.\fP
if updating an old installation
    merge locally modified files back into new system

run the iraf install script to complete the installation
checkout the new system
.XE
.PP
If problems should arise during the installation help is available via the
IRAF HOTLINE (602-323-4160), or by sending email to \f(CWiraf@noao.edu\fP.

.NH 2
Prepare the root IRAF directory
.NH 3
If updating an existing IRAF installation...
.PP
If you are updating an existing IRAF installation then you will be replacing
IRAF by the new version, and IRAF should already have an account and root
directory on the desired host system.  You should save any locally modified
files and delete the old system, e.g., login as IRAF and enter something
like the following.
.XS
% cd $iraf\(dg
% tar -cf /scr0/oiraf.tar local dev unix/hlib
% /bin/rm -rf *
.XE
.FS
\(dg\0\(CW$iraf\fP symbolizes the UNIX pathname of the root IRAF directory.
If no "iraf" environment variable is defined just supply the actual pathname.
.FE
.PP
There are many possible variations on this, e.g., you could use \fImv\fR to
move the above directories to someplace outside the main IRAF directory
tree.  Although it is probably simplest and safest to save entire
directories as in the example, in practice only a few files are likely to
have been modified.  These are the following.
.XS
dev/graphcap
dev/hosts
dev/tapecap
dev/termcap
hlib/extern.pkg
hlib/login.cl
hlib/zzsetenv.def
local/.login
.XE
.PP
Once the old system has been deleted you are ready to install the new one,
as described in \(sc2.2.  It is important to delete the old system first to
avoid creating junk files or directories when the new system is installed
(due to file or directory name changes or deletions).  Once the new system
has been restored to disk, do \fInot\fR merely restore the files saved above,
as you will need to carefully merge local changes into the versions of the
files supplied with the new IRAF release (more on this later).
.NH 3
If installing IRAF for the first time...
.PP
If you are installing IRAF for the first time then the first step is to set
up a new account for the user `\f(CWiraf\fP'.  This is necessary for IRAF
system management, which should always be done from the IRAF account.  The
IRAF account has special login files which set up a custom UNIX environment
for IRAF system management.  Having an IRAF account provides a convenient
place (the IRAF system manager's login directory) to keep scratch files
created during system configuration.
.PP
The location of the IRAF root directory is arbitrary.  Our practice here is
to locate the software in a system file storage area separate from the SunOS
files (to simplify SunOS upgrades), and then use a symbolic link such as
/iraf or /usr/iraf to point to the actual root directory.  This makes life
simpler if IRAF is NFS mounted on several machines and it is later necessary
to move the IRAF files.  Try to keep the path to the physical IRAF root
directory short to avoid filename truncation problems when IRAF is run.
.PP
The login directory for the iraf account should be $iraf/local (e.g.,
/iraf/iraf/local), rather than the IRAF root directory $iraf as one might
expect.  This is done to provide a work area for local files separate from
the main IRAF directory tree, to simplify updates and make it easier to keep
track of what has been locally added and what is standard IRAF.  In any
case, make sure that when the IRAF account is set up the login directory is
set correctly, or the IRAF environment will not be set up properly, and
later problems are sure to result.
.PP
A typical IRAF installation consists of the main IRAF release, a number of
BIN directories (the IRAF binaries), and additional directories for layered
software such as STSDAS, PROS, and so on.  If sufficient disk space is
available to keep everything in one area the following directory structure
is recommended.
.XS
/iraf/iraf                  \fR# iraf root directory ($iraf)\fP
/iraf/iraf/local            \fR# iraf login directory (~iraf)\fP
/iraf/irafbin               \fR# iraf BIN directories\fP
/iraf/irafbin/bin.sparc     \fR# sparc binaries iraf core system\fP
/iraf/irafbin/bin.f68881    \fR# f68881 binaries iraf core system\fP
/iraf/stsdas                \fR# layered package\fP
/iraf/xray                  \fR# layered package\fP
    \fI(etc.)\fP
.XE
.PP
For the purpose of this example we assume that the IRAF files are stored in
/iraf; as we say this might be a link and the actual directory is
arbitrary.  Given this directory the IRAF root $iraf would be "/iraf/iraf/"
and the login directory for the IRAF account would be /iraf/iraf/local.  The
sparc binaries for the core IRAF system would be in /iraf/irafbin/bin.sparc,
with a link $iraf/bin.sparc pointing to this directory (more on this
later).
.PP
Given the above directory structure the \f(CWpasswd\fR file entry for the
IRAF account would be something like the following.
.XS
iraf:##iraf:312:12:IRAF system login:/iraf/iraf/local:/bin/csh
.XE
.PP
On 386i systems Sun recommends placing exportable layered products such as
IRAF in the 386i-specific directory /files/vol.  Hence the recommended
root directory for IRAF on the 386i is /files/vol/iraf.  Due to the
mandatory yellow pages feature of the 386i, the easiest and most reliable
way to create a new user account is with SNAP.  If SNAP is used to create
the IRAF account the home directory will be set to /home/iraf.
.PP
To create the iraf root and login directories on a 386i (and only on a 386i)
and make them the default for the iraf account, do the following as root:
.XS
# rm -rf /home/iraf
# mkdir /files/vol/iraf /files/vol/iraf/local
# /etc/chown -R iraf /files/vol/iraf
# ln -s /files/vol/iraf/local /home/iraf
.XE
.PP
Do not worry about configuring the environment files for the new account as
these will be created when the iraf system is later restored to disk.

.NH 2
Install the files
.PP
If you are installing from tape skip forward to \(sc2.2.2.  If you are
installing from a network distribution (i.e., from disk) continue with the
next section.
.NH 3
Installing from a network distribution
.PP
Sun/IRAF is available over the network via anonymous ftp from the node
\f(CWiraf.noao.edu\fR, in the subdirectory \f(CWiraf/v\fInnn\fP/SOS4\fR,
where "\f(CWv\fInnn\fR" is the IRAF version number, e.g., subdirectory
\f(CWiraf/v210/SOS4\fR for V2.10 Sun/IRAF.
.PP
If IRAF is being installed from a network distribution all the architecture
independent IRAF files for both the core IRAF system and the NOAO packages
will be in the distribution file \f(CWas.sos4.gen\fR.  This "file" is stored
in the network archive as a directory wherein the large distribution file
has been split into a number of smaller pieces, e.g.,
.XS
% ls as.sos4.gen
CHECKSUMS           as.sos4.gen.Z.12    as.sos4.gen.Z.26
FILES.Z             as.sos4.gen.Z.13    as.sos4.gen.Z.27
as.sos4.gen.Z.00    as.sos4.gen.Z.14    as.sos4.gen.Z.28
as.sos4.gen.Z.01    as.sos4.gen.Z.15    as.sos4.gen.Z.29
as.sos4.gen.Z.02    as.sos4.gen.Z.16    as.sos4.gen.Z.30
as.sos4.gen.Z.03    as.sos4.gen.Z.17    as.sos4.gen.Z.31
       \fI(etc.)\fP
.XE
.LP
Assume that the directory \f(CWas.sos4.gen\fR as shown above has been
recreated somewhere on the machine on which IRAF is to be installed.
We can restore the main IRAF source tree as follows.
.XS
% whoami
iraf
% cd $iraf
% cat /\fIpath\fP/as.sos4.gen/as.* | uncompress | tar -xpf -
.XE
After the above finishes the root IRAF directory should appears as follows
(this is for V2.10).
.XS
HS.SOS4.GEN	bin.ffpa	doc		mkpkg		tags
IS.PORT.GEN	bin.generic	lib		noao		unix
bin		bin.sparc	local		pkg
bin.f68881	dev		math		sys
.XE
The files \f(CWbin.f68881, bin.sparc,\fR etc. are links to the IRAF BIN
directories (for binary executables), which probably do not exist yet.
Configuring the BIN directories is discussed in section \(sc2.2.3.
.NH 3
Installing from tape
.PP
If you have not already done so, log into the IRAF account so that the files
when restored will belong to IRAF.  Mount the distribution tape, which may
be a cartridge tape, a 6250 bpi 9 track tape, a DAT tape, an Exabyte, or
whatever.
.PP
IRAF distribution tapes consist of multiple files separated by tape marks,
with a TOC (table of contents) file as the first file on the tape.  To find
out what is on the tape, rewind it and read out the TOC file as follows (the
actual device name will likely be different than that shown in the examples).
.XS
% mt -f /dev/nrst0 rew;  cat /dev/nrst0
.XE
This should cause a TOC file to be listed similar to the following, except
for the file names which will vary depending upon what type of distribution
you have (also the file sizes are now somewhat larger than what is shown).
The example below is for a distribution of Sun/IRAF for SunOS-4, with the
f68881, ffpa, and sparc binaries.
.XS
0   Table of Contents

1   AS.SOS4.GEN   36.2Mb   IRAF, NOAO packages and Sun/OS sources
2   IB.SOS4.F68    8.5Mb   f68881 binaries for IRAF system
3   IB.SOS4.FPA    9.1Mb   ffpa binaries for IRAF system
4   IB.SOS4.SPC   11.3Mb   sparc binaries for IRAF system
5   NB.SOS4.F68    6.9Mb   f68881 binaries for NOAO packages
6   NB.SOS4.FPA    7.4Mb   ffpa binaries for NOAO packages
7   NB.SOS4.SPC    8.7Mb   sparc binaries for NOAO packages
.XE
.PP
Here, the first column is the file number on the tape, the TOC file being
file zero (the first distribution file is numer one), the second column is
the name of the tape file, the third column is the file size in megabytes
(this tells you how much space will be needed to unpack the file on disk),
and the last column is a description of the file contents.
.PP
There are three types of tape files in the example shown: the \f(CWAS\fR
file, which is all the IRAF sources (the core IRAF system, NOAO packages,
and the SunOS host system interface), the \f(CWIB\fR files, or IRAF core
system binaries, one for each architecture, and the \f(CWNB\fR files, or
NOAO package binaries.  The NOAO package sources are included in the
\f(CWAS\fR file since most people requesting IRAF are expected to want the
astronomical reduction software, although IRAF can be configured without
this if desired.  All of the file objects are UNIX \fItar\fR format files,
with the exception of the TOC file which is a simple text file.  The
distribution files may be compressed if this was necessary to fit all the
files on a tape.
.PP
In the above example, the \f(CWSOS4\fR in the file names indicates that
these files are for SunOS version 4.  A SunOS version 3 distribution is
indicated by a \f(CWSOS3\fR in the file names, and a 386i distribution is
indicated by a \f(CWS386\fP.  In principle a given distribution tape may
contain any combination of these files.
.PP
The following commands would suffice to restore the main IRAF system to
disk, given the distribution tape described by the TOC file in our example
above.  Once again, the tape device file and block size shown in the example
will very likely have to be changed to whatever is needed for the tape
device being used (the example is for a cartridge drive).
.XS
% whoami
iraf
% cd $iraf
% mt -f /dev/nrst0 rew;  mt -f /dev/nrst0 fsf 1
% tar -xpbf 126 /dev/nrst0
.XE
.PP
After the above tar file read operation, the tape is left positioned to
\fIjust before the EOF of the file just read\fR, since \fItar\fP stops
reading the file data before reading the physical EOF.  Hence, an
\fImt\0fsf\fR will be required to position to the next file on the tape.
Any combination of \fIfsf\fP (forward skip file) or \fIbsf\fR (backward skip
file) operations may be used to position to a file on a 9 track tape.  On a
cartridge tape, it is best to plan things so that only forward file skips
are used, using a rewind and forward skip if it is necessary to position to
an earlier file on the tape.
.PP
Once the main system, containing only sources, is installed it is possible to
create one or more empty BIN directories for the executables, then compile
and link the full system.  More commonly one will merely read the precompiled
executables off the distribution tape, as we discuss in the next section.
.NH 3
Configuring the BIN directories
.PP
In IRAF all the files specific to any particular architecture, e.g., sparc
(Sun-4 or sparcstation) or f68881 (Sun-3, mc68020 based) are contained in
a single directory called the BIN, or "binary", directory.  To run IRAF
you must install not only the \f(CWAS\fR (all-sources) directory tree, but
the BIN directory for each architecture.  The IRAF core system and the
NOAO packages have separate BIN directories.
.PP
The BIN directories for the IRAF core system or a layered package (such as
NOAO) are located, logically or physically, in the root directory of the
IRAF core system or layered package.  Every layered package has its own set
of BIN directories.  In the distributed V2.10 system you will find the
following BIN files (directories or symbolic links) at the IRAF root.
.XS
link         bin -> bin.generic
directory    bin.generic
link         bin.sparc  -> ../irafbin/bin.sparc
link         bin.f68881 -> ../irafbin/bin.f68881
link         bin.ffpa   -> ../irafbin/bin.ffpa
.XE
.PP
If the IRAF directory structure is set up as described in \(sc2.1.2, with
$iraf located at iraf/iraf and the BIN directories stored in iraf/irafbin,
then these links will not have to be modified.  If a different directory
structure is used you will have to modify the links accordingly.
.PP
The \fIbin\fR link and the \fIbin.generic\fR directory are required for the
correct operation of the IRAF system software (\fImkpkg\fR) and are
maintained automatically by the IRAF software management utilities.
\fIUnder no circumstances should "bin" or "bin.generic" be modified or
deleted\fR.  It is a very common error to manually delete the bin link and
manually set it to bin.sparc or some other architecture.  The links
bin.sparc, bin.ffpa, and bin.f68881 can be modified as desired but bin and
bin.generic should be left alone.
.PP
Assume that the bin.sparc directory has been created somewhere (e.g. in the
iraf/irafbin directory) and that the \f(CWib.sos4.spc\fR distribution files
for the core IRAF system sparc binaries have been downloaded from the
network archive.  We can restore the sparc binaries with the following
commands.
.XS
% cd $iraf/bin.sparc
% cat /\fIpath\fP/ib.sos4.spc/ib.* | uncompress | tar -xpf -
.XE
Similarly, to restore the NOAO package sparc binaries:
.XS
% cd $iraf/noao/bin.sparc
% cat /\fIpath\fP/nb.sos4.spc/nb.* | uncompress | tar -xpf -
.XE
This process is repeated for each architecture.  For example, a central
IRAF distribution installed on a server machine with both Sun-3 and Sun-4
clients might well require the sparc, f68881, and ffpa architectures, or
six BIN directories in all.
.PP
The procedure for restoring a BIN directory from a tape distribution is
similar to that described in \(sc2.2.2 for the core system.  For example,
.XS
% cd $iraf/bin.sparc
% mt -f /dev/nrst0 rew;  mt -f /dev/nrst0 fsf 4
% tar -xpbf 126 /dev/nrst0
.XE
would restore the core system bin.sparc directory from a cartridge tape
containing an uncompressed \f(CWib.sos4.spc\fR as file 4 on the tape.
.NH 3
Deleting unused HSI binaries
.PP
Unlike the main IRAF system and external packages like NOAO, the host system
interface (HSI) comes with its binaries pre-installed.  The \f(CWS386\fP
(Sun 386i) HSI comes with only a single set of HSI binaries which you will
surely need if you are installing on a 386i.  The \f(CWSOS4\fP (SunOS-4)
HSI, on the other hand, comes with pre-installed sparc and mc68020 (Sun-3)
HSI binaries.  These binaries are stored in the bin.sparc and bin.mc68020
subdirectories in $iraf/unix.
.PP
If you will not be needing either of these HSI BINs (because you don't have
both Sun-3 and Sun-4 clients) you may wish to delete the contents of one or
the other of these directories to save disk space.  For example, if IRAF is
installed on a standalone Sun-4 system only the binaries in bin.sparc will
be needed, and the contents of $iraf/unix/bin.mc68020 can be deleted.

.NH 2
Merge local revisions back into the new system
.PP
If this is a new IRAF installation this step can be skipped.  Otherwise,
once the new system has been restored to disk any local revisions made to
the previous IRAF installation should be merged back into the new system.
See \(sc2.1.1 for a list of the files most likely to be affected.
When propagating revisions made to these files, be sure to not replace the
entire file with your saved version, as the version of the file in the new
release of IRAF will often contain important additions or changes which
must be preserved.  It is best to merge your revisions into the version of
the file which comes with the new IRAF.\(dg
.FS
\(dgThe UNIX utility \fIdiff\fP is useful for comparing files to see
what has changed.
.FE
This task will be easier if the revisions have been localized as far as
possible, e.g., keep all \f(CWtermcap\fP additions together at the head of
the file, so that they may merely be transferred to the new file with the
editor.  The task of propagating revisions will also be much easier if
detailed notes have been kept of all revisions made since the the last
release was installed.
.PP
Beginning with IRAF version 2.8, one should no longer install locally added
software in the core system LOCAL package.  This significantly complicates
updates and is no longer necessary as, due to the layered software
enhancements introduced in V2.8 IRAF, it is now straightforward for each
site to maintain their own custom LOCAL package external to the core IRAF
system.  The core system LOCAL is now only a \fItemplate-local\fR to be
copied and used as the starting point for a custom LOCAL layered package.
The layered software enhancements, and the procedure for building a custom
LOCAL, are discussed further in the \fISun/IRAF Site Manager's Guide\fR.

.NH 2
Run the INSTALL Script
.PP
Once all of the IRAF files have been restored to disk the Sun/IRAF installation
script must be run to complete the system installation.  The install script
modifies the system as necessary to reflect the new root directory and new
default image storage and local bin directories, checks the mode and ownership
of a number of files, installs a small set of IRAF commands in UNIX, and so on.
.LP
To make a trial run of the install script, enter the following commands:
.XS
% setenv iraf /\fIpath\fP/iraf/
% cd $iraf/unix/hlib
% source irafuser.csh
% ./install -n
.XE
and answer the questions (don't forget the trailing `/' in the "setenv
iraf").  The "-n" argument tells install to go through the motions without
actually doing anything, so that one can see what will be done before
committing to it.
.PP
Installing IRAF requires a few changes to be made to system directories
outside the IRAF directory tree.  Two fifo device entries are made in /dev.
A symbolic link "iraf.h" is created in /usr/include.  A number of links (cl,
mkiraf, etc.) are made in /usr/local/bin or some similar directory which
most users can be expected to have in their search path.  The tape
allocation task alloc.e is made suid root (there are no known security
loopholes, although we cannot make any guarantees).  A symbolic link
imtoolrc is created in /usr/local/lib.  If installing the SunView support,
the gterm and imtool executables are copied into /usr/bin.
.PP
Following one or more trial "no execute" ("-n") runs to see what the install
script will do, the install script should be run without the "-n" to
complete the installation.  This must be done by the superuser as superuser
permission is required to carry out the necessary additions to UNIX.
.PP
It is necessary to run the install script separately on each node from which
IRAF will be used.  If a single version of IRAF is installed on a server and
NFS mounted on one or more clients, the install script must be run first on
the server and then on \fIeach\fR client (when installing on a client there
will be warnings about insufficient permission to make changes to files on
the NFS mounted partitions, which can be ignored).  To install IRAF on a
diskless client it may be necessary to run the install script \fIon the
server\fR to do the install for the client, since the client's /usr/include
and /dev directories may only be writable by root on the server.  On some
systems /usr is mounted read-only, and must be unmounted and remounted
read-write before doing the installation to allow an entry to be made in
/usr/include.  Once the installation is complete the default mount access
mode may be restored.
.PP
The exchange with the install script will be along the lines of the
following (this example is for a sparc server):
.XS
% ./install -n
new iraf root directory (/iraf/iraf):
default root image storage directory (/d0/iraf):
local unix commands directory (/usr/local/bin):
install iraf for machine type sparc
old iraf root = /usr/iraf, old imdir = /d0/iraf
installing iraf at /iraf/iraf, imdir=/d0/iraf, lbindir=/usr/local/bin
proceed with installation? (yes):
.XE
.LP
The "iraf root directory" is the value of $iraf (minus the trailing `/'in
this case).  The "root image storage directory" is the default place to put
image data for users; the program may prompt with /tmp if it cannot find any
likely looking data storage areas on your system, but /tmp is not a good
place to put image data as the contents are deleted whenever the system
reboots.  The value entered should be the path to a public iraf subdirectory
of a designated data or scratch disk on your system.  Lastly, the "local
unix command directory" is where the UNIX callable IRAF startup commands
will be defined.  This should be a UNIX directory which is in the default
path of anyone who might want to use IRAF; /usr/local/bin is the most common
value.
.PP
After answering with "yes" or hitting return in response to the "proceed with
installation" query, the script will issue a series of messages as it checks
the system and performs the installation, possibly answering additional
questions in the process.

.NH
System Checkout
.PP
The basic IRAF system should be usable once the files have been restored to
disk, the binaries have been configured or generated, and the install script
has been run.  To verify that the basic system comes up and runs
successfully, login as iraf and startup the CL (IRAF command language) from
the iraf account.  You should be able to login as IRAF and type "cl" to
start IRAF, using the login files which come with the distributed system.
.XS
% login iraf
% cl
.XE
.LP
To more thoroughly test the installation it is a good idea to test IRAF from
a user account.  To do this you login to a user account and run the
\fImkiraf\fR task to set up the IRAF login files.  This will create or
initialize the user's \f(CWuparm\fP (user parameter) directory, and create a
new \f(CWlogin.cl\fP file.  It may also be desirable to edit the
user's \f(CW.login\fP file to modify the way the environment variable
\f(CWIRAFARCH\fP is defined.  This variable, required for software
development but optional for merely using IRAF, must be set to the name of
the desired machine architecture, e.g., sparc, f68881, etc.
.XS
% mkiraf
Initialize uparm? (y|n): y
Terminal types: xterm,gterm,vt640,vt100,etc."
Enter terminal type: gterm
A new LOGIN.CL file has been created in the current directory.
You may wish to review and edit this file to change the defaults.
.XE
The \fIcl\fR command should now start up the CL, which will clear the screen
and print out a startup message.  The standard test procedure included in
Volume 1A of the \fIIRAF User Handbook\fP should be run to verify the
installation.

.bp
.SH
Appendix A.  A Complete Example
.PP
Assume we are installing IRAF for the first time on a sparcstation.  The
IRAF directories will be located at /u3/iraf using a symbolic link /iraf to
point to this location.  We will configure only the sparc binaries, locating
the BIN directories in the directory /iraf/irafbin.  The local user commands
will be placed in /usr/local/bin.  We will be installing from a network
distribution with the distribution files located in /scr0.
.PP
The first step is for the superuser to create an account for the fictitious
user `\f(CWiraf\fP', with home directory /iraf/iraf/local and shell
/bin/csh.  The /u3/iraf directory is created owned by IRAF, and pointed to
by the link /iraf.  We then login as IRAF (a warning message will be printed
since there is no login directory) and proceed as follows.
.XS
% whoami
iraf
%
% setenv iraf /iraf/iraf/               \fR# set root directory\fP
% mkdir /iraf/iraf
%         
% cd $iraf                              \fR# unpack main IRAF distribution\fP
% cat /scr0/as.sos4.gen/as.* | uncompress | tar -xpf -
%
% cd /iraf                              \fR# create BIN directories\fP
% mkdir irafbin
% mkdir irafbin/bin.sparc
% mkdir irafbin/noao.bin.sparc
%
% cd $iraf/bin.sparc                    \fR# unpack core bin.sparc\fP
% cat /scr0/ib.sos4.spc/ib.* | uncompress | tar -xpf -
%
% cd $iraf/noao/bin.sparc               \fR# unpack NOAO bin.sparc\fP
% cat /scr0/nb.sos4.spc/nb.* | uncompress | tar -xpf -
%
% cd $iraf/unix/hlib                    \fR# run the INSTALL script\fP
% source irafuser.csh
% ./install -n
% su
# ./install
# exit
%
% cd
% source .login                         \fR# read new .login\fP
% rehash                                \fR# pick up new iraf commands\fP
% cl                                    \fR# verify that the CL runs\fP
.XE
.LP
This will fully install IRAF on a server or a standalone system.  If this
version of IRAF will be accessed via NFS by client nodes then the IRAF
install script must be run on each client node as well.  Installing IRAF
does not allow one to access local tape drives, printers, and so on.
Refer to the \fISun/IRAF Site Manager's Guide\fR for information on how
to configure IRAF for the local site.
