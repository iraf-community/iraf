.RP
.TL
Sun/IRAF Installation Guide
.AU
Doug Tody
.AI
Central Computer Services
.br
.K2 "" "" "\(dg"
.br
June 1989

.AB
This document describes how to install IRAF on a Sun workstation, or update
an existing installation.  Both standalone and networked, multiple architecture
configurations are described.  Only those issues which one must understand
to install Sun/IRAF are discussed here; a companion document, \fISun/IRAF Site
Manager's Guide\fR, deals with other issues such as interfacing new devices,
configuring the IRAF networking system, Sun/IRAF shared libraries, adding
layered software, and so on.
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
2.\h'|0.4i'\fBInstalling Sun/IRAF\fP\l'|5.6i.'\0\02
.br
\h'|0.4i'2.1.\h'|0.9i'Prepare the root IRAF directory\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.1.\h'|1.5i'If updating an existing IRAF installation...\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.2.\h'|1.5i'If installing IRAF for the first time...\l'|5.6i.'\0\03
.br
\h'|0.4i'2.2.\h'|0.9i'Install the files\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.1.\h'|1.5i'Distribution tape format\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.2.\h'|1.5i'Installing the main system\l'|5.6i.'\0\05
.br
\h'|0.9i'2.2.3.\h'|1.5i'Configuring the BIN directories\l'|5.6i.'\0\05
.br
\h'|0.9i'2.2.4.\h'|1.5i'Deleting unused HSI binaries\l'|5.6i.'\0\07
.br
\h'|0.9i'2.2.5.\h'|1.5i'Network software distributions\l'|5.6i.'\0\07
.br
\h'|0.4i'2.3.\h'|0.9i'Merge local revisions back into the new system\l'|5.6i.'\0\08
.br
\h'|0.4i'2.4.\h'|0.9i'Run the INSTALL Script\l'|5.6i.'\0\08
.sp
3.\h'|0.4i'\fBSystem Checkout\fP\l'|5.6i.'\0\09
.sp
\fBAppendix A.\0A Complete Example\fP\l'|5.6i.'\0\11
.nr PN 0
.bp

.NH
Introduction
.PP
Before installing Sun/IRAF, one must 1) obtain an appropriate Sun/IRAF
distribution from the IRAF project, 2) select the server or node on which the
system is to be installed, and arrange for sufficient disk space to hold the
system, and 3) set aside sufficient time to do the installation.  If these
directions are followed carefully and mistakes are avoided 1-2 hours should
suffice to do the installation.
.PP
At the present time there are three main distributions of Sun/IRAF,
representing the range of workstations currently marketed by Sun, all of which
are directly supported by the IRAF project.  These are the following:
.sp
.RS
.IP \(bu
Sun-3 workstations running SunOS-3.
.IP \(bu
All Sun-3 and Sun-4 workstations running SunOS-4.
.IP \(bu
The 386i (Roadrunner), running SunOS-4.
.RE
.sp
.LP
These different distributions are all very similar at the source level (they
are identical except for several files) but very different binaries are
needed for a "load-and-go" installation on the different systems.
Sun/IRAF V2.8 will be the last release supporting OS-3.  Sun/IRAF can also
be made to run on the older Sun-2 workstations, although this is not directly
supported by IRAF since we don't have access to any Sun-2 systems.
.PP
The new SPARCstation class of machines is supported by the Sun-4 OS-4 binary
distribution, which will run on the SPARCstation, although the current version
of Sun/IRAF cannot yet be compiled under the new version of SunOS (4.0.3)
provided with the SPARCstation.  For normal runtime usage the 386i can be
supported as an architecture (BIN) under the OS-4 distribution, e.g. from a
central Sun-4 server, but for full programming functionality a separate 386i
based system is required.
.PP
The amount of disk space required to install IRAF depends upon the system
configuration, primarily the number of architectures (SPARC, 386i, Sun-3
with f68881, Sun-3 with fpa, etc.) one needs to support.  The main system,
including both the core system and NOAO package sources, requires about 36 Mb,
less if stripped after installation.  Each core system binary requires about
9 Mb and each NOAO package binary requires about 7 Mb (assuming OS-4 and
shared libraries).  The actual numbers will vary slightly depending upon the
architecture.

.NH
Installing Sun/IRAF
.PP
Although the details of how Sun/IRAF is installed or updated depend upon
the type of distribution and the desired local system configuration,
the basic procedure is always the same.  First one obtains a distribution,
usually by writing to NOAO and requesting the normal tape distribution,
then one follows the procedure outlined below to install the system.
Most of these steps should be performed while logged in as `iraf';
superuser permission is required in the final stages of the installation,
to run the \f(CWinstall\fP script.
.DS
# Prepare the root IRAF directory.
\fHif (new installation) then
    create `iraf' account
else if (updating an old installation) then
    save locally modified files
    delete old version of iraf
endif\fP

# Install the files.
\fHuse the `tar' program to unpack the distribution files
configure the BIN directory for each supported architecture\fP

# Merge local revisions into new system.
\fHif (updating an old installation) then
    merge locally modified files back into new system
endif\fP

# Run the INSTALL script (as superuser).
# Checkout the new system.
.DE
.LP
It is important to realize before beginning the installation that IRAF is not
an isolated program or collection of programs, but a complex system in its own
right, providing a full programming environment, support for the addition of
layered software (including locally added software), support for multiple
architectures, and so on.  Someone who is familiar with the usual installation
procedures for UNIX add-on programs will get tripped up if they try to follow
similar procedures for installing IRAF, without first reading these
installation instructions carefully.
.PP
If problems should arise during the installation help is available by calling
the IRAF HOTLINE (602-323-4160), or by sending electronic mail to
\f(CWiraf@noao.edu\fP (internet) or \f(CW5355::iraf\fR (SPAN).

.NH 2
Prepare the root IRAF directory
.NH 3
If updating an existing IRAF installation...
.PP
If you are updating an existing IRAF installation then you will be replacing
IRAF by the new version, and IRAF should already have an account and root
directory on the desired host system.  You should save any locally modified
files and delete the old system, e.g., login as `\f(CWiraf\fP' and enter:
.DS
\f(CW% cd $iraf\fP\(dg
\f(CW% tar -cf /tmp/SAVE.tar local dev unix/hlib
% /bin/rm -rf *\fP
.DE
.FS
\(dg\0\(CW$iraf\fP symbolizes the UNIX pathname of the root IRAF directory.
.FE
.LP
There are many variants on this, e.g., you could run \f(CWfind\fR to determine
which files need to be saved and later merged back in, and you could copy
these files to some other directory, rather than making a full tar backup.
Although we suggest saving the entire directories listed above, in practice
only a few files are likely to have been modified, e.g., 
.DS
\(CWdev/devices
dev/hosts
dev/termcap
dev/graphcap
hlib/extern.pkg
hlib/login.cl
hlib/zzsetenv.def
local/.login\fP
.DE
.LP
Once the old system has been deleted you are ready to install the new one,
as described in \(sc2.2.  Note that it is essential to delete the
old system to avoid creating junk files or directories when the new system
is installed (due to file or directory name changes or deletions).
.NH 3
If installing IRAF for the first time...
.PP
If you are installing IRAF for the first time then the first step is to set up
a new account for the fictitious user `\f(CWiraf\fP'.  This is unconventional
but is recommended for the following reasons:
.DS
.IP \(bu
All IRAF system management should be performed using some derivative of the
environment provided by the "." files in the \f(CWiraf\fP login directory.
If this is not done important environment definitions may be missing which
are required for the correct execution of the software (this affects only
IRAF system management, not normal runtime usage).
.IP \(bu
Multiple people may need to be IRAF system manager.  Having a separate account
avoids the need for one user to know another user's password.  Even if there
is only one site manager at your site, it may be necessary to give login
information to the IRAF HOTLINE personnel to allow them to investigate a
problem.
.IP \(bu
Having IRAF owned by root is not a good solution as then anyone who needs to
serve as IRAF site manager would require the root password.
.DE
.LP
The common practice on Sun-3 and Sun-4 systems is to locate the IRAF root at
\f(CW/usr/iraf\fP, although any other directory would do (try to keep the
path to the root short to avoid later filename truncation when IRAF is run).
Note that the \fIlogin\fR directory for the iraf account should be
\f(CW$iraf/local\fR (e.g., \f(CW/usr/iraf/local\fP), rather than the more
conventional \f(CW$iraf\fR or root directory, as we want to keep all the
locally modified files in subdirectories off the iraf root, to simplify
site management.  If this point is missed the iraf environment will not be
set up properly, and later problems are sure to result.
.PP
On 386i systems Sun recommends placing exportable layered products such as
IRAF in the 386i-specific directory \f(CW/files/vol\fP.  Hence the recommended
root directory for IRAF on the 386i is \f(CW/files/vol/iraf\fP.  Due to the
mandatory yellow pages feature of the 386i, the easiest and most reliable
way to create a new user account is with SNAP.  If SNAP is used to create
the IRAF account the home directory will be set to \f(CW/home/iraf\fP.
.PP
To create the iraf root and login directories and make them the default
for the iraf account, do the following (as superuser):
.DS
\f(CW# rm -rf /home/iraf
# mkdir /files/vol/iraf /files/vol/iraf/local
# /etc/chown -R iraf /files/vol/iraf
# ln -s /files/vol/iraf/local /home/iraf\fP
.DE
.LP
Do not worry about configuring the environment files for the new account as
these will be created when the iraf system is later restored to disk.

.NH 2
Install the files
.PP
If you have not already done so, log into the iraf account so that the files
when restored will belong to iraf.  Mount the distribution tape, which should
be either a 6250 bpi 9 track tape, or a Sun cartridge tape.
.PP
If you are installing IRAF on a system which has a local tape drive you can
skip what follows go to \(sc2.2.1.
.PP
If the tape drive is on a remote node connected via the network
then it is simplest to copy the files to a temporary disk on the remote node,
e.g., with the unix utility \f(CWdd\fP, then use \f(CWrsh\fR and \f(CWcat\fR
to pipe the remote file into the standard input of \f(CWtar\fR to unpack it
on the local node.  Do not use NFS to access the remote file on disk directly,
as NFS is not an error corrected transfer protocol and data corruption can
result if there are any problems with the networking interfaces on your
systems (using \f(CWrsh\fR is also more efficient).
.LP
For example, if
.DS
\f(CW% tar -xpf /dev/nrmt8\fP
.DE
would be used to unpack a tarfile from tape to disk on the local node,
.DS
\f(CW% dd if=/dev/nrmt8 of=file.tar bs=10240       # remote node
% rsh \fInode\fP "cat file.tar" | tar -xpf -         # local node\fR
.DE
will accomplish the same thing using the network and an intermediate disk
file (the block size shown is for a standard tar file on a 9 track tape).
It is also possible to remotely execute \f(CWdd\fR to read directly from
the tape, eliminating the disk file, if you are certain of the current file
position of the tape.
.NH 3
Distribution tape format
.PP
Beginning with IRAF version 2.8, distribution tapes consist of multiple
files separated by tape marks, with a TOC (table of contents) file as the
first file on the tape.  To find out what is on the tape, rewind it and
read out the TOC file as follows (the device names required for your site
may vary from those shown).
.LP
For a 9 track drive:
.DS
\f(CW% mt -f /dev/nrmt8 rew;  cat /dev/nrmt8\fP
.DE
For a cassette drive:
.DS
\f(CW% mt -f /dev/nrst0 rew;  cat /dev/nrst0\fP
.DE
This should cause a TOC file to be listed similar to the following,
except for the file names which will vary depending upon what type of
distribution you have.  The example below is for a distribution of Sun/IRAF
for SunOS-4, with the f68881, ffpa, and sparc binaries.
.DS
.ps -2
\f(CW0   Table of Contents

1   AS.SOS4.GEN   36.2Mb   IRAF, NOAO packages and Sun/OS4 sources
2   IB.SOS4.F68    8.5Mb   f68881 binaries for IRAF system - Sun/OS4
3   IB.SOS4.FPA    9.1Mb   ffpa binaries for IRAF system - Sun/OS4
4   IB.SOS4.SPC   11.3Mb   sparc binaries for IRAF system - Sun/OS4
5   NB.SOS4.F68    6.9Mb   f68881 binaries for NOAO packages - Sun/OS4
6   NB.SOS4.FPA    7.4Mb   ffpa binaries for NOAO packages - Sun/OS4
7   NB.SOS4.SPC    8.7Mb   sparc binaries for NOAO packages - Sun/OS4\fP
.ps
.DE
.LP
Here, the first column is the file number on the tape, the TOC file being file
zero, the second column is the name of the tape file, the third column is
the file size in megabytes (this tells you how much space will be needed
to unpack the file on disk), and the last column is a description of the
file contents.
.PP
There are three types of tape files in the example shown: the \fBAS\fR file,
which is all the IRAF sources (core system, NOAO packages, and SunOS-4 host
system interface), the \fBIB\fR files, or IRAF core system binaries, one for
each architecture (machine cpu type or floating point accelerator option),
and the \fBNB\fR files, or NOAO package binaries.  The NOAO package sources
are included in the AS file since most people requesting IRAF are expected
to want the astronomical reduction software, although IRAF can be configured
without these if desired.  All of the file objects are UNIX \f(CWtar\fR
format files, with the exception of the TOC file which is a simple text file.
.PP
In the above example, the \f(CWSOS4\fR in the file names indicates that these
files should be installed on a workstation running SunOS-4.  A SunOS-3
distribution is indicated by a \f(CWSOS3\fR in the file names, and a 386i
distribution is indicated by a \f(CWS386\fP.  In principle a given distribution
tape may contain any combination of these files.
.NH 3
Installing the main system
.PP
To install the main IRAF system, login as iraf, set the current directory to
\f(CW$iraf\fP, and read and unpack the \f(CWAS\fR file from the tape, e.g.,
for a nine track tape, given the example TOC file shown above, where the
\f(CWAS\fP file is file 1 on the tape:
.DS
\f(CW% mt -f /dev/nrmt8 rew;  mt -f /dev/nrmt8 fsf 1
% tar -xpf /dev/nrmt8\fP
.DE
In the case of a cartridge tape the block size should also be specified,
and the command becomes:
.DS
\f(CW% mt -f /dev/nrst0 rew;  mt -f /dev/nrst0 fsf 1
% tar -xpbf 126 /dev/nrst0\fP
.DE
If the last operation performed on the tape was to read the TOC file the tape
will already be positioned to file 1 (which is the \f(CWAS\fP file in our
example), and the rewind/forward-skip step can be omitted.  After reading and
unpacking the tape file the current directory should be listed to verify that
the correct tape file was read.  If the correct tape file was read, the tape
file name (e.g., \f(CWAS.SOS4.GEN\fP) will appear as a zero length file in the
current directory after the unpack operation.
.PP
After either of the above tar file read operations, the tape is left
positioned to \fIjust before the EOF of the file just read\fR,
since \f(CWtar\fP stops reading the file data before reading the physical EOF.
Hence, an \f(CWmt\0fsf\fR will be required to position
to the next file on the tape.  Any combination of \f(CWfsf\fP (forward skip
file) or \f(CWbsf\fR (backward skip file) operations may be used to position
to a file on a 9 track tape.  On a cartridge tape, it is best to plan things so
that only forward file skips are used, using a rewind and forward skip if it
is necessary to position to an earlier file on the tape.
.PP
Once the main system, containing only sources, is installed it is possible to
create one or more empty BIN directories for the executables, then compile
and link the full system.  More commonly one will merely read the precompiled
executables off the distribution tape, as we discuss in the next section.
.NH 3
Configuring the BIN directories
.PP
In Sun/IRAF V2.8 and subsequent versions, all binaries (object files, object
libraries, and executables) for a software product are contained in a single
directory, the so-called BIN directory.  Each supported architecture, e.g.,
sparc, 386i, Sun-3 f68881, etc., has its own BIN directory containing all the
binaries for that architecture.  To be able to run IRAF, or use IRAF for
software development, on a machine of a particular architecture one
must first configure the BIN for that architecture.  Often a single copy of
IRAF will be installed on a server and NFS mounted on client nodes of various
architectures, and it is the use of multiple BIN directories which makes
such sharing of a single copy of IRAF possible.  Each major IRAF based software
product, e.g., the IRAF core system or an external layered product such as
the NOAO packages, STSDAS, LOCAL, etc., has its own independent set of BIN
directories.
.PP
Continuing with our example, suppose we want to configure IRAF and the NOAO
packages with the sparc binaries so that we can run the system on a Sun-4 or
SPARCstation.  Two BIN directories will be required, one for the core system,
and one for the NOAO packages.  If we later decide to add support for another
architecture two more BIN directories would be required, and so on for each
supported architecture.  Since a BIN can be fairly large it is common to
locate the BIN directory somewhere outside the IRAF directory tree, to provide
maximum flexibility in allocating the remaining free space in the available
disk partitions.
.PP
Having decided upon the architecture to be configured and where the BIN will
be located, two steps are required to configure the BIN:
.DS
.IP [1]
Create the BIN directory and read the binaries from the distribution tape
into the new directory.
.IP [2]
In the root directory of the package tree to which the BIN is being added
(e.g., \f(CW$iraf\fP for the core system or \f(CW$iraf/noao\fP for the NOAO
packages), create a symbolic link \f(CWbin.\fIarch\fR pointing to the BIN,
where \fIarch\fR is the name of the architecture, e.g., \f(CWf68881\fP,
\f(CWffpa\fP, \f(CWsparc\fP, or \f(CWi386\fP (\fInot\fP \f(CW386i\fP).
If the directory created in step [1] is already located in the package
root directory and has the right name, no link is required and this step
can be skipped.
.DE
.LP
In our example, let's assume we have a directory \f(CW/u3\fR with sufficient
space for our 11 Mb sparc BIN.  Assuming the sparc BIN is file 4 on the 9
track distribution tape and we have just unpacked tar file 1, leaving the
tape positioned to just before file 2, the following commands would suffice
to read the BIN (tape file \f(CWIB.SOS4.SPC\fP) onto disk:
.DS
\f(CW% mkdir /u3/bin.sparc
% cd /u3/bin.sparc
% mt -f /dev/nrmt8 fsf 3
% tar -xpf /dev/nrmt8\fP
.DE
Alternatively we could have rewound the tape and done an \f(CWfsf\04\fP to
get to file 4 on the tape.  The next step is to tell IRAF where the new BIN
directory is:
.DS
\f(CW% cd $iraf
% rm bin.sparc                     # remove old link, if any
% ln -s /u3/bin.sparc bin.sparc\fP
.DE
The procedure for configuring the sparc BIN for the NOAO packages (or any
other layered product) is very similar.  In that case we might create a BIN
directory \f(CW/u3/noao.bin.sparc\fP and link it to a \f(CWbin.sparc\fR in
the NOAO package root directory (normally \f(CW$iraf/noao\fP).
.PP
If we were to continue on in this vein and configure the IRAF core system
for use with the \f(CWsparc\fP, \f(CWf68881\fP, and \f(CWffpa\fP architectures,
the IRAF root directory might look as follows when we were done.
.DS
\f(CWHS.SOS4.GEN   bin.ffpa      doc         mkpkg       unix
IS.PORT.GEN   bin.generic   lib         noao
bin           bin.sparc     local       pkg
bin.f68881    dev           math        sys\fP
.DE
.LP
Here, \f(CWbin\fP and \f(CWbin.generic\fP are preexisting files which should
be left alone, and the remaining \f(CWbin.\fP files are links to external
BIN directories added by the IRAF site manager.
.NH 3
Deleting unused HSI binaries
.PP
Unlike the main IRAF system and external packages like NOAO, the host system
interface (HSI) comes with its binaries pre-installed.  The \f(CWSOS3\fP
(SunOS-3) and \f(CWS386\fP (Sun 386i) HSIs each come with only a single
set of HSI binaries which you will surely need if you are using one of these
systems.  The \f(CWSOS4\fP (SunOS-4) HSI, on the other hand, comes with
pre-installed \f(CWsparc\fP and \f(CWmc68020\fP (Sun-3) HSI binaries,
each of which is about 1.3-1.5 Mb in size.  These binaries are stored in
the obvious \f(CWbin\fP subdirectories in \f(CW$iraf/unix\fP.
.PP
If you will not be needing either of these HSI BINs (because you don't have
both Sun-3 and Sun-4 machines) you may wish to delete the contents of one or
the other of these directories to save disk space.  For example, if IRAF is
installed on a standalone Sun-3 system only the binaries in \f(CWbin.mc68020\fP
will be needed, and the contents of \f(CW$iraf/unix/bin.sparc\fP can be
deleted.
.NH 3
Network software distributions
.PP
Although most IRAF installations or updates will be made from a distribution
tape, it is also possible to install IRAF from compressed disk tar files
acquired via FTP from the IRAF network archive.  The procedure followed is
very similar to installing IRAF from a tape, except that the file objects are
stored in the FTP archive rather than on tape, and the content of the
distribution files is slightly different.  Most significantly, the binaries
are omitted hence if a network installation is attempted it will be necessary
to recompile the full system.
.PP
The main difference between the distribution tape and the network archive is
that the \f(CWAS\fR (all sources) file object is gone, being replaced by the
following files, which one has to manually combine to produce the equivalent
of the \f(CWAS\fP.
.DS
.IP \f(CWHS.\fIsys\fP.GEN\fR 20
The host system interface (HSI) for a particular host system.
.IP \f(CWIS.PORT.GEN\fR
The IRAF core system sources (for any system).
.IP \f(CWNS.PORT.GEN\fR
The NOAO package sources (for any system).
.DE
The \f(CWIS\fP and \f(CWNS\fP (core system and NOAO sources) are portable and
may be combined with the \f(CWHS\fP for any host machine to produce an IRAF
for that host.  For example, to configure IRAF for a Sun-3 or Sun-4
under SunOS-4, one would use \f(CWHS.SOS4.GEN\fP.  To configure both the
\f(CWSOS4\fP and \f(CWS386\fP (386i) systems one would fetch the former three
files plus the 386i HSI, \f(CWHS.S386.GEN\fP.  These files are stored in the
IRAF network archive in compressed form, hence the actual file names will have
a \f(CW.Z\fP appended and will have to be uncompressed with the UNIX program
\f(CWuncompress\fR before being unpacked with \f(CWtar\fP.
.PP
To build, for example, the \f(CWSOS4\fP IRAF using the network file objects,
starting from an empty root directory belonging to IRAF, with the compressed
archive files stored in \f(CW/tmp\fP:
.DS
\f(CW% cd $iraf
% uncompress < /tmp/IS.PORT.GEN.Z | tar -xpf -
% uncompress < /tmp/HS.SOS4.GEN.Z | tar -xpf -
% mkdir noao; cd noao
% uncompress < /tmp/NS.PORT.GEN.Z | tar -xpf -\fR
.DE
After running the INSTALL script to configure the programming environment
(see \(sc2.4), one might then configure, e.g., an empty \f(CWsparc\fP BIN
directory in the usual fashion, then enter
.DS
\f(CW% cd $iraf
% mkpkg sparc
% setarch sparc
% mkpkg >& spool\fR
.DE
which would configure the core system for the \f(CWsparc\fP architecture
and compile all the binaries (\f(CWsetarch\fR is an alias defined in
the \f(CW.login\fP for the iraf account).  To do the same for the NOAO packages,
one would configure the empty bin and then compile the system as follows:
.DS
\f(CW% cd $iraf/noao
% mkpkg sparc
% setarch sparc
% mkpkg -p noao >& spool\fR
.DE
As we see, the commands are the same except for the root directory and the
additional argument required to tell mkpkg the name of the non-core system
package being compiled.  To prepare BINs for other architectures one would
repeat the sequence of creating an empty BIN and doing a \f(CWmkpkg\fP to
compile the system for each architecture.
.PP
At the present time, anyone wishing to access files from the IRAF network
archive should first contact the IRAF group to determine the status of the
archive and how to access it.  In addition to the standard release products,
various updates, bug fixes, and add-on packages may be retrieved from the
archive without having to wait for a major release of the full system.

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
\(dgThe UNIX utility \f(CWdiff\fP is useful for comparing files to see
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
enhancements introduced in V2.8 IRAF, it is now straightforward for each site
to maintain their own custom LOCAL package external to the core IRAF system.
The core system LOCAL is now only a \fBtemplate-local\fR to be copied and
used as the starting point for a custom LOCAL.  The layered software
enhancements, and the procedure for building a custom LOCAL,
are discussed further in the \fISun/IRAF Site Manager's Guide\fR.

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
.DS
\f(CW% setenv iraf /\fIpath\fP/iraf/
% cd $iraf/unix/hlib
% source irafuser.csh
% ./install -n\fP
.DE
and answer the questions.  The "-n" argument tells install to go through the
motions without actually doing anything, so that one can see what will be done
before committing to it.
.PP
Following one or more trial "no execute" ("-n") runs, the install script
should be run without the "-n" to complete the installation.  This must be
done by the superuser as superuser permission is required to carry out the
necessary additions to UNIX.  Note that it is necessary to run the install
script separately on each node from which IRAF will be used.
Also, \f(CW/usr\fR, which is normally mounted read-only on a 386i,
must be mounted read-write before doing the installation to allow an entry
to be made in \f(CW/usr/include\fR.  Once the installation is complete the
default mount access mode may be restored.
.PP
The exchange with the install script will be along the lines of the
following (this example is for a 386i):
.DS
.ps -2
\f(CW% ./install -n
new iraf root directory (/files/vol/iraf): 
default root image storage directory (/tmp3/iraf): 
local unix commands directory (/usr/local/bin): 
install iraf for machine type i386
old iraf root = /files/vol/iraf, old imdir = /tmp3/iraf
installing iraf at /files/vol/iraf, imdir=/tmp3/iraf, lbindir=/usr/local/bin
proceed with installation? (yes):\fP
.ps
.DE
.LP
The "iraf root directory" is the value of \f(CW$iraf\fR.  The "root image
storage directory" is the default place to put image data for users; the
program may prompt with \f(CW/tmp\fR if it cannot find any likely looking
data storage areas on your system, but \f(CW/tmp\fR is not a good place to
put image data as the contents are deleted whenever the system reboots.
The value entered should be the path to a public iraf subdirectory of a
designated data or scratch disk on your system.  Lastly, the "local unix
command directory" is where the UNIX callable IRAF startup commands will
be defined.  This should be a UNIX directory which is in the default path
of anyone who might want to use IRAF; \f(CW/usr/local/bin\fR is the most
common value.
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
has been run.  To verify that the basic system comes up and runs successfully,
login as iraf and startup the CL (command language) from the iraf account.
.PP
Before starting up IRAF from the iraf account, following the installation of
a new system, it will be necessary to execute the \f(CWmkiraf\fP task.
This will (optionally) initialize the \f(CWuparm\fP directory for the iraf
account, and create a new \f(CWlogin.cl\fP file.  It may also be necessary
to edit the \f(CW.login\fP file to modify the way the environment variable
\f(CWIRAFARCH\fP is defined.  This variable, required for software
development but optional for merely using IRAF, must be set to the name
of the desired machine architecture, e.g., \f(CWsparc\fP, \f(CWf68881\fP, etc.
If it is set to the name of an architecture for which there are no binaries,
e.g., \f(CWgeneric\fP, the CL will not run.
.DS
.ps -2
\f(CW% mkiraf
Initialize uparm? (y|n): 
Terminal types: gterm=ttysw+graphics,vt640=(vt100+retrographics),etc.
Enter terminal type: gterm
A new LOGIN.CL file has been created in the current directory.
You may wish to review and edit this file to change the defaults.
.DE
The CL should then run.  For Sun/IRAF the CL is normally run from a GTERM
window under SunView, e.g.:
.DS
\f(CW% sun                 # \fRstartup SunView\fP
% cl                  # \fRstartup IRAF (in GTERM window)\fP
.DE
.LP
This should startup the CL, which will clear the screen and print out a
startup message.  The alias \fIsun\fP will startup SunView with the text,
graphics, and image display windows normally used to run IRAF on a Sun
workstation.  The standard test procedure included in Volume 1A of the
\fIIRAF User Handbook\fP should be run to verify the installation.

.bp
.SH
Appendix A.  A Complete Example
.PP
Assume we are installing IRAF for the first time on a Sun-3.
The IRAF root will be \f(CW/usr/iraf\fP.  We will configure both the f68881
and ffpa binaries, locating the BIN directories in the directory \f(CW/u2\fP.
The local user commands will be placed in \f(CW/usr/local/bin\fP.  We will
be reading a 9 track tape distribution from device \f(CW/dev/nrmt8\fP.
.PP
The first step is for the superuser to create an account for the fictitious
user `\f(CWiraf\fP', with home directory \f(CW/usr/iraf/local\fP and shell
\f(CW/bin/csh\fP.  We then login as iraf (a warning message will be printed
since there is no login directory) and proceed as follows:
.sp
.RS
.nf
.ps -2
.vs 8
\f(CW% setenv iraf /usr/iraf/
% source $iraf/unix/hlib/irafuser.csh
% cd $iraf
%
% whoami
iraf
%
% mt -f /dev/nrmt8 rew
% cat /dev/nrmt8
0  Table of Contents
1  AS.SOS4.GEN  36.2Mb  IRAF, NOAO packages and Sun/OS4 sources
2  IB.SOS4.F68   8.5Mb  f68881 binaries for IRAF system - Sun/OS4
3  IB.SOS4.FPA   9.1Mb  ffpa binaries for IRAF system - Sun/OS4
4  NB.SOS4.F68   6.9Mb  f68881 binaries for NOAO packages - Sun/OS4
5  NB.SOS4.FPA   7.4Mb  ffpa binaries for NOAO packages - Sun/OS4
%
% tar -xpf /dev/nrmt8           # unpack AS.SOS4.GEN
%
% mkdir /u2/bin.f68881
% cd /u2/bin.f68881
% mt -f /dev/nrmt8 fsf
% tar -xpf /dev/nrmt8           # unpack IB.SOS4.F68
%
% mkdir /u2/bin.ffpa
% cd /u2/bin.ffpa
% mt -f /dev/nrmt8 fsf
% tar -xpf /dev/nrmt8           # unpack IB.SOS4.FPA
%
% mkdir /u2/noao.bin.f68881
% cd /u2/noao.bin.f68881
% mt -f /dev/nrmt8 fsf
% tar -xpf /dev/nrmt8           # unpack NB.SOS4.F68
%
% mkdir /u2/noao.bin.ffpa
% cd /u2/noao.bin.ffpa
% mt -f /dev/nrmt8 fsf
% tar -xpf /dev/nrmt8           # unpack NB.SOS4.FPA
%
% cd $iraf                      # install IRAF BINs
% rm bin.f68881
% ln -s /u2/bin.f68881 bin.f68881
% rm bin.ffpa
% ln -s /u2/bin.ffpa bin.ffpa
%
% cd $iraf/noao                 # install NOAO BINs
% rm bin.f68881
% ln -s /u2/noao.bin.f68881 bin.f68881
% rm bin.ffpa
% ln -s /u2/noao.bin.ffpa bin.ffpa
%
% cd $iraf/unix/hlib            # run the INSTALL script
% ./install -n
% su
# ./install
# exit
%
% cd
% source .login                 # read .login now that we have one
% rehash                        # pick up new iraf commands
% mkiraf                        # initialize iraf environment
%
% setarch f68881                # run f68881 version of system
% sun                           # fire up SunView
% cl                            # verify that the CL runs\fP
.ps
.vs
.ke
.RE
