.RP
.TL
BSD-UNIX/IRAF Installation Guide
.AU
Doug Tody
.AI
Central Computer Services
.br
.K2 "" "" "\(dg"
.br
July 1989

.AB
This document describes how to install IRAF on a BSD-UNIX system, or update
an existing installation.  Both standalone and networked, multiple architecture
configurations are described.  Only those issues which one must understand
to install BSD-UNIX/IRAF are discussed here;
a companion document, \fIUNIX/IRAF Site Manager's Guide\fR,
deals with other issues such as interfacing new devices,
configuring the IRAF networking system, adding layered software, and so on.
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
2.\h'|0.4i'\fBInstalling BSD-UNIX/IRAF\fP\l'|5.6i.'\0\02
.br
\h'|0.4i'2.1.\h'|0.9i'Prepare the root IRAF directory\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.1.\h'|1.5i'If updating an existing IRAF installation...\l'|5.6i.'\0\02
.br
\h'|0.9i'2.1.2.\h'|1.5i'If installing IRAF for the first time...\l'|5.6i.'\0\03
.br
\h'|0.4i'2.2.\h'|0.9i'Install the files\l'|5.6i.'\0\03
.br
\h'|0.9i'2.2.1.\h'|1.5i'Distribution tape format\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.2.\h'|1.5i'Installing the main system\l'|5.6i.'\0\04
.br
\h'|0.9i'2.2.3.\h'|1.5i'Configuring the BIN directories\l'|5.6i.'\0\05
.br
\h'|1.5i'2.2.3.1.\h'|2.2i'BIN directories under the IRAF root.\l'|5.6i.'\0\05
.br
\h'|1.5i'2.2.3.2.\h'|2.2i'BIN directories outside the IRAF root.\l'|5.6i.'\0\06
.br
\h'|0.9i'2.2.4.\h'|1.5i'Network software distributions\l'|5.6i.'\0\06
.br
\h'|0.4i'2.3.\h'|0.9i'Merge local revisions back into the new system\l'|5.6i.'\0\07
.br
\h'|0.4i'2.4.\h'|0.9i'Run the INSTALL Script\l'|5.6i.'\0\08
.sp
3.\h'|0.4i'\fBSystem Checkout\fP\l'|5.6i.'\0\08
.sp
\fBAppendix A.\0A Complete Example\fP\l'|5.6i.'\0\10

.bp
.NH
Introduction
.PP
Before installing BSD-UNIX/IRAF, one must 1) obtain an appropriate
BSD-UNIX/IRAF distribution from the IRAF project, 2) select the machine on
which the system is to be installed, and arrange for sufficient disk space
to hold the system (approximately 65 Mb for BSD-UNIX/IRAF),
and 3) set aside sufficient time to do the installation.
If these directions are followed carefully and mistakes are avoided 1-2
hours should suffice to do the installation.
.PP
This Installation Guide is intended primarily for sites installing IRAF on
a VAX running Berkeley UNIX 4.3, although it may be used as a general guide
when doing ports to BSD-like systems.
Other popular UNIX systems for which IRAF is available, e.g. SunOS and Ultrix,
have their own system specific IRAF installation guides.
.PP
The device and system configuration tables in the standard IRAF distribution
come configured for the NOAO systems on which the distribution tapes were made,
and will have to be modified once the system is installed.
These modifications are discussed in detail in the companion document
\fIUNIX/IRAF Site Manager's Guide\fP.
To simplify the installation process as well as future upgrades, we have tried
to isolate the site dependent files to the minimum number of directories, i.e.,
\f(CWdev\fR, \f(CWhlib\fR (a subdirectory of \f(CW$iraf/unix\fR), and
\f(CWlocal\fR.
The remainder of the system should not require any modifications.
.PP
In IRAF V2.8 local additions to the system are no longer installed directly
in IRAF; the \fIlayered software\fP enhancements allow maintainance of a
custom LOCAL package outside IRAF.  Sites which had previously maintained their
own IRAF software in \f(CW$iraf/local\fR will have a one-time conversion job,
discussed in the Site Manager's Guide; in future releases only a single file
will need to be edited to reinstall the local additions.
.sp
.TS
center;
cb s s
l l l.
IRAF HOTLINE
.sp
telephone	\f(CW(602) 323-4160\fP
internet	\f(CWiraf@noao.edu\fP
span/hepnet	\f(CWnoao::iraf\fP	(noao = 5355)
uucp	\f(CW{arizona,decvax,ncar}!noao!iraf\fP or
uucp	\f(CWuunet!noao.edu!iraf\fP
bitnet	\f(CWiraf@noao.edu\fP (through a gateway)
.TE
.PP
Issues such as interfacing new graphics terminals, plotters, or image displays
are also described in the Site Manager's Guide.  Help is available via the
IRAF Hotline if any problems should arise while installing the system or
interfacing new devices.

.NH
Installing BSD-UNIX/IRAF
.PP
Installing BSD-UNIX/IRAF on an actual BSD-UNIX system is straightforward.
First one obtains a distribution,
usually by writing to NOAO and requesting the normal tape distribution,
then one follows the procedure outlined below to install the system.
Most of these steps should be performed while logged in as `iraf';
superuser permission is required in the final stages of the installation,
to run the \f(CWinstall\fP script.
.DS
# Prepare the root IRAF directory.
\f(CWif (new installation) then
    create `iraf' account
else if (updating an old installation) then
    save locally modified files
    delete old version of iraf
endif\fP

# Install the files.
\f(CWuse the `tar' program to unpack the distribution files
configure the BIN directory for each supported architecture\fP

# Merge local revisions into new system.
\f(CWif (updating an old installation) then
    merge locally modified files back into new system
endif\fP

# Run the INSTALL script (as superuser).
# Checkout the new system.
.DE
.LP
It is important to realize before beginning the installation that IRAF is not
an isolated program or collection of programs, but a complex system in its own
right, providing a full programming environment, support for the addition of
layered software (including locally added software), and so on.
Someone who is familiar with the usual installation
procedures for UNIX add-on programs will get tripped up if they try to follow
similar procedures for installing IRAF, without first reading these
installation instructions carefully.  A complete example for the simplest
type of installation is provided in Appendix A.

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
\(dg\0$iraf symbolizes the UNIX pathname of the root IRAF directory.
.FE
.LP
There are many variants on this, e.g., you could run \f(CWfind\fR to determine
which files need to be saved and later merged back in, and you could copy
these files to some other directory, rather than making a full tar backup.
Although we suggest saving the entire directories listed above, in practice
only a few files are likely to have been modified, e.g., 
.DS
\f(CWdev/devices
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
old system as described above to avoid creating junk files or
directories when the new system
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
information to the IRAF Hotline personnel to allow them to investigate a
problem.
.IP \(bu
Having IRAF owned by root is not a good solution as then anyone who needs to
serve as IRAF site manager would require the root password.
.DE
.LP
The common practice on most BSD systems is to locate the IRAF root at
\f(CW/usr/iraf\fP, although any other directory would do (try to keep the
path to the root short to avoid later filename truncation when IRAF is run).
Note that the \fIlogin\fR directory for the iraf account should be
\f(CW$iraf/local\fR (e.g., \f(CW/usr/iraf/local\fP), rather than the more
conventional \f(CW$iraf\fR or root directory, as we want to keep all the
locally modified files in subdirectories off the iraf root, to simplify
site management.  If this point is missed the iraf environment will not be
set up properly, and later problems are sure to result.
.PP
It is not necessary to place the entire system on the same disk; the binary
files and external packages like \f(CWnoao\fP may be located on a separate
disk from the core system if desired; see \(sc2.2.1 for the sizes of the
different components.
.PP
Do not worry about configuring the \f(CW.login\fP or other environment
files for the new account as these will be created when the iraf system
is later restored to disk.

.NH 2
Install the files
.PP
If you have not already done so, log into the iraf account so that the files
when restored will belong to iraf.  Mount the distribution tape, which might
be, for example, a 1600\(dg or 6250 bpi 9 track tape, or a TK50 cartridge tape.
.FS
\(dgDistributions for a 1600 bpi tape require two tapes.  Instructions for
reading in IRAF from two tapes are contained in a separate cover letter.
.FE
.PP
If you are installing IRAF on a system which has a local tape drive you can
skip what follows and go to \(sc2.2.1.
.PP
If the tape drive is on a remote node connected via the network
then it is simplest to copy the files to a temporary disk on the remote node,
e.g., with the unix utility \f(CWdd\fP, then use \f(CWrsh\fR and \f(CWcat\fR
to pipe the remote file into the standard input of \f(CWtar\fR to unpack it
on the local node.  If you have NFS, do not use it to access the remote file
on disk directly,
as NFS is not an error corrected transfer protocol and data corruption can
result if there are any problems with the networking interfaces on your
systems (using \f(CWrsh\fR is also more efficient).
.LP
For example, if
.DS
\f(CW% tar -xpf /dev/nrmt8\fP
.DE
would be used to unpack a tarfile from tape to disk on the local node as in
\(sc2.2.1, then
.DS
\f(CW% dd if=/dev/nrmt8 of=file.tar bs=10240       # remote node
% rsh \fInode\fP "cat file.tar" | tar -xpf -         # local node\fR
.DE
will accomplish the same thing using the network and an intermediate disk
file.  The block size shown is for a standard tar file on a 9 track tape.
It is also possible to execute \f(CWdd\fR remotely to read directly from
the tape, eliminating the disk file, if you are certain of the current file
position of the tape.
.NH 3
Distribution tape format
.PP
Beginning with IRAF version 2.8, distribution tapes consist of multiple
files separated by tape marks, with a TOC (table of contents) as the
first file on the tape.  To find out what is on the tape, rewind it and
read out the TOC file as follows (the device name required for your site
may vary from that shown):
.DS
\f(CW% mt -f /dev/nrmt8 rew;  cat /dev/nrmt8\fP
.DE
This should cause a TOC file similar to the following to be
listed for a normal BSD-only distribution; the sizes of the files will
change for different releases:
.DS
.ps -2
\f(CW0    Table of Contents
1    AS.VBSD.GEN     44.9Mb   IRAF, NOAO packages and VAX/BSD sources
2    IB.VBSD.VAX      9.9Mb   IRAF system binaries for VAX/BSD
3    NB.VBSD.VAX     12.3Mb   NOAO packages binaries for VAX/BSD
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
which is all the IRAF sources (core system, NOAO packages, and VAX/BSD host
system interface), the \fBIB\fR file, or IRAF core system binaries,
and the \fBNB\fR file, or NOAO binaries.  The NOAO sources
are included in the AS file since most people requesting IRAF are expected
to want the astronomical reduction software, although IRAF can be configured
without these if desired.  All of the file objects are UNIX \f(CWtar\fR
format files, with the exception of the TOC file which is a simple text file.
.NH 3
Installing the main system
.PP
To install the main IRAF system, login as \f(CWiraf\fR, set the current
directory to
\f(CW$iraf\fP, and read and unpack the \f(CWAS\fR file from the tape, e.g.,
for a nine track tape, given the example TOC file shown above, where the
\f(CWAS\fP file is file 1 on the tape:
.DS
\f(CW% mt -f /dev/nrmt8 rew;  mt -f /dev/nrmt8 fsf 1
% tar -xpf /dev/nrmt8\fP
.DE
If the last operation performed on the tape was to read the TOC file, the tape
will already be positioned to file 1 (which is the \f(CWAS\fP file in our
example), and the rewind/forward-skip step can be omitted.  After reading and
unpacking the tape file the current directory should be listed to verify that
the correct tape file was read.  If the correct tape file was read, the tape
file name (e.g., \f(CWAS.VBSD.GEN\fP) will appear as a zero length file in the
current directory after the unpack operation.
.PP
After either of the above tar file read operations, the tape is left
positioned to \fIjust before the EOF of the file just read\fR,
since \f(CWtar\fP stops reading the file data before reading the physical EOF.
Hence, an \f(CWmt\0fsf\fR will be required to position
to the next file on the tape.  Any combination of \f(CWfsf\fP (forward skip
file) or \f(CWbsf\fR (backward skip file) operations may be used to position
to a file on a 9 track tape.
.PP
Once the main system, containing only sources, is installed it is possible to
create one or more empty BIN directories for the executables, then compile
and link the full system.  More commonly one will merely read the precompiled
executables off the distribution tape, as we discuss in the next section.
.NH 3
Configuring the BIN directories
.PP
The executables for a software product such as the IRAF core system or the
NOAO packages are contained in a single directory, the so-called BIN directory.
In some cases the system object files and libraries may also reside in the BIN,
e.g., to support software development for multiple architectures.
In the rest of this document, we will assume a single IRAF system, supporting
only the VAX/BSD architecture both for the runnable system and for software
development.  A system configured for multiple architecture support will have
multiple BIN directories, one for each architecture.
Further information on multiple architecture support is given in the
\fIUNIX/IRAF Site Manager's Guide\fP.
.PP
For the default VAX/BSD-only configuration, two BIN directories will be
required: one for the core system, and one for the NOAO packages.  
Since a BIN can be fairly large one may want to
locate the BIN directory somewhere outside the IRAF directory tree, to provide
maximum flexibility in allocating the remaining free space in the available
disk partitions.  A BIN may be located either in the root directory of the
system to which it belongs, or in an external directory, replacing the entry
in the package root directory by a symbolic link.  The procedures for
configuring the BINs in each case are outlined below.
.NH 4
BIN directories under the IRAF root.
.PP
Go to \(sc2.2.3.2 if you want the binaries outside the IRAF root.
If the executable files are to reside in the same directory tree as the
rest of IRAF, it is necessary first to remove the \f(CWbin.vax\fP link
just created when the \f(CWAS\fP file was read in, create a new \f(CWbin.vax\fP
subdirectory, enter it and read the BIN contents from tape.
Assuming the vax BIN is file 2 on the 9
track distribution tape and we have just unpacked tar file 1, leaving the
tape positioned to just before file 2, the following commands would suffice
to read the BIN (tape file \f(CWIB.VBSD.VAX\fP) onto disk:
.DS
\f(CW% cd $iraf
% rm bin.vax                    # remove old symbolic link
% mkdir bin.vax                 # create real bin directory
% cd bin.vax
% mt -f /dev/nrmt8 fsf 1        # position tape to IB.VBSD.VAX
% tar -xpf /dev/nrmt8           # read iraf binaries\fP
.DE
Alternatively we could have rewound the tape and done an \f(CWfsf\02\fP to
get to tape file 2.  Now read in the NOAO binaries.
.DS
\f(CW% cd $iraf/noao
% rm bin.vax                    # remove old symbolic link
% mkdir bin.vax                 # create real bin directory
% cd bin.vax
% mt -f /dev/nrmt8 fsf 1        # position tape to NB.VBSD.VAX
% tar -xpf /dev/nrmt8           # read iraf binaries\fP
.DE
.LP
You are now finished reading the tape and may proceed to \(sc2.3 (or 2.4 for
a new IRAF installation).
.NH 4
BIN directories outside the IRAF root.
.PP
Let's assume we have a directory \f(CW/u3\fR with sufficient
space for our 10 Mb vax BIN.  Assuming the vax BIN is file 2 on the 9
track distribution tape and we have just unpacked tar file 1, leaving the
tape positioned to just before file 2, the following commands would suffice
to read the BIN (tape file \f(CWIB.VBSD.VAX\fP) onto disk:
.DS
\f(CW% mkdir /u3/bin.vax
% cd /u3/bin.vax
% mt -f /dev/nrmt8 fsf 1
% tar -xpf /dev/nrmt8\fP
.DE
Alternatively we could have rewound the tape and done an \f(CWfsf\02\fP to
get to tape file 2.  Now read in the NOAO binaries.
.DS
\f(CW% mkdir /u3/noao.bin.vax
% cd /u3/noao.bin.vax
% mt -f /dev/nrmt8 fsf 1
% tar -xpf /dev/nrmt8\fP
.DE
.LP
The next step is to tell IRAF where the new BIN directories are:
.DS
\f(CW% cd $iraf
% rm bin.vax                     # remove old link, if any
% ln -s /u3/bin.vax bin.vax
% cd noao
% rm bin.vax                     # remove old link, if any
% ln -s /u3/noao.bin.vax bin.vax\fP
.DE
.NH 3
Network software distributions
.PP
Although most IRAF installations or updates will be made from a distribution
tape, it is also possible to install IRAF from compressed disk tar files
acquired via FTP from the IRAF network archive.  The procedure followed is
very similar to installing IRAF from a tape, except that the file objects are
stored in the FTP archive rather than on tape, and the content of the
distribution files is slightly different.  Most significantly, the binaries
outside the HSI
are omitted hence if a network installation is attempted it will be necessary
to recompile the full system.
.PP
The main difference between the distribution tape and the network archive is
that the \f(CWAS\fR (all sources) file object is gone, being replaced by the
following files, which one has to manually combine to produce the equivalent
of the \f(CWAS\fP.
.DS
.IP \f(CWHS.VBSD.GEN\fR 20
The host system interface (HSI) for VAX BSD-UNIX, including the HSI binaries.
.IP \f(CWIS.PORT.GEN\fR
The IRAF core system sources (for any system).
.IP \f(CWNS.PORT.GEN\fR
The NOAO package sources (for any system).
.DE
.PP
The \f(CWIS\fP and \f(CWNS\fP (core system and NOAO sources) are portable and
may be combined with the \f(CWHS\fP for any host machine to produce an IRAF
for that host.  These files are stored in the
IRAF network archive in compressed form, hence the actual file names will have
a \f(CW.Z\fP appended and will have to be uncompressed with the UNIX program
\f(CWuncompress\fR before being unpacked with \f(CWtar\fP.
.PP
To build \f(CWVBSD\fP IRAF using the network file objects,
starting from an empty root directory belonging to IRAF, with the compressed
archive files stored in \f(CW/tmp\fP:
.DS
\f(CW% cd $iraf
% uncompress < /tmp/IS.PORT.GEN.Z | tar -xpf -
% uncompress < /tmp/HS.VBSD.GEN.Z | tar -xpf -
% mkdir noao; cd noao
% uncompress < /tmp/NS.PORT.GEN.Z | tar -xpf -\fR
.DE
After running the INSTALL script to configure the programming environment
(see \(sc2.4), one should then configure an empty \f(CWvax\fP BIN
directory and start a SYSGEN.  This should be done from the IRAF account.
The following assumes that the BIN is to be placed in a subdirectory
rather than being a link to a remote directory (see \(sc2.2.3).
.DS
\f(CW% cd $iraf
% mkdir bin.vax
% ln -s bin.vax bin
% mkpkg >& spool\fR
.DE
This would compile all the binaries.  To do the same for the NOAO packages,
one could configure the empty bin and then compile the system as follows.
.DS
\f(CW% cd $iraf/noao
% mkdir bin.vax
% ln -s bin.vax bin
% mkpkg -p noao >& spool\fR
.DE
As we see, the commands are the same except for the root directory and the
additional argument required to tell \f(CWmkpkg\fP the
name of the non-core system package being compiled.  
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
When propagating revisions made to these files, be sure not to replace the
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
to maintain its own custom LOCAL package external to the core IRAF system.
The core system LOCAL is now only a \fBtemplate-local\fR to be copied and
used as the starting point for a custom LOCAL.  The layered software
enhancements, and the procedure for building a custom LOCAL,
are discussed further in the \fIUNIX/IRAF Site Manager's Guide\fR.

.NH 2
Run the INSTALL Script
.PP
Once all of the IRAF files have been restored to disk the IRAF install
script (\f(CWhlib/install\fP) must be run to complete the system installation.
The install script modifies the system as necessary to reflect the new root
directory and new default image storage and local BIN directories,
checks the mode and ownership of a number of files, installs a small set
of IRAF commands in UNIX, and so on.
.LP
To make a trial run of the install script, enter the following commands:
.DS
\f(CW% setenv iraf /\fIpath\fP/iraf/
% cd $iraf/unix/hlib
% source irafuser.csh
% ./install -n\fP
.DE
and answer the questions.  The "\f(CW-n\fP" argument tells \f(CWinstall\fP to
go through the
motions without actually doing anything, so that one can see what will be done
before committing to it.
.PP
Following one or more trial "no execute" ("\f(CW-n\fP") runs, the install script
should be run without the "\f(CW-n\fP" to complete the installation.
This must be
done by the superuser as superuser permission is required to carry out the
necessary additions to UNIX.
.LP
The exchange with the install script will be along the lines of the
following:
.DS
.ps -2
\f(CW% ./install -n
new iraf root directory (/iraf/iraf): /usr/iraf
default root image storage directory (/tmp2/iraf): 
local unix commands directory (/local/bin): /usr/local/bin
install iraf for machine type vax
old iraf root = /iraf/iraf, old imdir = /tmp2/iraf
installing iraf at /usr/iraf, imdir=/tmp2/iraf, lbindir=/usr/local/bin
proceed with installation? (yes):\fP
.ps
.DE
.PP
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
login as \f(CWiraf\fP and startup the CL
(command language) from the iraf account.
.PP
Before starting up IRAF from the iraf account, following the installation of
a new system, it will be necessary to execute the \f(CWmkiraf\fP task.
This will (optionally) initialize the \f(CWuparm\fP directory for the iraf
account, and create a new \f(CWlogin.cl\fP file.
.DS
.ps -2
\f(CW% mkiraf
Initialize uparm? (y|n): 
Terminal types: gterm=ttysw+graphics,vt640=(vt100+retrographics),etc.
Enter terminal type: vt640
A new LOGIN.CL file has been created in the current directory.
You may wish to review and edit this file to change the defaults.
.DE
.LP
The default terminal type option (\f(CWvt640\fP in the example) is very
site dependent and you will probably want to enter a different value
from that shown.  Look in the file \f(CW$iraf/dev/termcap\fP to see what
terminals are supported.  Instructions for interfacing new terminals are
given in the \fISite Manager's Guide\fP.  The \f(CWstty\fP task may be
used to display or change the terminal type after logging into the CL.
A graphics terminal must be specified to be able to run IRAF tasks which
use interactive graphics.
.LP
Once \f(CWmkiraf\fP has been run to initialize the IRAF environment
the CL may be started, e.g.:
.DS
\f(CW% cl                  # \fRstartup IRAF\fP
.DE
.LP
This should startup the CL, which will clear the screen and print out a
startup message.  The standard test procedure included in Volume 1A of the
\fIIRAF User Handbook\fP should be run to verify the installation.

.bp
.SH
Appendix A.  A Complete Example
.PP
Here we present a complete sample IRAF installation for a VAX running Berkeley
UNIX.  This is the simplest possible installation, i.e. it is a new
installation, the BIN directories are located under the IRAF root,
and the archives are read sequentially off a 9 track distribution tape.
.PP
The first step is for the superuser to create an account for the fictitious
user `\f(CWiraf\fP', with home directory \f(CW/usr/iraf/local\fP and shell
\f(CW/bin/csh\fP.  The directory \f(CW/usr/iraf\fP should exist, but that
is all that is needed.  We then login as iraf (a warning message will be printed
since there is no login directory) and proceed as follows:
.sp
.RS
.nf
.ps -1
.vs 8
\f(CW% setenv iraf /usr/iraf/
% cd $iraf
%
% whoami
iraf
%
% mt -f /dev/nrmt8 rew
% cat /dev/nrmt8

0  Table of Contents
1  AS.VBSD.GEN     44.9Mb   IRAF, NOAO packages and VAX/BSD sources
2  IB.VBSD.VAX      9.9Mb   IRAF system binaries for VAX/BSD
3  NB.VBSD.VAX     12.3Mb   NOAO packages binaries for VAX/BSD

%
% tar -xpf /dev/nrmt8           # unpack AS.VBSD.GEN
%
% rm bin.vax                    # remove old symbolic link
% mkdir bin.vax                 # create actual directory
% cd bin.vax
% mt -f /dev/nrmt8 fsf
% tar -xpf /dev/nrmt8           # unpack IB.VBSD.VAX
%
% cd $iraf/noao
% rm bin.vax                    # remove old symbolic link
% mkdir bin.vax                 # create actual directory
% cd bin.vax
% mt -f /dev/nrmt8 fsf
% tar -xpf /dev/nrmt8           # unpack NB.VBSD.VAX
%
% cd $iraf/unix/hlib            # run the INSTALL script
% source irafuser.csh           # pick up environment defs for install
% ./install -n
% su
# ./install
# exit
%
% cd; pwd
/usr/iraf/local
% source .login                 # read .login now that we have one
% rehash                        # pick up new iraf commands
% mkiraf                        # initialize iraf environment
%
% cl                            # verify that the CL runs\fP
.ps
.vs
.ke
.RE
