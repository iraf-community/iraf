.OM
.TO
distribution
.FR
Doug Tody
.SU
IRAF package reorganization
.PP
The system was extensively revised over the weekend to implement the new NOAO
package discussed earlier.  For those not familiar with the issue, this was
necessary for the upcoming STScI/SDAS release.  The NOAO optical astronomy
software is now nicely packaged in a single directory tree like SDAS and any
similar packages added in the future.  There is a clean interface to the
IRAF system software, making it easier for outside sites to add new packages.
Having the NOAO science software in a single directory tree will make it
easier to install future updates of the science software without affecting
the system software.
.PP
Due to the extensive nature of the revisions required (325 files were edited!),
the reorganization was carried out in a temporary copy
of the system located at \fL/tmp2/u/\fR on lyra.  It is possible that a bug
or two may have crept into the system in the process of editing that many
files, so some testing is advisable before we install the system at \fL/iraf\fR.
In particular, make sure the \fL.keys\fR cursor mode files are accessible,
the runtime linelist files, and any other runtime files.
Some existing packages were extensively modified (the old \fBdataio\fR,
\fButilities\fR, and \fBlocal\fR, as discussed below) and should therefore
get special attention.
.PP
Recall that it is necessary to add a \fL.cshrc\fR file to your UNIX login
directory when working with an experimental version of the system installed
at a nonstandard root directory.  This is the only time you need such a file.
I suggest making a copy of the file \fL/u2/tody/.cshrc\fR (this is just
hlib$irafuser.csh with the new root declared).  The \fBmkiraf\fR task should
be run before logging in under the new system, since there were some login.cl
changes in the new system.
.PP
The new directory structure concentrates all source and runtime files used
by the new NOAO package tree into a new directory tree \fLiraf$noao\fR.
The NOAO directories include a local library which is used for all runtime
files (cursor mode and linelists).  The old \fLlib$pkg\fR and \fLlib$scr\fR
directories still exist but all the NOAO files therein were moved to the
new NOAO library.  Note that the XTOOLS library still uses \fLlib$pkg\fR
since XTOOLS is an IRAF library (it is used by \fBimages\fR, etc.).
At some point in the future we may add a similar library for use only by
the NOAO package, but probably not until XTOOLS has been cleaned up.
.LP
The new CLPACKAGE (root) menu is as follows:
.DS
\fLdataio      images      lists       noao        sdas        system
dbms        language    local       plot        softools    utilities\fR
.DE
The NOAO menu is as follows:
.DS
\fLartdata     astutil     focas       mtntape     proto       twodspec
astrometry  digiphot    imred       onedspec    surfphot\fR
.DE
.LP
Most of these packages were unaffected by the modifications, except for
changes to the manual pages, help directory files, package script tasks,
and an occasional source file to reflect the new pathname (noao$imred etc.).
All manual pages were updated to show the package pathname in the manual
page header.  This has been our policy for several months but many old
manual pages were out of conformance; please try to remember to do this
in the future (it helps users determine what packages to load to run a task).
All the "revisions" entries in the \fL.men\fR package menu files were deleted
since this is feature will soon go away, and we will be reprinting the
documentation soon and I want it to be as accurate as possible.  In any event,
a system wide facility should not be documented in the user interface of
every package.
.PP
Three new packages were added and three old packages were extensively
revised.  The NOAO mountain tape readers were moved from the \fBdataio\fR
package into the new \fBmtntape\fR package.  The astronomically oriented
utility tasks were moved from the \fButilities\fR package to the new
\fBastutil\fR package.  The old \fBlocal\fR package was renamed \fBproto\fR,
and a new \fBlocal\fR package was added in the directory \fLiraf$local/tasks\fR.
.PP
The concept of the new \fBproto\fR package is appropriate for what the old
\fBlocal\fR package was used for, i.e., prototype, temporary, or contributed
tasks which are part of the NOAO package and which are exported with the
system, but which are expected to eventually disappear or be replaced by
planned system facilities.  The new \fBlocal\fR package is a place to put
tasks of strictly local interest, or tasks which are not portable, e.g.,
foreign tasks and the Peritec package.  The \fBlocal\fR package should be
particularly useful for outside sites as it gives them a place to put locally
added tasks which will not be affected by future updates of the system.
Also, the framework (\fLmkpkg, local.cl\fR, etc.) is all set up, making it
easier for outside sites to add their own software without having to figure
out how to set up an IRAF package.
.PP
All code in the PKG and NOAO directories was newly compiled and linked over
the weekend.  I would like to install the new system at /iraf by the middle
of the week (before I make the IMIO, etc. modifications), so lets try to
get the basic testing done Monday.  Despite the number of files edited,
very little actual code needed to be modified by this revision, hence only
minor problems should be encountered.
.CT
IRAF group
E. Anderson
J. Barnes

D. Phillips (CTIO)
S. Ridgway (info)
S. Grandi (info)
P. Seitzer (info)
G. Jacoby (info)
