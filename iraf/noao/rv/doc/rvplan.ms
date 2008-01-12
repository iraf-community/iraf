.OM
.TO
RV folks
.FR
Doug Tody
.SU
RV development plans
.PP
The purpose of this memo is to introduce the RV "level zero specifications"
document enclosed, to explain what that means and what our plans for future
RV package development are, and how the new programming team approach being
used for RV and other projects works.
.SH
Programming Teams
.PP
As was mentioned in our RV meeting a while back, we are experimenting
with the use of small programmer teams for major projects such as RV.
In the case of RV, the programming team is Mike and Frank, with Mike being
the programmer in charge of the project, and Frank the reviewer and critic.
Frank and Mike work together on a daily basis and every few weeks the three
of us have a team meeting to review progress.  Similar teams are being set
up for other projects, e.g., Mike (reviewer-critic) and Lindsey (programmer)
are the team for the new PHOTCAL package.
.PP
For each project we maintain a project directory where all design
specifications, email, etc. for the project are kept.  You are welcome
to browse this if you wish to know more about the details of the project.
In the case of RV, the project directory is /u2/fitz/iraf/rvproject on
tucana.  All of the email exchanged by the team members during the package
design, or mail exchanged with scientists or other users, is saved in the
rvmail file in this directory.  If you have detailed comments on any feature
of a package like RV, it is best to send them to one of the team members via
email so that they get logged for the rest of us to read.
.SH
RV Development Plans
.PP
Based upon your input in the RV review meeting a while back, we have come
up with the following plan for the next phase in the development of the
RV package.  This plan is intended to serve our immediate needs for radial
velocity analysis as quickly as possible, while providing a fully featured
package in the longer term.
.RS
.IP \(bu
\fBLevel Zero Task\fR.  This is intended to provide a basic radial
velocity capability, providing the most needed functions but avoiding
the more ambitious features planned for the eventual full IRAF
package.  It is the specifications for the level zero task which are
enclosed for you to review and comment on.  Once completed, the level
zero task will be frozen and will continue to be available
indefinitely, without change, providing a stable tool for basic radial
velocity analysis while development and testing of the full package
goes forward.  Although the level zero task will provide limited
functionality, those algorithms and features provided are intended to
be about as good as can be done (please let us know if you think
otherwise!) and will have undergone lots of testing with real and
artificial data by the time the software is released.
.IP \(bu
\fBBaseline Package\fR.  This will be more or less what Mike has in the
current prototype version of RV, most likely with many changes reflecting
what we learn from the level zero task.
.IP \(bu
\fBFull Package\fR.  This will be a second version of the baseline release
adding all the desirable but lower priority functions.  Many changes to the
baseline package are sure to be needed once user testing begins, and these
will be incorporated into the full package along with the remaining tasks
not planned for the baseline package.
.RE
.PP
In addition to the above, the current prototype RV package will continue
to be available for use while the new software is under development.
.PP
As you will see from the draft specifications prepared by Mike and
Frank, the level zero task provides fourier cross correlation for both
raw pixel arrays and wavelength calibrated data, including those
features we thought were necessary for even the level zero task for it
to be useful.  These include automatic log-lambda mapping of the input
data, continuum fitting and subtraction, fourier filtering, weighting
of the correlation peak fit to minimize sampling errors, and optional
correction to heliocentric velocities.  Important changes to the user
interface have also taken place.  The database capabilities (RVSELECT)
have been left out, but will be provided in the baseline version of the
package (the lists.fields task can be used to make lists of selected
quantities from the output of the level zero task).  Other important
but less essential functions such as the fourier quotient algorithm,
redshift and deredshift functions, etc., will also be deferred to the
baseline package.
.PP
Comments on our plans for further development of the package, or the
draft specifications for level zero, are most welcome.  If you need
some feature in level zero which doesn't appear to be there, try to let
us know as soon as possible.
