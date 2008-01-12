.nr PS 9
.nr VS 11
.RP
.ND
.TL
IMRED Package Revisions Summary: IRAF Version 2.10
.AU
Francisco Valdes
.AI
IRAF Group - Central Computer Services
.K2
P.O. Box 26732, Tucson, Arizona 85726
September 1990
.AB
This paper summarizes the changes in Version 2 of the IRAF \fBimred\fR
package which is part of IRAF Version 2.10.  The major changes are:

.IP \(bu
New multifiber reduction packages \fBargus\fR, \fBnessie\fR, and
\fBkpcoude.fiber\fR.
.IP \(bu
New spectrophotmetric slit spectra reduction packages \fBgoldcam\fR,
\fBspecred\fR, and \fBkpcoude.slit\fR.
.IP \(bu
New versions of the \fBmsred\fR and \fBechelle\fR packages based on
the new versions of \fBapextract\fR and \fBonedspec\fR.
.AE
.NH
Introduction
.PP
A number of new specialized image reduction packages and new versions
of the generic echelle and multiobject spectroscopy packages have been
added to the \fBimred\fR package in IRAF Version 2.10.  The new
subpackages will be made available as external packages prior to
the release of V2.10.  The major changes are:

.IP \(bu
New multifiber reduction packages \fBargus\fR, \fBnessie\fR, and
\fBkpcoude.fiber\fR.
.IP \(bu
New spectrophotmetric slit spectra reduction packages \fBgoldcam\fR,
\fBspecred\fR, and \fBkpcoude.slit\fR.
.IP \(bu
New versions of the \fBmsred\fR and \fBechelle\fR packages based on
the new versions of \fBapextract\fR and \fBonedspec\fR.

.LP
In additions there have been some minor changes in the other
spectroscopy packages required by changes in the \fBonedspec\fR package.
.PP
The new packages are specialized to specific instruments or types of
data.  They contain tasks collected from the various general spectroscopy
packages which are appropriate for a particular type of data.
However, the most important contribution of these packages are
special reduction tasks which are streamlined to perform the complete
calibration and reduction of the data in as simple and automated
manner as possible.  The tasks combine operations from both two
dimensional extraction and one dimensional spectral calibrations
and collects all the useful parameters in two parameter sets while
fixing and hiding parameters which are irrelevent.
.PP
The new packages are as follows.  The \fBargus\fR package is for the
flat fielding, throughput correction, extraction, dispersion correction,
and sky correction of data from the CTIO \fIArgus\fR multifiber instrument.
The \fBnessie\fR package is similar and is for the KPNO \fINessie\fR
multifiber plugboard instrument.  The \fBkpcoude.fiber\fR package is
specialized for the three fiber (two arc and one object) instrument
at the KPNO Coude.  It is similar to the other multifiber packages
except there is no sky subtraction.
.PP
The other three packages are for sky subtracted extraction,
dispersion correction, extinction correction, and flux calibration
of slit instruments.  The packages are for the KPNO \fIGoldcam\fR,
the KPNO Coude, and for the CTIO \fI2DFRUTTI\fR.  They are all
fairly general and could be used for other instruments.  They are
distinguished by choices of default parameters.
.PP
There are user's guides for the powerful new reduction tasks in
the new packages.  These are available both as nicely typeset
documents and as on-line IRAF manual pages.
.PP
Tasks from the revised \fBapextract\fR and \fBonedspec\fR packages
appear in many of the \fBimred\fR packages.  In particular the
\fBechelle\fR and \fBmsred\fR packages are now based on this new
software.
.PP
Some minor changes are the replacement of the \fBspecphot\fR package
by \fBspecred\fR and the renaming and reorganization of the
\fBcoude\fR package.
