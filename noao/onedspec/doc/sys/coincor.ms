.EQ
delim $$
.EN
.OM
.TO
IIDS Users
.FR
F. Valdes
.SU
IIDS count rate corrections
.PP
The IRAF task \fBcoincor\fR transforms the observed count rates to
something proportional to the input count rate.  The correction applied
to the observed count rates depends upon the count rate and is instrument
dependent.  One correction common to photomultiplier detectors and the
IIDS is for coincident events, which is the origin of the task name.  
The parameter \fIccmode\fR selects a particular type of correction.
The value \fIccmode\fR = "iids" applies the following transformation to
observed IIDS count rates.

.EQ (1)
	C sup ' ~=~(- ln (1- deadtime C)/ deadtime ) sup power
.EN

where $C$ is the orginal count rate, $C sup '$ is the corrected count
rate, and $deadtime$ and $power$ are \fBcoincor\fR parameters.  The term
inside the parenthesis is the correction for dead-time in the counting
of coincident events on the back phospher of the image tube.  The power
law correction is due to the non-linearity of the IIDS image tube chain.
.PP
The correction applied with the Mountain Reduction Code is only for
coincidences, i.e. equation (1) with $power = 1$.  To obtain just this
correction with \fBcoincor\fR set $power = 1$.  To take mountain reduced
data and correct only for the non-linearity set \fIccmode\fR = "power".
With raw IIDS data use \fBcoincor\fR with the default
parameters.

.LP
References:
.IP (1)
L. Goad, \fBSPIE 172\fR, 1979, p. 86.
.IP (2)
G. Jacoby, Some Notes on the ONEDSPEC Package, \fBIRAF Handbook\fR
.IP (3)
P. Massey and J. De Veny, How Linear is the IIDS, \fBNOAO Newsletter\fR,
#6, June 1986.
