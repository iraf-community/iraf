.EQ
delim	$$
.EN
.OM
.TO
IRAF ONEDSPEC Users
.FR
Frank Valdes
.SU
SENSFUNC Corrections
.LP
This memorandum describes the meaning of the corrections
computed by the \fBonedspec\fR task \fBsensfunc\fR.
The basic equation is

.EQ (1)
I( lambda )~=~I sub obs ( lambda )~10 sup {0.4~(s( lambda )~+
~A~e( lambda )~+~roman {fudge~terms})}
.EN

where $I sub obs$ is the observed spectrum corrected to counts per second,
$I$ is the flux calibrated spectrum, $s( lambda )$ is the sensitivity
correction needed to produce
flux calibrated intensities, $A$ is the air mass at the time of the
observation, $e( lambda )$ is a standard extinction function, and,
finally, additional terms appropriately called \fIfudge\fR terms.  Expressed
as a magnitude correction this equation is

.EQ (2)
DELTA m( lambda )~=s( lambda )~+~A~e( lambda )~+~roman {fudge~terms}
.EN

In \fBsensfunc\fR the standard extinction function is applied so that ideally
the $DELTA m$ curves (defining the sensitivity function) obtained from
observations of different stars and at different air masses are identical.
However, at times this is not the case because the observations were taken
through non-constant or nonstandard extinction.

There are two types of fudge terms used in \fBsensfunc\fR, called \fIfudge\fR
and \fIgrey\fR.  The \fIfudge\fR correction is a separate constant,
independent of wavelength or air mass, applied to each observation to shift
the sensitivity curves to the same level on average.  This is done to
determine the shape of the sensitivity curve only.
The fudge correction for each observation is obtained by determining
the average magnitude shift over all wavelenths relative to the observation
with the smallest sensitivity correction.  A composite sensitivity curve
is then determined from the average of all the fudged observations.
The fudge terms are not incorporated in the sensitivity or extinction
corrections applied to calibrate the spectra.  Thus, after applying the
sensitivity and extinction corrections to the standard star spectra there
will be absolute flux scale errors due to the observing conditions.

If the observer believes that there is an actual calibratible error in
the standard extinction then \fBsensfunc\fR can be used to determine a
correction which is a linear function of the air mass.  This is done by
relating the fudge values (the magnitude shifts needed to bring observations
to the same sensitivity level) to the air mass of the observations.
The \fIgrey\fR term is obtained by the least square fit to 

.EQ (3)
f sub i~=~G~DELTA A sub i~=~G~A sub i~+~C
.EN

where the $f sub i$ are the fudge values relative to the observation with
the smallest sensitivity correction and the $DELTA A sub i$ are the
air mass differences relative to this same observation.  The slope constant
$G$ is what is refered to as the \fIgrey\fR term.  The constant term,
related to the air mass of the reference observation to which the other
spectra are shifted, is absorbed in the sensitivity function.
The modified equation (2) is

.EQ (4)
DELTA m( lambda )~=~s ( lambda ) + A~(e( lambda )~+~G)
.EN

It is important to realize that equation (3) can lead to erroneous results
if there is no real relation to the air mass or the air mass range is
too small.  In other words applying the grey term correction will produce
some number for $G$ but it may be worse than no correction.  A plot of
the individual fudge constants, $f sub i$, and the air mass or
air mass differences would be useful to evaluate the validity of the
grey correction.  The actual magnitude of the correction is not $G$
but $DELTA A~G$ where $DELTA A$ is the range of observed air mass.
