.de FX
.nf
.ps -2
.ss 25
.cs R 25
..
.de EX
.ps +2
.ss
.cs R
.fi
..
.EQ
delim $$
.EN
.RP
.TL
Algorithms for the Multi-Spectra Extraction Package
.AU
Francisco Valdes
.K2
.TU
.AB
The algorithms for the Multi-Spectra Extraction Package (\fBmultispec\fR)
in the Image Reduction and Analysis Facility (\fBIRAF\fR) is described.
The basic aspects of the general two dimensional aperture spectra extraction
problem are first discussed.
The specific algorithms for extraction of multi-aperture plate and
Echelle digital data are presented.  Results of the authors experiments
with this type of data are included.
The detailed specification of the package is given in a second document,
\fIDetailed Specifications for the Multi-Spectra Extraction Package\fB.
.AE
.NH
Introduction
.PP
There are an increasing number of astronomical instruments which produce
multiple spectra on a two dimensional detector.
The basic concept is to use one dimension for wavelength,
the dispersion dimension, and the other, the cross dimension, for
packing additional information during a single exposure.
For example, the cross dimension can be different objects or
different spectral orders.  The classic multi-spectra instrument is
the Echelle spectrograph.  New instruments are the aperture plate and
Medusa spectrographs.
.PP
There is an additional aspect of the multi-spectra format; namely,
the individual spectra can contain spatial data.  An example of
this would be multiple slit spectra in which each slit spectra contains
sky signal and object signal.  In the following
discussion we limit the spectra to be simple aperture spectra in
which we only desire to sum the intensities at each wavelength to form
a one dimensional spectrum.
.PP
The analysis of multi-spectra aperture data consists of two steps; the
separation and extraction into individual aperture spectra
and the calibration and measurement of the spectra.  These steps can
either be incorporated into one analysis package or two separate
packages.  There are advantages to the first approach since some
aspects of the individual spectra are directly related by the physical
geometry of the multi-spectra format.  However, because packages for
the analysis of individual spectra exist we begin by dividing the
reduction into separate extraction and analysis tasks.  It is
important to realize, however, that the existing analysis tools are not well
suited to reducing the larger number of spectra and treating sets of
spectra together.
.PP
The latter part of this paper describes the algorithms for the
extraction of two types of data; the multi-aperture plate (MAP)
and Echelle used with digital detectors.  However,
it is important to keep the more general problem in mind
and the remainder of this introduction considers the different
conceptual aspects of the multi-spectra extraction task.
Table 1 lists many of the general properties of multi-spectra aperture data.
The other two columns give possible alternatives that each property may take.

.TS
center box;
c s s
c s s
c c s
= = =
c || c | c.
Table 1:  Aspects of Multi-Spectral Data

Property	Alternatives
detector	digital	photographic
alignment	aligned	skewed
blending	blended	unblended
aperture	holes	slits
spectral resolution	low	high
.TE

.PP
The detector determines what kind of calibration procedures are
required to produce intensity from the measurements.
A digital detector requires sensitivity calibrations on all scales.
This is the "flat field" problem.  There are also corrections for
bias and dark current.  Photographic detectors require
intensity calibration.  Data which are not aligned with the natural
dimensions of the digital image require extra procedures.  Two types
of non-alignment are a rotation of the dispersion dimension relative
to the pixel dimension and a "wiggling" or "snaking" of the dispersion
dimension.  Blending refers to the degree of contamination along the
cross dimension.  Blended data requires extra effort to correct for
the overlap between different spectra and to determine the background.
The aperture defines the extent of the spectra in the cross dimension.
The two most relevant choices are holes and slits.  In some
instruments, like the Echelle, the size of the aperture can be varied
at the time of the observations.  Finally, the spectral resolution is
important in conjunction with digital detectors.  If the resolution is
high then quartz flat field calibrations are relatively easy because
the spectral
signature need not be considered.  Otherwise, the flat field problem
is more difficult because the gain variations of the detector
must be separated from the natural spectral intensity variation of the
quartz.
.PP
There is always some confusion of terms when talking about multi-spectra
data; in particular, the terms x, y, line, and band.
The image pixel dimensions are refered to as x and y.  We assume
for the moment that the alignment of the multi-spectra format is such
that x corresponds to the cross dimension and y to the dispersion
dimension.  If this is not the case a rotation or interpolation
program can be used to approximately orient the data in this way.
A line is the set of intensity values as a function of x at constant y.
In other words, a line is a cut across the dispersion dimension.
A band is the average of more than one line.
The image residing on disk will generally be organized
such that x varies more rapidly and a line of the image is easily
obtained.  In this form a display of the image will have the spectra
running vertically.  In the Cyber extraction package the data is
organized with x corresponding to the dispersion dimension.
.NH
Multi-Spectra Image Formats
.PP
The remainder of this paper will refer to two specfic and very
different multi-spectra formats; the Kitt Peak Multi-Aperture Plate
System and the Kitt Peak Echelle Spectrograph.
.NH 2
Kitt Peak Multi-Aperture Plate System
.PP
The reduction of data from multi-aperture plate observations is the 
driving force for the development of a multi-spectra extraction
package.  This application turns out to have most of the worse aspects
of the properties listed in Table 1.  The multi-aperture plate spectrograph uses
digital dectectors with low resolution, the spectra are blended and
change alignment along the pixel dimension.  Furthermore, the camera
has a variable point-spread function and focus,
suffers from flexture problems, has a different illumination for
the quartz than object exposures, and unexplained background level
variations (in the CRYOCAM).  There are two detectors which have been
used with the multi-aperture plate system, the Cryogenic Camera
(CRYOCAM) and the High Gain Video Spectrometer (HGVS).
.NH 2
Echelle
.PP
As some of the algorithms were developed the Echelle data was brought
to my attention.  It is considerably simpler than the MAP data because
it is unblended and of high spectral resolution.
Furthermore, the quartz exposure
can be made wider than the object exposures and flexture is not a
problem.  The principle problem in this data was the
prevalence of cosmic rays.  It pointed to the need to maintain generality
in dealing with both the MAP data and other types of
multi-spectra data which have different profiles, may or may not be
merged, and may or may not have different widths in quartz and object.
Dealing with the cosmic ray problem lead to a very effective solution
usable in both the Echelle and multi-aperture plate data.
.NH
User Level Extraction Logic
.PP
The user should generally only be concerned with the logical steps of
extracting the individual spectra from the multi-spectra image.  This
means that apart from specifying the detector system and the format
he should not deal with details of the detector and the format.
In the paper, 
\fIDetailed Specifications for the Multi-Spectra Extraction Package\fB,
the \fBIRAF\fR extraction package design and program specifications
are described.
.NH
Flat Fields
.PP
There are two types of flat field situations depending on the spectral
resolution.  When the resolution is high then the spectral signature of
the continum calibration source, a quartz exposure, will be unimportant
and variations in the signal will be due to detector sensitivity variations.
In this case the quartz frame, or average of several frames, is the flat
field and division of the object frames by the quartz frame is all that
is required.  However, a special
image division program is desirable to handle the region of low or absent
signal between the the spectra.  This is described in section 4.2.
.PP
In the alternate case of lower resolution the quartz spectral signature is
larger than the detector response variations.  A flat
field in which the intrinsic quartz spectrum is removed is produced by
assuming that the true value of a pixel is given by the smoothed average
of the pixels near that point in position and wavelength and taking
the ratio of the data value to the smoothed value.
This requires a special smoothing program described in section 4.1.
After the flat field is generated then the same image division
program used for the Echelle data is applied.
The image division and smoothing programs are general image operators and
not specific to the Multi-Spectra Extraction Package.
.NH 2
MULTISPEC_FLAT
.PP
The multi-aperture plate data varies in both dimensions.  Thus, any averaging
to smooth the image must take this variation into account.  In the Cyber
a flat field for the multi-aperture plate data smooths across the dispersion
by modeling the spectra.  This is a difficult task to do accurately because
the true shape of the spectra is not known and the counts vary greatly
and rapidly in this dimension.  This approach has the further difficulty
that it is not possible to average several quartz exposures directly.
.PP
The alternate approach to modeling is statistical averaging.
Averaging across the dispersion requires very high order polynomials
because of the rapid variations;
the spectra are typically spaced about 8 pixels apart and there are on
the order of 50 spectra.  On the other hand, variations along the dispersion
are much slower even if the spectra are slightly skewed; a bad case would
have two peaks in 800 pixels along the y dimension.  This kind
of variation is tractable with relatively simple averaging polynomials
and is the one adopted for the multi-aperture plate data.
.PP
The flat fields are produced by a quadratic moving average along the
y direction.  This means that the region centered at a given pixel
is fitted by a least-squares quadratic polynomial and the value of the
polynomial at that point is the appropriate statistical average.
The width of the moving average is an adjustable parameter.
At the edges of the frame where it is not possible to center a region of
the specified width about the desired pixel the polynomial fit is used to
extrapolate the average value to the edge.
.PP
Because the quadratic fit will
be influenced by bad pixels an attempt is made to detect and smooth over
the bad pixels.  This is accomplished by comparing the smoothed values to
the observed values and ignoring pixels with a value of

.EQ (1)
	chi = | observed - smoothed | / sqrt smoothed
.EN

greater than a specified value.  Then the smoothing is recalculated and tested
again for bad pixels.  This iteration continues until no further pixels
are rejected.
.PP
Following the smoothing the flat field is produced by ratioing the raw quartz
to the smoothed quartz.  Pixels of low signal (specified by the
parameter \fIconstant\fR )
are treated by the equation

.EQ
	r = (data + (constant - smoothed) ) / constant  .
.EN

The resultant flat field image is then divided into the object frames in
the manner described in the next section.
.PP
Experience with data from the Cryogenic Camera has proved very good.
The flat field which is produced can be examined on a display.  It
shows fringing at red wavelengths and is not too strongly affected
by bad pixels.  Some further effort, however, could go into smoothing
over the bad pixels.
.PP
The smoothing operation on data from the Cryogenic Camera actually
consists of four steps.  The quartz exposures are first averaged.
The average quartz is rotated so that the dispersion
direction is the most rapidly varying or x dimension.  Then the
smoothing is performed along x followed by another rotation to return
the flat field image to its original orientation.  The reason for the
rotations is that they can be done quickly and efficiently whereas
smoothing along the y dimension is very slow and coding an efficient
version is much more complicated than doing a single line at a time.
.NH 2
FLAT_DIVIDE
.PP
The Echelle data has quartz frames which can be used directly as flat fields.
One just has to divide the object frames by the quartz or average of several
quartz.  However, in image division consideration has to be given the
problem of division by zero or very small numbers.  In direct imaging this
may not be much of a problem but in multi-spectra data the region between
the spectra and near the edges of the spectra will have very low counts.
Another aspect of image division for making flat field corrections is the
scaling of the result.  The flat field integer image data must be large
to give accurate relative response values.  However, one wants to divide
an object frame by values near unity.
This section describes a special image division operator allowing the user
to specify how to handle these cases.
.PP
The parameters are a \fIdivision threshold\fR
(default of zero) and a \fIthreshold violation value\fR.  Values of the
denominator above the \fIthreshold\fR are treated separatedly from those
below the \fIthreshold\fR.  The denominator image is scaled to have an
average of one for pixels above the \fIthreshold\fR.  The pixel by pixel
division is then performed for those points for which the denominator
is above the \fIthreshold\fR.  Pixels for which the denominator is below the
\fIthreshold\fR are set to the \fIthreshold violation value\fR in the resultant
image if the \fIviolation value\fR is specified.  If the value is not
specified then the numerator value is taken as the resultant value.
The divisions can be done in place or the result put into a new image file.
.PP
For the multi-spectra situation where the object spectra have a
smaller width than the quartz, as in the Echelle, one can either
set the \fIthreshold
violation value\fR to zero or not set it at all resulting in either
exactly zero or background values between the spectra while still flattening
the spectra.  This allows looking at the flattened spectra without the
annoying "grass" between the spectra caused by dividing by small
values.
.NH
Extraction
.NH 2
MULTIAP_EXTRACT
.PP
The extraction of spectra from multi-aperture plate images consists of
a series of steps.  The steps are executed from a script.
The command

.FX
ms> multiap_extract "ap165.*", "", 165, 50
.EX

will take the flattened images, ap165.*, from aperture plate 165 with 50
spectra and automatically locate the spectra, model the profiles, and
extract the one dimensional spectra.  The script consists of
a number of steps as described below.
.PP
\fBFind_spectra\fR (section 6) initializes the \fBmultispec\fR data file
and does a peak search to determine the initial positions of the
spectra.
\fBFind_bckgrnd\fR fits a polynomial of order 1 (or more) for the pixels which
are not near the spectra as defined by \fBfind_spectra\fR.
.PP
The spectra are then modeled in bands of 32 lines by the model profiles
described in section 8.1.  The first \fBmodel_fit\fR uses three Gaussian
parameters for
each spectra measuring the peak intensity, peak position, and width.
The second \fBmodel_fit\fR adds a fourth parameter to modify the wings of the
profile.
.PP
The \fBmodel_extract\fR program extracts the spectra line by line and also
detects and removes cosmic ray events which do not fit the model
profiles (see section 9).
In outline, the extraction of blended spectral data uses the
model profiles to determine the fraction of light
from each of the neighboring spectra at the pixel in question.  The
appropriate fraction of the
.ul
observed
pixel intensity (minus the background) is
assigned to the luminosities of the spectra.  There are two versions
of the \fBmodel_extract\fR extraction.  The first simultaneously fits the
peak intensity of all the spectra and the second uses the
data value at the peak of each spectra to normalize the model.  The
first method is slow and accurate and the second is fast and approximate.  
Because the models are used in extraction only to define the relative
contributions of neighboring spectra to the total observed pixel luminosity
the speed of the approximate method far outweighs the need for
accuracy.  However, cleaning the spectra of cosmic rays is a different
matter and is discussed further in section 12.
.PP
After the extraction the spectra are correlated with the aperture plate
description using \fBap_plate\fR (see Section 10) to determine the
relative wavelength offsets and assign identification information to
the spectra.
.PP
For successive frames it is not necessary to resort to the initial
steps of finding the spectra and fitting from scratch.  The \fBcopy_params\fR
routine makes a new copy of the fitting database.  Small shifts in positions
of the spectra and the peak intensities are determined by doing a two
parameter fit for the peak and position using the previously determined
shape parameters.
Changes in the shape of the spectra are then determined by the three
and four parmater fits.  Because the solution is likely to be close to
the previously determined shape the transfering of one solution from a
previously solved image is faster than starting from scratch.
Note that the shapes as well as the positions and peak intensities
of all frames including the object exposures are allowed to change.
.PP
The spectra are then extracted from the image by \fBmodel_extract\fR and the
process repeats for the succeeding images.
.PP
One useful feature is the ability to specify the bands or lines to be
modeled or extracted.
This feature is useful for diagnosising the programs quickly.
The default is all bands or lines.
.NH 2
ECHELLE_EXTRACT
.PP
The extraction of the unblended Echelle spectra is performed
begins in a similar way with \fBfind_spectra\fR and \fBfind_bckgrnd\fR.
The extraction and cleaning, however, uses \fBstrip_extract\fR which
adds up the instrumental counts for each unblended spectra at each
wavelength to get the total luminosity.
.NH
FIND_SPECTRA -- Finding the Spectra
.PP
The first step in the extraction and processing of multi-spectra data is
to locate the spectra.  This can be done interactively by
the user but it is far preferable to automate the process;
particularly since multi-spectra data can have a large number of
spectra and frames.  The approach is to find the peaks in a line, or
average of lines, sort the peaks found in some manner, such as by
strength, and select the expected number of peaks from the top of the
list.
Beyond this simple outline are several algorithmic details such as how
to define and locate valid peaks and how to sort the list of peaks.
Peak finding is a general problem and a subroutine for peak finding is
described below.  The \fBfind_spectra\fR program provides an
interface between the \fBmultispec\fR data file and the
general peak finding algorithm.
.PP
The \fBpeaks\fR function takes arrays of x (position) and y (value) points
and the number of
points in the arrays and returns the number of peaks found.  It also
returns the estimated positions of the peaks in the x array and the
extimated peak values in the y array in order of peak likelihood.
There is one user parameter, the smoothing \fIwidth\fR.
The choice of the \fIwidth\fR parameter is dicatated by how closely and how
wide the peaks are to be expected.
The algorithm takes a region of \fIwidth\fR points
centered on each x point and fits a quadratic;

.EQ
y sub fit = a + b x + c x sup 2~.
.EN

A peak is defined
when the slopes, $b sub 1$ and $b sub 2$, of two neighboring points
$x sub 1$ and $x sub 2$ change
sign from positive to negative and the curvatures, $c sub 1$ and $c
sub 2$, are less than -0.001 for both points.
The quadratic polynomials define two estimated peak positions

.EQ
x sub 1 sub peak = x sub 1 - b sub 1 / (2 * c sub 1 ),~~
x sub 2 sub peak = x sub 2 - b sub 2 / (2 * c sub 2 )~.
.EN

The offsets are then normalized to give a linear interpolation
fraction
$f = ( x sub 1 sub peak - x sub 1 ) / ( x sub 2 sub peak - x sub 1 sub
peak )$ in the interval between the two points.
The estimated position of the peak is then

.EQ
x sub peak = f * ( x sub 1 - x sub 2 )
.EN

and the estimated peak value is the average value of the two quadratic
polynomials at $x sub peak$.  The curvature at the peak is
estimated by $c sub peak = c sub 1 + f * (c sub 1 - c sub 2 )$.
Finally, the peaks are sorted by the magnitude of the peak curvature.
.PP
This peak finding algorithm works quite well.  I have also used it to
automatically locate peaks in the extracted one dimensional spectra
and then do peak correlations between spectra to find a relative
wavelength solution.  Some such use of this program may be implemented
in either future additions to the Multi-Spectra Extraction Package or
the Spectral Reduction Package.
.PP
In \fBfind_spectra\fR the number of spectra to be found is specified by
the user.  The user should have previously looked at an image
on a display or done a profile plot across the
dispersion to count the observed spectra.
Additional parameters specify the columns in which the spectra
are to be found and the minimum separation and width of the spectra.
The column specification allows the elimination of problems with defective
areas of the detector such as the LED in the Cryogenic Camera.  The minimum
width and separation provide algorithmic constraints to the spectra finding
procedure.
.PP
The peaks are found at two or more points in the
multi-spectra image for a band of 32 lines using a
\fBpeaks\fR \fIwidth\fR parameter of 5.  After the peaks are found
at a number of bands in the image a linear fit is made to determine any small
slope of the spectra relative to the columns.
The reason for specifying only a few bands is that the process of
finding the peaks is moderately slow and only two bands are needed for
determining the initial position angle of the spectra to the y
dimension.  Furthermore, some bands do not give a satisfactory result
because of extraneous data such as the LED in the CRYOCAM or bad
focus.  Another possibility is that a spectrum may go off the edge
and in order to "find" the spectrum for partial extraction bands that
include the on image part of the spectrum must be specified.
.NH
FIND_BCKGRND -- Background
.PP
The background on a multi-spectra image is the result of very broad
scattering as opposed to the narrower scattering which produces
distinguishable wings on individual spectra.
Modeling of the background in a Cryogenic Camera multi-aperture plate
image shows that the background is well explained by a broad
scattering function.
It is not reasonable, however, to model the scattering to this detail
in actual extractions.
Instead a smooth polynomial is fitted to the pixels not covered by
spectra.  The order of the polynomial is a specified parameter.
For the CRYOCAM MAP data a quadratic is appropriate.
.PP
The algorithm is the same for all multi-spectra data except for the
choice of parameters.  First, the location of the spectra must be
determined.  This is done by the \fBfind_spectra\fR program.  There
are two principle parameters, a buffer distance and the order of the
fitting polynomial.  Each line, or average of several lines, is fitted
by least-squares for the points lying farther than the buffer
distance from any spectra.  If there are no points which completely
stradle the spectra, i.e. located on each side of the spectra, then
the order of the fitting polynomial is ignored and a constant, or
first order polynomial, is determined.
A hidden parameter specifying the columns allowed for searching for
background points is available so that bad parts of the image can be
ignored.
.PP
A difference in philosophy with the Cyber programs is that the
determined background is not subtracted from the image data.  It is
instead kept in the database for the image.  Generally, it is better to
modify the basic image data as little as possible.  This is the approach
used in the Multi-Spectra Extraction Package.
.NH
Spectra Profiles
.NH 2
MODEL_FIT -- Models for Multi-Spectra Images
.PP
The object of modeling is to separate blended spectra for extraction
and to remove artifacts, such as cosmic rays, which do not fit
the model.  The models should have the minimum number of parameters
which give residuals approaching the detector statistics, they
should incorporate constraints  based on the physics of the
detector/camera system,  and the models must be ammenable to a
statistical fitting algorithm which is stable.
There are a large number of possibilities.
.PP
An important point to bear in mind during the following discussion is
the necessary accuracy of the model fitting.  In the design proposed
here the model fitting is not used for determining the smooth quartz.
Use of a model for making a flat field would require a very accurate
model and using an average quartz is not possible.  However, for
extraction the model is used only to indicate the
relative fraction of light for each spectrum when the spectra are
blended.  The cleaning application is more critical but not nearly so
much as in the flat field modeling.  Thus, though we do a good job of
model fitting (better the the Cyber modeling) some additional features
such as smoothing along the spectra are not included.
Also, though some improvement can be gained by the additional shape
parameters in the fit, they are not necessary for the required purpose
and can be left out leading to a faster extraction.
.PP
During the course of my investigation I tried more than one hundred
models and combinations of constraints.  Some general results of this
study follow.
The model which I find gives the best results has six parameters not
counting the background.  The model is defined by the following
equations where x is the cross dimension.

.EQ (1)
I = I sub 0 exp (-s * ( DELTA x sup 2 ))
.EN
.EQ
DELTA x = (x - x sub 0 )
.EN
.EQ
s = s sub 0 + s sub 1 y sup 2 + s sub 2 y sup 3
.EN
.EQ
y = DELTA x / sqrt { DELTA x sup 2 + x sub 1 sup 2 }
.EN

The model consists of a intensity scale parameter, $I sub 0$,
and a profile which is
written in a Gaussian form.  The center of the profile is given by
the parameter $x sub 0$.  The profile is not exactly Gaussian because the
scale, $s$, is not a constant but depends on $DELTA x$.  The scale
function has three terms; a constant term, $s sub 0$, which is the scale
near the center of the profile, and even and odd terms, $s sub 1$
and $s sub 2$,
which change the scale in the wings of the profile.
.PP
The characteristic of the profile which must be satisfied is that at
large distances from the profile center the scale is positive.  This
requirement means that the profile will be monotonically decreasing at
large distances and will have a finite luminosity.  This point was
crucial in determining the form of the scale function.  A straight
power series in $DELTA x$ does not work because power series diverge.
Instead, the scale function is defined in terms of a separation
variable $y$ which is bounded by -1 and 1 at infinite separation and is
zero at zero separation.  The parameter $x sub 1$ defines a characteristic
distance where the character of $y$ changes from going as $DELTA x$ to
asymptotic to 1.  The parameters are, thus, $I sub 0$, $x sub 0$, $s sub 0$,
$s sub 1$, $s sub 2$, $x sub 1$.
.PP
An important property of this model is that the terms have a physical
interpretation.  The profile scale and profile center are obvious and
any model must include them.  It is the remaining terms, $s sub 0$,
$s sub 1$, $s sub 2$,
and $x sub 1$, which are called the shape parameters, which are interesting.
In an ideal aperture plate system the shape of a profile would be
given by the projection of the circular aperture into the cross dimension:

.EQ
P( DELTA x ) = sqrt {1 - a DELTA x sup 2}
.EN

where the constant a is related to the size of the hole by 

.EQ
a = 1 / r sup 2
.EN

For small $DELTA x$ the profile can be expressed in the Gaussian form with
a scale

.EQ
s = a( 1/2  + a DELTA x sup 2 + ...)
.EN

Thus, even in a perfect aperture plate system a Gaussian form shows the
scale increasing from a central value determined by the size of the hole.
In other words, the profile decreases more sharply than a Gaussian.
.PP
However, no aperture plate system is ideal because the thickness of
the aperture plate is finite and there is scattering and changes in
the focus of the system.  One must
convolve the profile above with a scattering/focus function.  One can show
that for reasonable functions, exponential and Gaussian,
with some scale b the final profile is a function of the ratio b/a.
If the ratio is less than 1 then the profile will be more like that of
the hole and the profile will be sharper than a Gaussian in the wings.
If the ratio is much greater than 1 then the profile will become the
scattering profile at large separations.  Simulations using Gaussian
and exponential scattering profiles show behaviors very much like the
profile (1) with $s sub 1$ greater than zero when b/a < 1
meaning the profile becomes sharper (than a Gaussian) in the wings
and $s sub 1$ < 0 when b/a > 1.
Thus, $s sub 1$ defines the scale of the scattering profile relative
to the hole size.
The size of the hole is incorporated into the parameter $x sub 1$.
The parameter $s sub 2$ allows an asymmetry in the profile.
.PP
An interesting property of the scale function is that it is all right
for it to be negative at small distances from the profile center.  This
occurs when $s sub 0$ is negative.  The effect of this, provided $s$
becomes positive at large distances, is to give a two horned profile.
This, in fact, is observed when the focus of the system becomes very
poor.
.PP
The best fits (least chi-square or rms residual) are
obtained when each spectrum at each wavelength has independent
parameters.  However, this sometimes gives rise to unphysical results.
If left entirely unconstrained the parameter fitting algorithm can
make one line broad and dominant and a neighboring line weak and
sharp.
This is not, of course, a property of the camera or detector.
Thus, constraints based on the physics of the
camera/detector are used.  This means that the shape
parameters $s sub 0$, $s sub 1$, $s sub 2$, and $x sub 1$
are coupled locally by making them vary as a polynomial of position
across the dispersion.  One might also
constrain the variation of the shape along the spectrum as is done in
the Cyber.  This is not needed because there are no drastic differences
between the fitted parameters at neighboring points along the spectra.
.PP
My experience with the Cyrogenic Camera system has shown the
following.  The focus ripples twice across the CCD with the
propagation angle being approximately 30 degrees from the long dimension.
The change in focus is partly just a scale change.  This is seen in
the correlation of $s sub 0$ with the image scale found by \fBap_plate\fR.
The shape parameter $s sub 1$ changes sign from positive to
negative indicating that when the focus is good the profile
decreases faster than a Gaussian and when the focus is bad it decreases
slower.  Occassionally the focus is very bad and $s sub
0$ is negative and $s sub 1$ is small and positive causing a broad two
horned profile.  The
assymetry parameter, $s sub 2$, is useful only when the signal is strong near
the peak of a quartz exposure.  It is not really necessary to include
it in the model fits.  The assymetry parameter was dominant, however,
in some Echelle data which were clearly asymmetric.  The value of
$x sub 1$ is
not highly sensitive and can be fixed for a given hole size.  Large
changes in the hole size would require resetting $x sub 1$.
The use of the four parameters, $I sub 0$, $x sub 0$, $s sub 0$,
and $s sub 1$, allow good fits
to all the data I've examined including those in which the peak/valley
intensity ratio across the spectra is about 1.1.  It is the importance
of the parameter $s sub 1$ which improves the fitting dramatically over the
Cyber three parameter fitting (in addition to a different fitting
algorithm).
.PP
The heart of profile fitting is the solution of the multi-parameter
least-squares problem.  In a blended multi-spectra image the profile
parameters of one spectra are affected by its neighbors which are,
in turn, affected by their neighbors and so forth.  The key to this
type of problem is to realize that only nearest neighbors affect the
profile parameters and this leads to a "banded" least-squares matrix.
A banded matrix is one in which cross terms far from the diagonal are
zero.  Solution of banded matrices is much more efficient than solving
the entire matrix.  This allows solution for more than 100 parameters
simultaneously in a short time.
.PP
Use of the banded multi-parameter solution has the restriction, however,
that there can be no parameters in the model which are not local to
the profiles.  This affects the way
global constraints are applied to the parameters.  In particular,
the way the shape parameters are constrained to vary smoothly across the
detector.
The shape parameters are first found as independent parameters by the
banded matrix solution and then smoothed by a polynomial in x.
.PP
An area which was extensively investigated was the appropriate
weighting to use for the model fitting.  The most likely choices are
weighting by $1 / sqrt data$ and unit weight corresponding to
$chi sup 2$
and least squares fitting.  It was found that the two methods
agreed fairly closely but that the least squares fitting was more
appropriate because the blending correction depends largely on the
value of the peak intensity and less on the exact fit of the wings.
With $chi sup 2$ the peak is fit with less accuracy in order to improve
the fit in the wings of the profile.  In some cases this gave clear
errors in estimating the peak intensity and, hence, the proper contributions
between the blended spectra were not made.
.PP
Now follows the details of the fitting algorithm.
The algorithm is a series of script steps in \fBmultiap_extract\fR
which call the model fitting program \fBmodel_fit\fR with different
parameters.  In the script all bands are fit, $x sub 1$ is fixed,
and the asymmetry shape parameter $s sub 2$ is ignored.
The four parameter fit is applied to bands of 32 lines.  The band
solutions are linearly interpolated to the full image and then only
the intensity scale parameter is calculated for each line during the
extraction of the spectra with \fBmodel_extract\fR.
.PP
The general fitting scheme proceeds as follows:
.LP
1.  Fit the three parameters $I sub 0$, $x sub 0$, $s sub 0$ with
$x sub 1$ fixed and $s sub 1$ and $s sub 2$
zero.  This is precisely a Gaussian fit.  The three parameters are
determined simultaneously for all the lines at once using the banded
matrix method.  Thus for 50 lines the solution has 150 variables.
After each fit the scale
parameter $s sub 0$ is smoothed by a polynomial in x.  The polynomial is
taken with seven terms.
.LP
2.  Once the improvement in each iteration becomes less than a
specified amount (2% in rms residual) the next parameter $s sub 1$ is added.
The solution has two steps: fit for $s sub 0$ and $s sub 1$ with $I sub 0$
and $x sub 0$ fixed and
then fit $I sub 0$ and $x sub 0$ with $s sub 0$ and $s sub 1$ fixed.  As before the scale terms
are smoothed by a seventh order polynomial.  Attempts to solve for all
four parameters a once gave unstable results for reasons I don't
understand.
.LP
3.  If desired, the last shape parameter $s sub 2$ can be added by solving
for $s sub 0$, $s sub 1$, and $s sub 2$ while holding $I sub 0$ and
$x sub 0$ fixed and then solving for
$I sub 0$ and $x sub 0$.  This provides some improvement when the signal is very
strong but is generally not needed in the multi-aperture plate data.
It can be an important term in the Echelle data.
.LP
4.  It is possible to then adjust $x sub 1$ followed by steps 2 or 3.
However, this gives very little improvement and $x sub 1$ should be fixed for
each type of data.
.LP
5.  During the final extraction when individual lines are evaluated a one
parameter fit is used to find $I sub 0$ for each spectra.  This is
rather slow, however, on the order of 3 hours per frame.  By using
the pixel value near $x sub 0$ as the value for $I sub 0$ the extraction is reduced
to 13 minutes per frame (see section 12).
.PP
In addition to the preceeding steps the fitting algorithm applies some
heuristic constraints.  These constraints limit how far the peak positions can
shift in each iteration, require the peak intensity to remain positive, and
limit the scale function to be positive at large values of y.
.NH 2
STRIP_EXTRACT -- Unblended Profiles
.PP
For unblended multi-spectra data the profiles can be anything.  The profiles
are obtained by averaging a number of lines (say 32) and normalizing
at some point like the peak value.  These profiles are then used for
detecting bad pixels, such as cosmic rays, and correcting for them as
discussed in the section on cleaning.  Modeling using the \fBmodel_fit\fR
program is only used on Echelle data to find peak positions
accurately in order to follow any curvature of the spectra.
.NH
Extraction and Cleaning
.PP
The extraction of spectra are done separately from the modeling.  It is
possible to extract spectra without any modeling at all using
\fBstrip_extract\fR.  The extraction step also allows the user to specify
if cleaning of the spectra for cosmic rays is desired.  Also modifying
the image is an option.
.NH 2
MODEL_EXTRACT
.PP
Extraction and cleaning using a model fit is described here.
First the $I sub 0$ values for the model profiles are determined for
all the spectra in a line either by multi-parameter fitting or by
taking the peak value.  The pixel values are then compared to the
model in a chi-squared way:

.EQ
r = (data - model) / sqrt model
.EN

If the value of r is larger than a set amount, say 5, then the pixel
value is set to that of the model.  Since the "bad" pixel may affect
the intensity scale $I sub 0$ the cleaning is iterated until no further
pixels are changed.
.PP
The fitting of the data from an individual line of data to the model profiles
is the key element in this scheme.  The best method is to use all the
data in a multi-parameter least square fit.  This minimizes the effect
of bad pixels on the estimated profile which is the essence of this
cleaning method.  However, while the time required to do this for one
line is not great, it adds up to nearly three hours for the 800 lines
in a CRYOCAM frame.  A quick alternative is to scale the model profile
by the data value at the peak position.  This is
quite fast.  However, if the peak has a cosmic ray event or is
otherwise bad then the estimated profile will not correspond closely
to the data profile and the cleaning procedure will make gross errors.
The limited experience I've had with the Echelle and MAP data
has worked well with using the peak estimate.  However, the possible
problems make me nervous and some compromise based on using more than
the peak to estimate the intensity scale of the profile to the data
needs to be found.  This is important because much of the feedback on
the \fBmultispec\fR package from Paul Hintzen and Caty Pilachowski
have dealt with
the particular usefulness of a good cosmic ray cleaning algorithm in
extracting multi-spectra data.
.NH 2
STRIP_EXTRACT
.PP
Removing cosmic rays is the major part of Echelle extraction.
Because these are unblended spectra of arbitrary shape a strip
extraction is all that is needed.
The cleaning is done by the same algorithm used for the multi-aperture
plates except that the profiles are found, as described earlier, by
averaging a number of lines.
The intensity scaling is determined from either a least-square fit
or the peak value.
The same question about the appropriate way to
determine the fit of the profiles to the data discussed previously
applies here except since the spectra are not blended the spectra
can be treated separately in any least square fitting.
.NH
AP_PLATE -- Aperture Plate Correlation
.PP
The final step in the extraction of a multi-aperture plate image is to
correlate the spectra with the on-line database description of the 
drilled hole positions.  This allows for estimates of relative wavelength
offsets and the identification of the spectra with the ID, RA, and DEC
parameters.
.PP
The spectra are fitted to the known aperture plate drilled positions, given in
millimeters, to find an \fIangle\fR for the aperture plate relative to the
detector x dimension and the image \fIscale\fR in pixels / millimeter,

.EQ
x sub fit = a + scale (x sub drill cos (angle) + y sub drill sin (angle))~.
.EN

If the number of spectra is less than that given by the aperture plate drilled
positions then a correlation is done leaving out sequences of
consecutive holes until the fit residual is minimized.  If the number of
spectra is greater than that supposedly drilled then sequences of
consecutive peaks are left out of the fit to minimize the residual.
The missing holes or extra peaks are printed out and, if allowed, the aperture
plate description file is modified, otherwise the program terminates.
In all cases if the final fit residual is greater than 1
pixel the program will terminate.
The program prints out the \fIangle\fR of the aperture plate and the \fIscale\fR
which is also stored in the database.
.PP
An indication that a large part of the focus variation is purely a
scale change is that the derived image \fIscale\fR correlates very well with
the width of the spectra as derived from the profile fitting.  I
estimate that at least 50% of the focus variation is a scale
variation.  This is good news in the sense that a scale variation will
be taken out in the dispersion solution and lines in different parts
of the detector will become more similiar after the solution.
However, the scale variation does not account for all the profile
shape changes and there is indeed a change in the point spread function
across the detector.
.NH
Problems
.PP
There a few problems which I have not been able to resolve or have not
had the time to consider.  The problems which are largely intractable
without a great deal of effort are the unexplained background
variations and deconvolving the spectra for the variation in the
point-spread-function.  The background variations are abrupt increases
in the background in parts of the CRYOCAM detector.  The step edge sometimes
occurs under the spectra and so any smooth polynomial fit to the
background will not be very good.  The modeling of the multi-aperture
plate profiles provides information about the point-spread function
but a deconvolution of the variation in the PSF is a difficult problem
and not warrented at this time.
.PP
I had expected that the large scale response of the CRYOCAM could be
corrected by determining an overall average quartz spectrum from all the
extracted quartz spectra and then dividing the object spectra in each
hole by the ratio of the average quartz spectra from that hole to the
overall average quartz spectrum.  This was attempted and it was found
to work only partially.  Specifically, while there might be a 20%
difference between a spectrum on the edge and one near the center of
the detector the quartz correction left a 10% difference in the object
spectra.  This is apparently due to a poor illumination by the quartz
light source which does not correspond to the telescope illumination.
This quartz correction technique may be made available to users if
desired.
.NH
Comparison with the Cyber Extraction Package
.PP
The discussion of this section must be qualified by the fact that I
have not used the Cyber Extraction Package and I base my understanding on the
algorithms from the Multi-Aperture Plate Data Reduction Manual and
conversations with knowledgable people.  There are many differences
both major and minor and this section only seeks to mention the
some of the important differences.  In the Cyber package:

The background is subtracted from the images as a preliminary process.

The background is either constant or linear across the spectra.

The flat fields are produced by modeling the quartz and data from
several quartz exposures cannot be easily combined.

The initial peak finding and aperture plate correlation algorithm is less
automated in determining missing or additional holes.

The model fitting uses only a three parameter Gaussian model
and the algorithms do not yield results when the focus becomes poor.

The fitting algorithm is neighbor subtraction rather than full
simultaneous solution for all the profiles.

The model fitting is applied only to a quartz and the model is transfered to
object exposures.  This does not allow the shape of the profiles to
change with time as the telescope moves.

The modelling does not couple solutions for neighboring spectra
across the dispersion as is suggested in this design and it does smooth
along the spectra which is not done in this proposal.

The extraction is only to some specified sigma in the model profile and
there is no attempt to correct for blending.

There is no cleaning of the spectra.
.NH
Discussion
.PP
The only data which has passed beyond the extraction phase using the
algorithms described here was that of Paul Hintzen.
Comparison of data reduced by the TV package for
spectra extracted by both the Cyber package and the techniques of the
suggested \fBmultispec\fR package were quite comparable.  To the level he
examined
the spectra there was no clear increase in accuracy though the \fBmultispec\fR
extractions generally had higher counts due to the full extraction of
the blended spectra.  The big advantages found were
the ability to extract all the data even when the focus
became very poor and the success of the cosmic ray cleaning
algorithm.  Thus, Hintzen feels that the need for speed in the extraction
(primarily dependent on the cleaning algorithm)
is modified significantly by the difficulty of dealing with cosmic
rays in the TV spectral analysis programs.  More exploration
of techniques for determining the profile intensity scale from the
model without the full multi-parameter solution is warrented for this
reason.
.PP
I have extracted some Echelle data including field flattening.  The
data had a considerable number of cosmic rays which were removed
quite well.  The extracted spectra were put into a CAMERA format
for further analysis.
.PP
The programs were recently applied to a long slit analysis problem
being studied by Vesa Junkkarinen.  The image was already flat fielded.
The data had two closely spaced and very faint diffuse objects and scattered
light from a nearby QSO.
The three spectra were so weak and closely spaced
that the automatic finding was not used.  However, the rest of the modeling
and extraction were applied directly.
The \fBfind_bckgrnd\fR program, whose original purpose was to correct for
scattered light, worked well to extrapolate the sky across the
image.  The model fitting accurately followed
the peaks of the spectra but the shape fitting was only moderately accurate
since the model shape parameters are not suited to modeling galaxies.
It successfully extracted spectra with a minimum of effort on my part.
Analysis of the extracted spectra and comparison with other techniques
must still be done.  The conclusions to be drawn from this experiment are
that with sufficiently general multi-spectra tools multiple objects in
long slit spectroscopy can be handled.
.PP
One area in which I do not have practical experience is
the extraction of HGVS data.  I believe
the proposed design will work on this type of data.
.PP
A point which needs to be considered in the final design are the
formats of the data files.  The currently used one dimensional spectra
formats are an IIDS format and a CAMERA image format.
The formating of data files for the current spectral analysis packages by
\fBto_iids\fR starts from the \fBmultispec\fR database and throws away a lot 
of information about the spectra.  
Some refinement of this database should focus on the format
to be used by a new \fBIRAF\fR spectral analysis package.
.PP
It should be pointed out that many of the operations can have
alternate algorithms substituted.  In particular, the smoothing
algorithm for the multi-aperture plate flat fields can be replaced by
some other scheme.  The links between the multi-parameter fitting
program and the model have been made very general for investigating
a broad range of models.  Thus, it is also possible to substitute
additional model profiles with relative ease.
.PP
Estimates of excution time are taken from the experimental C programs
implementing the algorithms of this design and they are only
approximate estimates.  The steps corresponding
to \fBdebias\fR, \fBmultispec_flat\fR, and \fBflat_divide\fR for
the multi-aperture data from the CRYOCAM take
about 1 hour for a typical set of frames, say 5 to 15.  This includes
debiasing, triming, computing a flat field from several quartz frames
and dividing the quartz into the object frames.
.PP
The CRYOCAM \fBmultiap_extract\fR phase takes about 40 minutes for the modeling of a frame using 32 lines per band and either 3 hours for an extraction
using the profile fitting
method or 14 minutes for extraction using the peak profile scaling
method.
.PP
Finally, the \fBto_iids\fR takes about 3 minutes per frame.  It takes
this long because it has to convert the \fBmultispec\fR database organized across
the dispersion into formats in which the data is stored as consecutive
spectra; i.e. a type of rotation operation.
