.EQ
delim $$
define sl '{s lambda}'
.EN
.RP
.TL
The IRAF APEXTRACT Package
.AU
Francisco Valdes
.AI
IRAF Group - Central Computer Services
.K2
P.O. Box 26732, Tucson, Arizona 85726
.AB
The IRAF \fBapextract\fR package provides tools for the extraction of
one and two dimensional spectra from two dimensional images
such as echelle, long slit, multi-fiber, and multi-slit spectra.
Apertures of fixed width along the spatial define the regions of
the two dimensional images to be extracted at each point along the
dispersion axis.  Apertures may follow changes in the positions of
the spectra as a function of position along the dispersion axis.
The spatial and dispersion axes may be oriented along either image axis.
Extraction to one dimensional spectra consists of a weighted sum of the pixels
within the apertures at each point along the dispersion axis.  The
weighting options provide the simple sum of the pixel values and a
weighting by the expected uncertainty of each pixel.  Two dimensional
extractions interpolate the spectra in the spatial axis to produce
image strips with the position of the  spectra exactly aligned with one
of the image dimensions.  The extractions also include optional
background subtraction, modeling, and bad pixel detection and replacement.
The tasks are flexible in their ability to define and edit apertures,
operate on lists of images, use apertures defined for reference
images, and operate both very interactively or noninteractively.
The extraction tasks are efficient and require only one pass through
the data.  This paper describes the tasks, the algorithms, the data
structures, as well as some examples and possible future developments.
.AE
.NH
Introduction
.PP
The IRAF \fBapextract\fR package provides tools for the extraction of
one and two dimensional aperture spectra from two dimensional format
images such as those produced by echelle, long slit, multi-fiber, and
multi-slit spectrographs.  This type of data is becoming increasingly
popular because of the efficiency of data collection and recent
technological improvements such as fibers and digital detectors.
The trend is also to greater and greater numbers of spectra per
image.  Extraction is one of the fundamental operations performed
on these types of two dimensional spectral images, so a great deal of effort
has gone into the design and development of this package.  
.PP
The tasks are flexible and have many options.  To make the best use of
them it is important to understand how they work.  This paper provides
a general description of the tasks, the algorithms, the data structures,
as well as some examples of usage.  Specific descriptions of parameters
and usage may be found in the IRAF help pages for the tasks included
as appendices to this paper.  The image reduction "cookbooks" also
provide complete examples of usage for specific instruments or types
of instruments.
.PP
The tasks  in the \fBapextract\fR pacakge are summarized below.

.ce
The \fBApextract\fR Package
.TS
center;
n.
apdefault \&- Set the default aperture parameters
apedit \&- Edit apertures interactively
apfind \&- Automatically find spectra and define apertures
apio \&- Set the I/O parameters for the APEXTRACT tasks
apnormalize \&- Normalize 2D apertures by 1D functions
apstrip \&- Extract two dimensional aperture strips
apsum \&- Extract one dimensional aperture sums
aptrace \&- Trace positions of spectra
.TE

The tasks are highly integrated so that one task may call another tasks
or use its parameters.  Thus, these tasks reflect the logical organization
of the package rather than a set of disparate tools.  One reason for
this organization is group the parameters by function into easy to manage
\fIparameter sets (psets)\fR.  The tasks \fBapdefault\fR and \fBapio\fR
are just psets for specifying the default aperture parameters and the
I/O parameters of the package; in other words, they do nothing but
provide a grouping of parameters.  Executing these tasks is a shorthand
for the command "eparam apdefault" or "eparam apio".  The other tasks
provide both a logical grouping of parameters and function.  For
example the task \fBaptrace\fR traces the positions of the spectra
in the images and has the parameters related to tracing.  The task
\fBapsum\fR, however, may trace the spectra as part of the overall
extraction process and it uses the functionality and parameters of
the \fBaptrace\fR task without requiring all the tracing parameters
be included as part of its parameter set.  As we examine each task
in detail it will become more apparent how this integration of function
and parameters works.
.PP
The \fBapextract\fR package identifies the image axes with the spatial
and dispersion axes.  Thus, during extraction pixels of constant
wavelength are those along a line or column.  In this paper the terms
\fIslit\fR or \fIspatial\fR axis and \fIdispersion\fR or \fIwavelength\fR
axis are used to refer to the image axes corresponding to the spatial
and dispersion axes.  Often a small degree of misalignment between the
image axes and the true dispersion and spatial axes is not important.
The main effect of misalignment is a broadening of the spectral
features due to the difference in wavelength on opposite sides of the
extraction aperture.  If the misalignment is significant, however, the
image may be rotated with the task \fBrotate\fR in the \fBimages\fR
package or remapped with the \fBlongslit\fR package tasks for
coordinate rectification.
.PP
It does not matter which image axis is the dispersion axis since the
tasks work equally well in either orientation.  However, the dispersion
axis must be defined, with the \fBtwodspec\fR task \fBsetdisp\fR,
before these tasks may be used.  This task is a simple script which
adds the parameter DISPAXIS to the image headers.  The \fBapextract\fR
tasks, like the \fBlongslit\fR tasks, look in the header to determine
the dispersion axis.
.NH
Apertures
.PP
Apertures are the basic data structures used in the package; hence the
package name.  An aperture defines a region of the two dimensional image
to be extracted.  The aperture definitions are stored in a database.
An aperture consists of the following components.

.IP ID
.br
An integer identification number.  The identification number must be
unique.  It is used as the default extension during extraction of
the spectra.  Typically the IDs are consecutive positive integers
ordered by increasing or decreasing slit position.
.IP BEAM
.br
An integer beam number.  The beam number need not be
unique; i.e. several apertures may have the same beam number.
The beam number will be recorded in the image header of the
the extracted spectrum.  By default the beam number is the same as
the ID.
.IP APAXIS
.IP CENTER[2]
.br
The center of the aperture along the slit and dispersion axes in the two
dimensional image.
.IP LOWER[2]
.br
The lower limits of the aperture, relative to the aperture center,
along the slit and dispersion axes.  The lower limits need not be less
than the center.
.IP UPPER[2]
.br
The upper limits of the aperture, relative to the aperture center,
along the slit and dispersion axes.  The upper limits need not be greater
than the center.
.IP CURVE
.br
An offset to be added to the center position for the \fIslit\fR axis which is
a function of the wavelength.  The function is one of the standard IRAF
types; a legendre polynomial, a chebyshev polynomial, a linear spline,
or a cubic spline.
.IP background
.br
Parameters for background subtraction based on the interactive
curve fitting (\fBicfit\fR) tools.

.PP
The aperture center is the only absolute coordinate (relative to the
image or image section).  The other aperture parameters and the
background fitting regions are defined relative to the center.  Thus,
an aperture may be repositioned easily by changing the center
coordinates.  Also a constant aperture size, shape (curve), and
background regions may be maintained for many apertures.  The center
and aperture limits, in image coordinates, along the slit axis are
given by:

.EQ I
  ~roman center ( lambda )~mark = roman cslit + roman curve ( lambda )
.EN
.EQ I
roman lower ( lambda )~lineup = roman center ( lambda ) + roman lslit
.EN
.EQ I
roman upper ( lambda )~lineup = roman center ( lambda ) + roman uslit
.EN

where $lambda$ is the wavelength coordinate.  Note that both the lower and
upper constants are added to the center defined by the aperture center and
the offset curve.  The aperture limits along the dispersion axis are
constant since there is no offset curve:

.EQ I
roman center (s)~lineup = roman cdisp
.EN
.EQ I
roman lower (s)~lineup = roman center (s) + roman ldisp
.EN
.EQ I
roman upper (s)~lineup = roman center (s) + roman udisp
.EN

.PP
Apertures for a particular image may be defined in several ways.  
These methods are arranged in a hierarchy.

.IP (1)
The database is first searched for previously defined apertures.
.IP (2)
If no apertures are found and a reference image is specified then the
database is searched for apertures defined for the reference image.
.IP (3)
The user may then edit the apertures interactively with graphics
commands if the \fIapedit\fR parameter is set.  This includes creating
new apertures and deleting or modifying existing apertures.  This
interactive editing procedure may be entered from any of the \fBapextract\fR
tasks.
.IP (4)
For the tasks \fBtrace\fR, \fBsumextract\fR, and \fBstripextract\fR
if no apertures are defined at this point a default aperture
is created consisting of the entire image with center at the center of
the image.  Note that if an image section is used then the aperture
spans the image section only.
.IP (5)
Any apertures created, modified, or adopted from a reference image are recorded
in the database for the image.

.PP
There are several important points to appreciate in the above logic.
First, any of the tasks may be used without prior use of the others.
For example one may use extract with the \fIapedit\fR switch set and
define the apertures to be extracted (except for tracing).
Alternatively the apertures may be defined with \fBapedit\fR
interactively and then traced and extracted noninteractively.  Second,
image sections may be used to define apertures (step 4).  For example
a list of image sections (such as are used in multislit spectra) may be
extracted directly and noninteractively.  Third, multiple images may
use a reference image to define the same apertures.  There are several
more options which are illustrated in the examples section.
.PP
Another subtlety is the way in which reference images may be
specified.  The tasks in the package all accept list of images
(including image sections).  Reference images may also be given as a
list of images.  The lists, however, need not be of the same length.
The reference images in the reference image list are paired in order
with the input images.  If the reference list ends before the image
list then the last reference image is used for the remaining images.
The most common situations are when there is no reference image, when
only one reference image is given for a set of input images, and when a
matching list of reference images is given.  In the second case the
reference image refers to all the input images while in the last case
each input image has a reference image.
.PP
There is a trick which may be played with the reference images.  If a list
of input images is given and the reference image is the same as the first
input image then only the first image need be done interactively.
This is because after the apertures for the first image have been defined
they are recorded in the database.  Then when the database is searched
for apertures for the second image, the apertures of the reference image
will be available.
.NH
.PP
\fBApedit\fR is a generally interactive task which graphs a line of
an image along the slit axis and allows the user to define and edit
apertures with the graphics cursor.  The defined apertures are recorded
in a database.  The task \fBtrace\fR traces the positions of the
spectrum profiles from one wavelength to other wavelengths in the image
and fits a smooth curve to the positions.  This allows apertures
to follow shifts in the spectrum along the slit axis.  The tasks
\fBsumextract\fR and \fBstripextract\fR perform the actual aperture
extraction to one and two dimensional spectra.  They have options for
performing background subtraction, detecting and replacing bad pixels,
modeling the spectrum profile, and weighting the pixels in the aperture
when summing across the dispersion.
.NH
Tracing
.PP
The spectra to be extracted are not always aligned exactly with the
image columns or lines (the extraction axes).
For consistent extraction it is important that the same
part of the spectrum profile be extracted at each wavelength point.
Thus, the extraction apertures allow for shifts along the spatial axis
at each dispersion point.  The shifts are defined by a curve which is a
function of the wavelength.  The curve is determined by tracing the
positions of the spectrum profile at a number of wavelengths and
fitting a function to these positions.
.PP
The task \fBtrace\fR performs the tracing and curve fitting and records
the curve in the database.  The starting point along the
dispersion axis (a line or column) for the tracing is specified by the
user.  The position of the profile is then determined using the
\fBcenter1d\fR algorithm described elsewhere (see the help page for
\fBcenter1d\fR or the paper \fIThe Long Slit Reduction Package\fR).
The user specifies a step along the dispersion axis.  At each step the
positions of the profiles are redetermined using the preceding
position as the initial guess.  In order to enhance and trace weak
spectra the user may specify a number of neighboring profiles to be
summed before determining the profile positions.
.PP
Once the
positions have been traced from the starting point to the ends of the
aperture, or until the positions become indeterminate, a curve of a
specified type and order is fit to the positions as a function of
wavelength.  The function fitting is performed with the \fBicfit\fR
tools (see the IRAF help page).  The curve fitting may be performed
interactively or noninteractively.  Note that when the curve is fit
interactively the actually positions measured are graphed.  However, the
curve is stored in the aperture definition as an offset relative to the
aperture center.
.PP
The tracing requires that the spectrum profile have a shape from which
\fBcenter1d\fR can determine a position.  This pretty much means
gaussian type profiles.  To extract a part of a long slit spectrum
which does not have such a profile the user must trace a profile from
another part of the image or a different image and then shift the
center of the aperture without changing the shape.  For example the
center of a extended galaxy spectrum can be traced and the aperture
shifted to other parts of the galaxy.
.NH
Extraction
.PP
There are two types of extraction; strip extraction and sum
extraction.  Strip extraction produces two dimensional images with
pixels corresponding to the center of an aperture aligned along the
lines or columns.  Sum extraction consists of the weighted sum of the
pixels within an aperture along the image axis nearest the spatial axis
at each point along the dispersion direction.  It is important to
understand that the extraction is along image lines or columns while
the actual dispersion/spatial coordinates may not be aligned exactly
with the image axes.  If this misalignment is important then for simple
rotations the task \fBrotate\fR in the \fBimages\fR package may be used
while for more complex coordinate rectifications the tasks in the
\fBlongslit\fR package may be used.
.NH 2
Sum Extraction
.PP
Denote the image axis nearest the spatial axis by the index $s$ and
the other image axis corresponding to the dispersion axis by $lambda$.
The extraction is defined by the equation

.EQ I (1)
f sub lambda~=~sum from s (W sub sl (I sub sl - B sub sl ) / P sub sl ) /
sum from s W sub sl
.EN

where the sums are over all pixels along the spatial axis within some
aperture.  The $W$ are weights, the $I$ are pixel intensities,
the $B$ are background intensities, and the $P$ are a normalized
profile model.
.PP
There are many possible choices for the extraction weights.  The extraction
task currently provides two:

.EQ I (2a)
W sub sl~mark =~P sub sl
.EN
.EQ I (2b)
W sub sl~lineup =~P sub sl sup 2 / V sub sl
.EN

where $V sub sl$ is the variance of the pixel intensities given by the
model

.EQ I
	V sub sl~=~v sub 0 + v sub 1~max (0,~I sub sl )~~~~if v sub 0~>~0
.EN
.EQ I
	V sub sl~=~v sub 1~max (1,~I sub sl )~~~~~~~~~if v sub 0~=~0
.EN

Substituting these weights in equation (1) yields the extraction equations

.EQ I (3a)
f sub lambda~mark =~sum from s (I sub sl - B sub sl )
.EN
.EQ I (3b)
f sub lambda~lineup =~sum from s (P sub sl (I sub sl - B sub sl ) / V sub sl ) /
sum from s (P sub sl sup 2 / V sub sl )
.EN

.PP
The first type of weighting (2a), called \fIprofile\fR weighting, weights
by the profile.  Since the weights cancel this gives the simple extraction (3a)
consisting of the direct summation of the pixels within the aperture.
It has the virtue of being simple and computationally fast (since the
profile model does not have to be determined).
.PP
The second type of weighting (2b), called \fIvariance\fR weighting,
uses a model for the variance of the pixel intensities.
The model is based on Poisson statistics for a linear quantum detector.
The first term is commanly call the \fIreadout\fR noise and the second term
is the Poisson noise.  The actual value of $v sub 1$ is the reciprical of
the number of photons per digital intensity unit (ADU).  A simple variant of
this type of weighting is to let $v sub 1$ equal zero.  Since the actual
scale of the variance cancels we can then set $v sub 0$ to unity to obtain

.EQ I (4)
f sub lambda~=~sum from s (P sub sl (I sub sl - B sub sl )) /
sum from s P sub sl sup 2 .
.EN

The interpretation of this extraction is that the variance of the intensities
is constant.  It gives greater weight to the stronger parts of the spectrum
profile than does the profile weighting (3a) since the weights are
$P sub sl sup 2$.  Equation (4) has the virtue that one need not know the
readout noise or the ADU to photon number conversion.
.NH 3
Optimal Extraction
.PP
Variance weighted extraction is sometimes called optimal extraction because
it is optimal in a statistical sense.  Specifically,
the relative contribution of a pixel to the sum is related to the uncertainty
of its intensity.  The uncertainty is measured by the expected variance of
a pixel with that intensity.  The degree of optimality depends on how well
the relative variances of the pixels are known.
.PP
A discussion of the concepts behind optimal extraction is given in the paper
\fIAn Optimal Extraction Algorithm for CCD Spectroscopy\fR by Keith Horne
(\fBPASP\fR, June 1986).  The weighting described in Horne's paper is the
same as the variance weighting described in this paper.  The differences
in the algorithms are primarily in how the model profiles $P sub sl$ are
determined.
.NH 3
Profile Determination
.PP
The profiles of the spectra along the spatial axis are determined when
either the detection and replacement of bad pixels or variance
weighting are specified.  The requirements on the profiles are that
they have the same shape as the image profiles at a each dispersion
point and that they be as noise free and uncontaminated as possible.
The algorithm used to create these profiles is to average a specified
number of consecutive background subtracted image profiles immediately
preceding the wavelength to which a profile refers.  When there are an
insufficient number of image profiles preceding the wavelength being
extracted then the following image profiles are also used to make up
the desired number.  The image profiles are interpolated to a common
center before averaging using the curve given in the aperture
definition.  The averaging reduces the noise in the image data while
the centering eliminates shifts in the spectrum as a function of
wavelength which would broaden the profile relative to the profile of a
single image line or column.  It is assumed that the spectrum profile
changes slowly with wavelength so that by using profiles near a given
wavelength the average profile shape will correctly reflect the profile
of the spectrum at that wavelength.
.PP
The average profiles are determined in parallel with the extraction,
which proceeds sequentially through the image.  Initially the first set
of spectrum profiles is read from the image and interpolated to a common
center.  The profiles are averaged excluding the first profile to be
extracted; the image profiles in the average never include the image
profile to be extracted.  Subsequently the average profile is updated
by adding the last extracted image profile and subtracting the image
profile which no longer belongs in the average.  This allows each image
profile to be accessed and interpolated only once and makes the
averaging computationally efficient.  This scheme also allows excluding
bad pixels from the average profile.  The average profile is used to
locate and replace bad pixels in the image profile being extracted as
discussed in the following sections.  Then when this profile is added
into the average for the next image profile the detected bad pixels are
no longer in the profile.
.PP
In summary this algorithm for determining the spectrum profile
has the following advantages:

.IP (1)
No model dependent smoothing is done.
.IP (2)
There is no assumption required about the shape of the profile.
The only requirement is that the profile shape change slowly.
.IP (3)
Only one pass through the image is required and each image profile
is accessed only once.
.IP (4)
The buffered moving average is very efficient computationally.
.IP (5)
Bad pixels are detected and removed from the profile average as the
extraction proceeds.

.NH 3
Detection and Elimination of Bad Pixels
.PP
One of the important features of the aperture extraction package is the
detection and elimination of bad pixels.  The average profile described
in the previous section is used to find pixels which deviate from this
profile.  The algorithm is straightforward.  A model spectrum of the
image profile is obtained by scaling the normalized profile to the
image profile.  The scale factor is determined using chi squared fitting:

.EQ I (6)
M sub sl~=~P sub sl~left { sum from s ((I sub sl - B sub sl ) P sub sl /
V sub sl)~/~ sum from s (P sub sl sup 2 / V sub sl ) right } .
.EN

The RMS of this fit is determined and pixels deviating by more than a
user specified factor times this RMS are rejected.  The fit is then
repeated excluding the rejected points.  These steps are repeated until
the user specified number of points have been rejected or no further deviant
points are detected.  The rejected points in the image profile are then
replaced by their model values.
.PP
This algorithm is based only on the assumption that the spatial profile
of the spectrum (no matter what it is) changes slowly with wavelength.
It is very sensitive at detecting departures from the expected profile.
Its main defect is that in the first pass at the fit all of the image profile
is used.  If there is a very badly deviant point and the rest of the profile
is weak then the scale factor may favor the bad pixel more than the
rest of the profile resulting in rejecting good profile points and not
the bad pixel.
.NH 3
Relation of Optimal Extraction to Model Extraction
.PP
Equation (1) defines the extraction process in terms of a weighted sum
of the pixel intensities.  However, the actual extraction operations
performed by the task \fBsumextract\fR are 

.EQ I (7a)
f sub lambda~mark =~sum from s (I sub sl - B sub sl )
.EN
.EQ I (7b)
f sub lambda~lineup =~sum from s M sub sl
.EN

where $M sub sl$ is the model spectrum fit to the background subtracted
image spectrum $(I sub sl - B sub sl )$
defined in the previous section (equation 6).  It is not obvious at first that
(7b) is equivalent to (3b).  However, if one sums (6) and uses the fact
that the sum of the normalized profile is unity one is left with equation (3b).
.PP
Equations (6) and (7b) provide an alternate way to think about the
extracted one dimensional spectra.  Sum extraction of the model spectrum
is used instead of the weighted sum for variance weighted extraction
because the model spectrum is a product of the profile determination
and the bad pixel cleaning process.  It is then more convenient
and efficient to use the simple equations (7).
.NH 2
Strip Extraction
.PP
The task \fBstripextract\fR uses one dimensional image interpolation
to shift the pixels along the spatial axes so that in the resultant
output image the center of the aperture is exactly aligned with the
image lines or columns.  The cleaning of bad pixels is an option
in this extraction using the methods described above.  In addition
the model spectrum described above may be extracted as a two
dimensional image.  In fact, the only difference between strip extraction
and sum extraction is whether the final step of summing the pixels
in the aperture along the spatial axis is performed.
.PP
The primary use of \fBstripextract\fR is as a diagnostic tool.  It
allows the user to see the background subtracted, cleaned and/or model
spectrum as an image before it is summed to a one dimensional spectrum.
In addition the two dimensional format allows use of other IRAF tools such as
smoothing operators.  When appropriate
it is a much simpler method of removing detector distortions and alignment
errors than the full two dimensional mapping and image transformation
available with the \fBlongslit\fR package.
.NH
Examples
.de CS
.nf
.ft L
..
.de CE
.fi
.ft R
..
.PP
This section is included because the flexibility and many options of
the tasks allows a wide range of applications.  The examples illustrate
the use of the task parameters for manipulating input images, output
images, and reference images, and setting apertures interactively and
noninteractively.  They do not illustrate the different possibilities
in extraction or the interactive aperture definition and editing
features.  These examples are meant to be relevant to actual data
reduction and analysis problems.  For the purpose of these examples we
will assume the dispersion axis is along the second image axis; i.e.
DISPAXIS = 2.
.PP
The simplest problem is the extraction of an object spectrum which
is centered on column 200.  To extract the spectrum with an aperture
width of 20 pixels an image section can be used.

.CS
cl> sumextract image[190:209,*] obj1d
cl> stripextract image[190:209,*] obj2d
.CE

To set the aperture center and limits interactively the edit option can be
used with or without the image section.  This also allows fractional pixel
centering and limits.
.PP
If the object slit position changes the spectrum profile can be traced first
and then extracted.

.CS
cl> trace image[190:209,*]
cl> sumextract image[190:209,*] obj1d
cl> stripextract image[190:209,*] obj2d
.CE

By default the apertures are defined and/or edited interactively in
\fBtrace\fR and editing is not the default in \fBsumextract\fR or
\fBstripextract\fR.
.PP
A more typical example involves many images.  In this case a list of images
is used though, of course, each image could be done separately as
in the previous examples.  There are three common forms of lists, a
pattern matching template, a comma separated list, and an "@" file.
In addition the template editing metacharacter, "%", may be used
to create new output image names based on input image names.
If the object positions are different in each image then we can select
apertures with image sections or using the editing option.  Some examples
are

.CS
cl> sumextract image1[10:29,*],image2[32:51] obj1,obj2
cl> sumextract image* e//image* edit+
cl> sumextract image* image%%ex%* edit+
cl> sumextract @images @images edit+
.CE

The "@" files can be created from the other two types of lists using the
\fBsections\fR task in the \fBimages\fR package.  An important feature
of the image templates is the use of the concatenation operator.  Note,
however, this a feature of image templates and not file templates.
Also the output root name may be the same as the input
name because an extension is added provided there are no image
sections in the input images.
.PP
If the object positions are the same then the apertures can be defined once
and the remaining objects can be extracted using a reference image.

.CS
cl> apedit image1
cl> sumextract image* image* ref=image1
.CE

Rather than using \fBapedit\fR one can use \fBsumextract\fR alone with
the edit switch set.  The command is

.CS
cl> sumextract image* image* ref=image1 edit+
.CE

The task queries whether to edit the apertures for each image.
For the first image respond with "yes" and set the apertures interactively.
For the second task respond with "NO".  Since the aperture for "image1"
was recorded when the first image was extracted it then acts as the reference
for the remaining images.  The emphatic response "NO" turns off the edit switch
for all the other images.  One difference between this example and the
previous one is that the task cannot be run as a background batch task.
.PP
The extension to using traced apertures in the preceding examples is
very similar.

.CS
cl> apedit image1
cl> trace image* ref=image1 edit-
cl> sumextract image* image*
cl> stripextract image* image*
.CE

.PP
Another common type of data has multiple spectra on each image.  Some examples
are echelle and multislit spectra.  Echelle extractions usually are done
interactively with tracing.  Thus, the commands are

.CS
cl> trace ech*
cl> sumextract ech* ech*
.CE

For multislit spectra the slitlets are usually referenced by creating
an "@" file containing the image sections.  The usage for extraction
is then

.CS
cl> sumextract @slits @slitsout
.CE

.PP
The aperture definitions can be transfered from a reference image to
other images using \fBapedit\fR.  There is no particular reason to
do this except that reference images would not be needed in 
\fBtrace\fR, \fBsumextract\fR or \fBstripextract\fR.  The transfer
is accomplished with the following command

.CS
cl> apedit image1
cl> apedit image* ref=image1 edit-
.CE

The above can also be combined into one step by editing the first image
and then responding with "NO" to the second image query.
.NH
Future Developments
.PP
The IRAF extraction package \fBapextract\fR is going to continue to
evolve because 1) the extraction of one and two dimensional spectra
from two dimensional images is an important part of reducing echelle,
longslit, multislit, and multiaperture spectra, 2) the final strategy
for handling multislit and multiaperture spectra produced by aperture
masks or fiber optic mapping has not yet been determined, and 3) the
extraction package and the algorithms have not received sufficient user
testing and evaluation.  Changes may include some of the following.

.IP (1)
Determine the actual variance from the data rather than using the Poisson
CCD model.
.IP (2)
Another task, possibly called \fBapfind\fR, is needed to automatically find
profile positions in multiaperture, multislit, and echelle spectra.
.IP (3)
The bad pixel detection and removal algorithm does not handle well the case
of a very strong cosmic ray event on top of a very weak spectrum profile.
A heuristic method to make the first fitting pass of the average
profile to the image data less prone to errors due to strong cosmic rays
is needed.
.IP (4)
The aperture definition structure is general enough to allow the aperture
limits along the dispersion dimension to be variable.  Eventually aperture
definition and editing will be available using an image display.  Then
both graphics and image display editing switches will be available.
An image display interface will make extraction of objective prism
spectra more convenient than it is now.
.IP (5)
Other types of extraction weighting may be added.
.IP (6)
Allow the extraction to be locally perpendicular to the traced curve.
