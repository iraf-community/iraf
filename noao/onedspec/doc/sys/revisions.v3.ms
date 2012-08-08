.nr PS 9
.nr VS 11
.RP
.ND
.TL
ONEDSPEC Package Revisions Summary: IRAF Version 2.10
.AU
Francisco Valdes
.AI
IRAF Group - Central Computer Services
.K2
P.O. Box 26732, Tucson, Arizona 85726
July 1990
.AB
This paper summarizes the changes in Version 3 of the IRAF \fBonedspec\fR
package which is part of IRAF Version 2.10.  The major new features and
changes are:

.IP \(bu
\fBIdentify\fR and \fBreidentify\fR now treat multispec format spectra
and two dimensional images as a unit.
.IP \(bu
\fBReidentify\fR supports both tracing (the old method) and always starting
with the primary reference vector when reidentifying other vectors in a
two dimensional reference image.
.IP \(bu
\fBReidentify\fR matches reference lines or apertures when reidentifying
those vectors in different images rather than tracing.
.IP \(bu
\fBReidentify\fR has an interactive capability to review
suspect reidentifications.
.IP \(bu
\fBReidentify\fR provides the capability to add new features.
.IP \(bu
The task \fBmsdispcor\fR provides for spatial interpolation of wavelength
zero point shifts from simultaneous arc spectra.
.IP \(bu
The new task \fBscopy\fR copies subsets of apertures and does format
conversions between the different spectrum formats.
.IP \(bu
The new task \fBsapertures\fR adds or modifies beam numbers and
aperture titles for selected apertures based on an aperture
identification file.
.IP \(bu
The new task \fBsfit\fR fits spectra and outputs the fits in various ways.
Apertures in  multispec and echelle format are fit independently.
.IP \(bu
The task \fBcontinuum\fR now does independent fits for multispec and
echelle format spectra.
.IP \(bu
\fBSplot\fR now allows deblending of any number of components and
allows simultaneous fitting of a linear background.
.IP \(bu
The new task \fBfitprofs\fR fits 1D gaussian profiles in images.
.AE
.NH
Introduction
.PP
Though  most of the ONEDSPEC package is unchanged there have been
significant changes to a number of the commonly used tasks in IRAF
Version 2.10.  The changes will be made available as part of an
external package prior to the release of V2.10.  This paper summarizes
the changes and new features.  The changes primarily apply to multispec
or echelle format spectra.
.PP
Tables 1 and 2 summarize most of the major and minor changes in the package.

.ce
TABLE 1:  Summary of Major New Features and Changes

.IP \(bu
\fBIdentify\fR and \fBreidentify\fR now treat multispec format spectra
and two dimensional images as a unit allowing easy movement between
different image lines or columns.  The database is only updated upon
exiting the image.
.IP \(bu
\fBReidentify\fR supports both tracing (the old method) and always starting
with the primary reference vector when reidentifying other vectors in a
two dimensional reference image.
.IP \(bu
\fBReidentify\fR matches reference lines or apertures when reidentifying
those vectors in different images rather than tracing.
.IP \(bu
\fBReidentify\fR has an interactive capability to review
suspect reidentifications.
.IP \(bu
\fBReidentify\fR provides the capability to add new features.
.IP \(bu
The task \fBmsdispcor\fR allows using
auxilary reference spectra to provide a shift in the wavelength
zero point to the primary dispersion functions.  This includes
spatial interpolation of simultaneous arc spectra in multifiber
spectrographs.
.IP \(bu
The new task \fBscopy\fR copies subsets of apertures and does format
conversions between the different spectrum formats.
.IP \(bu
The new task \fBsapertures\fR adds or modifies beam numbers and
aperture titles for selected apertures based on an aperture
identification file.
.IP \(bu
The new task \fBsfit\fR fits spectra and outputs the fits in various ways.
This includes a new feature to replace deviant points by the fit.
Apertures in  multispec and echelle format are fit independently.
.IP \(bu
The task \fBcontinuum\fR now does independent fits for multispec and
echelle format spectra.
.IP \(bu
\fBSplot\fR now allows deblending of any number of components and
allows simultaneous fitting of a linear background.
.IP \(bu
The new task \fBfitprofs\fR fits 1D gaussian profiles to spectral lines or
features in an image line or column.  This is done noniteractively and
driven by an input list of feature positions.
.bp
.LP
.ce
TABLE 2:  Summary of Other New Features and Changes

.IP \(bu
The \fBidentify\fR database format uses aperture numbers rather than
image sections for multispec format spectra.
.IP \(bu
The apertures in multispec format images need not be in the same order
or even the same number of apertures as the reference image in
\fBreidentify\fR or \fBmsdispcor\fR.
.IP \(bu
An automatic write parameter has been added to \fBidentify\fR.
.IP \(bu
The tasks \fBmsdispcor\fR and \fBspecplot\fR support the extra information
in the third dimension of multispec format spectra which is optionally
output by the \fBapextract\fR package.
.IP \(bu
\fBMsdispcor\fR and \fBspecplot\fR now include a logfile.
.IP \(bu
\fBSplot\fR selects spectra from multispec or echelle format by their
aperture number.  Also a new keystroke was added to select a new
line/aperture without having to enter the image name again.
.IP \(bu
The task \fBspecplot\fR may select apertures from a multispec or
echelle format spectrum.
.IP \(bu
The aperture identification in multispec format is used, if present,
for labeling in \fBsplot\fR, \fBspecplot\fR, and \fBstandard\fR.
.NH
IDENTIFY and REIDENTIFY
.PP
These tasks have been modified for greater flexibility when dealing with
two dimensional images and multispec format spectra in particular.  These
tasks were initially designed primarily to work on one dimensional images
with provisions for two dimensional images through image sections and
tracing to other parts of the image.  Now these tasks treat such images
as a unit.
.PP
The task \fBidentify\fR has three added keystrokes, 'j', 'k', and 'o'.
These provide for moving between lines and columns of a two dimensional
image and different apertures in a multispec format spectrum.  When
changing vectors the last set of features and fit are recalled, if they
have been previously defined, or the last set of features and fit are
inherited.  For efficiency and to minimize queries, the feature
information from all the lines or apertures is not written to the
database until you quit the image (or explicitly write it) rather than
one at a time.  A new parameter was also added, \fIautowrite\fR, which
may be set to automatically write the results to the database rather
than querying as is currently done.
.PP
The format of the database entries have also been slightly modified in
the case of multispec format images.  Instead of using image sections
as part of the image name to define different vectors in the image
(this is still the case for regular two dimensional images) the aperture
number is recorded.  This decouples the solutions for an aperture from
the specific image line allowing reference images to have a different
aperture order and additional or missing apertures.
.PP
While the changes to \fBidentify\fR are minor as far as usage, the task
\fBreidentify\fR is quite different and is essentially a new program.
Much of the complexity in this task relates to two dimensional images.
Two additions that apply to both one and two dimensional images is the
capability to add features from a coordinate list and to interactively
review the reidentifications using \fBidentify\fR.  The addition of new
features may be useful in cases where the signal-to-noise varies or to
compensate for lost features when tracing across an image.  The review
capability first prints the statistical results and then ask the user if
they want to examine the results interactively .  This allows
basing the decision to interactively examine the features and fit based
on this information.  Ideally, only a few of the worst cases need be
examined interactively.
.PP
There are two phases of reidentifications which apply to two
dimensional and multispec format images.  In the first phase, one needs
to expand the identifications in the reference image from an initial,
interactively defined line, column, or aperture to other parts of the
reference image.  A very important change is that there are now two
ways to transfer the features list; by successive steps (tracing) using
the previous results as a starting point (the only method provided in
the previous version) or always starting from the original reference
list.  The first method is suitable for long slit spectra which have
significant positional trends across the image.  If a feature is lost,
however, the feature remains missing (barring automatic addition as
mentioned above) for all following lines or columns.  The latter method
is best if there are only small variations relative to the initial
reference or in multispec format spectra where there is no inherent
relation between apertures.
.PP
The second phase of reidentifications is between the reference image
and other images.  In the previous version the primary reference vector
was transferred to the new image and then tracing would be applied
again.  This compounds the problem with losing features during tracing
and prevents any possible reidentifications from multispec images in
which the wavelength range may vary greatly.  In the new version there
is a direct reidentification from the same line, column, or aperture in
the reference to that of the next image.  In the case where different
apertures may have significantly different wavelength coverage, as
occurs with aperture masks, it will at least be possible to
interactively identify features and coordinate functions for each
aperture, using the scrolling capability in the new \fBidentify\fR, in
just a single image and then correctly transfer the features to
additional images.
.PP
For multispec format spectra the database information is organized by
aperture number independent of image line number.  Thus, it is possible
to reidentify features in multispec format spectra even if the aperture
order is different.  If there is only a partial overlap in the aperture
set only those apertures having an entry in the reference image will be
done.
.NH
MSDISPCOR
.PP
The task \fBmsdispcor\fR dispersion corrects (rebins to a linear
dispersion function) multispec format spectra.  It was introduced in
V2.8 of IRAF in the prototype \fBimred.msred\fR package.  A number of
changes have been made in this task as summarized here.
.PP
The most fundamental change is support for spatial interpolation of
reference dispersion functions from a subset of apertures to other
apertures originating at different positions in a two dimensional
image.  This is primarily intended for the case of comparison arc
spectra which are interspersed with object spectra in multifiber
spectrographs.  It would also be useful in digitized photographic
spectra having calibration spectra exposed next to the object
spectrum.  While usable directly, this feature is intended for the
processing scripts in the new \fBimred\fR fiber instrument packages.
.PP
The interpolation is only for a wavelength zero point shift, as determined
by \fBreidentify\fR with \fIrefit\fR=no.  The full dispersion function
is still provided by a calibration image covering all apertures.  Thus,
the simultaneous arc apertures are used to monitor shifts in the
detector relative to the full calibration which includes the relative
differences between each aperture and the arc monitoring apertures.
.PP
The multispec spectra containing the apertures used for the spatial
wavelength zero point corrections are specified in the image header
using the keywords REFSHFT1 and REFSHFT2.  These are analogous to
the REFSPEC keywords used to define the reference dispersion functions
for the apertures.
.PP
As part of the general theme of multispec format support the
multispec dispersion reference spectra may have additional spectra and
need not be in the same order.  However, all aperture in the
images being dispersion corrected must have dispersion relations
in the database.  Multispec format spectra may include additional
data in the 3rd image dimension produced by the new
\fBapextract\fR package.  \fBMsdispcor\fR rebins this information
in the same way as the spectra, thus, preserving the information
but now in linear wavelength sampling.
.PP
A new parameter, \fIlogfile\fR, has been added to capture information
about the dispersion correction process.
.NH
SCOPY and SAPERTURES
.PP
The task \fBscopy\fR is intended to bridge the gap between the various
spectrum formats and provide a tool to flexibly manipulate multispec
format spectra.  It replaces the more primitve tasks
\fBmsred.msselect\fR and \fBechelle.ecselect\fR.  Basically, this task
copies all or selected spectra from one format to a new image or images
of the same or different format.  The typical uses are:

.IP \(bu
Extract selected spectra from a multispec format image.
.IP \(bu
Allow converting the voluminous onedspec format from previous reductions
done before the multispec format was introduced into the more compact
multispec format.
.IP \(bu
Splice selected apertures from different multispec images into a new
multispec image.
.IP \(bu
Provide a quick way to convert lines or columns from two dimensional
long slit images into one dimensional spectra.  This replaces
the task \fBproto.toonedspec\fR.
.PP
Because \fBscopy\fR can easily change the number and order of apertures
in the multispec image format it is important that the other tasks which
use the multispec format have been modified to be insensitive to which
line a spectrum is in and generally key off the aperture number.
.PP
The task \fBsapertures\fR is a simple way to set the aperture identifications,
APID keyword, and beam number, second field of APNUM keyword, based on
the aperture number and a simple text file.  The text file contains lines
with aperture number, beam number, and (optional) title.  This file is
used by the \fBapextract\fR package as well.  Its likely usage is
to change image titles which might be wrong because of being inherited
from an aperture reference image during extraction.
.NH
SFIT, CONTINUUM, and ECCONTINUUM
.PP
The original version of \fBcontinuum\fR was a simple script based on
the task \fBfit1d\fR.  The problem is that \fBfit1d\fR is intended to
process all the lines or columns in a two dimensional image
noninteractively.  To do this it applies the same fitting parameters to
every line or column.  The interactive step in this task is simply to
adjust fitting parameters.  For spectra, particularly multispec and
echelle format spectra, one often needs to fit each spectrum
interactively and independently.  When this problem was encountered for
the \fBechelle\fR package Rob Seaman wrote a nice program,
\fBeccontinuum\fR, which allows fitting a set of orders and keeps track
of which orders have been fit.
.PP
The general feature of the continuum fitting tasks is that they fit
spectra using the \fBicfit\fR interactive function fitting interface.
The results of the fit may be output as the fit itself, the difference
or residuals, the ratio, or the input data with rejected points replaced
by the fitted values.  The last feature is new an provides a useful
spectrum cleaning option.  The general equivalent to \fBfit1d\fR is
the new task \fBsfit\fR which provides the same independent fitting and
image line selection capabilites as \fBeccontinuum\fR.  Note this task
is line oriented and does not select by aperture or order number.  The
revised version of \fBcontinuum\fR is now based on \fBsfit\fR and
provides the independent continuum fitting capability for onedspec and
multispec format spectra that \fBeccontinuum\fR provides for echelle
format spectra.  Technically what has been done is that \fBsfit\fR,
\fBcontinuum\fR, and \fBeccontinuum\fR are the same task; essentially
the task written by Seaman for echelle data.  They differ in the
default parameters with the continuum fitting task having default
parameters providing continuum normalization (ratio) output and
iterative rejection values for excluding lines.
.NH
SPLOT, FITPROFS, and SPECPLOT
.PP
\fBSplot\fR has been modified to better support multispec and echelle
format images.  The line selection for multispec and echelle format
spectra is now in terms of the aperture number rather than the image
line.  The aperture title is used in place of the image title
if present.
.PP
The restriction to a maximum of four lines in the gaussian fitting and
deblending option of \fBsplot\fR has been lifted.  Any number of
lines may be fit simultaneously, though execution time will become
long for a large number.  In addition the fitting allows determining
a simultaneous linear background as well as using the cursor defined
points.  The positions of the lines to be fit may be marked with
the cursor, typed in, or read from a file.  The last choice is a new
feature.
.PP
In the past many people have used \fBsplot\fR for bulk, noninteractive
gaussian fitting by going through the trouble of redirecting the cursor
input, ukey input, text output, and graphics output.  The main reason
this has been done is the lack of a one dimensional gaussian fitting
task.  The task \fBfitprofs\fR has been added to provide simultaneous
gaussian fitting.  This task takes a list of positions and optional
sigmas and fits gaussians to a list of images or spectra.  The lines,
columns, or apertures may be selected.  In addition a linear
background may be specified or included in the fitting.  The output
consists of any combination of text similiar to the \fBsplot\fR
logfile, plots showing the data and fit, and image output of the fit or
the difference.  This task is noninteractive; the interactive version
is the deblend command of \fBsplot\fR.  The multiparameter, nonlinear
fitting software is the same as used in \fBsplot\fR.
.PP
\fBFitprofs\fR complements the task \fBstsdas.fitting.ngaussfit\fR from
the \fBstsdas\fR package (available from the Space Telescope Science
Institute).  This task is similar in character to \fBfit1d\fR and has
an interactive one dimensional nonlinear function fitting interface
similar to \fBicfit\fR.
.PP
The task \fBspecplot\fR has a new parameter to select apertures to
plot.  Previously there was no way to limit the apertures plotted other
than with image sections.  All associated lines of a multispec
spectrum (those in the third dimension) are also plotted for the
selected apertures.  This extra information is a new option of the
\fBapextract\fR package.
