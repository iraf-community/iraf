.EQ
delim	$$
.EN
.TL
The Multi-Spectra Extraction Package (multispec)
.AU
Francisco Valdes
.AI
IRAF Group
.K2
October 1984
.NH
Introduction
.PP
This document provides an introduction and overview of the multi-spectra
extraction package \fBmultispec\fR.  Detailed descriptions and usage
information for the tasks of the package are available in the manual
pages.  The tasks in the package are:

.TS
center;
n.
findpeaks \&- Find the peaks
fitfunction \&- Fit a function to the spectra parameter values
fitgauss5 \&- Fit spectra profiles with five parameter Gaussian model
modellist \&- List data and model pixel values
msextract \&- Extract spectra
mslist \&- List entries in a MULTISPEC database
msplot \&- Plot a line of image and model data
msset \&- Set entries in a MULTISPEC database
newextraction \&- Create a new MULTISPEC extraction database
newimage \&- Create a new multi-spectra image
.TE

.PP
The \fBmultispec\fR package is a subpackage of the \fBtwodspec\fR package.
It provides tools to locate, model, clean, correct for blending,
and extract integrated or strip spectra from two dimensional, multi-spectra
images.  These tools may be used directly or combined in scripts to
extract specific types of spectra or spectra from specific instruments.
Examples of the latter usage are the tasks in the image reduction package
\fBcryomap\fR.
.PP
The extraction of spectra consists of locating pixels along each
image line which intersect the spectra and recording either the sum of
the pixels, \fIintegrated spectra\fR (some times referred to as
one-dimensional spectra), or the set of pixels,
\fIstrip spectra\fR, for each line and for each spectrum as output.
The size and limits of the intersection region are specified by the
user relative to the centers of the spectra.
The locations of the spectra in each image line are determined separately
so that the spectra need not be aligned along the columns of the image nor
be perfectly straight.  However, since the extraction is done by image line,
if the spectra are not aligned with the columns then the spectral resolution
will be decreased.  If the spectra are aligned with the image lines then
the image should be rotated or transposed using \fBimtranspose\fR.
.PP
The \fBmultispec\fR extraction produces three dimensional images with
one image band (the third dimension) for each extracted spectrum
and one line (the second dimension) for each extracted image line.
For integrated spectra there is only one column
while for strip spectra, the number of columns is equal to the extraction
strip width.  The strips are aligned to the same positions relative to the
spectra centers by image interpolation.  If desired the output extractions can
be reformated in a variety of ways.
.PP
In addition to direct extraction of the image spectra the \fBmultispec\fR
package provides for modeling the spectrum profiles.  The model
may be extracted instead of the image spectra as either integrated or
strip spectra.  The model may be used to correct for blending of the spectra
and to detect and replace bad pixels.  The cleaning replaces data pixels which
are discrepant from the model by the model values.
.PP
The modeling and cleaning features of the \fBmultispec\fR package can also
be used for creating new multi-spectra images.  In other words a new
image is created containing cleaned or model spectra for selected
lines.
.PP
Section 2 gives an overview of the \fBmultispec\fR package and the extraction
process.  The next section briefly describes the tasks in the package.
This is followed by a description of the extraction database.
The final section defines the model profiles used in the \fBmultispec\fR
package.
.NH
Overview of the Multispec Package and the Extraction Process
.PP
The \fBmultispec\fR package consists of general and flexible tools
for creating and manipulating databases which describe multi-spectra
images.  The contents of the databases are described in a later section.
Each database is associated with a particular image and is referenced
through the image name.  The first positional argument in all the
\fBmultispec\fR tasks is an image.  In the current version of the package each
database exists as a separate binary file with a filename formed by adding
the extension '.db' to the image name.  Note, however, that this
need not be the case in the future.
.PP
The organization of the package as a set of tools operating on a database
allows room for the package to evolve.  Different algorithms may be
designed for different types of multi-spectra images by using combinations
of the existing tools and by adding new tools.  The discussion below
points out areas where new tasks might be added as well as citing the
applicable existing tasks.
.PP
The extraction of spectra from a multi-spectra image consists of two
basic steps; determining the locations of the spectra in the image and
extracting the spectra.  The positions of the spectra in a multi-spectra
image are determined at a set of "sample" image lines.  These positions
are used to fit an interpolation function defining the spectrum positions
at all the image lines.  This function is then used in the extraction of
the spectra.
.PP
The sample image lines are chosen by the user when the database is first
created by the task \fBnewextraction\fR.  An exception to this is when
a template image is used (discussed below).  However, in this case the
sample image lines are still those chosen by the user when the template
image database was created.  The sample image lines may consist of
anywhere from one image line to all the image lines.  The purpose
of the sample lines is to sample the image often enough to follow changes
in the positions and shapes of the spectra but to still minimize the
time spent in finding the spectra and the size of the database.  The choice
of sample lines also depends on the algorithm used to determine the
positions of the spectra; a large number of sample
lines for a fast, approximate method and a smaller number of lines
for a complex and accurate method.  For example, in order to deal with
very blended spectra the task \fBfitgauss5\fR provides a sophisticated
model fitting algorithm.  This technique is computationally slow and, so,
the user should not choose too many sample lines.
.PP
After the database has been created the minimum information needed
for extraction is the spectrum positions at the sample lines.  There
are many ways in which the positions may be determined.  Some
possibilities are listed below.

.IP (1)
Enter the spectrum positions from a list using \fBmsset\fR.  The
list might be generated from a graphics/cursor task.
This is method is very time consuming when the number of spectra and
the number of images are large.
.IP (2)
Determine the spectrum positions automatically by finding the peaks in
each sample image line.  The task \fBfindpeaks\fR performs this function.
.IP (3)
Determine the spectrum positions at just one sample image line
using either (1) or (2) and trace the spectra by a fast and refined
peak finding method.  Such a task is desirable but is not a part of the
current package.
.IP (4)
Determine the spectrum positions at just one sample image line
using either (1) or (2) and trace the spectra by fitting model
spectrum profiles.  The task \fBfitgauss5\fR does this using
the model gauss5 described in section 5.  Additional model fitting
tasks can be added as needed.
.IP (5)
Use the positions determined for a previous image and, if necessary,
refine the positions.  \fBFitgauss5\fR is used to
refine the spectrum positions at each sample line independently.

.PP
Several position finding algorithms may be used in stages to achieve
the degree of accuracy required by the user.
Thus, the first position determinations may be relatively crude and
then, if needed, more sophisticated methods may be applied to refine the
positions.  The task \fBfindpeaks\fR is a crude peak finder.  The positions
are only determined to the nearest pixel.  The task \fBfitgauss5\fR is
a sophisticated model fitting techique which is used after \fBfindpeaks\fR
first determines the approximate positions of the spectra.
.PP
The determination of the spectra locations may be performed independently
at each sample line as in (1) and (2) above or the spectra locations may
be traced starting from one sample line as in (3) and (4).  The second method
is preferable.  Generally, \fBfindpeaks\fR is used at only one sample line
to initially determine the number and approximate locations of the spectra.
\fBFitgauss5\fR then fits model gauss5 to the spectrum profiles and
the model solution is used at the next sample line as the starting
point for the next model fit.  In this manner the positions of
the spectra are determined at the other sample image lines.
.PP
The results of the peak finding and profile fitting are improved
by using an average of many image lines about the sample image line rather
than just the sample image line by itself.  Both \fBfindpeaks\fR and
\fBfitgauss5\fR have this ablility.
.PP
It is often the case that several multi-spectra images have essentially
the same format; i.e. the same image size, the same number of spectra,
and the same positions (either approximately or identically).
Commonly, one of the images is used for calibrations and has strong,
high signal-to-noise spectra while the other images have weaker spectra.
In this case it is not necessary to repeat the position determinations.
The spectrum positions in one of the images, generally the one with
the strong calibration spectra, are determined first.  This image is
then used as a "template" to provide the initial position estimates for
the other images.  If the positions are identical no further work is needed,
otherwise, the positions can be refined to correct for small changes in the
positions and shapes of the spectra.
.PP
The task \fBnewextraction\fR creates new databases.  If a template image
is specified then a copy is made of the template image database.  This means
that the number of spectra and the sample image lines remain the same.
If the spectrum positions are slightly different from the template image
then the task \fBfitgauss5\fR is used to determine the new positions.
.PP
The spectrum positons and possibly any model parameters are interpolated
from the sample lines to the remaining image lines by fitting a function
to values at the sample lines.  In addition, the function fits may
leave out poorly determined points and also smooth the values at the
sample lines.  The task \fBfitfunction\fR fits selected functions of
specified order to the selected spectra and sample image lines.
.PP
The extraction of the spectra from multi-spectra images is performed by
the task \fBmsextract\fR.  The task extracts either integrated or strip
spectra, either data or model values, with or without blending corrections,
and with or without replacing bad pixels by model values.
The user specifies the limits of the extraction
strip as well as the spectra and image lines to be extracted.
.PP
For the simplest type of data extractions (basically strip extraction)
no modeling is required.  Other types of extractions, such as model
extractions and/or with cleaning and blending corrections require some
degree of modeling.  There are two models which may be used;
"smooth" and "gauss5".  These models are described in section 5.
The model parameters for model gauss5 must be set by \fBfitgauss5\fR
before \fBmsextract\fR is used.  Additional models may added for
extraction as well as for the spectrum position determinations.
.PP
The model based features of \fBmsextract\fR -- model extractions
and cleaning -- are available in the related task \fBnewimage\fR.
This task creates new images which consist of either model spectra
or cleaned data spectra.
.PP
The models in the \fBmultispec\fR package assume that the profiles
go to zero; i.e. there is no background light.  Background light
may be removed using \fBbackground\fR.  In the future a task will
be provided create a mask defining the locations of the spectra from
the database which can be used with general surface fitting tasks
to create a background surface to be subtracted from the image.
.PP
The final step in using the \fBmultispec\fR package is to convert the
extraction output to the desired format.  This may include graphs,
card image formats, and files for the \fBonedspec\fR and \fBlongslit\fR
packages.  Currently, the available formats are images and IIDS
card images.
.NH
The Tasks of the Multispec Package
.PP
Use of the \fBmultispec\fR package begins with \fBnewextraction\fR and
ends, usually, with \fBmsextract\fR.  In between there are tasks which
update, refine or change the database and tasks which provide diagnositic
information.  The informational tasks can be combined with tasks from
other packages to produce tabular or graphical output.  The task
\fBmsplot\fR is an example.  In this section a brief description of
each task is given.  Further information about the tasks, including usage,
is available in the manual pages.
.SH
findpeaks
.IP
Selected sample image lines are examined to determine the number and
column positions of data peaks in the line.  An average of a number of image
lines surrounding the sample lines is formed in which the local maxima
are located.  Various criteria are applied to cull the list of local
maxima to the desired peaks.  These criteria include a peak threshold,
a maximum peak-to-peak contrast, a minimum peak separation, and a
maximum number of peaks.  This task is used to determine crude, initial
estimates for the spectrum positions.  It could be used alone for
simple extractions.
.SH
fitfunction
.IP
This task has two roles.  It's primary role is to define the
interpolation/extrapolation function for the spectra
positions between the sample lines.  The fitting function can be
either purely interpolative or may also provide smoothing of the
parameters from the sample lines.  The second role is to provide
smoothing of the model parameters along the dispersion and the
ability to replace bad values by the function fit to the remaining
parameters.  In this second role the user may iterate between
smoothing and model fittng.  The functions are always defined between
the first and last image lines.
.SH
fitgauss5
.IP
The model profiles gauss5, described in section 5, are fit to the
selected spectra and sample lines.  The parameters to be determined
and the fitting algorithm may also be selected.
The model parameters are recorded in the database.
The model may be tracked from a starting line to other sample image
lines or each sample line may be fitted independently.
This task is used to accurately determine the spectrum positions
and provide an extraction model for heavily blended spectra.
.SH
modellist
.IP
For the selected sample image lines and image columns data
and model values are listed.  This task is used to check how well
the model fitting tasks (currently just \fBfitgauss5\fR) have fit
the sample image line.  The task \fBmsplot\fR is used to produce
graphical output.
.SH
msextract
.IP
This task does the actual extraction of spectra.  It requires that
the spectrum positions are defined by fitting functions in the
database.  If model gauss5 is to be used then the database must
also contain the model parameters for the sample image lines.  It
extracts integrated or strip spectra, using data or model values,
with or without blending corrections, and with or without cleaning
of bad pixels.
.SH
mslist
.IP
Of the diagnositic or informational tasks \fBmslist\fR is the most
general.  The user selects the type of information from the database
which is desired and it is then printed.  The types of information
include the database header, the database comments, the spectra
positions and model parameter values for the sample lines, and the
interpolation/smoothing function values for any desired set of
image lines.
.SH
msplot
.IP
This task extracts data and models values and plots them superposed.
This task is used as a diagnositic tool to inspect how well model fitting
represents the image spectra.
.SH
msset
.IP
This task is a general tool for modifying or setting some of the quantities 
in the database.  The quantity to be changed or set is
selected by a keyword and the values are input in two ways;
with a list structured parameter (such a file containing the list of
values or the standard input) or as a parameter value.  This task
is the way a user may enter comments in the database or manually
set the number and positions of the spectra.  It is also used to
set the initial values for the gauss5 model parameters s0, s1, and s2
prior to using \fBfitgauss5\fR.
.SH
newextraction
.IP
This task has three important roles.  First it creates the database
associated with the multi-spectra image.  Second, it defines the sample
image lines to be used.  The user can specify as many or as few sample lines
as desired.  It should be kept in mind that the more sample lines used
the larger the database becomes and the longer the processing time when
modeling the spectra.  Finally, \fBnewextraction\fR allows
a database from another image (called a template image) to initialize the
database for the new multi-spectra image.  The template image is generally
a calibration image with strong, well-defined spectra.
Initializing a database with a template image saves time, reduces problems
with bad pixels, and is more accurate when an image with weak spectra is
to be extracted.
.SH
newimage
.IP
This task is similar to \fBmsextract\fR; it uses the same algorithms
and parameters.  It differs in the type output.
Rather than producing extracted integrated or strip spectra this task
produces new image lines.  It is particularly useful for extracting
model images to be compared against the original image or to
produce images which have been cleaned.
.NH
The Multispec Database
.PP
The tasks in the \fBmultispec\fR package create and manipulate a database.
The database contains a description of the multi-spectra image which
is modified, refined, examined, or otherwise used by the tasks in the package.
In the current version the database is a separate binary file with a filename
formed by appending ".db" to the image name described by the database.
.PP
The database contains four basic types of data; general information,
comments and history, position parameters, and model parameters.
The data in the database is examined with the task \fBmslist\fR.
The general information section, called the database header, contains the
the name of the image, the size of the image, and the number of spectra in
the image.  Once the number of spectra in the image has
been entered in the database it is an error to attempt to change this
number.  The database must be deleted and a new database created in order
to change the number of spectra.
.PP
The comment and history section of the database contains text
strings.  Each task which modifies the contents of the database places
a dated history line in this section.  The user may also add comments
with \fBmsset\fR.  Currently this information is not passed on to
the extraction output.
.PP
There are three types of position information in the database.  The
first is a set of sample image lines.  The sample lines are set when
the database is created by \fBnewextraction\fR.  The sample lines select
which image lines from the multi-spectra image are to be examined and used
during the extraction.  Information from these sample lines, and only
these sample lines, is entered in the database.  The sample lines
may be listed with \fBmslist\fR.
.PP
The second type of position information is the positions of the
spectra (centers) at each sample line.  These positions are initially
set by either \fBfindpeaks\fR or, manually, by \fBmsset\fR.  The
position information is refined by fitting model profiles.
.PP
The third type of position information is a function fit to the
positions from all the sample lines for each spectrum.
These function fits are produced by \fBfitfunction\fR.
The functions define the positions of the spectra at all the image
lines.  The spectra positions at the sample lines or the function
evaluation for any image line may be listed with \fBmslist\fR.
.PP
The finally type of basic data contained in the database are
model parameter values.  A model need not be used in the extraction
but if one is used then the parameters determining the model profiles
are recorded in the database.  The specific parameters depend on the
model.  Currently the only model is \fIgauss5\fR.  The model and its
parameters are described in section 5.
.PP
As with the spectra positions the parameters are stored in the database
in two forms; as values for each spectrum at each sample image line
and as function fits to the values at the sample lines which interpolate
them to any image line.  The sample line values are
set by the model fitting tasks and the function fits are set by
\fBfitfunction\fR.  The parameter values at the sample lines or the
function evaluations for any image lines may be listed with \fBmslist\fR.
.NH
Multispec Spectrum Profile Models
.PP
The spectra profiles in the image are modeled for many reasons:
To provide accurate, subpixel position determinations, to extract model
spectra or model images, to detect and replace bad pixels, and
to estimate and correct for blending between the spectra.
There are currently two models used in the \fBmultispec\fR package, "gauss5"
and "smooth".
.NH 2
Model Gauss5
.PP
The gauss5 model profiles are Gaussian but with a scale which varies
smoothly between the center and the edge of the profile.  There
are five parameters:

.RS
.IP x0
The column position in the image line of the center of the profile.
.IP i0
The intensity scale of the profile.  It corresponds to the intensity
of the center of the profile.
.IP s0
The zeroth order, constant, term in the Gaussian scale.
.IP s1
The even first order term in the Gaussian scale.
.IP s2
The odd first order term in the Gaussian scale.
.RE

.PP
The mathematical form of the the model is shown in equation (1):
.EQ (1)
roman profile (x)~=~i0 exp~left { -s( DELTA x )~DELTA x sup 2 right }
.EN
where
.EQ
DELTA x ~=~x~-~x0~,
.EN
.EQ
s( DELTA x)~=~s0~+~s1~|y| +~s2~y~,
.EN
and
.EQ
y~=~ DELTA x / ( DELTA x sup 2 + alpha ) sup half ~.
.EN
The profile is defined within the user specified limits \fIlower\fR and
\fIupper\fR measured relative to the the profile center and
$alpha~=~(upper-lower)/4$.  The quantity $y$ lies in the range
-1 to 1 over the interval in which the profile is defined.  The odd
and even terms, s1 and s2, allow for symmetric and antisymmetric profile
changes relative to a simple Gaussian profile.
.PP
The task \fBfitgauss5\fR fits the gauss5 model to the spectrum profiles in
the sample image lines to determine one or more of the model parameters for
each spectrum.  The parameter values are stored in the database for the image.
In \fBmsextract\fR the model profiles for each
image line are obtained by interpolating the profile shapes from the sample
lines (with the model parameters in the database determined by
\fBfitgauss5\fR) and then fitting only the intensity scale "i0".
There are a number of technical details associated with the model fitting
in each of these tasks which are discussed in the manual pages.
.PP
The gauss5 model is used to accurately determine the positions of the
spectrum centers at the sample image lines.  Fitting simultaneously
for the model parameters allows the spectra to be blended.
This is the chief advantage of this model.
This model is also used during extraction to correct for blending of
the spectra and to detect and replace bad pixels.
.NH 2
Model Smooth
.PP
The spectrum profiles from the lines immediately preceeding
the image line in which the spectrum profile is to be fit are shifted
to a common center and averaged to form the model profile.
An intensity scale factor is then determined which best fits the model
profile to the image profile.  This is done for each spectrum in the
image.  The scale factors are determined by least squares with
possible bad pixel rejection.  Rejected pixels are eliminated
when the image line is later used in forming new average model profiles.
.PP
The advantages of this model are that the image spectrum profiles may
have any shape and the least squares fitting with bad pixel rejection
is fast and rigorous.  By passing through the image lines sequentially
the image lines need be accessed only once and the profile averages
can be quickly updated for the next image line.
.PP
The disadvantages of this model are that the spectrum profiles cannot
be blended and the model does not measure profile positions.
This means that the spectrum profile positions must be
known.  This model is suitable for model extractions and cleaning of
bad pixels in unblended multi-spectra images.  It is available in
the task \fBmsextract\fR.
.bp
.SH
Glossary
.LP
\fBmultispec\fR
.IP
Acronym for Multi-Spectra Extraction as in \fBmultispec\fR Package.
.LP
integrated spectra
.IP
The spectra are extracted by integrating the pixel values across the spectrum
to produce a single aperture luminosity value.
.LP
sample image line
.IP
The spectra positions and model profile shapes are determined at a set
of image lines selected when the database is created.
.LP
strip spectra
.IP
The spectra are extracted as a strip of fixed with the spectra shifted by
image interpolation to a common center.
