.nr PS 10
.nr VS 12
.RP
.ND
.TL
NOAO Spectroscopy Packages Revisions: IRAF Version 2.10.3
.AU
Francisco Valdes
.AI
IRAF Group - Central Computer Services
.K2
P.O. Box 26732, Tucson, Arizona 85726
March 1993
.AB
This paper summarizes the changes in Version 3.1 of the IRAF/NOAO
spectroscopy packages, \fBonedspec\fR, \fBlongslit\fR, \fBapextract\fR, and
those in \fBimred\fR.  These changes are part of IRAF Version 2.10.3.  A
list of the revisions is:

.in +2
.nf
\(bu A simplified \fIequispec\fR image header format
\(bu \fIEquispec\fR format allows a larger number of apertures in an image
\(bu Extensions to allow tasks to work on 3D images
\(bu New task \fBspecshift\fR for applying a zeropoint dispersion shift
\(bu Revised \fBsapertures\fR to edit spectrum coordinate parameters
\(bu Revised \fBdispcor\fR to easily apply multiple dispersion corrections
\(bu Revised \fBscombine\fR weighting and scaling options
\(bu Revised \fBscopy\fR to better handle bands in 3D images
\(bu Revised \fBcalibrate, deredden, dopcor\fR, and \fBspecshift\fR to work on 2D/3D images
\(bu Extended \fBidentify\fR and \fBreidentify\fR to work on 3D images
\(bu New color graphics capabilities in \fBsplot, specplot, sensfunc\fR, and \fBidentify\fR
\(bu All spectral tasks use a common package dispersion axis parameter
\(bu A more complete suite of tasks in the \fBlongslit\fR package
\(bu The \fBimred\fR reductions scripts can now be used with any image format
\(bu A \fIdatamax\fR parameter in the \fBimred\fR reduction scripts for better cleaning
\(bu Revised the \fBimred\fR reduction scripts to abort on non-CCD processed data
\(bu Revised fiber reduction tasks to include a scattered light subtraction option
\(bu Revised fiber reduction tasks to allow as many sky apertures as desired
\(bu Revised \fBdoslit\fR to take the reference arc aperture from the first object
\(bu Bug fixes
.fi
.in -2
.AE
.NH
Spectral Image Formats and Dispersion World Coordinate Systems
.LP
As with the original release of V2.10 IRAF, the primary changes in the
NOAO spectroscopy
software in V2.10.3 are in the area of spectral image formats and dispersion
world coordinate systems (WCS).  A great deal was learned from experience
with the first release and the changes in this release attempt to
address problems encountered by users.  The main revisions are:

.in +2
.nf
\(bu A new WCS format called \fIequispec\fR.
\(bu Extensions to allow use of 3D images with arbitrary dispersion axis.
\(bu Elimination of limits on the number of apertures in an image under certain conditions.
\(bu Improved tools for manipulating the spectral coordinate systems.
\(bu Bug fixes and solutions to problems found in the previous release.
.fi
.in -2

In the previous version all images with multiple spectra used a coordinate
system called \fImultispec\fR.  This type of WCS is complex and difficult
to manipulate by image header editing tools.  Only the case of a single
linearized spectrum per image, sometimes called \fIonedspec\fR format,
provided a simple header format.  However, the \fBapextract\fR package
used the \fImultispec\fR format even in the case of extracting a single
spectrum so to get to the simple format required use of \fBscopy\fR.
.LP
In many cases all the spectra in a multispectrum image have the same linear
dispersion function.  The new \fIequispec\fR format uses a simple linear
coordinate system for the entire image.  This format is produced by the
spectral software whenever possible.  In addition to being simple and
compatible with the standard FITS coordinate representation, the
\fIequispec\fR format also avoids a limitation of the \fImultispec\fR WCS
on the number of spectra in a single image.  This has specific application
to multifiber spectrographs with more than 250 fibers.
.LP
For multiple spectrum data in which the spectra have differing
dispersion functions (such as echelle orders) or when the spectra are
not linearized but use nonlinear dispersion functions, the \fImultispec\fR
format is still used.  It is the most general WCS representation.
The difficulties with modifying this coordinate system, \fBhedit\fR
cannot be used, are addressed by enhancing the \fBsapertures\fR task
and by the new task \fBspecshift\fR which covers the common case of
modifying the dispersion zeropoint.
.LP
A feature of the spectral tasks which operate on one dimensional spectra
is that they can operate on two dimensional long slit spectra by
specifying a dispersion axis and a summing factor.  This feature has
been extended to three dimensional spectra such as occur with
Fabry-Perot and multichannel radio synthesis instruments.  The
dispersion axis may be along any axis as specified by the DISPAXIS
image header keyword or by the \fIdispaxis\fR package parameter.  The
summing factor parameter \fInsum\fR is now a string which may have
one or two values to allow separate summing factors along two spatial
axes.  Also, some additional tasks which previously did not support this
feature are \fBcalibrate\fR, \fBderedden\fR, \fBdopcor\fR, and \fBspecshift\fR.
.LP
The gory details of the spectral image formats and world coordinate
systems are laid out in the new help topic \fIspecwcs\fR (also
available in a postscript version in the IRAF network documentation
archive as iraf/docs/specwcs.ps.Z).
.LP
Some of the bug fixes and solutions to problems found in the previous
release concerning the image formats and WCS are a problem with the WCS
dimensionality (WCSDIM keyword) with 3D images and problems reading various
imported nonstandard formats.  It is hoped that all such formats, including
previous IRAF spectral formats will all be allowed by the software in the
latest release.
.NH
DISPCOR
.LP
The previous versions of \fBdispcor\fR, the dispersion correction task, was
designed to prevent accidental repeated application; it is incorrect to
apply the dispersion function from the original data to a linearized
spectrum.  However, it is valid to determine a new dispersion solution, say
from a dispersion calibrated arc, and apply that as a second correction.
\fBDispcor\fR would not use a new dispersion function, as specified by the
REFSPEC keywords, if the dispersion calibration flag was set.  In order to
override this the user needed to manually change this flag to indicate the
spectrum was uncorrected.  The problem was that it was difficult to do this
with \fImultispec\fR format spectra because the flag is part of the complex
WCS attribute strings.
.LP
\fBDispcor\fR was revised to use a different logic to prevent accidental
recalibration using an unintended dispersion function.  The logic is as
follows.  Previously \fBdispcor\fR would simply change the dispersion
calibration flag after correcting a spectrum while leaving the dispersion
function reference spectrum keywords alone as a record.  The revised
\fBdispcor\fR keeps this useful record but moves this it to a new keyword
DCLOGn (where n is a sequential integer).  Because the REFSPEC keyword is
removed after each application of \fBdispcor\fR it now takes an explicit
act by the user to assign another dispersion function to a spectrum and so
it is not possible to accidentally reapply the same dispersion function
twice.  Thus this version will apply additional dispersion functions by
simply adding new REFSPEC keywords.  If they are absent the task resamples
the spectra based on the current dispersion relation as was the case
before.
.LP
The new version can also tell whether the data was calibrated by the
previous version.  In this case the check on the dispersion calibration
flag is still used so that during the transition users are still protected
against accidentally applying the same reference dispersion function
twice.  The new task \fBsapertures\fR can now be used to change the
dispersion calibration flag to override this checking more easily than was
the case previously.
.NH
New Tasks
.LP
In this release there is only one completely new task and one task which
was significantly redesigned.  The new task is \fBspecshift\fR.  It is
relatively simple, it adds a zero point shift to the dispersion coordinates
of spectra.  This was the most common request for manipulating the spectral
world coordinate system.  In this regard there was a common confusion about
the distinction between shifting the coordinate system and shifting the
pixel data.  Generally what people want is to apply a shift such that
features in the spectrum move to the desired wavelength.  One thought is to
apply the tasks \fBimshift\fR or \fBshiftlines\fR.  The surprise is that
this does not to work.  The pixels are actually shifted in the image array,
but these tasks also apply the same shift to the coordinate system so that
features in the spectrum remain at the same wavelength.  What is really
required is to leave the pixel data alone and shift only the coordinate
system.  That is what \fBspecshift\fR does.
.LP
While one hopefully does not need to directly manipulate the image header
keywords describing the coordinate system or other aspects of the spectra,
instead using such tasks as \fBspecshift\fR, there always seem to be cases
where this is needed or desired.  In the V2.10 release of the spectral
software this was difficult because the general \fImultispec\fR format was
the norm and it has information encoded in the complex WCS attribute
strings.  As mentioned previously several changes have been made reduce the
complexity.  Now \fIequispec\fR format will generally be the rule and this
format has keywords which are more easily manipulated with \fBhedit\fR and
\fBwcsedit\fR.  However, the task \fBsapertures\fR was revised to provide
an editing capability specifically for spectral images, in either
\fImultispec\fR or \fIequispec\fR format, with options to change various
parameters globally or aperture-by-aperture.
.NH
New Features
.LP
There were a number of miscellaneous minor revisions and bug fixes.  One of
the major new capabilities available with V2.10.3 is support for color
graphics if the graphics device supports it.  \fBXgterm\fR supports color
on X-window systems with color monitors.  Several of the spectral tasks
were modified to use different colors for marks and overplots.  These tasks
include \fBsplot\fR, \fBspecplot\fR, \fBidentify\fR, and \fBsensfunc\fR.
In the case of \fBsensfunc\fR the user controls the various color
assignments with a task parameter or \fBgtools\fR colon command while in
other cases the next available color is used.
.LP
There were several changes to \fBscombine\fR equivalent to those in
\fBimcombine\fR.  The weighting, when selected, was changed from the square
root of the exposure time or spectrum statistics to the value with no
square root.  This corresponds to the more commonly used variance
weighting.  Other options were added to specify the scaling and weighting
factors.  These allow specifying an image header keyword or a file
containing the scale or weighting factors.  A new parameter, "nkeep" has
been added to allow controlling the maximum number of pixels rejected by the
clipping algorithms.  Previously it was possible to reject all pixels even
when some of the data was good though with a higher scatter than estimated;
i.e. all pixels might be greater than 3 sigma from the mean without being
cosmic rays or other bad values.  Finally a parameter \fIsnoise\fR was
added to include a sensitivity or scale noise component to a Poisson noise
model.
.LP
In \fBsplot\fR the 'p' and 'u' keys which assign and modify the dispersion
coordinates now include options for applying a zero point shift or a
doppler shift in addition to defining an absolute wavelength for a feature
or starting and ending wavelengths.  There are also bug fixes to the
equivalent width calculations, it did not handle flux calibrated data, and
the scroll keys '(' and ')'.
.LP
There were several changes to make it easier to deal with with three
dimensional \fImultispec\fR and \fIequispec\fR data; that is the additional
data from the "extras" option in the \fBapextract\fR tasks.  One was to fix
problems associated with an incorrect WCSDIM keyword.  This allows use of
image sections or \fBimcopy\fR for extracting specific bands and
apertures.  Another was to add a "bands" parameter in \fBscopy/sarith\fR to
allow selection of bands.  Also the "onedspec" output format in \fBscopy\fR
copies any selected bands to separate one dimensional images.
.LP
As mentioned earlier, many of the \fBonedspec\fR tasks have been extended
to work on 2D and 3D spatial spectra.  Some tasks which now have this
capability in this version and not the previous one are \fBcalibrate\fR and
\fBdopcor\fR.  \fBIdentify\fR and \fBredentify\fR were extended to operate
on 3D images.  This involved extending the syntax for the section parameter
selecting the image vector and the parameter specifying any summing
across the vector direction.
.NH
LONGSLIT
.LP
With the applicability of more \fBonedspec\fR tasks to long slit data
the \fBlongslit\fR package was modified to add many new tasks.
This required adding additional package parameters.  One new task
to point out is \fBcalibrate\fR.  This task is now the preferred one
to use for extinction and flux calibration of long slit spectra
rather than the obsolete \fBextinction\fR and \fBfluxcalib\fR.
The obsolete tasks are still present in this release.
.NH
APEXTRACT
.LP
The \fBapextract\fR package had a few, mostly transparent, changes.  In
the previous version the output image header format was always \fImultispec\fR
even when there was a single spectrum, either because only one aperture
was defined or because the output format parameter was "onedspec".
In this release the default WCS format is the simpler \fIequispec\fR.
.LP
In the \fBonedspec\fR and \fBimred\fR spectral reduction packages there is
a dispersion axis package parameter which is used to defined the dispersion
axis for images without a DISPAXIS keyword.  This applies to all tasks.
However, the \fBapextract\fR tasks had the dispersion axis defined by their
own task parameters resulting in some confusion.  To make things consistent
the dispersion axis parameter in \fBapextract\fR has been moved from the
tasks to a package parameter.  Now in the \fBimred\fR spectral reduction
packages, there is just one dispaxis parameter in the package parameters
which applies to all tasks in those packages, both those from
\fBonedspec\fR and those from \fBapextract\fR.
.LP
Some hidden algorithm parameters were adjusted so that the cleaning and
variance weighting options perform better in some problem cases without
requiring a great deal of knowledge about things to tweak.
.NH
IMRED Spectroscopic Reduction Tasks
.LP
The various spectroscopic reductions tasks, those beginning with "do", have
had some minor revisions and enhancements in addition to those which apply
to the individual tasks which make up these scripts.  In the latter class
is the output WCS format is \fBequispec\fR except for the echelle tasks and
when dispersion linearization is not done.  Related to this is that the
multifiber tasks can operate on data with more than 250 fibers which was a
limitation of the \fBmultispec\fR format.
.LP
In the previous version only the OIF format images were allowed (the ".imh"
extensions).  This has been generalized to allow selecting the image format
by setting the environment parameter \fIimtype\fR.  Only images with the
specified extension will be processed and created.
.LP
The dispersion axis parameter in the reduction tasks and in the other tasks
in the \fBimred\fR spectroscopy packages, such as the \fBapextract\fR
tasks, is now solely a package parameter.
.LP
All the scripts now check the input spectra for the presence of the CCDPROC
keyword and abort if it is not found.  This keyword indicates that the data
have been processed for basic CCD calibrations, though it does not check
the operations themselves.  For data reduced using \fBccdproc\fR this
keyword will be present.  If these tasks are used on data not processed by
\fBccdproc\fR then it is a simple matter to add this keyword with
\fBhedit\fR.  Obviously, the purpose of this change is to avoid
inadvertently operating on raw data.
.LP
All the "do" tasks now have a parameter "datamax".  This minimizes the
effects of very strong cosmic rays during the extraction of object spectra;
it does not apply to flat field or arc spectra.  When there is a very large
difference between data pixel values and cosmic ray pixel values,
especially true for very weak spectra, the cosmic ray cleaning operation
does not always work well.  If it is possible to specify a threshold value
between the maximum real data value and cosmic rays then the cosmic ray
cleaning can be significantly improved by immediately rejecting those
pixels above the threshold.  Of course the user must be careful that real
data does not exceed this value since such data will be excluded.
.LP
The fiber reduction tasks, \fBdoargus, dohydra, dofibers, dofoe\fR, and
\fBdo3fiber\fR have a new processing option for subtracting scattered
light.  This is particularly useful if there is significant scattered light
in producing uniform sky spectra for sky subtraction since the fiber
throughput calibration does not generally correct for this.
.LP
The fiber reduction tasks also had a limit on the number of sky fibers
which could be used with the interactive sky editing.  This limit has
been eliminated so that it is possible, for example, to have one object
fiber and 99 sky fibers.
.LP
The slit reduction task \fBdoslit\fR previously required that the spectrum
for the reference arc cover the middle of the input data images.  There
were cases of instrument configurations  where this was not true requiring
additional manipulation to use this task.  This requirement has been
eliminated.  Instead when the reference arc needs to be extracted it uses
the aperture definition from one of the input object spectra since
definition of the object apertures occurs prior to setting up the
dispersion calibration.
.LP
In addition the task \fBdoslit\fR and \fBdoecslit\fR had a bug in which
the order of the arcs specified by the user was ignored and alphabetical
order was used instead.  This has been fixes so that the first arc
specified by the use is the reference arc.
