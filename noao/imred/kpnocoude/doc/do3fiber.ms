.nr PS 9
.nr VS 11
.de V1
.ft CW
.nf
..
.de V2
.fi
.ft R
..
.de LS
.br
.in +2
..
.de LE
.br
.sp .5v
.in -2
..
.ND February 1993
.TL
Guide to the Coude Three Fiber Reduction Task DO3FIBER
.AU
Francisco Valdes
.AI
IRAF Group - Central Computer Services
.K2
.DY

.AB
The \fBdo3fiber\fR reduction task is specialized for scattered light
subtraction, extraction, flat
fielding, and wavelength calibration of multifiber data in which some
fibers are used to take object spectra and other fibers are used to
take simultaneous arc spectra.  A three fiber instrument of this
type (one object and two arc fibers) is available at the KPNO coude feed.
The default parameters are set for this configuration.
If there are a large number of fibers and fiber throughput and sky
fiber subtraction is needed the \fBdofiber\fR task should be used.
.LP
The \fBdo3fiber\fR task is a command language script which collects
and combines the functions and parameters of many general purpose tasks to
provide a single complete data reduction path.  The task provides a degree
of guidance, automation, and record keeping necessary when dealing with
this type of multifiber data.
.AE
.NH
Introduction
.LP
The \fBdo3fiber\fR reduction task is specialized for scattered light
subtraction, extraction, flat
fielding, and wavelength calibration of multifiber data in which some
fibers are used to take object spectra and other fibers are used to
take simultaneous arc spectra.  A three fiber instrument of this
type (one object and two arc fibers) is available at the KPNO coude feed.
The default parameters are set for this configuration.
If there are a large number of fibers and fiber throughput and sky
fiber subtraction is needed the \fBdofiber\fR task should be used.
.LP
The \fBdo3fiber\fR task is a command language script which collects
and combines the functions and parameters of many general purpose tasks to
provide a single complete data reduction path.  The task provides a degree
of guidance, automation, and record keeping necessary when dealing with
this type of multifiber data.
.LP
The general organization of the task is to do the interactive setup steps
first using representative calibration data and then perform the majority
of the reductions automatically, and possibly as a background process, with
reference to the setup data.  In addition, the task determines which setup
and processing operations have been completed in previous executions of the
task and, contingent on the \f(CWredo\fR and \f(CWupdate\fR options, skip or
repeat some or all the steps.
.LP
The description is divided into a quick usage outline followed by details
of the parameters and algorithms.  The usage outline is provided as a
checklist and a refresher for those familiar with this task and the
component tasks.  It presents only the default or recommended usage.  Since
\fBdo3fiber\fR combines many separate, general purpose tasks the
description given here refers to these tasks and leaves some of the details
to their help documentation.
.NH
Usage Outline
.LP
.IP [1] 6
The images are first processed with \fBccdproc\fR for overscan,
bias, and dark corrections.
The \fBdo3fibers\fR task will abort if the image header keyword CCDPROC,
which is added by \fBccdproc\fR, is missing.  If the data processed outside
of the IRAF \fBccdred\fR package then a dummy CCDPROC keyword should be
added to the image headers; say with \fBhedit\fR.
.IP [2]
Set the \fBdo3fiber\fR parameters with \fBeparam\fR.  Specify the object
images to be processed, the flat field image as the aperture reference and
the flat field, and one or more arc images.  If there are many
object or arc spectra per setup you might want to prepare "@ files".
.IP [3]
Run the task.  This may be repeated multiple times with different
observations and the task will generally only do the setup steps
once and only process new images.  Queries presented during the
execution for various interactive operations may be answered with
"yes", "no", "YES", or "NO".  The lower case responses apply just
to that query while the upper case responses apply to all further
such queries during the execution and no further queries of that
type will be made.
.IP [4]
The apertures are defined using the specified aperture reference image
which is usually a flat field in which both the object and arc fibers are
illuminated.  The specified number of fibers are found automatically and
sequential apertures assigned.
.IP [5]
A query is given allowing the apertures to be interactively reviewed.
In this mode one may adjust the aperture widths as desired either
explicitly (:lower and :upper), with the cursor ('l' and 'u'), at a
particular flux level ('y'), or with an automatic algorithm ('z')
as described by \fBapresize\fR.  To exit type 'q'.
.IP [6]
The fiber positions at a series of points along the dispersion are measured
and a function is fit to these positions.  This may be done interactively to
adjust the fitting parameters.  Not all fibers need be examined and the "NO"
response will quit the interactive fitting.  To exit the interactive
fitting type 'q'.
.IP [7]
If scattered light subtraction is to be done the flat field image is
used to define the scattered light fitting parameters interactively.
If one is not specified then the aperture reference image is used for
this purpose.

There are two queries for the interactive fitting.  A graph of the
data between the defined reference apertures separated by a specified
buffer distance is first shown.  The function order and type may be
adjusted.  After quiting with 'q' the user has the option of changing
the buffer value and returning to the fitting, changing the image line
or column to check if the fit parameters are satisfactory at other points,
or to quit and accept the fit parameters.  After fitting all points
across the dispersion another graph showing the scattered light from
the individual fits is shown and the smoothing parameters along the
dispersion may be adjusted.  Upon quiting with 'q' you have the option
of checking other cuts parallel to the dispersion or quiting and finishing
the scattered light function smoothing and subtraction.

If there is a throughput image then this is corrected for scattered light
noninteractively using the previous fitting parameters.
.IP [8]
If flat fielding is to be done the flat field spectra are extracted.  The
average spectrum over all fibers is determined and a function is fit
interactively (exit with 'q').  This function is generally of sufficiently
high order that the overall shape is well fit.  This function is then used
to normalize the individual flat field spectra.
The final response spectra are normalized to a unit
mean over all fibers.
.IP [9]
If dispersion correction is selected the first arc in the arc list is
extracted.  The middle fiber is used to identify the arc lines and define
the dispersion function using the task \fBautoidentify\fR.  The
\fIcrval\fR and \fIcdelt\fR parameters are used in the automatic
identification.  Whether or not the automatic identification is
successful you will be shown the result of the arc line identification.
If the automatic identification is not successful identify a few arc
lines with 'm' and use the 'l' line list identification command to
automatically add additional lines and fit the dispersion function.  Check
the quality of the dispersion function fit with 'f'.  When satisfied exit
with 'q'.
.IP [10]
The remaining fibers are automatically reidentified.  You have the option
to review the line identifications and dispersion function for each fiber
and interactively add or delete arc lines and change fitting parameters.
This can be done selectively, such as when the reported RMS increases
significantly.
.IP [11]
If the spectra are to be resampled to a linear dispersion system
(which will be the same for all spectra) default dispersion parameters
are printed and you are allowed to adjust these as desired.
.IP [12]
The object spectra are now automatically scattered light subtracted,
 extracted, flat fielded, and dispersion corrected.
The reference apertures are first assigned
to the object spectra.  If the \f(CWrecenter\fR option is set the apertures
will have a shift applied based on recentering the fiber profiles.
If the \f(CWedit\fR option is set you may review and modify
the aperture definitions interactively.  Any new
arcs assigned to the object images are automatically extracted and
dispersion functions determined.  A zero point wavelength correction
is computed from the arc fiber spectra and applied to the object spectrum.
.IP [13]
The option to examine the final spectra with \fBsplot\fR may be given.
To exit type 'q'.
.IP [14]
If scattered light is subtracted from the input data a copy of the
original image is made by appending "noscat" to the image name.
If the data are reprocessed with the \fIredo\fR flag the original
image will be used again to allow modification of the scattered
light parameters.

The final spectra will have the same name as the original 2D images
with a ".ms" extension added.
.NH
Spectra and Data Files
.LP
The basic input consists of multifiber object and calibration spectra
stored as IRAF images.
The type of image format is defined by the
environment parameter \fIimtype\fR.  Only images with that extension will
be processed and created.
There are two types of calibration images.  These
are flat fields and comparison lamp arc spectra.  The raw CCD images must
be processed to remove overscan, bias, and dark count effects.  This is
generally done using the \fBccdred\fR package.
The \fBdo3fiber\fR task will abort if the image header keyword CCDPROC,
which is added by \fBccdproc\fR, is missing.  If the data processed outside
of the IRAF \fBccdred\fR package then a dummy CCDPROC keyword should be
added to the image headers; say with \fBhedit\fR.
Flat fielding is generally
not done at this stage but as part of \fBdo3fiber\fR.  If for some reason
the flat field or calibration arc spectra have separate exposures through
different fibers they may be simply added.
.LP
The assignment of arc calibration exposures to object exposures is
generally done by selecting the nearest in time and interpolating.
However, the optional \fIarc assignment table\fR may be used to explicitly
assign arc images to specific objects.  The format of this file is
described in the task \fBrefspectra\fR.
.LP
The final reduced spectra are recorded in one, two or three dimensional IRAF
images.  The images have the same name as the original images with an added
".ms" extension.  Each line in the reduced image is a one dimensional
spectrum with associated aperture, wavelength, and identification
information.  With a single object spectrum the image will be one dimensional
and with multiple object spectra the image will be two dimensional.
When the \f(CWextras\fR parameter is set the images will be three
dimensional (regardless of the number of apertures) and the lines in the
third dimension contain additional information (see
\fBapsum\fR for further details).  These spectral formats are accepted by the
one dimensional spectroscopy tasks such as the plotting tasks \fBsplot\fR
and \fBspecplot\fR.
.NH
Package Parameters
.LP
The \fBkpnocoude\fR package parameters, shown in Figure 1, set parameters
affecting all the tasks in the package.  Some of the parameters are not
applicable to the \fBdo3fiber\fR task.
.KS
.V1

.ce
Figure 1: Package Parameters for KPNOCOUDE

                           I R A F
            Image Reduction and Analysis Facility
PACKAGE = imred
   TASK = kpnocoude

(extinct= onedstds$kpnoextinct.dat) Extinction file
(caldir = onedstds$spec50cal/) Standard star calibration directory
(observa=  observatory) Observatory of data
(interp =        poly5) Interpolation type
(dispaxi=            2) Image axis for 2D images
(nsum   =            1) Number of lines/columns to sum for 2D images

(databas=     database) Database
(verbose=           no) Verbose output?
(logfile=      logfile) Log file
(plotfil=             ) Plot file

(records=                     ) Record number extensions
(version= KPNOCOUDE V3: January 1992)

.KE
.V2
The observatory parameter is only required for data
without an OBSERVAT header parameter (currently included in NOAO data).
The spectrum interpolation type might be changed to "sinc" but with the
cautions given in \fBonedspec.package\fR.  The dispersion axis parameter is
only needed if a DISPAXIS image header parameter is not defined.  The other
parameters define the standard I/O functions.  The verbose parameter
selects whether to print everything which goes into the log file on the
terminal.  It is useful for monitoring what the \fBdo3fiber\fR task does.  The
log and plot files are useful for keeping a record of the processing.  A
log file is highly recommended.  A plot file provides a record of
apertures, traces, and extracted spectra but can become quite large.
The plotfile is most conveniently viewed and printed with \fBgkimosaic\fR.
.NH
Processing Parameters
.LP
The \fBdo3fiber\fR parameters are shown in Figure 2.
.KS
.V1

.ce
Figure 2: Parameter Set for DO3FIBER

                           I R A F
            Image Reduction and Analysis Facility
PACKAGE = kpnocoude
   TASK = do3fiber

objects =               List of object spectra
(apref  =             ) Aperture reference spectrum
(flat   =             ) Flat field spectrum
(arcs   =             ) List of arc spectra
(arctabl=             ) Arc assignment table (optional)

.KE
.V1
(readnoi=      RDNOISE) Read out noise sigma (photons)
(gain   =         GAIN) Photon gain (photons/data number)
(datamax=        INDEF) Max data value / cosmic ray threshold
(fibers =            3) Number of fibers
(width  =           6.) Width of profiles (pixels)
(crval  =        INDEF) Approximate wavelength
(cdelt  =        INDEF) Approximate dispersion
(objaps =            2) Object apertures
(arcaps =          1,3) Arc apertures

(scatter=           no) Subtract scattered light?
(fitflat=          yes) Fit and ratio flat field spectrum?
(recente=          yes) Recenter object apertures?
(edit   =           no) Edit/review object apertures?
(clean  =           no) Detect and replace bad pixels?
(dispcor=          yes) Dispersion correct spectra?
(splot  =          yes) Plot the final spectrum?
(redo   =           no) Redo operations if previously done?
(update =          yes) Update spectra if cal data changes?
(batch  =           no) Extract objects in batch?
(listonl=           no) List steps but don't process?

(params =             ) Algorithm parameters

.V2
The input images are specified by image lists.  The lists may be
a list of explicit, comma separate image names, @ files, or image
templates using pattern matching against file names in the directory.
The aperture reference spectrum is used to find the spectrum profiles and trace
them.  Thus, this requires an image with good signal in all fibers
which usually means a flat field spectrum.  It is recommended that
flat field correction be done using one dimensional extracted spectra
rather than as two dimensional images.  This is done if a flat field
spectrum is specified.  The arc assignment table is used to specifically
assign arc spectra to particular object spectra and the format
of the file is described in \fBrefspectra\fR.
.LP
The detector read out noise and gain are used for cleaning and variance
(optimal) extraction.
The variance
weighting and cosmic-ray cleanning are sensitive to extremely strong
cosmic-rays; ones which are hundreds of times brighter than the
spectrum.  The \fIdatamax\fR is used to set an upper limit for any
real data.  Any pixels above this value will be flagged as cosmic-rays
and will not affect the extractions.
The dispersion axis defines the wavelength direction
of spectra in the image if not defined in the image header by the keyword
DISPAXIS.  The width parameter (in pixels) is used for the profile finding and
centering algorithm (\fBcenter1d\fR).
.LP
The number of fibers is fairly obvious.  It is the number of
fibers, including the arc fibers, to be automatically found and
assigned apertures.  The apertures are assigned aperture
numbers sequentially.  The object and arc fibers are identified
by these aperture numbers as specified by the \f(CWobjaps\fR and
\f(CWarcaps\fR parameters.  The defaults are for the case of three
fibers in the sequence arc fiber, object fiber, and arc fiber.
.LP
The approximate central wavelength and dispersion are used for the
automatic identification of the arc reference.  They may be specified
as image header keywords or values.  The INDEF values search the
entire range of the coordinate reference file but the automatic
line identification algorithm works much better and faster if
approximate values are given.
.LP
The next set of parameters select the processing steps and options.  The
scattered light option allows fitting and subtracting a scattered light
surface from the input object and flat field.  If there is significant
scattered light which is not subtracted the fiber throughput correction
will not be accurate.  The
flat fitting option allows fitting and removing the overall shape of the
flat field spectra while preserving the pixel-to-pixel response
corrections.  This is useful for maintaining the approximate object count
levels and not introducing the reciprocal of the flat field spectrum into
the object spectra.
.LP
The apertures defined for the aperture reference image are assigned to
each image.  For the object images the apertures may be shifted across
the dispersion by recentering the strongest profiles and averaging
the individual shifts to form a single shift for all apertures.  This
corrects for shifts in the detector during the observations.  The
\f(CWrecenter\fR parameter selects whether to apply this shift or not.
.LP
The \f(CWedit\fR option allows you to be queried to review the apertures
assigned to each object image.  If selected and the query answered
affirmatively the apertures may be interactively shifted and resized.  The
query may also be answered with "NO" to turn off this option during
processing.  Note that the initial aperture definitions for the aperture
reference image always allows editing.
.LP
The \f(CWclean\fR option invokes a profile fitting and deviant
point rejection algorithm as well as a variance weighting of points in the
aperture.  These options require knowing the effective (i.e. accounting for
any image combining) read out noise and gain.  For a discussion of cleaning
and variance weighted extraction see \fBapvariance\fR and
\fBapprofiles\fR.
.LP
The dispersion correction option selects whether to extract arc spectra,
determine dispersion functions, assign them to the object spectra, and,
possibly, resample the spectra to a linear (or log-linear) wavelength
scale.
.LP
The \f(CWsplot\fR option allows a query (which may be answered with "YES"
or "NO" to eliminate the query) and then plotting of the final object
spectra if answered affirmatively.  The plotting is done with the
task \fBsplot\fR.
.LP
Generally once a spectrum has been processed it will not be reprocessed if
specified as an input spectrum.  However, changes to the underlying
calibration data can cause such spectra to be reprocessed if the
\f(CWupdate\fR flag is set.  The changes which will cause an update are a new
reference image, new flat field, and a new arc reference image.  If all
input spectra are to be processed regardless of previous processing the
\f(CWredo\fR flag may be used.  Note that reprocessing clobbers the
previously processed output spectra.
.LP
The \f(CWbatch\fR processing option allows object spectra to be processed as
a background or batch job.  This will only occur if the aperture editing
and final spectrum plotting have been turned off, either with the task
option parameter or by answering "NO" to the queries.  The \f(CWlistonly\fR
option prints a summary of the processing steps which will be performed on
the input spectra without actually doing anything.  This is useful for
verifying which spectra will be affected if the input list contains
previously processed spectra.  The listing does not include any arc spectra
which may be extracted to dispersion calibrate an object spectrum.
.LP
The last parameter (excluding the task mode parameter) points to another
parameter set for the algorithm parameters.  The way \fBdo3fiber\fR works
this may not have any value and the parameter set \fBparams\fR is always
used.  The algorithm parameters are discussed further in the next section.
.NH
Algorithms and Algorithm Parameters
.LP
This section summarizes the various algorithms used by the \fBdo3fiber\fR
task and the parameters which control and modify the algorithms.  The
algorithm parameters available to the user are collected in the parameter
set \fBparams\fR.  These parameters are taken from the various general
purpose tasks used by the \fBdo3fiber\fR processing task.  Additional
information about these parameters and algorithms may be found in the help
for the actual task executed.  These tasks are identified in the parameter
section listing in parenthesis.  The aim of this parameter set organization
is to collect all the algorithm parameters in one place separate from the
processing parameters and include only those which are relevant for
this type of data.  The parameter values can be changed from the
defaults by using the parameter editor,
.V1

	cl> epar params

.V2
or simple typing \f(CWparams\fR.  The parameter editor can also be
entered when editing the \fBdo3fiber\fR parameters by typing \f(CW:e
params\fR or simply \f(CW:e\fR if positioned at the \f(CWparams\fR
parameter.  Figure 3 shows the parameter set.
.KS
.V1

.ce
Figure 3: Algorithm Parameter Set

                           I R A F
            Image Reduction and Analysis Facility
PACKAGE = kpnocoude
   TASK = params

(line   =        INDEF) Default dispersion line
(nsum   =           10) Number of dispersion lines to sum
(extras =           no) Extract sky, sigma, etc.?

                        -- DEFAULT APERTURE LIMITS --
(lower  =          -3.) Lower aperture limit relative to center
(upper  =           3.) Upper aperture limit relative to center

                        -- AUTOMATIC APERTURE RESIZING PARAMETERS --
(ylevel =         0.05) Fraction of peak or intensity for resizing

.KE
.KS
.V1
                        -- TRACE PARAMETERS --
(t_step =           10) Tracing step
(t_funct=      spline3) Trace fitting function
(t_order=            2) Trace fitting function order
(t_niter=            1) Trace rejection iterations
(t_low  =           3.) Trace lower rejection sigma
(t_high =           3.) Trace upper rejection sigma

.KE
.KS
.V1
                        -- SCATTERED LIGHT PARAMETERS --
(buffer =           1.) Buffer distance from apertures
(apscat1=             ) Fitting parameters across the dispersion
(apscat2=             ) Fitting parameters along the dispersion

.KE
.KS
.V1
                        -- APERTURE EXTRACTION PARAMETERS --
(weights=         none) Extraction weights (none|variance)
(pfit   =        fit1d) Profile fitting algorithm (fit1d|fit2d)
(lsigma =           3.) Lower rejection threshold
(usigma =           3.) Upper rejection threshold
(nsubaps=            1) Number of subapertures

.KE
.KS
.V1
                        -- FLAT FIELD FUNCTION FITTING PARAMETERS --
(f_inter=          yes) Fit flat field interactively?
(f_funct=      spline3) Fitting function
(f_order=           20) Fitting function order

.KE
.KS
.V1
                        -- ARC DISPERSION FUNCTION PARAMETERS --
(coordli= linelists$idhenear.dat) Line list
(match  =          10.) Line list matching limit in Angstroms
(fwidth =          3.5) Arc line widths in pixels
(cradius=           4.) Centering radius in pixels
(i_funct=     legendre) Coordinate function
(i_order=            3) Order of dispersion function
(i_niter=            3) Rejection iterations
(i_low  =           3.) Lower rejection sigma
(i_high =           3.) Upper rejection sigma
(refit  =          yes) Refit coordinate function when reidentifying?
(addfeat=           no) Add features when reidentifying?

.KE
.KS
.V1
                        -- AUTOMATIC ARC ASSIGNMENT PARAMETERS --
(select =       interp) Selection method for reference spectra
(sort   =           jd) Sort key
(group  =          ljd) Group key
(time   =           no) Is sort key a time?
(timewra=          17.) Time wrap point for time sorting

.KE
.KS
.V1
                        -- DISPERSION CORRECTION PARAMETERS --
(lineari=          yes) Linearize (interpolate) spectra?
(log    =           no) Logarithmic wavelength scale?
(flux   =          yes) Conserve flux?

.KE
.V2
.NH 2
Aperture Definitions
.LP
The first operation is to define the extraction apertures, which include
the aperture width and position dependence with wavelength, for the object
and arc fibers.  This is done on a reference spectrum which is usually a
flat field taken through both fibers.  Other spectra will inherit the
reference apertures and may apply a correction for any shift of the orders
across the dispersion.  The reference apertures are defined only once
unless the \f(CWredo\fR option is set.
.LP
The selected number of fibers are found automatically by selecting the
highest peaks in a cut across the dispersion.  Apertures are assigned with
a limits set by the \f(CWlower\fR and \f(CWupper\fR parameter and numbered
sequentially.  A query is then given allowing the apertures to be reviewed
interactively.  If answered affirmatively a cut across the orders is shown
with the apertures marked and an interactive aperture editing mode is
entered (see \fBapedit\fR).  The main thing to be concerned about is that
the aperture numbers agree with the \f(CWobjaps\fR and \f(CWarcaps\fR
definitions.  The aperture numbers may be changed with the 'i' or 'o'
keys.  The apertures may also be resized from the default limits.
To exit the background and aperture editing steps type 'q'.
.LP
Next the positions of the fiber profiles at various points along the
dispersion are measured and a "trace function" is fit.  The user is asked
whether to fit the trace function interactively.  This is selected to
adjust the fitting parameters such as function type and order.  When
interactively fitting a query is given for each aperture.  After the first
aperture one may skip reviewing the other traces by responding with "NO".
Queries made by \fBdo3fiber\fR generally may be answered with either lower
case "yes" or "no" or with upper case "YES" or "NO".  The upper case
responses apply to all further queries and so are used to eliminate further
queries of that kind.
.LP
The above steps are all performed using tasks from the \fBapextract\fR
package and parameters from the \fBparams\fR parameters.  As a quick
summary, the dispersion direction of the spectra are determined from the
package \fBdispaxis\fR parameter if not defined in the image header.  The default
line or column for finding the orders and the number of image lines or
columns to sum are set by the \f(CWline\fR and \f(CWnsum\fR parameters.  A line
of INDEF (the default) selects the middle of the image.  The automatic
finding algorithm is described for the task \fBapfind\fR and basically
finds the strongest peaks.  The tracing is done as described in
\fBaptrace\fR and consists of stepping along the image using the specified
\f(CWt_step\fR parameter.  The function fitting uses the \fBicfit\fR commands
with the other parameters from the tracing section.
.NH 2
Extraction
.LP
The actual extraction of the spectra is done by summing across the fixed
width apertures at each point along the dispersion.  The default is to
simply sum the pixels using partial pixels at the ends.  There is an
option to weight the sum based on a Poisson noise model using the
\f(CWreadnoise\fR and \f(CWgain\fR detector parameters.  Note that if the
\f(CWclean\fR option is selected the variance weighted extraction is used
regardless of the \f(CWweights\fR parameter.  The sigma thresholds for
cleaning are also set in the \fBparams\fR parameters.
.LP
The cleaning and variance weighting options require knowing the effective
(i.e. accounting for any image combining) read out noise and gain.  These
numbers need to be adjusted if the image has been processed such that the
intensity scale has a different origin (such as a background light
subtraction) or scaling (such as caused by unnormalized flat fielding).
For optimal extraction and cleaning to work it is recommended that the
\f(CWfitflat\fR option be used.  For further discussion of cleaning and
variance weighted extraction see \fBapvariance\fR and \fBapprofiles\fR as
well as  \fBapsum\fR.
.NH 2
Scattered Light Subtraction
.LP
Scattered light may be subtracted from the input two dimensional image as
the first step.  This is done using the algorithm described in
\fBapscatter\fR.  This can be important if there is significant scattered
light since the flat field/throughput correction will otherwise be
incorrect.  The algorithm consists of fitting a function to the data
outside the defined apertures by a specified \fIbuffer\fR at each line or
column across the dispersion.  The function fitting parameters are the same
at each line.  Because the fitted functions are independent at each line or
column a second set of one dimensional functions are fit parallel to the
dispersion using the evaluated fit values from the cross-dispersion step.
This produces a smooth scattered light surface which is finally subtracted
from the input image.  Again the function fitting parameters are the
same at each line or column though they may be different than the parameters
used to fit across the dispersion.
.LP
The first time the task is run with a particular flat field (or aperture
reference image if no flat field is used) the scattered light fitting
parameters are set interactively using that image.  The interactive step
selects a particular line or column upon which the fitting is done
interactively with the \fBicfit\fR commands.  A query is first issued
which allows skipping this interactive stage.  Note that the interactive
fitting is only for defining the fitting functions and orders.  When
the graphical \fBicfit\fR fitting is exited (with 'q') there is a second prompt
allowing you to change the buffer distance (in the first cross-dispersion
stage) from the apertures, change the line/column, or finally quit.
.LP
The initial fitting parameters and the final set parameters are recorded
in the \fBapscat1\fR and \fBapscat2\fR hidden parameter sets.  These
parameters are then used automatically for every subsequent image
which is scattered light corrected.
.LP
The scattered light subtraction modifies the input 2D images.  To preserve
the original data a copy of the original image is made with the same
root name and the word "noscat" appended.  The scattered light subtracted
images will have the header keyword "APSCATTE" which is how the task
avoids repeating the scattered light subtraction during any reprocessing.
However if the \fIredo\fR option is selected the scattered light subtraction
will also be redone by first restoring the "noscat" images to the original
input names.
.NH 2
Flat Field Correction
.LP
Flat field corrections may be made during the basic CCD processing; i.e.
direct division by the two dimensional flat field observation.  In that
case do not specify a flat field spectrum; use the null string "".  The
\fBdo3fiber\fR task provides an alternative flat field response correction
based on division of the extracted object spectra by the extracted flat field
spectra.  A discussion of the theory and merits of flat fielding directly
verses using the extracted spectra will not be made here.  The
\fBdo3fiber\fR flat fielding algorithm is the \fIrecommended\fR method for
flat fielding since it works well and is not subject to the many problems
involved in two dimensional flat fielding.
.LP
The first step is extraction of the flat field spectrum, if specified,
using the reference apertures.  Only one flat field is allowed so if
multiple flat fields are required the data must be reduced in groups.
If the \f(CWfitflat\fR
option is selected (the default) the extracted flat field spectra are
averaged together and a smooth function is fit.  The default fitting
function and order are given by the parameters \f(CWf_function\fR and
\f(CWf_order\fR.  If the parameter \f(CWf_interactive\fR is "yes" then the
fitting is done interactively using the \fBfit1d\fR task which uses the
\fBicfit\fR interactive fitting commands.
.LP
The fitted function is divided into the individual flat field spectra to
remove the basic shape of the spectrum while maintaining the relative
individual pixel responses and any fiber to fiber differences.  This step
avoids introducing the flat field spectrum shape into the object spectra
and closely preserves the object counts.
.LP
The final step is to normalize the flat field spectra by the mean counts over
all the fibers.  This normalization step is simply to preserve the average
counts of the extracted object and arc spectra after division by the
response spectra.
.NH 2
Dispersion Correction
.LP
If dispersion correction is not selected, \f(CWdispcor\fR=no, then the object
spectra are simply extracted.  If it is selected the arc spectra are used
to dispersion calibrate the object spectra.  There are four steps involved;
determining the dispersion functions relating pixel position to wavelength,
assigning the appropriate dispersion function to a particular observation,
determining a zero point wavelength shift from the arc fibers to be applied
to the object fiber dispersion functions, and either storing the nonlinear
dispersion functions in the image headers or resampling the spectra to
evenly spaced pixels in wavelength.
.LP
The first arc spectrum in the arc list is used to define the reference
dispersion solution.  It is extracted using the reference aperture
definitions.  The interactive task \fBautoidentify\fR is used to
automatically define the dispersion function in one fiber.  Whether or not
it is successful the user is presented with the interactive identification
graph.  The automatic identifications can be reviewed and a new solution or
corrections to the automatic solution may be performed.  The dispersion
functions for the other fibers are then determined automatically by
reference to the first fiber using the task \fBreidentify\fR.  Except in
batch mode a query is given allowing the reidentified arc spectra to be
examined interactively with \fBidentify\fR.  This would normally be done
only if the information about the reidentification printed on the terminal
indicates a problem such as a large increase in the RMS.  This query may be
eliminated in the usual way.
.LP
The set of arc dispersion function parameters are from \fBidentify\fR and
\fBreidentify\fR.  The parameters define a line list for use in
automatically assigning wavelengths to arc lines, a parameter controlling
the width of the centering window (which should match the base line
widths), the dispersion function type and order, parameters to exclude bad
lines from function fits, and parameters defining whether to refit the
dispersion function, as opposed to simply determining a zero point shift,
and the addition of new lines from the line list when reidentifying
additional arc spectra.  The defaults should generally be adequate and the
dispersion function fitting parameters may be altered interactively.  One
should consult the help for the two tasks for additional details of these
parameters and the operation of \fBidentify\fR.
.LP
If resampling of the spectra is selected by the parameter \f(CWlinearize\fR
all the arc dispersion functions are combined to provide a default
starting and ending wavelength and dispersion with the same number of
pixels is determined and the user is queried for any changes.  This
linear dispersion system will be applied to all spectra so that all
the final processed object spectra will have the same dispersion
sampling.
.LP
Once the reference dispersion functions are defined other arc spectra are
extracted as they are assign to the object spectra.  The assignment of
arcs is done either explicitly with an arc assignment table (parameter
\f(CWarctable\fR) or based on a header parameter such as a time.
The assignments are made by the task \fBrefspectra\fR.  When two arcs are
assigned to an object spectrum an interpolation is done between the two
dispersion functions.  This makes an approximate correction for steady
drifts in the dispersion.  Because the arc fibers monitor any zero point
shifts in the dispersion functions, due to translation and rotation of the
detector, it is probably only necessary to have one or two arc spectra, one
at the beginning and/or one at the end of the night.
.LP
The tasks \fBsetjd\fR and \fBsetairmass\fR are automatically run on all
spectra.  This computes and adds the header parameters for the Julian date
(JD), the local Julian day number (LJD), the universal time (UTMIDDLE), and
the air mass at the middle of the exposure.  The default arc assignment is
to use the Julian date grouped by the local Julian day number.  The
grouping allows multiple nights of data to be correctly assigned at the
same time.
.LP
When the object spectra are extracted so are the simultaneous arc spectra.
A zero point shift of the arc spectra relative to the dispersion solutions
of an assigned full arc observation is computed using \fBreidentify\fR.
The zero point shifts from the arc fibers are then
interpolated across the detector based on the positions of the arc
apertures to the positions of the object apertures.  A linear interpolation
is used which accounts for a rotation of the detector as well as a
translation along the dispersion.  The interpolated zero point wavelength
shifts are then added to the dispersion functions from the full arc
observation for the object fibers.  Note that this does not assume that the
object and arc fiber dispersion functions are the same or have the same
wavelength origin, but only that the interpolated shifts in wavelength zero
point apply to all fibers.  When there are two assigned full arc spectra
the above steps are done independently and the final pair of zero point
corrected dispersion functions for each object fiber are combined using the
assigned weights.  Once the dispersion function correction is determined
from the extracted arc fiber spectra they are deleted leaving only the
object spectra.
.LP
The last step of dispersion correction is setting the dispersion
of the object spectra.  There are two choices here.
If the \f(CWlinearize\fR parameter is not set the nonlinear dispersion
functions are stored in the image header.  Other IRAF tasks interpret
this information when dispersion coordinates are needed for plotting
or analysis.  This has the advantage of not requiring the spectra
to be interpolated and the disadvantage that the dispersion
information is only understood by IRAF tasks and cannot be readily
exported to other analysis software.
.LP
If the \f(CWlinearize\fR parameter is set then the spectra are resampled to a
linear dispersion relation either in wavelength or the log of the
wavelength.  The linear dispersion parameters are those defined
previously for the arc reference image.
.LP
The linearization algorithm  parameters allow selecting the interpolation
function type, whether to conserve flux per pixel by integrating across the
extent of the final pixel, and whether to linearize to equal linear or
logarithmic intervals.  The latter may be appropriate for radial velocity
studies.  The default is to use a fifth order polynomial for interpolation,
to conserve flux, and to not use logarithmic wavelength bins.  These
parameters are described fully in the help for the task \fBdispcor\fR which
performs the correction.
.NH
References
.NH 2
IRAF Introductory References
.LP
Work is underway on a new introductory guide to IRAF.  Currently, the
work below is the primary introduction.
.IP
P. Shames and D. Tody, \fIA User's Introduction to the IRAF Command
Language\fR, Central Computer Services, NOAO, 1986.
.NH 2
CCD Reductions
.IP
F. Valdes, \fIThe IRAF CCD Reduction Package -- CCDRED\fR, Central
Computer Services, NOAO, 1987.
.IP
F. Valdes, \fIUser's Guide to the CCDRED Package\fR, Central
Computer Services, NOAO, 1988.  Also on-line as \f(CWhelp ccdred.guide\fR.
.IP
P. Massey, \fIA User's Guide to CCD Reductions with IRAF\fR, Central
Computer Services, NOAO, 1989.
.NH 2
Aperture Extraction Package
.IP
F. Valdes, \fIThe IRAF APEXTRACT Package\fR, Central Computer Services,
NOAO, 1987 (out-of-date).
.NH 2
Task Help References
.LP
Each task in the \fBspecred\fR packages and tasks used by \fBdofibers\fR have
help pages describing the parameters and task in some detail.  To get
on-line help type
.V1

cl> help \fItaskname\fR

.V2
The output of this command can be piped to \fBlprint\fR to make a printed
copy.

.V1
      apall - Extract 1D spectra (all parameters in one task)
  apdefault - Set the default aperture parameters
     apedit - Edit apertures interactively
     apfind - Automatically find spectra and define apertures
 aprecenter - Recenter apertures
   apresize - Resize apertures
      apsum - Extract 1D spectra
    aptrace - Trace positions of spectra

      bplot - Batch plot of spectra with SPLOT
  calibrate - Apply extinction and flux calibrations to spectra
  continuum - Fit and normalize the continuum of multispec spectra
   deredden - Apply interstellar extinction corrections
    dispcor - Dispersion correct spectra
     dopcor - Doppler correct spectra
   identify - Identify arc lines and determine a dispersion function
   msresp1d - Create fiber response spectra from flat field and sky spectra
 refspectra - Assign reference spectra to observations
 reidentify - Reidentify arc lines and determine new dispersion functions
 sapertures - Set or change aperture header information
     sarith - Spectrum arithmetic
   scombine - Combine spectra
      scopy - Copy spectra including aperture selection and format changes
   sensfunc - Create sensitivity function
 setairmass - Compute effective airmass and middle UT for an exposure
      setjd - Compute and set Julian dates in images
      slist - List spectrum headers
   specplot - Stack and plot multiple spectra
      splot - Plot and analyze spectra
   standard - Identify standard stars to be used in sensitivity calc

   do3fiber - Process KPNO coude three fiber spectra
      demos - Demonstrations and tests

            Additional help topics

   onedspec.package - Package parameters and general description of package
  apextract.package - Package parameters and general description of package
 approfiles - Profile determination algorithms
 apvariance - Extractions, variance weighting, cleaning, and noise model
   center1d - One dimensional centering algorithm
      icfit - Interactive one dimensional curve fitting
.V2
.SH
Appendix A: DO3FIBER Parameters
.LP
.nr PS 8
.nr VS 10
objects
.LS
List of object spectra to be processed.  Previously processed spectra are
ignored unless the \f(CWredo\fR flag is set or the \f(CWupdate\fR flag is set and
dependent calibration data has changed.  Extracted spectra are ignored.
.LE
apref = ""
.LS
Aperture reference spectrum.  This spectrum is used to define the basic
extraction apertures and is typically a flat field spectrum.
.LE
flat = "" (optional)
.LS
Flat field spectrum.  If specified the one dimensional flat field spectra
are extracted and used to make flat field corrections.
.LE
arcs = "" (at least one if dispersion correcting)
.LS
List of primary, all fiber arc spectra.  These spectra are used to define
the dispersion functions for each fiber apart from a possible zero point
correction made with simultaneous arc calibration fibers in the object
spectra.  One fiber from the first spectrum is used to mark lines and set
the dispersion function interactively and dispersion functions for all
other fibers and arc spectra are derived from it.
.LE
arctable = "" (optional) (refspectra)
.LS
Table defining arc spectra to be assigned to object
spectra (see \fBrefspectra\fR).  If not specified an assignment based
on a header parameter, \f(CWparams.sort\fR, such as the observation time is made.
.LE

readnoise = "RDNOISE" (apsum)
.LS
Read out noise in photons.  This parameter defines the minimum noise
sigma.  It is defined in terms of photons (or electrons) and scales
to the data values through the gain parameter.  A image header keyword
(case insensitive) may be specified to get the value from the image.
.LE
gain = "GAIN" (apsum)
.LS
Detector gain or conversion factor between photons/electrons and
data values.  It is specified as the number of photons per data value.
A image header keyword (case insensitive) may be specified to get the value
from the image.
.LE
datamax = INDEF (apsum.saturation)
.LS
The maximum data value which is not a cosmic ray.
When cleaning cosmic rays and/or using variance weighted extraction
very strong cosmic rays (pixel values much larger than the data) can
cause these operations to behave poorly.  If a value other than INDEF
is specified then all data pixels in excess of this value will be
excluded and the algorithms will yield improved results.
This applies only to the object spectra and not the flat field or
arc spectra.  For more
on this see the discussion of the saturation parameter in the
\fBapextract\fR package.
.LE
fibers = 3 (apfind)
.LS
Number of fibers.  This number is used during the automatic definition of
the apertures from the aperture reference spectrum.
.LE
width = 6. (apedit)
.LS
Approximate base full width of the fiber profiles.  This parameter is used
for the profile centering algorithm.
.LE
crval = INDEF, cdelt = INDEF (autoidentify)
.LS
These parameters specify an approximate central wavelength and dispersion.
They may be specified as numerical values, INDEF, or image header keyword
names whose values are to be used.  If one or both of these parameters are
specified as INDEF the search for a solution will be slower and more likely
to fail.
.LE
objaps = "2", arcaps = "1,3"
.LS
List of object and arc aperture numbers.  These are used to
identify arc apertures for wavelength calibration and object apertures
for the final results.
.LE

scattered = no (apscatter)
.LS
Smooth and subtracted scattered light from the object and flat field
images.  This operation consists of fitting independent smooth functions
across the dispersion using data outside the fiber apertures and then
smoothing the individual fits along the dispersion.  The initial
flat field, or if none is given the aperture reference image, are
done interactively to allow setting the fitting parameters.  All
subsequent subtractions use the same fitting parameters.
.LE
fitflat = yes (flat1d)
.LS
Fit the composite flat field spectrum by a smooth function and divide each
flat field spectrum by this function?  This operation removes the average
spectral signature of the flat field lamp from the sensitivity correction to
avoid modifying the object fluxes.
.LE
recenter = yes (aprecenter)
.LS
Recenter reference apertures for each object spectrum?
.LE
edit = no (apedit)
.LS
Review aperture definitions for each object spectrum?  Note that this does
not apply to the initial reference aperture which always allows
interactive review of the aperture definitions.
.LE
clean = no (apsum)
.LS
Detect and correct for bad pixels during extraction?  This is the same
as the clean option in the \fBapextract\fR package.  If yes this also
implies variance weighted extraction and requires reasonably good values
for the readout noise and gain.  In addition the datamax parameters
can be useful.
.LE
dispcor = yes
.LS
Dispersion correct spectra?  Depending on the \f(CWparams.linearize\fR
parameter this may either resample the spectra or insert a dispersion
function in the image header.
.LE
splot = yes
.LS
Plot the final spectra with the task \fBsplot\fR?
.LE
redo = no
.LS
Redo operations previously done?  If no then previously processed spectra
in the objects list will not be processed (unless they need to be updated).
.LE
update = yes
.LS
Update processing of previously processed spectra if aperture, flat
field, or dispersion reference definitions are changed?
.LE
batch = no
.LS
Process spectra as a background or batch job provided there are no interactive
options (\f(CWedit\fR and \f(CWsplot\fR) selected.
.LE
listonly = no
.LS
List processing steps but don't process?
.LE

params = "" (pset)
.LS
Name of parameter set containing additional processing parameters.  The
default is parameter set \fBparams\fR.  The parameter set may be examined
and modified in the usual ways (typically with "epar params" or ":e params"
from the parameter editor).  Note that using a different parameter file
is not allowed.  The parameters are described below.
.LE

.ce
-- PACKAGE PARAMETERS

Package parameters are those which generally apply to all task in the
package.  This is also true of \fBdo3fiber\fR.

observatory = "observatory"
.LS
Observatory at which the spectra were obtained if not specified in the
image header by the keyword OBSERVAT.  For NOAO data the image headers
identify the observatory as "kpno" or "ctio" so this parameter is not used.
For data from other observatories this parameter may be used
as describe in \fBobservatory\fR.
.LE
interp = "poly5" (nearest|linear|poly3|poly5|spline3|sinc)
.LS
Spectrum interpolation type used when spectra are resampled.  The choices are:

.V1
	nearest - nearest neighbor
	 linear - linear
	  poly3 - 3rd order polynomial
	  poly5 - 5th order polynomial
	spline3 - cubic spline
	   sinc - sinc function
.V2
.LE
dispaxis = 2
.LS
Default dispersion axis.  The dispersion axis is 1 for dispersion
running along image lines and 2 for dispersion running along image
columns.  If the image header parameter DISPAXIS is defined it has
precedence over this parameter.
.LE
database = "database"
.LS
Database (directory) used for storing aperture and dispersion information.
.LE
verbose = no
.LS
Print verbose information available with various tasks.
.LE
logfile = "logfile", plotfile = ""
.LS
Text and plot log files.  If a filename is not specified then no log is
kept.  The plot file contains IRAF graphics metacode which may be examined
in various ways such as with \fBgkimosaic\fR.
.LE
records = ""
.LS
Dummy parameter to be ignored.
.LE
version = "KPNOCOUDE: ..."
.LS
Version of the package.
.LE

.ce
PARAMS PARAMETERS

The following parameters are part of the \fBparams\fR parameter set and
define various algorithm parameters for \fBdo3fiber\fR.

.ce
--  GENERAL PARAMETERS --

line = INDEF, nsum = 10
.LS
The dispersion line (line or column perpendicular to the dispersion
axis) and number of adjacent lines (half before and half after unless
at the end of the image) used in finding, recentering, resizing,
editing, and tracing operations.  A line of INDEF selects the middle of the
image along the dispersion axis.
.LE
extras = no (apsum)
.LS
Include extra information in the output spectra?  When cleaning or using
variance weighting the cleaned and weighted spectra are recorded in the
first 2D plane of a 3D image, the raw, simple sum spectra are recorded in
the second plane, and the estimated sigmas are recorded in the third plane.
.LE

.ce
-- DEFAULT APERTURE LIMITS --

lower = -3., upper = 3. (apdefault)
.LS
Default lower and upper aperture limits relative to the aperture center.
These limits are used when the apertures are first found and may be
resized automatically or interactively.
.LE

.ce
-- AUTOMATIC APERTURE RESIZING PARAMETERS --

ylevel = 0.05 (apresize)
.LS
Data level at which to set aperture limits during automatic resizing.
It is a fraction of the peak relative to a local background.
.LE

.ce
-- TRACE PARAMETERS --

t_step = 10 (aptrace)
.LS
Step along the dispersion axis between determination of the spectrum
positions.  Note the \f(CWnsum\fR parameter is also used to enhance the
signal-to-noise at each step.
.LE
t_function = "spline3", t_order = 2 (aptrace)
.LS
Default trace fitting function and order.  The fitting function types are
"chebyshev" polynomial, "legendre" polynomial, "spline1" linear spline, and
"spline3" cubic spline.  The order refers to the number of
terms in the polynomial functions or the number of spline pieces in the spline
functions.
.LE
t_niterate = 1, t_low = 3., t_high = 3. (aptrace)
.LS
Default number of rejection iterations and rejection sigma thresholds.
.LE

.ce
-- SCATTERED LIGHT PARAMETERS --

buffer = 1. (apscatter)
.LS
Buffer distance from the aperture edges to be excluded in selecting the
scattered light pixels to be used.
.LE
apscat1 = "" (apscatter)
.LS
Fitting parameters across the dispersion.  This references an additional
set of parameters for the ICFIT package.  The default is the "apscat1"
parameter set.
.LE
apscat2 = "" (apscatter)
.LS
Fitting parameters along the dispersion.  This references an additional
set of parameters for the ICFIT package.  The default is the "apscat2"
parameter set.
.LE

.ce
-- APERTURE EXTRACTION PARAMETERS --

weights = "none" (apsum)
.LS
Type of extraction weighting.  Note that if the \f(CWclean\fR parameter is
set then the weights used are "variance" regardless of the weights
specified by this parameter.  The choices are:

"none"
.LS
The pixels are summed without weights except for partial pixels at the
ends.
.LE
"variance"
.LS
The extraction is weighted by the variance based on the data values
and a poisson/ccd model using the \f(CWgain\fR and \f(CWreadnoise\fR
parameters.
.LE
.LE
pfit = "fit1d" (apsum) (fit1d|fit2d)
.LS
Profile fitting algorithm for cleaning and variance weighted extractions.
The default is generally appropriate for most data but users
may try the other algorithm.  See \fBapprofiles\fR for further information.
.LE
lsigma = 3., usigma = 3. (apsum)
.LS
Lower and upper rejection thresholds, given as a number of times the
estimated sigma of a pixel, for cleaning.
.LE
nsubaps = 1 (apsum)
.LS
During extraction it is possible to equally divide the apertures into
this number of subapertures.
.LE

.ce
-- FLAT FIELD FUNCTION FITTING PARAMETERS --

f_interactive = yes (fit1d)
.LS
Fit the composite one dimensional flat field spectrum interactively?
This is used if \f(CWfitflat\fR is set and a two dimensional flat field
spectrum is specified.
.LE
f_function = "spline3", f_order = 20 (fit1d)
.LS
Function and order used to fit the composite one dimensional flat field
spectrum.  The functions are "legendre", "chebyshev", "spline1", and
"spline3".  The spline functions are linear and cubic splines with the
order specifying the number of pieces.
.LE

.ce
-- ARC DISPERSION FUNCTION PARAMETERS --

threshold = 10. (autoidentify/identify/reidentify)
.LS
In order for a feature center to be determined the range of pixel intensities
around the feature must exceed this threshold.
.LE
coordlist = "linelists$idhenear.dat" (autoidentify/identify)
.LS
Arc line list consisting of an ordered list of wavelengths.
Some standard line lists are available in the directory "linelists$".
.LE
match = -3. (autoidentify/identify)
.LS
The maximum difference for a match between the dispersion function prediction
value and a wavelength in the coordinate list.
.LE
fwidth = 3.5 (autoidentify/identify)
.LS
Approximate full base width (in pixels) of arc lines.
.LE
cradius = 4. (reidentify)
.LS
Radius from previous position to reidentify arc line.
.LE
i_function = "legendre", i_order = 3 (autoidentify/identify)
.LS
The default function and order to be fit to the arc wavelengths as a
function of the pixel coordinate.  The functions choices are "chebyshev",
"legendre", "spline1", or "spline3".
.LE
i_niterate = 3, i_low = 3.0, i_high = 3.0 (autoidentify/identify)
.LS
Number of rejection iterations and sigma thresholds for rejecting arc
lines from the dispersion function fits.
.LE
refit = yes (reidentify)
.LS
Refit the dispersion function?  If yes and there is more than 1 line
and a dispersion function was defined in the arc reference then a new
dispersion function of the same type as in the reference image is fit
using the new pixel positions.  Otherwise only a zero point shift is
determined for the revised fitted coordinates without changing the
form of the dispersion function.
.LE
addfeatures = no (reidentify)
.LS
Add new features from a line list during each reidentification?
This option can be used to compensate for lost features from the
reference solution.  Care should be exercised that misidentified features
are not introduced.
.LE

.ce
-- AUTOMATIC ARC ASSIGNMENT PARAMETERS --

select = "interp" (refspectra)
.LS
Selection method for assigning wavelength calibration spectra.
Note that an arc assignment table may be used to override the selection
method and explicitly assign arc spectra to object spectra.
The automatic selection methods are:

average
.LS
Average two reference spectra without regard to any sort parameter.
If only one reference spectrum is specified then it is assigned with a
warning.  If more than two reference spectra are specified then only the
first two are used and a warning is given.
This option is used to assign two reference spectra, with equal weights,
independent of any sorting parameter.
.LE
following
.LS
Select the nearest following spectrum in the reference list based on the
sorting parameter.  If there is no following spectrum use the nearest preceding
spectrum.
.LE
interp
.LS
Interpolate between the preceding and following spectra in the reference
list based on the sorting parameter.  If there is no preceding and following
spectrum use the nearest spectrum.  The interpolation is weighted by the
relative distances of the sorting parameter.
.LE
match
.LS
Match each input spectrum with the reference spectrum list in order.
This overrides the reference aperture check.
.LE
nearest
.LS
Select the nearest spectrum in the reference list based on the sorting
parameter.
.LE
preceding
.LS
Select the nearest preceding spectrum in the reference list based on the
sorting parameter.  If there is no preceding spectrum use the nearest following
spectrum.
.LE
.LE
sort = "jd", group = "ljd" (refspectra)
.LS
Image header keywords to be used as the sorting parameter for selection
based on order and to group spectra.
A null string, "", or the word "none" may be use to disable the sorting
or grouping parameters.
The sorting parameter
must be numeric but otherwise may be anything.  The grouping parameter
may be a string or number and must simply be the same for all spectra within
the same group (say a single night).
Common sorting parameters are times or positions.
In \fBdo3fiber\fR the Julian date (JD) and the local Julian day number (LJD)
at the middle of the exposure are automatically computed from the universal
time at the beginning of the exposure and the exposure time.  Also the
parameter UTMIDDLE is computed.
.LE
time = no, timewrap = 17. (refspectra)
.LS
Is the sorting parameter a 24 hour time?  If so then the time origin
for the sorting is specified by the timewrap parameter.  This time
should precede the first observation and follow the last observation
in a 24 hour cycle.
.LE

.ce
-- DISPERSION  CORRECTION PARAMETERS --

linearize = yes (dispcor)
.LS
Interpolate the spectra to a linear dispersion sampling?  If yes the
spectra will be interpolated to a linear or log linear sampling
If no the nonlinear dispersion function(s) from the dispersion function
database are assigned to the input image world coordinate system
and the spectral data are not interpolated.
.LE
log = no (dispcor)
.LS
Use linear logarithmic wavelength coordinates?  Linear logarithmic
wavelength coordinates have wavelength intervals which are constant
in the logarithm of the wavelength.
.LE
flux = yes (dispcor)
.LS
Conserve the total flux during interpolation?  If \f(CWno\fR the output
spectrum is interpolated from the input spectrum at each output
wavelength coordinate.  If \f(CWyes\fR the input spectrum is integrated
over the extent of each output pixel.  This is slower than
simple interpolation.
.LE

.ce
ENVIRONMENT PARAMETERS
.LP
The environment parameter \fIimtype\fR is used to determine the extension
of the images to be processed and created.  This allows use with any
supported image extension.  For STF images the extension has to be exact;
for example "d1h".
