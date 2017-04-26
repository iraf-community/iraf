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
Guide to the Slit Spectra Reduction Task DOSLIT
.AU
Francisco Valdes
.AI
IRAF Group - Central Computer Services
.K2
.DY

.AB
\fBDoslit\fR extracts, sky subtracts, wavelength calibrates, and flux
calibrates simple two dimensional slit spectra which have been processed to
remove the detector characteristics; i.e. CCD images have been bias, dark
count, and flat field corrected.  It is primarily intended for
spectrophotometry or radial velocities of stellar spectra with the spectra
aligned with one of the image axes; i.e. the assumption is that extractions
can be done by summing along image lines or columns.  The alignment does
not have to be precise but only close enough that the wavelength difference
across the spectrum profiles is insignificant.  The task is available
in the \fBctioslit\fR, \fBkpnoslit\fR, \fBkpnocoude\fR, and \fBspecred\fR
packages.
.AE
.NH
Introduction
.LP
\fBDoslit\fR extracts, sky subtracts, wavelength calibrates, and flux
calibrates simple two dimensional slit spectra which have been processed to
remove the detector characteristics; i.e. CCD images have been bias, dark
count, and flat field corrected.  It is primarily intended for
spectrophotometry or radial velocities of stellar spectra with the spectra
aligned with one of the image axes; i.e. the assumption is that extractions
can be done by summing along image lines or columns.  The alignment does
not have to be precise but only close enough that the wavelength difference
across the spectrum profiles is insignificant.  Extended objects requiring
accurate geometric alignment over many pixels are reduced using the
\fBlongslit\fR package.
.LP
The task is a command language script which collects and combines the
functions and parameters of many general purpose tasks to provide a single,
complete data reduction path and a degree of guidance, automation, and
record keeping.  In the following description and in the parameter section
the various general tasks used are identified.  Further
information about those tasks and their parameters may be found in their
documentation.  \fBDoslit\fR also simplifies and consolidates parameters
from those tasks and keeps track of previous processing to avoid
duplications.
.LP
The general organization of the task is to do the interactive setup steps,
such as the reference dispersion function
determination, first using representative calibration data and then perform
the majority of the reductions automatically, possibly as a background
process, with reference to the setup data.  In addition, the task
determines which setup and processing operations have been completed in
previous executions of the task and, contingent on the \f(CWredo\fR and
\f(CWupdate\fR options, skip or repeat some or all the steps.
.LP
The description is divided into a quick usage outline followed by details
of the parameters and algorithms.  The usage outline is provided as a
checklist and a refresher for those familiar with this task and the
component tasks.  It presents only the default or recommended usage
since there are many variations possible.
.NH
Usage Outline
.LP
.IP [1] 6
The images are first processed with \fBccdproc\fR for overscan,
zero level, dark count, and flat field corrections.
.IP [2]
Set the \fBdoslit\fR parameters with \fBeparam\fR.  Specify the object
images to be processed,
one or more arc images, and one or more standard
star images.  If there are many object, arc, or standard star images
you might prepare "@ files".  Set the detector and data
specific parameters.  Select the processing options desired.
Finally you might wish to review the \f(CWsparams\fR algorithm parameters
though the defaults are probably adequate.
.IP [3]
Run the task.  This may be repeated multiple times with different
observations and the task will generally only do the setup steps
once and only process new images.  Queries presented during the
execution for various interactive operations may be answered with
"yes", "no", "YES", or "NO".  The lower case responses apply just
to that query while the upper case responses apply to all further
such queries during the current execution and no further queries of that
type will be made.
.IP [4]
Apertures are defined for all the standard and object images.  This is only
done if there are no previous aperture definitions for the image.
The highest peak is found and centered and the default aperture limits
are set.  If the resize option is set the aperture is resized by finding
the level which  is 5% (the default) of the peak above local background.
If not using the quicklook option you now have the option
of entering the aperture editing loop to check the aperture position,
size, and background fitting parameters, and possibly add additional
apertures.  This is step is highly recommended.
It is important to check the background regions with the 'b'
key.  To exit the background mode and then
to exit the review mode use 'q'.
.IP
The spectrum positions at a series of points along the dispersion are
measured and a function is fit to these positions.  If not using the
quicklook option the traced positions may be examined interactively and the
fitting parameters adjusted.  To exit the interactive fitting type 'q'.
.IP [5]
If dispersion correction is selected the first arc in the arc list is
extracted.  The dispersion function is defined using the task
\fBautoidentify\fR.  The \fIcrval\fR and \fIcdelt\fR parameters are used in
the automatic identification.  Whether or not the automatic identification
is successful you will be shown the result of the arc line identification.
If the automatic identification is not successful identify a few arc lines
with with 'm' and and use the 'l' line list identification command to
automatically add additional lines and fit the dispersion function.  Check
the quality of the dispersion function fit with 'f'.  When satisfied exit
with 'q'.
.IP [6]
If the flux calibration option is selected the standard star spectra are
processed (if not done previously).  The images are
extracted and wavelength calibrated.  The appropriate arc
calibration spectra are extracted and the dispersion function refit
using the arc reference spectrum as a starting point.  The standard star
fluxes through the calibration bandpasses are compiled.  You are queried
for the name of the standard star calibration data file.
.IP
After all the standard stars are processed a sensitivity function is
determined using the interactive task \fBsensfunc\fR.  Finally, the
standard star spectra are extinction corrected and flux calibrated
using the derived sensitivity function.
.IP [7]
The object spectra are now automatically
extracted, wavelength calibrated, and flux calibrated.
.IP [8]
The option to examine the final spectra with \fBsplot\fR may be given.
To exit type 'q'.  In quicklook mode the spectra are plotted
noninteractively with \fBbplot\fR.
.IP [9]
The final spectra will have the same name as the original 2D images
with a ".ms" extension added.
.NH
Spectra and Data Files
.LP
The basic input consists of two dimensional slit object, standard star, and
arc calibration spectra stored as IRAF images.
The type of image format is defined by the
environment parameter \fIimtype\fR.  Only images with that extension will
be processed and created.
The raw CCD images must be
processed to remove overscan, bias, dark count, and flat field effects.
This is generally done using the \fBccdred\fR package.  Lines of constant
wavelength should be closely aligned with one of the image axes though a
small amount of misalignment only causes a small loss of resolution.  For
large misalignments one may use the \fBrotate\fR task.  More complex
geometric problems and observations of extended objects should be handled
by the \fBlongslit\fR package.
.LP
The arc
spectra are comparison arc lamp observations (they must all be of the same
type).  The assignment of arc calibration exposures to object exposures is
generally done by selecting the nearest in time and interpolating.
However, the optional \fIarc assignment table\fR may be used to explicitly
assign arc images to specific objects.  The format of this file is
described in task \fBrefspectra\fR.
.LP
The final reduced spectra are recorded in one, two or three dimensional IRAF
images.  The images have the same name as the original images with an added
".ms" extension.  Each line in the reduced image is a one dimensional
spectrum with associated aperture, wavelength, and identification
information.  With a single aperture the image will be one dimensional
and with multiple apertures the image will be two dimensional.
When the \f(CWextras\fR parameter is set the images will be three
dimensional (regardless of the number of apertures) and the lines in the
third dimension contain additional information (see
\fBapsum\fR for further details).  These spectral formats are accepted by the
one dimensional spectroscopy tasks such as the plotting tasks \fBsplot\fR
and \fBspecplot\fR.
.NH
Package Parameters
.LP
The package parameters, shown in Figure 1 for the \fBspecred\fR package,
set parameters which change infrequently and define the standard I/O functions.
.KS
.V1

.ce
Figure 1: Package Parameter Set for DOSLIT Packages

                           I R A F
            Image Reduction and Analysis Facility
PACKAGE = imred
   TASK = specred

(extinct= onedstds$kpnoextinct.dat) Extinction file
(caldir = onedstds$spec16redcal/) Standard star calibration directory
(observa=  observatory) Observatory of data
(interp =        poly5) Interpolation type
(dispaxi=            2) Image axis for 2D images
(nsum   =            1) Number of lines/columns to sum for 2D images

(databas=     database) Database
(verbose=           no) Verbose output?
(logfile=      logfile) Log file
(plotfil=             ) Plot file

(records=             ) Record number extensions
(version= SPECRED V3: April 1992)

.KE
.V2
The extinction file
is used for making extinction corrections and the standard star
calibration directory is used for determining flux calibrations from
standard star observations.  The calibration directories contain data files
with standard star fluxes and band passes.  The available extinction
files and flux calibration directories may be listed using the command:
.V1

	cl> help onedstds

.V2
The extinction correction requires computation of an air mass using the
task \fBsetairmass\fR.  The air mass computation needs information
about the observation and, in particular, the latitude of the observatory.
This is determined using the OBSERVAT image header keyword.  If this
keyword is not present the observatory parameter is used.  See the
task \fBobservatory\fR for more on defining the observatory parameters.
.LP
The spectrum interpolation type is used whenever a spectrum needs to be
resampled for linearization or performing operations between spectra
with different sampling.  The "sinc" interpolation may be of interest
as an alternative but see the cautions given in \fBonedspec.package\fR.
.LP
The general direction in which the spectra run is specified by the
dispersion axis parameter.  Recall that ideally it is the direction
of constant wavelength which should be aligned with an image axis and
the dispersion direction may not be exactly aligned because atmospheric
dispersion.
.LP
The verbose parameter selects whether to print everything which goes
into the log file on the terminal.  It is useful for monitoring
what the \fBdoslit\fR task does.  The log and plot files are useful for
keeping a record of the processing.  A log file is highly recommended.
A plot file provides a record of the apertures, traces, and extracted
spectra but can become quite large.
The plotfile is most conveniently viewed and printed with \fBgkimosaic\fR.
.NH
Processing Parameters
.LP
The \fBdoslit\fR parameters are shown in Figure 2.
.KS
.V1

.ce
Figure 2: Parameter Set for DOSLIT

                           I R A F
            Image Reduction and Analysis Facility
PACKAGE = specred
   TASK = doslit

objects =               List of object spectra
(arcs   =             ) List of arc spectra
(arctabl=             ) Arc assignment table (optional)
(standar=             ) List of standard star spectra

.KE
.V1
(readnoi=      rdnoise) Read out noise sigma (photons)
(gain   =         gain) Photon gain (photons/data number)
(datamax=        INDEF) Max data value / cosmic ray threshold
(width  =           5.) Width of profiles (pixels)
(crval  =        INDEF) Approximate wavelength
(cdelt  =        INDEF) Approximate dispersion

(dispcor=          yes) Dispersion correct spectra?
(extcor =           no) Extinction correct spectra?
(fluxcal=           no) Flux calibrate spectra?
(resize =           no) Automatically resize apertures?
(clean  =           no) Detect and replace bad pixels?
(splot  =           no) Plot the final spectrum?
(redo   =           no) Redo operations if previously done?
(update =           no) Update spectra if cal data changes?
(quicklo=           no) Minimally interactive quick-look?
(batch  =           no) Extract objects in batch?
(listonl=           no) List steps but don't process?

(sparams=             ) Algorithm parameters

.V2
The input images are specified by image lists.  The lists may be
explicit comma separate image names, @ files, or image
templates using pattern matching against file names in the directory.
To allow wildcard image lists to be used safely and conveniently the
image lists are checked to remove extracted images (the .ms images)
and to automatically identify object and arc spectra.  Object and arc
images are identified by the keyword IMAGETYP with values of "object",
"OBJECT", "comp", or "COMPARISON" (the current practice at NOAO).
If arc images are found in the object list they are transferred to the
arc list while if object images are found in the arc list they are ignored.
All other image types, such as biases, darks, or flat fields, are
ignored.  This behavior allows simply specifying all images with a wildcard
in the object list with automatic selections of arc spectra or a
wildcard in the arc list to automatically find the arc spectra.
If the data lack the identifying information it is up to the user
to explicitly set the proper lists.
.LP
The arc assignment table is a file which may be used to assign
specific arc spectra to specific object and standard star spectra.
For more on this option see \fBrefspectra\fR.
.LP
The next set of parameters describe the noise characteristics and
spectrum characteristics.  The read out noise and gain are used when
"cleaning" cosmic rays and when using variance or optimal weighting.  These
parameters must be fairly accurate.  Note that these are the effective
parameters and must be adjusted if previous processing has modified the
pixel values; such as with an unnormalized flat field.
The variance
weighting and cosmic-ray cleanning are sensitive to extremely strong
cosmic-rays; ones which are hundreds of times brighter than the
spectrum.  The \fIdatamax\fR is used to set an upper limit for any
real data.  Any pixels above this value will be flagged as cosmic-rays
and will not affect the extractions.
.LP
The profile width should be approximately the full width
at the profile base.  This parameter is used for centering and tracing
of the spectrum profiles.
.LP
The approximate central wavelength and dispersion are used for the
automatic identification of the arc reference.  They may be specified
as image header keywords or values.  The INDEF values search the
entire range of the coordinate reference file but the automatic
line identification algorithm works much better and faster if
approximate values are given.
.LP
The next set of parameters select the processing steps and options.  The
various calibration steps may be done simultaneously, that is at the same
time as the basic extractions, or in separate executions of the task.
Typically, all the desired operations are done at the same time.
Dispersion correction requires at least one arc spectrum and flux
calibration requires dispersion correction and at least one standard star
observation.
.LP
The \f(CWresize\fR option resets the edges of the extraction aperture based
on the profile for each object and standard star image.  The default
resizing is to the 5% point relative to the peak measured above the
background.  This allows following changes in the seeing.  However, one
should consider the consequences of this if attempting to flux calibrate
the observations.  Except in quicklook mode, the apertures for each object
and standard star observation may be reviewed graphically and
adjustments made to the aperture width and background regions.
.LP
The \f(CWclean\fR option invokes a profile
fitting and deviant point rejection algorithm as well as a variance weighting
of points in the aperture.  See the next section for more about
requirements to use this option.
.LP
Generally once a spectrum has been processed it will not be reprocessed if
specified as an input spectrum.  However, changes to the underlying
calibration data can cause such spectra to be reprocessed if the
\f(CWupdate\fR flag is set.  The changes which will cause an update are a
new arc reference image and new standard stars.  If all input spectra are to be
processed regardless of previous processing the \f(CWredo\fR flag may be
used.  Note that reprocessing clobbers the previously processed output
spectra.
.LP
The final step is to plot the spectra if the \f(CWsplot\fR option is
selected.  In non-quicklook mode there is a query which may be
answered either in lower or upper case.  The plotting uses the interactive
task \fBsplot\fR.  In quicklook mode the plot appears noninteractively
using the task \fBbplot\fR.  
.LP
The \f(CWquicklook\fR option provides a simpler, less interactive, mode.
In quicklook mode a single aperture is defined using default parameters
without interactive aperture review or trace fitting and
the \f(CWsplot\fR option selects a noninteractive plot to be
shown at the end of processing of each object and standard star
spectrum.  While the algorithms used in quicklook mode are nearly the same
as in non-quicklook mode and the final results may be the same it is
recommended that the greater degree of monitoring and review in
non-quicklook mode be used for careful final reductions.
.LP
The batch processing option allows object spectra to be processed as a
background or batch job.  This will occur only if the interactive
\f(CWsplot\fR option is not active; either not set, turned off during
processing with "NO", or in quicklook mode.  In batch processing the
terminal output is suppressed.
.LP
The \f(CWlistonly\fR option prints a summary of the processing steps
which will be performed on the input spectra without actually doing
anything.  This is useful for verifying which spectra will be affected
if the input list contains previously processed spectra.  The listing
does not include any arc spectra which may be extracted to dispersion
calibrate an object spectrum.
.LP
The last parameter (excluding the task mode parameter) points to
another parameter set for the algorithm parameters.  The default
parameter set is called \f(CWsparams\fR.  The algorithm parameters are
discussed further in the next section.
.NH
Algorithms and Algorithm Parameters
.LP
This section summarizes the various algorithms used by the
\fBdoslit\fR task and the parameters which control and modify the
algorithms.  The algorithm parameters available to you are
collected in the parameter set \fBsparams\fR.  These parameters are
taken from the various general purpose tasks used by the \fBdoslit\fR
processing task.  Additional information about these parameters and
algorithms may be found in the help for the actual
task executed.  These tasks are identified below.  The aim of this
parameter set organization is to collect all the algorithm parameters
in one place separate from the processing parameters and include only
those which are relevant for slit data.  The parameter values
can be changed from the defaults by using the parameter editor,
.V1

cl> epar sparams

.V2
or simple typing \f(CWsparams\fR.
The parameter editor can also be entered when editing the \fBdoslit\fR
parameters by typing \f(CW:e\fR when positioned at the \f(CWsparams\fR
parameter.  Figure 3 shows the parameter set.
.KS
.V1

.ce
Figure 3: Algorithm Parameter Set

                           I R A F
            Image Reduction and Analysis Facility
PACKAGE = specred
   TASK = sparams

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
(t_order=            1) Trace fitting function order
(t_niter=            1) Trace rejection iterations
(t_low  =           3.) Trace lower rejection sigma
(t_high =           3.) Trace upper rejection sigma

.KE
.KS
.V1
                        -- APERTURE EXTRACTION PARAMETERS --
(weights=         none) Extraction weights (none|variance)
(pfit   =        fit1d) Profile fitting algorithm (fit1d|fit2d)
(lsigma =           3.) Lower rejection threshold
(usigma =           3.) Upper rejection threshold

.KE
.KS
.V1
                        -- BACKGROUND SUBTRACTION PARAMETERS --
(backgro=          fit) Background to subtract
(b_funct=     legendre) Background function
(b_order=            1) Background function order
(b_sampl=  -10:-6,6:10) Background sample regions
(b_naver=         -100) Background average or median
(b_niter=            1) Background rejection iterations
(b_low  =           3.) Background lower rejection sigma
(b_high =           3.) Background upper rejection sigma

.KE
.KS
.V1
                        -- ARC DISPERSION FUNCTION PARAMETERS --
(coordli=linelists$idhenear.dat) Line list
(match  =          -3.) Line list matching limit in Angstroms
(fwidth =           4.) Arc line widths in pixels
(cradius=          10.) Centering radius in pixels
(i_funct=      spline3) Coordinate function
(i_order=            1) Order of dispersion function
(i_niter=            0) Rejection iterations
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
.KS
.V1
                        -- SENSITIVITY CALIBRATION PARAMETERS --
(s_funct=      spline3) Fitting function
(s_order=            1) Order of sensitivity function
(fnu    =           no) Create spectra having units of FNU?

.KE
.V2
.NH 2
Aperture Definitions
.LP
The first operation is to define the extraction apertures, which include the
aperture width, background regions, and position dependence with
wavelength, for the input slit spectra and, if flux calibration is
selected, the standard star spectra.  This is done only for spectra which
do not have previously defined apertures unless the \f(CWredo\fR option is
set to force all definitions to be redone.  Thus, apertures may be
defined separately using the \fBapextract\fR tasks.  This is particularly
useful if one needs to use reference images to define apertures for very
weak spectra which are not well centered or traced by themselves.
.LP
Initially a single spectrum is found and a default aperture defined
automatically.  If the \f(CWresize\fR parameter is set the aperture width is
adjusted to a specified point on the spectrum profile (see
\fBapresize\fR).  If not in "quicklook" mode (set by the \f(CWquicklook\fR
parameter) a query is printed to select whether to inspect and modify the
aperture and background aperture definitions using the commands described
for \fBapedit\fR.  This option allows adding
apertures for other objects on the slit and adjusting
background regions to avoid contaminating objects.  The query may be
answered in lower case for a single spectrum or in upper case to
permanently set the response for the duration of the task execution.  This
convention for query responses is used throughout the task.  It is
recommended that quicklook only be used for initial quick extractions and
calibration and that for final reductions one at least review the aperture
definitions and traces.
.LP
The initial spectrum finding and aperture definitions are done at a specified
line or column.  The positions of the spectrum at a set of other lines or
columns is done next and a smooth function is fit to define the aperture
centers at all points in the image.  In non-quicklook mode the user has the
option to review and adjust the function fitting parameters and delete bad
position determinations.  As with the initial aperture review there is a
query which may be answered either in lower or upper case.
.LP
The above steps are all performed using tasks from the \fBapextract\fR
package and parameters from the \fBsparams\fR parameters.  As a quick
summary, the dispersion direction of the spectra are determined from the
package \fBdispaxis\fR parameter if not defined in the image header.  The default
line or column for finding the object position on the slit and the number
of image lines or columns to sum are set by the \f(CWline\fR and \f(CWnsum\fR
parameters.  A line of INDEF (the default) selects the middle of the image.
The automatic finding algorithm is described for the task
\fBapfind\fR and is basically finds the strongest peak.  The default
aperture size, background parameters, and resizing are described in
the tasks \fBapdefault\fR and \fBapresize\fR and the
parameters used are also described there.
The tracing is done as described in \fBaptrace\fR and consists of
stepping along the image using the specified \f(CWt_step\fR parameter.  The
function fitting uses the \fBicfit\fR commands with the other parameters
from the tracing section.
.NH 2
Extraction
.LP
The actual extraction of the spectra is done by summing across the
fixed width apertures at each point along the dispersion.
The default is to simply sum the pixels using
partial pixels at the ends.  There is an option to weight the
sum based on a Poisson variance model using the \f(CWreadnoise\fR and
\f(CWgain\fR detector parameters.  Note that if the \f(CWclean\fR
option is selected the variance weighted extraction is used regardless
of the \f(CWweights\fR parameter.  The sigma thresholds for cleaning
are also set in the \fBsparams\fR parameters.
.LP
The cleaning and variance weighting options require knowing the effective
(i.e. accounting for any image combining) read out noise and gain.  These
numbers need to be adjusted if the image has been processed such that the
intensity scale has a different origin (such as applying a separate
background subtraction operation) or scaling (such as caused by
unnormalized flat fielding).  These options also require using background
subtraction if the profile does not go to zero.  For optimal extraction and
cleaning to work it is recommended that any flat fielding be done using
normalized flat fields (as is done in \fBccdproc\fR) and using background
subtraction if there is any appreciable sky.  For further discussion of
cleaning and variance weighted extraction see \fBapvariance\fR and
\fBapprofiles\fR as well as  \fBapsum\fR.
.LP
Background sky subtraction is done during the extraction based on
background regions and parameters defined by the default parameters or
changed during the interactive setting of the apertures.  The background
subtraction options are to do no background subtraction, subtract the
average, median, or minimum of the pixels in the background regions, or to
fit a function and subtract the function from under the extracted object
pixels.  The background regions are specified in pixels from
the aperture center and follow changes in center of the spectrum along the
dispersion.  The syntax is colon separated ranges with multiple ranges
separated by a comma or space.  The background fitting uses the \fBicfit\fR
routines which include medians, iterative rejection of deviant points, and
a choice of function types and orders.  Note that it is important to use a
method which rejects cosmic rays such as using either medians over all the
background regions (\f(CWbackground\fR = "median") or median samples during
fitting (\f(CWb_naverage\fR < -1).  The background subtraction algorithm and
options are described in greater detail in \fBapsum\fR and
\fBapbackground\fR.
.NH 2
Dispersion Correction
.LP
If dispersion correction is not selected, \f(CWdispcor\fR=no, then the object
spectra are simply extracted.  The extracted spectra may be plotted
by setting the \f(CWsplot\fR option.  This produces a query and uses
the interactive \fBsplot\fR task in non-quicklook mode and uses the
noninteractive \fBbplot\fR task in quicklook mode.
.LP
Dispersion corrections are applied to the extracted spectra if the
\f(CWdispcor\fR processing parameter is set.  There are three basic steps
involved; determining the dispersion functions relating pixel position to
wavelength, assigning the appropriate dispersion function to a particular
observation, and either storing the nonlinear dispersion function in the
image headers or resampling the spectra to evenly spaced pixels in
wavelength.
.LP
The first arc spectrum in the arc list is used to define the reference
dispersion solution.  It is extracted at middle of the image with no
tracing.  Note extractions of arc spectra are not background subtracted.
The task \fBautoidentify\fR is attempts to define the dispersion function
automatically using the \fIcrval\fR and \fIcdelt\fR parameters.  Whether or
not it is successful the user is presented with the interactive
identification graph.  The automatic identifications can be reviewed and a
new solution or corrections to the automatic solution may be performed.
.LP
The arc dispersion function parameters are for \fBautoidentify\fR and it's
related partner \fBreidentify\fR.  The parameters define a line list for
use in automatically assigning wavelengths to arc lines, a centering width
(which should match the line widths at the base of the lines), the
dispersion function type and orders, parameters to exclude bad lines from
function fits, and defining whether to refit the dispersion function as
opposed to simply determining a zero point shift.  The defaults should
generally be adequate and the dispersion function fitting parameters may be
altered interactively.  One should consult the help for the two tasks for
additional details of these parameters and the interactive operation of
\fBautoidentify\fR.
.LP
The extracted reference arc spectrum is then dispersion corrected.
If the spectra are to be linearized, as set by the \f(CWlinearize\fR
parameter, the default linear wavelength parameters are printed and
you have the option to adjust them.  The dispersion system defined at
this point will be applied automatically to all other spectra as they
are dispersion corrected.
.LP
Once the reference dispersion function is defined other arc spectra are
extracted as required by the object spectra.  The assignment of arcs is
done either explicitly with an arc assignment table (parameter
\f(CWarctable\fR) or based on a header parameter such as a time.
This assignments are made by the task
\fBrefspectra\fR.  When two arcs are assigned to an object spectrum an
interpolation is done between the two dispersion functions.  This makes an
approximate correction for steady drifts in the dispersion.
.LP
The tasks \fBsetjd\fR and \fBsetairmass\fR are automatically run on all
spectra.  This computes and adds the header parameters for the Julian date
(JD), the local Julian day number (LJD), the universal time (UTMIDDLE), and
the air mass at the middle of the exposure.  The default arc assignment is
to use the Julian date grouped by the local Julian day number.  The
grouping allows multiple nights of data to be correctly assigned at the
same time.
.LP
The assigned arc spectra are then extracted using the object aperture
definitions (but without background subtraction or cleaning) so that the
same pixels on the detector are used.  The extracted arc spectra are then
reidentified automatically against the reference arc spectrum.  Some
statistics of the reidentification are printed (if not in batch mode) and
the user has the option of examining the lines and fits interactively if
not in quicklook mode.  The task which does the reidentification is called
\fBreidentify\fR.
.LP
The last step of dispersion correction is setting the dispersion
of the object image from the arc images.  There are two choices here.
If the \f(CWlinearize\fR parameter is not set the nonlinear dispersion
function is stored in the image header.  Other IRAF tasks interpret
this information when dispersion coordinates are needed for plotting
or analysis.  This has the advantage of not requiring the spectra
to be interpolated and the disadvantage that the dispersion
information is only understood by IRAF tasks and cannot be readily
exported to other analysis software.
.LP
If the \f(CWlinearize\fR parameter is set then the spectra are resampled to a
linear dispersion relation either in wavelength or the log of the
wavelength using the dispersion coordinate system defined previously
for the arc reference spectrum.
.LP
The linearization algorithm parameters allow selecting the interpolation
function type, whether to conserve flux per pixel by integrating across the
extent of the final pixel, and whether to linearize to equal linear or
logarithmic intervals.  The latter may be appropriate for radial velocity
studies.  The default is to use a fifth order polynomial for interpolation,
to conserve flux, and to not use logarithmic wavelength bins.  These
parameters are described fully in the help for the task \fBdispcor\fR which
performs the correction.
.NH 2
Flux Calibration
.LP
Flux calibration consists of an extinction correction and an instrumental
sensitivity calibration.  The extinction correction only depends on the
extinction function defined by the package parameter \f(CWextinct\fR and
determination of the airmass from the header parameters (the air mass is
computed by \fBsetairmass\fR as mentioned earlier).  The sensitivity
calibration depends on a sensitivity calibration spectrum determined from
standard star observations for which there are tabulated absolute fluxes.
The task that applies both the extinction correction and sensitivity
calibration to each extracted object spectrum is \fBcalibrate\fR.  Consult
the manual page for this task for more information.
.LP
Generation of the sensitivity calibration spectrum is done before
processing any object spectra since it has two interactive steps and
requires all the standard star observations.  The first step is tabulating
the observed fluxes over the same bandpasses as the calibrated absolute
fluxes.  The standard star tabulations are done after each standard star is
extracted and dispersion corrected.  You are asked for the name of the
standard star as tabulated in the absolute flux data files in the directory
\f(CWcaldir\fR defined by the package parameters.
The tabulation of the standard star
observations over the standard bandpasses is done by the task
\fBstandard\fR.  The tabulated data is stored in the file \f(CWstd\fR.  Note
that if the \f(CWredo\fR flag is not set any new standard stars specified in
subsequent executions of \fBdoslit\fR are added to the previous data in
the data file, otherwise the file is first deleted.  Modification of the
tabulated standard star data, such as by adding new stars, will cause any
spectra in the input list which have been previously calibrated to be
reprocessed if the \f(CWupdate\fR flag is set.
.LP
After the standard star calibration bandpass fluxes are tabulated the
information from all the standard stars is combined to produce a
sensitivity function for use by \fBcalibrate\fR.  The sensitivity function
determination is interactive and uses the task \fBsensfunc\fR.  This task
allows fitting a smooth sensitivity function to the ratio of the observed
to calibrated fluxes verses wavelength.  The types of manipulations one
needs to do include deleting bad observations, possibly removing variable
extinction (for poor data), and possibly deriving a revised extinction
function.  This is a complex operation and one should consult the manual
page for \fBsensfunc\fR.  The sensitivity function is saved as a one
dimensional spectrum with the name \f(CWsens\fR.  Deletion of this image
will also cause reprocessing to occur if the \f(CWupdate\fR flag is set.
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
DOSLIT Task
.IP
P. Massey, \fIUser's Guide to Slit Spectra Reductions\fR,
Central Computer Services, NOAO, 1992.
.NH 2
Task Help References
.LP
Each task in the \fBspecred\fR packages and tasks used by \fBdoslit\fR have
help pages describing the parameters and task in some detail.  To get
on-line help type
.V1

cl> help \fItaskname\fR

.V2
The output of this command can be piped to \fBlprint\fR to make a printed
copy.

.V1
       apall - Extract 1D spectra (all parameters in one task)
   apdefault - Set the default aperture parameters and apidtable
      apedit - Edit apertures interactively
      apfind - Automatically find spectra and define apertures
       apfit - Fit 2D spectra and output the fit, difference, or ratio
   apflatten - Remove overall spectral and profile shapes from flat fields
      apmask - Create and IRAF pixel list mask of the apertures
 apnormalize - Normalize 2D apertures by 1D functions
  aprecenter - Recenter apertures
    apresize - Resize apertures
   apscatter - Fit and subtract scattered light
       apsum - Extract 1D spectra
     aptrace - Trace positions of spectra
 
autoidentify - Automatically identify arc lines and a dispersion function
       bplot - Batch plot of spectra with SPLOT
   calibrate - Extinction and flux calibrate spectra
   continuum - Fit the continuum in spectra
    deredden - Apply interstellar extinction correction
     dispcor - Dispersion correct spectra
      dopcor - Doppler correct spectra
    fitprofs - Fit gaussian profiles
    identify - Identify features in spectrum for dispersion solution
    msresp1d - Create 1D response spectra from flat field and sky spectra
  refspectra - Assign wavelength reference spectra to other spectra
  reidentify - Automatically reidentify features in spectra
  sapertures - Set or change aperture header information
      sarith - Spectrum arithmetic
    scombine - Combine spectra
       scopy - Select and copy apertures in different spectral formats
    sensfunc - Compute instrumental sensitivity from standard stars
  setairmass - Compute effective airmass and middle UT for an exposure
       setjd - Compute and set Julian dates in images
        sfit - Fit spectra and output fit, ratio, or difference
      skysub - Sky subtract extracted multispec spectra
       slist - List spectrum header parameters
    specplot - Scale, stack, and plot multiple spectra
       splot - Preliminary spectral plot/analysis
    standard - Tabulate standard star counts and fluxes
 
      doslit - Process slit spectra
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
Appendix A: DOSLIT Parameters
.LP
.nr PS 8
.nr VS 10
objects
.LS
List of object images to be processed.  Previously processed spectra are
ignored unless the \f(CWredo\fR flag is set or the \f(CWupdate\fR flag is set
and dependent calibration data has changed.  If the images contain the
keyword IMAGETYP then only those with a value of "object" or "OBJECT"
are used and those with a value of "comp" or "COMPARISON" are added
to the list of arcs.  Extracted spectra are ignored.
.LE
arcs = "" (at least one if dispersion correcting)
.LS
List of arc calibration spectra.  These spectra are used to define
the dispersion functions.  The first spectrum is used to mark lines
and set the dispersion function interactively and dispersion functions
for all other arc spectra are derived from it.  If the images contain
the keyword IMAGETYP then only those with a value of "comp" or
"COMPARISON" are used.  All others are ignored as are extracted spectra.
.LE
arctable = "" (optional) (refspectra)
.LS
Table defining which arc spectra are to be assigned to which object
spectra (see \fBrefspectra\fR).  If not specified an assignment based
on a header parameter, \f(CWsparams.sort\fR, such as the Julian date
is made.
.LE
standards = "" (at least one if flux calibrating)
.LS
List of standard star spectra.  The standard stars must have entries in
the calibration database (package parameter \f(CWcaldir\fR).
.LE

readnoise = "rdnoise", gain = "gain" (apsum)
.LS
Read out noise in photons and detector gain in photons per data value.
This parameter defines the minimum noise sigma and the conversion between
photon Poisson statistics and the data number statistics.  Image header
keywords (case insensitive) may be specified to obtain the values from the
image header.
.LE
datamax = INDEF (apsum.saturation)
.LS
The maximum data value which is not a cosmic ray.
When cleaning cosmic rays and/or using variance weighted extraction
very strong cosmic rays (pixel values much larger than the data) can
cause these operations to behave poorly.  If a value other than INDEF
is specified then all data pixels in excess of this value will be
excluded and the algorithms will yield improved results.
This applies only to the object spectra and not the standard star or
arc spectra.  For more
on this see the discussion of the saturation parameter in the
\fBapextract\fR package.
.LE
width = 5. (apedit)
.LS
Approximate full width of the spectrum profiles.  This parameter is used
to define a width and error radius for the profile centering algorithm.
.LE
crval = INDEF, cdelt = INDEF (autoidentify)
.LS
These parameters specify an approximate central wavelength and dispersion.
They may be specified as numerical values, INDEF, or image header keyword
names whose values are to be used.
If both these parameters are INDEF then the automatic identification will
not be done.
.LE

dispcor = yes
.LS
Dispersion correct spectra?  This may involve either defining a nonlinear
dispersion coordinate system in the image header or resampling the
spectra to uniform linear wavelength coordinates as selected by
the parameter \f(CWsparams.linearize\fR.
.LE
extcor = no
.LS
Extinction correct the spectra?
.LE
fluxcal = no
.LS
Flux calibrate the spectra using standard star observations?
.LE
resize = no (apresize)
.LS
Resize the default aperture for each object based on the spectrum profile?
.LE
clean = no (apsum)
.LS
Detect and correct for bad pixels during extraction?  This is the same
as the clean option in the \fBapextract\fR package.  If yes this also
implies variance weighted extraction.  In addition the datamax parameters
can be useful.
.LE
splot = no
.LS
Plot the final spectra with the task \fBsplot\fR?  In quicklook mode
this is automatic and in non-quicklook mode it is queried.
.LE
redo = no
.LS
Redo operations previously done?  If no then previously processed spectra
in the object list will not be processed unless required by the
update option.
.LE
update = no
.LS
Update processing of previously processed spectra if the
dispersion reference image or standard star calibration data are changed?
.LE
quicklook = no
.LS
Extract and calibrate spectra with minimal interaction?  In quicklook mode
only the initial dispersion function solution and standard star setup are
done interactively.  Normally the \f(CWsplot\fR option is set in this mode to
produce an automatic final spectrum plot for each object.  It is
recommended that this mode not be used for final reductions.
.LE
batch = yes
.LS
Process spectra as a background or batch job provided there are no interactive
steps remaining.
.LE
listonly = no
.LS
List processing steps but don't process?
.LE

sparams = "" (pset)
.LS
Name of parameter set containing additional processing parameters.  This
parameter is only for indicating the link to the parameter set
\fBsparams\fR and should not be given a value.  The parameter set may be
examined and modified in the usual ways (typically with "eparam sparams"
or ":e sparams" from the parameter editor).  The parameters are
described below.
.LE

.ce
-- GENERAL PARAMETERS --

line = INDEF, nsum = 10
.LS
The dispersion line (line or column perpendicular to the dispersion
axis) and number of adjacent lines (half before and half after unless
at the end of the image) used in finding, resizing,
editing, and tracing operations.  A line of INDEF selects the middle of the
image along the dispersion axis.
.LE
extras = no (apsum)
.LS
Include raw unweighted and uncleaned spectra, the background spectra, and
the estimated sigmas in a three dimensional output image format.
See the discussion in the \fBapextract\fR package for further information.
.LE

.ce
-- DEFAULT APERTURE LIMITS --

lower = -3., upper = 3. (apdefault)
.LS
Default lower and upper aperture limits relative to the aperture center.
These limits are used when the apertures are first defined.
.LE

.ce
-- AUTOMATIC APERTURE RESIZING PARAMETERS --

ylevel = 0.05 (apresize)
.LS
Fraction of the peak to set aperture limits during automatic resizing.
.LE

.ce
-- TRACE PARAMETERS --

t_step = 10 (aptrace)
.LS
Step along the dispersion axis between determination of the spectrum
positions.  Note the \f(CWnsum\fR parameter is also used to enhance the
signal-to-noise at each step.
.LE
t_function = "spline3", t_order = 1 (aptrace)
.LS
Default trace fitting function and order.  The fitting function types are
"chebyshev" polynomial, "legendre" polynomial, "spline1" linear spline, and
"spline3" cubic spline.  The order refers to the number of terms in the
polynomial functions or the number of spline pieces in the spline
functions.
.LE
t_niterate = 1, t_low = 3., t_high = 3. (aptrace)
.LS
Default number of rejection iterations and rejection sigma thresholds.
.LE

.ce
-- APERTURE EXTRACTION PARAMETERS --

weights = "none" (apsum) (none|variance)
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
pfit = "fit1d" (apsum and approfile) (fit1d|fit2d)
.LS
Type of profile fitting algorithm to use.  The "fit1d" algorithm is
preferred except in cases of extreme tilt.
.LE
lsigma = 3., usigma = 3. (apsum)
.LS
Lower and upper rejection thresholds, given as a number of times the
estimated sigma of a pixel, for cleaning.
.LE

.ce
-- DEFAULT BACKGROUND PARAMETERS --

background = "fit" (apsum) (none|average|median|minimum|fit)
.LS
Type of background subtraction.  The choices are "none" for no background
subtraction, "average" to average the background within the background
regions, "median" to use the median in the background regions, "minimum" to
use the minimum in the background regions, or "fit" to fit across the
dispersion using the background within the background regions.  Note that
the "average" option does not do any medianing or bad pixel checking,
something which is recommended.  The fitting option is slower than the
other options and requires additional fitting parameter.
.LE
b_function = "legendre", b_order = 1 (apsum)
.LS
Default background fitting function and order.  The fitting function types are
"chebyshev" polynomial, "legendre" polynomial, "spline1" linear spline, and
"spline3" cubic spline.  The order refers to the number of
terms in the polynomial functions or the number of spline pieces in the spline
functions.
.LE
b_sample = "-10:-6,6:10" (apsum)
.LS
Default background sample.  The sample is given by a set of colon separated
ranges each separated by either whitespace or commas.  The string "*" refers
to all points.  Note that the background coordinates are relative to the
aperture center and not image pixel coordinates so the endpoints need not
be integer.  It is recommended that the background regions be examined
and set interactively with the 'b' key in the interactive aperture
definition mode.  This requires \f(CWquicklook\fR to be no.
.LE
b_naverage = -100 (apsum)
.LS
Default number of points to average or median.  Positive numbers
average that number of sequential points to form a fitting point.
Negative numbers median that number, in absolute value, of sequential
points.  A value of 1 does no averaging and each data point is used in the
fit.
.LE
b_niterate = 1 (apsum)
.LS
Default number of rejection iterations.  If greater than zero the fit is
used to detect deviant fitting points and reject them before repeating the
fit.  The number of iterations of this process is given by this parameter.
.LE
b_low_reject = 3., b_high_reject = 3. (apsum)
.LS
Default background lower and upper rejection sigmas.  If greater than zero
points deviating from the fit below and above the fit by more than this
number of times the sigma of the residuals are rejected before refitting.
.LE

.ce
-- ARC DISPERSION FUNCTION PARAMETERS --

threshold = 10. (autoidentify/reidentify)
.LS
In order for a feature center to be determined the range of pixel intensities
around the feature must exceed this threshold.
.LE
coordlist = "linelists$idhenear.dat" (autoidentify)
.LS
Arc line list consisting of an ordered list of wavelengths.
Some standard line lists are available in the directory "linelists$".
.LE
match = -3. (autoidentify)
.LS
The maximum difference for a match between the dispersion function computed
value and a wavelength in the coordinate list.
.LE
fwidth = 4. (autoidentify)
.LS
Approximate full base width (in pixels) of arc lines.
.LE
cradius = 10. (reidentify)
.LS
Radius from previous position to reidentify arc line.
.LE
i_function = "spline3", i_order = 1 (autoidentify)
.LS
The default function and order to be fit to the arc wavelengths as a
function of the pixel coordinate.  The functions choices are "chebyshev",
"legendre", "spline1", or "spline3".
.LE
i_niterate = 0, i_low = 3.0, i_high = 3.0 (autoidentify)
.LS
Number of rejection iterations and sigma thresholds for rejecting arc
lines from the dispersion function fits.
.LE
refit = yes (reidentify)
.LS
Refit the dispersion function?  If yes and there is more than 1 line
and a dispersion function was defined in the initial arc reference then a new
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
Average two reference spectra without regard to any
sort or group parameters.
If only one reference spectrum is specified then it is assigned with a
warning.  If more than two reference spectra are specified then only the
first two are used and a warning is given.  There is no checking of the
group values.
.LE
following
.LS
Select the nearest following spectrum in the reference list based on the
sort and group parameters.  If there is no following spectrum use the
nearest preceding spectrum.
.LE
interp
.LS
Interpolate between the preceding and following spectra in the reference
list based on the sort and group parameters.  If there is no preceding and
following spectrum use the nearest spectrum.  The interpolation is weighted
by the relative distances of the sorting parameter (see cautions in
DESCRIPTION section).
.LE
match
.LS
Match each input spectrum with the reference spectrum list in order.
This overrides any group values.
.LE
nearest
.LS
Select the nearest spectrum in the reference list based on the sort and
group parameters.
.LE
preceding
.LS
Select the nearest preceding spectrum in the reference list based on the
sort and group parameters.  If there is no preceding spectrum use the
nearest following spectrum.
.LE
.LE
sort = "jd" (setjd and refspectra)
.LS
Image header keyword to be used as the sorting parameter for selection
based on order.  The header parameter must be numeric but otherwise may
be anything.  Common sorting parameters are times or positions.
.LE
group = "ljd" (setjd and refspectra)
.LS
Image header keyword to be used to group spectra.  For those selection
methods which use the group parameter the reference and object
spectra must have identical values for this keyword.  This can
be anything but it must be constant within a group.  Common grouping
parameters are the date of observation "date-obs" (provided it does not
change over a night) or the local Julian day number.
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
spectra will be interpolated to a linear or log linear sampling using
the linear dispersion parameters specified by other parameters.  If
no the nonlinear dispersion function(s) from the dispersion function
database are assigned to the input image world coordinate system
and the spectral data is not interpolated.  Note the interpolation
function type is set by the package parameter \f(CWinterp\fR.
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
-- SENSITIVITY CALIBRATION PARAMETERS --

s_function = "spline3", s_order = 1 (sensfunc)
.LS
Function and order used to fit the sensitivity data.  The function types
are "chebyshev" polynomial, "legendre" polynomial, "spline3" cubic spline,
and "spline1" linear spline.  Order of the sensitivity fitting function.
The value corresponds to the number of polynomial terms or the number of
spline pieces.  The default values may be changed interactively.
.LE
fnu = no (calibrate)
.LS
The default calibration is into units of F-lambda. If \f(CWfnu\fR = yes then
the calibrated spectrum will be in units of F-nu.
.LE

.ce
PACKAGE PARAMETERS

The following package parameters are used by this task.  The default values
may vary depending on the package.

dispaxis = 2
.LS
Default dispersion axis.  The dispersion axis is 1 for dispersion
running along image lines and 2 for dispersion running along image
columns.  If the image header parameter DISPAXIS is defined it has
precedence over this parameter.  The default value defers to the
package parameter of the same name.
.LE
extinction (standard, sensfunc, calibrate)
.LS
Extinction file for a site.  There are two extinction files in the
NOAO standards library, onedstds$, for KPNO and CTIO.  These extinction
files are used for extinction and flux calibration.
.LE
caldir (standard)
.LS
Standard star calibration directory.  A directory containing standard
star data files.  Note that the directory name must end with '/'.
There are a number of standard star calibrations directories in the NOAO
standards library, onedstds$.
.LE
observatory = "observatory" (observatory)
.LS
The default observatory to use for latitude dependent computations.
If the OBSERVAT keyword in the image header it takes precedence over
this parameter.
.LE
interp = "poly5" (nearest|linear|poly3|poly5|spline3|sinc) (dispcor)
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
database = "database"
.LS
Database name used by various tasks.  This is a directory which is created
if necessary.
.LE
verbose = no
.LS
Verbose output?  If set then almost all the information written to the
logfile is also written to the terminal except when the task is a
background or batch process.
.LE
logfile = "logfile"
.LS
If specified detailed text log information is written to this file.
.LE
plotfile = ""
.LS
If specified metacode plots are recorded in this file for later review.
Since plot information can become large this should be used only if
really desired.
.LE

.ce
ENVIRONMENT PARAMETERS
.LP
The environment parameter \fIimtype\fR is used to determine the extension
of the images to be processed and created.  This allows use with any
supported image extension.  For STF images the extension has to be exact;
for example "d1h".
